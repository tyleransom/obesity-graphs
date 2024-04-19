library(tidyverse)
library(magrittr)
library(pdftools)


#-------------------------------------------------------------------------------
# Functions for later use
#-------------------------------------------------------------------------------
# Function like Stata's inrange()
inrange <- function(x, lower, upper) {
    x >= lower & x <= upper
}

# Function like Stata's inlist()
inlist <- function(x, ...) {
    values <- unlist(list(...))
    x %in% values
}

# Function to clean the PDF
pdfcleaner <- function(fp,pg){
    # Extract the PDF
    text  <- pdf_text(fp)[pg]
    
    # Process the extracted text to structure it for a data frame
    lines <- str_split(text, "\n") %>% unlist()
    relevant_lines <- str_subset(lines, "\\.\\.\\.+")
    
    # collect into a tibble
    tibb <- tibble(txt = relevant_lines)
    
    # remove * and # if year >= 2005
    # i.e. if last three characters of filename is 05 or larger
    if (as.numeric(substr(fp, nchar(fp)-5, nchar(fp)-4)) >= 5){
        tibb <- tibb %>%
            mutate(txt = str_remove_all(txt, "\\*|#"))
    }
    
    # insert a space between [0-9] and "(" in 2005 and later
    if (as.numeric(substr(fp, nchar(fp)-5, nchar(fp)-4)) >= 7){
        tibb <- tibb %>%
            mutate(txt = str_replace_all(txt, "([0-9])(\\()", "\\1 \\2"))
    }
    
    # split into columns
    df <- tibb %>%
          separate(txt, into = c("Range", "Values"), 
                   sep = "\\.{3,}\\s+", 
                   extra = "merge", 
                   fill = "right")
    
    # further split each element by spaces
    df <- df %>%
        mutate(SplitSecondary = map(Values, ~ str_split(.x, "\\s+", simplify = FALSE))) %>%
        select(-Values) # Optionally remove intermediate columns
    
    # flatten each list of lists in SplitSecondary into a simple list
    df <- df %>%
        mutate(SplitSecondary = map(SplitSecondary, ~ flatten_chr(.x)))
    
    df_long <- df %>%
        mutate(id = row_number()) %>% # Ensure each row has a unique identifier
        # Convert SplitSecondary into a tibble (or list of tibbles)
        mutate(SplitSecondary = map(SplitSecondary, ~ as_tibble(.x, .name_repair = "universal"))) %>%
        unnest(SplitSecondary) %>%
        group_by(id) %>%
        # Create dynamic column names with numbering reset for each id
        mutate(name = paste0("Value", row_number())) %>%
        ungroup() # Remove the grouping
    
    # Now pivot wider with these adjusted names
    df_wide <- df_long %>%
        pivot_wider(names_from = name, values_from = value, id_cols = c(id, Range))
    
}

# Function to process each dataset
process_dataset <- function(fpp, yr, pg_rg, nwnm) {
    data <- pdfcleaner(fpp, pg_rg[1])
    for (i in pg_rg[-1]) {
        data <- left_join(data, pdfcleaner(fpp, i), by = c("id"))
    }
    
    data %>%
        mutate(year = yr) %>%
        select(-starts_with("Range")) %>%
        mutate(across(everything(), ~str_remove_all(.x, "\\*|#|\\-\\-"))) %>%
        mutate(across(everything(), ~as.numeric(.x))) %>%
        select(where(~ !all(is.na(.x)))) %>%
        set_names(nwnm) %>%
        mutate(gengrp = case_when(
            year<2010 & inrange(id,  1, 10) ~ "Male",
            year<2010 & inrange(id, 11, 20) ~ "Female",
            year<2010 & inrange(id, 21, 21) ~ "Both",
            year>2010 & inrange(id,  1, 12) ~ "Male",
            year>2010 & inrange(id, 13, 24) ~ "Female",
            year>2010 & inrange(id, 25, 27) ~ "Both"
        )) %>%
        mutate(agegrp = case_when(
            year<2010 & inlist(id,  1, 11) ~ "2-5",
            year<2010 & inlist(id,  2, 12) ~ "6-11",
            year<2010 & inlist(id,  3, 13) ~ "12-19",
            year<2010 & inlist(id,  4, 14) ~ "20-29",
            year<2010 & inlist(id,  5, 15) ~ "30-39",
            year<2010 & inlist(id,  6, 16) ~ "40-49",
            year<2010 & inlist(id,  7, 17) ~ "50-59",
            year<2010 & inlist(id,  8, 18) ~ "60-69",
            year<2010 & inlist(id,  9, 19) ~ "70 and over",
            year<2010 & inlist(id, 10, 20) ~ "20 and over",
            year<2010 & inlist(id,     21) ~ "2 and over",
            year>2010 & inlist(id,  1, 13) ~ "2-5",
            year>2010 & inlist(id,  2, 14) ~ "6-11",
            year>2010 & inlist(id,  3, 15) ~ "12-19",
            year>2010 & inlist(id,  4, 16) ~ "20-29",
            year>2010 & inlist(id,  5, 17) ~ "30-39",
            year>2010 & inlist(id,  6, 18) ~ "40-49",
            year>2010 & inlist(id,  7, 19) ~ "50-59",
            year>2010 & inlist(id,  8, 20) ~ "60-69",
            year>2010 & inlist(id,  9, 21) ~ "70 and over",
            year>2010 & inlist(id, 10, 22, 25) ~ "2-19",
            year>2010 & inlist(id, 11, 23, 26) ~ "20 and over",
            year>2010 & inlist(id, 12, 24, 27) ~ "2 and over"
        )) %>%
        mutate(`PFA 18:4` = replace_na(`PFA 18:4`, 0)) %>%
        mutate(omega6 = `PFA 18:2` + `PFA 20:4`,
               omega3 = `PFA 18:3` + `PFA 18:4` + `PFA 20:5` + `PFA 22:5` + `PFA 22:6`) %>%
        select(groupid = id, year, gengrp, agegrp, everything())
}


#-------------------------------------------------------------------------------
# load and clean the food diary data from "What We Eat in America" surveys
#    (source: NHANES, via USDA website; see README.md in raw data folder)
#-------------------------------------------------------------------------------
# Define the details for each dataset in a list
datasets <- list(
    list(fpath = "../../../data/wweia/raw/Table_1_BIA_GEN_01.pdf", 
         year = 2001, 
         page_range = 1:7,
         new_names = c("id", "N", "kcal", "protein", "carbs", "sugars", "fiber", "fat", 
                       "sat fat", "mono unsat fat", "poly unsat fat", "cholesterol",
                       "vit E", "retinol", "vit A", "alpha-carotene", "beta-carotene", 
                       "beta-cryptoxanthin", "lycopene", "lutein_zeaxanthin", "thiamin",
                       "riboflavin", "niacin", "vit B6", "folic acid", "food folate", 
                       "folate", "vit B12", "vit C", "vit K", "calcium", "phosphorus", 
                       "magnesium", "iron", "zinc", "copper", "sodium", "potassium", 
                       "selenium", "SFA 4:0", "SFA 6:0", "SFA 8:0", "SFA 10:0", 
                       "SFA 12:0", "SFA 14:0", "SFA 16:0", "SFA 18:0", "MFA 16:1", 
                       "MFA 18:1", "MFA 20:1", "MFA 22:1", "PFA 18:2", "PFA 18:3", 
                       "PFA 18:4", "PFA 20:4", "PFA 20:5", "PFA 22:5", "PFA 22:6", 
                       "caffeine", "theobromine", "alcohol", "year")),
    list(fpath = "../../../data/wweia/raw/Table_1_NIF_GEN_03.pdf", 
         year = 2003, 
         page_range = 1:7,
         new_names = c("id", "N", "kcal", "protein", "carbs", "sugars", "fiber", "fat", 
                       "sat fat", "mono unsat fat", "poly unsat fat", "cholesterol",
                       "vit E", "added vit E", "retinol", "vit A", "alpha-carotene", "beta-carotene", 
                       "beta-cryptoxanthin", "lycopene", "lutein_zeaxanthin", "thiamin",
                       "riboflavin", "niacin", "vit B6", "folic acid", "food folate", 
                       "folate", "vit B12", "added vit B12", "vit C", "vit K", "calcium", "phosphorus", 
                       "magnesium", "iron", "zinc", "copper", "sodium", "potassium", 
                       "selenium", "SFA 4:0", "SFA 6:0", "SFA 8:0", "SFA 10:0", 
                       "SFA 12:0", "SFA 14:0", "SFA 16:0", "SFA 18:0", "MFA 16:1", 
                       "MFA 18:1", "MFA 20:1", "MFA 22:1", "PFA 18:2", "PFA 18:3", 
                       "PFA 18:4", "PFA 20:4", "PFA 20:5", "PFA 22:5", "PFA 22:6", 
                       "caffeine", "theobromine", "alcohol", "year")),
    list(fpath = "../../../data/wweia/raw/Table_1_NIN_GEN_05.pdf", 
         year = 2005, 
         page_range = 1:8,
         new_names = c("id", "N", "kcal", "protein", "carbs", "sugars", "fiber", "fat", 
                       "sat fat", "mono unsat fat", "poly unsat fat", "cholesterol",
                       "retinol", "vit A", "alpha-carotene", "beta-carotene", 
                       "beta-cryptoxanthin", "lycopene", "lutein_zeaxanthin", "thiamin",
                       "riboflavin", "niacin", "vit B6", "folic acid", "food folate", 
                       "folate", "choline", "vit B12", "added vit B12", "vit C", 
                       "vit E", "added vit E", "vit K", "calcium", "phosphorus", 
                       "magnesium", "iron", "zinc", "copper", "selenium", "potassium", "sodium", 
                       "caffeine", "theobromine", "alcohol", 
                       "SFA 4:0", "SFA 6:0", "SFA 8:0", "SFA 10:0", 
                       "SFA 12:0", "SFA 14:0", "SFA 16:0", "SFA 18:0", "MFA 16:1", 
                       "MFA 18:1", "MFA 20:1", "MFA 22:1", "PFA 18:2", "PFA 18:3", 
                       "PFA 18:4", "PFA 20:4", "PFA 20:5", "PFA 22:5", "PFA 22:6", "year")),
    list(fpath = "../../../data/wweia/raw/Table_1_NIN_GEN_07.pdf", 
         year = 2007, 
         page_range = 1:8,
         new_names = c("id", "N", "kcal", "protein", "carbs", "sugars", "fiber", "fat", 
                       "sat fat", "mono unsat fat", "poly unsat fat", "cholesterol",
                       "retinol", "vit A", "alpha-carotene", "beta-carotene", 
                       "beta-cryptoxanthin", "lycopene", "lutein_zeaxanthin", "thiamin",
                       "riboflavin", "niacin", "vit B6", "folic acid", "food folate", 
                       "folate", "choline", "vit B12", "added vit B12", "vit C", "vit D",
                       "vit E", "added vit E", "vit K", "calcium", "phosphorus", 
                       "magnesium", "iron", "zinc", "copper", "selenium", "potassium", "sodium", 
                       "caffeine", "theobromine", "alcohol", 
                       "SFA 4:0", "SFA 6:0", "SFA 8:0", "SFA 10:0", 
                       "SFA 12:0", "SFA 14:0", "SFA 16:0", "SFA 18:0", "MFA 16:1", 
                       "MFA 18:1", "MFA 20:1", "MFA 22:1", "PFA 18:2", "PFA 18:3", 
                       "PFA 18:4", "PFA 20:4", "PFA 20:5", "PFA 22:5", "PFA 22:6", "year")),
    list(fpath = "../../../data/wweia/raw/Table_1_NIN_GEN_09.pdf", 
         year = 2009, 
         page_range = 1:8,
         new_names = c("id", "N", "kcal", "protein", "carbs", "sugars", "fiber", "fat", 
                       "sat fat", "mono unsat fat", "poly unsat fat", "cholesterol",
                       "retinol", "vit A", "alpha-carotene", "beta-carotene", 
                       "beta-cryptoxanthin", "lycopene", "lutein_zeaxanthin", "thiamin",
                       "riboflavin", "niacin", "vit B6", "folic acid", "food folate", 
                       "folate", "choline", "vit B12", "added vit B12", "vit C", "vit D",
                       "vit E", "added vit E", "vit K", "calcium", "phosphorus", 
                       "magnesium", "iron", "zinc", "copper", "selenium", "potassium", "sodium", 
                       "caffeine", "theobromine", "alcohol", 
                       "SFA 4:0", "SFA 6:0", "SFA 8:0", "SFA 10:0", 
                       "SFA 12:0", "SFA 14:0", "SFA 16:0", "SFA 18:0", "MFA 16:1", 
                       "MFA 18:1", "MFA 20:1", "MFA 22:1", "PFA 18:2", "PFA 18:3", 
                       "PFA 18:4", "PFA 20:4", "PFA 20:5", "PFA 22:5", "PFA 22:6", "year")),
    list(fpath = "../../../data/wweia/raw/Table_1_NIN_GEN_11.pdf", 
         year = 2011, 
         page_range = 1:8,
         new_names = c("id", "N", "kcal", "protein", "carbs", "sugars", "fiber", "fat", 
                       "sat fat", "mono unsat fat", "poly unsat fat", "cholesterol",
                       "retinol", "vit A", "alpha-carotene", "beta-carotene", 
                       "beta-cryptoxanthin", "lycopene", "lutein_zeaxanthin", "thiamin",
                       "riboflavin", "niacin", "vit B6", "folic acid", "food folate", 
                       "folate", "choline", "vit B12", "added vit B12", "vit C", "vit D",
                       "vit E", "added vit E", "vit K", "calcium", "phosphorus", 
                       "magnesium", "iron", "zinc", "copper", "selenium", "potassium", "sodium", 
                       "caffeine", "theobromine", "alcohol", 
                       "SFA 4:0", "SFA 6:0", "SFA 8:0", "SFA 10:0", 
                       "SFA 12:0", "SFA 14:0", "SFA 16:0", "SFA 18:0", "MFA 16:1", 
                       "MFA 18:1", "MFA 20:1", "MFA 22:1", "PFA 18:2", "PFA 18:3", 
                       "PFA 18:4", "PFA 20:4", "PFA 20:5", "PFA 22:5", "PFA 22:6", "year")),
    list(fpath = "../../../data/wweia/raw/Table_1_NIN_GEN_13.pdf", 
         year = 2013, 
         page_range = 1:8,
         new_names = c("id", "N", "kcal", "protein", "carbs", "sugars", "fiber", "fat", 
                       "sat fat", "mono unsat fat", "poly unsat fat", "cholesterol",
                       "retinol", "vit A", "alpha-carotene", "beta-carotene", 
                       "beta-cryptoxanthin", "lycopene", "lutein_zeaxanthin", "thiamin",
                       "riboflavin", "niacin", "vit B6", "folic acid", "food folate", 
                       "folate", "choline", "vit B12", "added vit B12", "vit C", "vit D",
                       "vit E", "added vit E", "vit K", "calcium", "phosphorus", 
                       "magnesium", "iron", "zinc", "copper", "selenium", "potassium", "sodium", 
                       "caffeine", "theobromine", "alcohol", 
                       "SFA 4:0", "SFA 6:0", "SFA 8:0", "SFA 10:0", 
                       "SFA 12:0", "SFA 14:0", "SFA 16:0", "SFA 18:0", "MFA 16:1", 
                       "MFA 18:1", "MFA 20:1", "MFA 22:1", "PFA 18:2", "PFA 18:3", 
                       "PFA 18:4", "PFA 20:4", "PFA 20:5", "PFA 22:5", "PFA 22:6", "year")),
    list(fpath = "../../../data/wweia/raw/Table_1_NIN_GEN_15.pdf", 
         year = 2015, 
         page_range = 1:8,
         new_names = c("id", "N", "kcal", "protein", "carbs", "sugars", "fiber", "fat", 
                       "sat fat", "mono unsat fat", "poly unsat fat", "cholesterol",
                       "retinol", "vit A", "alpha-carotene", "beta-carotene", 
                       "beta-cryptoxanthin", "lycopene", "lutein_zeaxanthin", "thiamin",
                       "riboflavin", "niacin", "vit B6", "folic acid", "food folate", 
                       "folate", "choline", "vit B12", "added vit B12", "vit C", "vit D",
                       "vit E", "added vit E", "vit K", "calcium", "phosphorus", 
                       "magnesium", "iron", "zinc", "copper", "selenium", "potassium", "sodium", 
                       "caffeine", "theobromine", "alcohol", 
                       "SFA 4:0", "SFA 6:0", "SFA 8:0", "SFA 10:0", 
                       "SFA 12:0", "SFA 14:0", "SFA 16:0", "SFA 18:0", "MFA 16:1", 
                       "MFA 18:1", "MFA 20:1", "MFA 22:1", "PFA 18:2", "PFA 18:3", 
                       "PFA 18:4", "PFA 20:4", "PFA 20:5", "PFA 22:5", "PFA 22:6", "year")),
    list(fpath = "../../../data/wweia/raw/Table_1_NIN_GEN_17.pdf", 
         year = 2017, 
         page_range = 1:8,
         new_names = c("id", "N", "kcal", "protein", "carbs", "sugars", "fiber", "fat", 
                       "sat fat", "mono unsat fat", "poly unsat fat", "cholesterol",
                       "retinol", "vit A", "alpha-carotene", "beta-carotene", 
                       "beta-cryptoxanthin", "lycopene", "lutein_zeaxanthin", "thiamin",
                       "riboflavin", "niacin", "vit B6", "folic acid", "food folate", 
                       "folate", "choline", "vit B12", "added vit B12", "vit C", "vit D",
                       "vit E", "added vit E", "vit K", "calcium", "phosphorus", 
                       "magnesium", "iron", "zinc", "copper", "selenium", "potassium", "sodium", 
                       "caffeine", "theobromine", "alcohol", 
                       "SFA 4:0", "SFA 6:0", "SFA 8:0", "SFA 10:0", 
                       "SFA 12:0", "SFA 14:0", "SFA 16:0", "SFA 18:0", "MFA 16:1", 
                       "MFA 18:1", "MFA 20:1", "MFA 22:1", "PFA 18:2", "PFA 18:3", 
                       "PFA 18:4", "PFA 20:4", "PFA 20:5", "PFA 22:5", "PFA 22:6", "year")),
    list(fpath = "../../../data/wweia/raw/Table_1_NIN_GEN_1720.pdf", 
         year = 2019, 
         page_range = 1:8,
         new_names = c("id", "N", "kcal", "protein", "carbs", "sugars", "fiber", "fat", 
                       "sat fat", "mono unsat fat", "poly unsat fat", "cholesterol",
                       "retinol", "vit A", "alpha-carotene", "beta-carotene", 
                       "beta-cryptoxanthin", "lycopene", "lutein_zeaxanthin", "thiamin",
                       "riboflavin", "niacin", "vit B6", "folic acid", "food folate", 
                       "folate", "choline", "vit B12", "added vit B12", "vit C", "vit D",
                       "vit E", "added vit E", "vit K", "calcium", "phosphorus", 
                       "magnesium", "iron", "zinc", "copper", "selenium", "potassium", "sodium", 
                       "caffeine", "theobromine", "alcohol", 
                       "SFA 4:0", "SFA 6:0", "SFA 8:0", "SFA 10:0", 
                       "SFA 12:0", "SFA 14:0", "SFA 16:0", "SFA 18:0", "MFA 16:1", 
                       "MFA 18:1", "MFA 20:1", "MFA 22:1", "PFA 18:2", "PFA 18:3", 
                       "PFA 18:4", "PFA 20:4", "PFA 20:5", "PFA 22:5", "PFA 22:6", "year"))
)

# Process each dataset using map_dfr to combine them into one dataframe
dc <- map_dfr(datasets, ~process_dataset(.x$fpath, .x$year, .x$page_range, .x$new_names))


#-------------------------------------------------------------------------------
# save as cleaned csv
#-------------------------------------------------------------------------------
fpath <- "../../../data/wweia/cleaned/wweia.csv"
write_csv(dc, file = fpath)
