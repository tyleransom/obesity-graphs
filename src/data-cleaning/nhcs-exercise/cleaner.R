library(tidyverse)
library(magrittr)
library(pdftools)


#-------------------------------------------------------------------------------
# load and clean exercise data from NCHS (via CDC website)
#-------------------------------------------------------------------------------

# Table 25. Participation in leisure-time aerobic and muscle-strengthening 
# activities that meet the federal 2008 Physical Activity Guidelines for 
# Americans among adults aged 18 and over, by selected characteristics: 
# United States, selected years 1998-2018
fpath <- "../../../data/nhcs-exercise/raw/025-508.pdf"
# Extract p. 1 of the PDF
text  <- pdf_text(fpath)[1]

# Process the extracted text to structure it for a data frame
lines <- str_split(text, "\n") %>% unlist()
relevant_lines <- str_subset(lines, "\\. \\. \\.+")

# Use " . ." as a delimiter to split the lines into columns
data_list <- map(relevant_lines, ~str_split(.x, " \\. .", simplify = FALSE))

# Clean the data by keeping only the first and last non-empty elements
data_list_cleaned <- map(data_list, ~{
    vec <- .x[[1]]
    non_empty_elements <- vec[vec != ""]
    if (length(non_empty_elements) > 1) {
        list(c(non_empty_elements[1], non_empty_elements[length(non_empty_elements)]))
    } else {
        list(non_empty_elements)
    }
})

# Format the cleaned data into single-row list elements with 11 columns each
data_list_formatted <- map(data_list_cleaned, ~{
    item <- .x[[1]]
    combined_string <- str_c(str_replace_all(item[1], " ", "_"), item[2], sep = " ")
    elements <- str_split(combined_string, "\\s+") %>% unlist()
    desired_length <- 11
    
    if (length(elements) < desired_length) {
        elements <- c(elements, rep(NA, desired_length - length(elements)))
    }
    
    elements[1:desired_length]
})

# Define column names for the data frame
column_names <- c("Group", "g1998", "g2000", "g2010", "g2017", "g2018", "b1998", "b2000", "b2010", "b2017", "b2018")

# Convert the list of vectors to a list of 1-row tibbles with named columns
list_of_tibbles <- map(data_list_formatted, ~{
    # Convert the vector to a tibble (1 row, multiple columns)
    .x %>% t(.) %>% as_tibble(., .name_repair = "minimal") %>% set_names(column_names)
})

dfe <- bind_rows(list_of_tibbles)

dfe$Group <- c("All",
               "Age 18+, raw ",
               "Age 18-44",
               "Age 18-24",
               "Age 25-44",
               "Age 45-64",
               "Age 45-54",
               "Age 55-64",
               "Age 65+",
               "Age 65-74",
               "Age 75+",
               "Male",
               "Female",
               "Male, Age 18-44",
               "Male, Age 45-54",
               "Male, Age 55-64",
               "Male, Age 65-74",
               "Male, Age 75+",
               "Female, Age 18-44",
               "Female, Age 45-54",
               "Female, Age 55-64",
               "Female, Age 65-74",
               "Female, Age 75+",
               "White",
               "Black",
               "American Indian",
               "Asian",
               "Hawaiian/Pacific Islander",
               "2+ races",
               "Hispanic",
               "Mexican",
               "Non-Hispanic",
               "Non-Hispanic White",
               "Non-Hispanic Black",
               "HS Dropout",
               "HS Grad or GED",
               "Some college or more")

dfe %<>% mutate_all(~str_replace_all(.x, "\\*", ""))
dfe %<>% mutate_all(~str_replace_all(.x, "---", ""))
dfe %<>% mutate(across(-Group, ~parse_number(as.character(.))))

dfe_long  <-  dfe %>% pivot_longer(
                                 cols = -Group, 
                                 names_to = c(".value", "year"), 
                                 names_sep = "(?<=^[gb])(?=[0-9])"
                                )
dfe_long %<>% mutate(year = as.numeric(year))

dfe_long %<>% select(Group, year, `Pct meeting all exercise guidelines` = g,
                                  `Pct not meeting any exercise guidelines` = b)

#-------------------------------------------------------------------------------
# save as cleaned csv
#-------------------------------------------------------------------------------
fpath <- "../../../data/nhcs-exercise/cleaned/exercise.csv"
write_csv(dfe_long, file = fpath)
