library(tidyverse)
library(magrittr)
library(rgho)

# get the USA obesity rate data from WHO's GHO product
obrat <- get_gho_data(
                      code = "NCD_BMI_30A",
                      filter = list(
                                    COUNTRY = "USA"
                                   )
                     )

# remove sex-specific values and other unnecessary variables
obrat %<>% filter(SEX=="SEX_BTSX") %>% 
           select(YEAR,NumericValue) %>%
           arrange(YEAR)

# rename variables
obrat %<>% select(year = YEAR, `obesity rate GHO` = NumericValue)

# manually input USA NHANES data from NCHS Data Brief No. 360 (Feb 2020)
nhanes <- tibble(year = seq(from = 1999, to = 2017, by = 2),
                 `obesity rate NHANES` = c(30.5, 30.5, 32.2, 34.3, 33.7, 35.7, 34.9, 37.7, 39.6, 42.4))

# join together
obrat %<>% full_join(nhanes, by=c("year"))

# save as CSV
write_csv(obrat, "../../../data/WHO-GHO/cleaned/obesityUSA.csv")


# All Major Countries
country_codes <- c("USA", "JPN", "KOR", "DEU", "ARE", "GBR", "FRA", "ESP", "ARG")

# get the obesity rate data from WHO's GHO product
obrat <- get_gho_data(
    code = "NCD_BMI_30A"
)

# remove sex-specific values and other unnecessary variables
obrat %<>% filter(SEX=="SEX_BTSX") %>% 
    select(YEAR,COUNTRY,NumericValue) %>%
    arrange(COUNTRY,YEAR)

# rename variables
obrat %<>% select(country = COUNTRY, year = YEAR, `obesity rate GHO` = NumericValue)

# save as CSV
write_csv(obrat, "../../../data/WHO-GHO/cleaned/obesityAll.csv")
