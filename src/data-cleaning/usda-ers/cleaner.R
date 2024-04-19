library(tidyverse)
library(readxl)
library(magrittr)
library(owidR)


#-------------------------------------------------------------------------------
# load and clean calories data from USDA ERS
#-------------------------------------------------------------------------------

# Table 5
# title: Average daily per capita calories from the U.S. food availability, 
#        adjusted for spoilage and other waste
fpath <- "../../../data/usda-ers/raw/calories/calories.xls"
dfc  <-  read_excel(fpath, sheet = "Totals", range = "A5:I53")
dfc %<>% rename(
    year = `...1`, protein = `...2`, dairy = `...3`, fruit = `...4`,
    vegetables = `...5`, grains = `...6`, `fats and oils` = `...7`,
    sugars = `...8`, total = `...9`
)
dfc %<>% mutate(year = str_replace(year, "\\*", "")) # get rid of * in "2000*"
dfc %<>% mutate(`fats and oils` = as.numeric(`fats and oils`),
                          total = as.numeric(total),
                           year = as.numeric(year))
dfc %<>% rename(calories = total)


#-------------------------------------------------------------------------------
# load and clean calories data from our world in data
#-------------------------------------------------------------------------------
dfcowid  <-  owid("daily-per-capita-caloric-supply") %>%
             filter(code == "USA")
dfcowid %<>% select(year,OWIDcalories = `Daily supply of calories per person`) %>%
             filter(year > 1960)


#-------------------------------------------------------------------------------
# load and clean caloric sweetener data
#-------------------------------------------------------------------------------

# Table 50
# title: US  per capita caloric sweeteners estimated deliveries for domestic
#        food and beverage use, by calendar year, since 1966
# note: "Per capita deliveries of sweeteners by U.S. processors and refiners 
#        and direct-consumption imports to food manufacturers, retailers, and 
#        other end users represent the per capita supply of caloric sweeteners. 
#        The data exclude deliveries to manufacturers of alcoholic beverages. 
#        Actual human intake of caloric sweeteners is lower because of uneaten 
#        food, spoilage, and other losses. See Table 51, 52 ,and 53 of the 
#        Sugar and Sweeteners Yearbook series for estimated per capita intake 
#        of sugar, high-corn fructose syrup, and other sweeteners, respectively"
fpath <- "../../../data/usda-ers/raw/sugar-sweeteners/Group 7 Tables - US Caloric Sweetener Consumption.xlsx"
df50  <-  read_excel(fpath, sheet = "Table50", range = "A7:J64")
df50 %<>% rename(
                 year = `...1`, pop = `...2`, `refined sugar` = `...3`,
                 HFCS = `...4`, glucose = `...5`, dextrose = `...6`,
                 `tot corn` = `...7`, honey = `...8`, `edible syrups` = `...9`,
                 `tot dry lbs` = `...10`
                )
df50 %<>% select(year, pop, `tot dry lbs`)

# Table 51
# title: Refined cane and beet sugar: estimated number of per capita calories 
#        consumed daily, by calendar year, since 1970
# note: "Estimated number of daily per capita calories calculated by adjusting 
#        sugar deliveries for domestic food and beverage use for food losses." 
df51  <-  read_excel(fpath, sheet = "Table51", range = "A8:O61")
df51 %<>% select(year = `...1`, `cane beet g/day` = `g/day`)

# Table 52
# title: High-fructose corn syrup: estimated number of per capita calories 
#        consumed daily, by calendar year, since 1970
# note: "Estimated number of daily per capita calories calculated by adjusting 
#        HFCS deliveries for domestic food and beverage use for food losses."
df52  <-  read_excel(fpath, sheet = "Table52", range = "A8:O61")
df52 %<>% select(year = `...1`, `hfcs g/day` = `g/daily`)

# Table 53
# title: Other sweeteners: estimated number of per capita calories consumed 
#        daily, by calendar year, since 1970
# note: "Estimated number of daily per capita calories of sweeteners other than
#        refined sugar and high-fructose corn syrup, calculated by adjusting 
#        deliveries for domestic food and beverage use for food losses."
df53  <-  read_excel(fpath, sheet = "Table53", range = "A8:O61")
df53 %<>% select(year = `...1`, `other g/day` = `g/daily`)



#-------------------------------------------------------------------------------
# load and clean oil crop data
#-------------------------------------------------------------------------------

# Table 5
# title: Soybean oil: U.S. supply, disappearance, and price, 1980/81-2022/23
# note: "Monthly production data not available for 2011/12-2014/15"
fpath <- "../../../data/usda-ers/raw/oil-crops/OilCropsAllTables.xlsx"
df5  <-  read_excel(fpath, sheet = "tab5", range = "A6:K49")
df5 %<>% rename(
    year = `...1`, `begin stock` = `...2`, production = `...3`,
    import = `...4`, `tot supply` = `...5`, consump = `...6`,
    biofuel = `...7`, export = `...8`, `tot disappearance` = `...9`,
    `end stock` = `...10`, price = `...11`
)
df5 %<>% separate(year, into = c("year", "end_year"), sep = "/") %>%
         mutate(year = as.integer(year),
                end_year = as.integer(end_year)) %>%
         select(-end_year)
df5 %<>% select(year, `soybean disapp` = consump)

# Table 20
# title: Cottonseed oil: U.S. supply, disappearance, and price, 1980/81-2022/23
# note: "Monthly production data not available for 2011/12-2014/15"
df20  <-  read_excel(fpath, sheet = "tab 20", range = "A7:J50")
df20 %<>% rename(
    year = `...1`, `begin stock` = `...2`, production = `...3`,
    import = `...4`, `tot supply` = `...5`, consump = `...6`,
    export = `...7`, `tot disappearance` = `...8`,
    `end stock` = `...9`, price = `...10`
)
df20 %<>% separate(year, into = c("year", "end_year"), sep = "/") %>%
    mutate(year = as.integer(year),
           end_year = as.integer(end_year)) %>%
    select(-end_year)
df20 %<>% select(year, `cottonseed disapp` = consump)

# Table 24
# title: Sunflowerseed oil: U.S. supply, disappearance, and price, 1980/81-2022/23
# note: "Monthly production data not available for 2011/12-2014/15"
df24  <-  read_excel(fpath, sheet = "tab24", range = "A7:J50")
df24 %<>% rename(
    year = `...1`, `begin stock` = `...2`, production = `...3`,
    import = `...4`, `tot supply` = `...5`, consump = `...6`,
    export = `...7`, `tot disappearance` = `...8`,
    `end stock` = `...9`, price = `...10`
)
df24 %<>% separate(year, into = c("year", "end_year"), sep = "/") %>%
    mutate(year = as.integer(year),
           end_year = as.integer(end_year)) %>%
    select(-end_year)
df24 %<>% select(year, `sunflowerseed disapp` = consump)

# Table 26:
# title: Canola oil: U.S. supply, disappearance, and price, 1991/92-2022/23
# note: "Monthly production data not available for 2011/12-2014/15"
df26  <-  read_excel(fpath, sheet = "tab26", range = "A6:L38")
df26 %<>% rename(
    year = `...1`, `begin stock` = `...2`, production = `...3`,
    import = `...4`, `tot supply` = `...5`, `dom consump` = `...6`,
    biofuel = `...7`, consump = `...8`, export = `...9`, `tot disappearance` = `...10`,
    `end stock` = `...11`, price = `...12`
)
df26 %<>% separate(year, into = c("year", "end_year"), sep = "/") %>%
    mutate(year = as.integer(year),
           end_year = as.integer(end_year)) %>%
    select(-end_year)
df26 %<>% select(year, `canola disapp` = consump)

# Table 31
# title: Linseed oil: U.S. supply, disappearance, and price, 1980/81-2022/23
df31  <-  read_excel(fpath, sheet = "tab31", range = "A6:I49")
df31 %<>% rename(
    year = `...1`, `begin stock` = `...2`, production = `...3`,
    `tot supply` = `...4`, consump = `...5`,
    export = `...6`, `tot disappearance` = `...7`,
    `end stock` = `...8`, price = `...9`
)
df31 %<>% separate(year, into = c("year", "end_year"), sep = "/") %>%
    mutate(year = as.integer(year),
           end_year = as.integer(end_year)) %>%
    select(-end_year)
df31 %<>% select(year, `linseed disapp` = consump)

# Table 33
# title: Corn oil: U.S. supply, disappearance, and price, 1991/92-2022/23
# note: "Monthly production data not available for 2011/12-2014/15"
df33  <-  read_excel(fpath, sheet = "tab33", range = "A6:M49")
df33 %<>% rename(
    year = `...1`, `begin stock` = `...2`, production = `...3`,
    import = `...4`, `tot supply` = `...5`, `dom consump` = `...6`,
    biofuel = `...7`, consump = `...8`, export = `...9`, `tot disappearance` = `...10`,
    `end stock` = `...11`, price = `...12`, `inedible distillers` = `...13`
)
df33 %<>% separate(year, into = c("year", "end_year"), sep = "/") %>%
    mutate(year = as.integer(year),
           end_year = as.integer(end_year)) %>%
    select(-end_year)
df33 %<>% select(year, `corn disapp` = consump)

# Table 35
# title: Lard: U.S. supply, disappearance, and price, 1980--2022
# note: "USDA, Economic Research Service estimates after 1989, which have been
#        revised from previous publications with a lower yield-per-hog 
#        conversion rate."
df35  <-  read_excel(fpath, sheet = "tab35", range = "A6:K49")
df35 %<>% rename(
    year = `...1`, `begin stock` = `...2`, production = `...3`,
    import = `...4`, `tot supply` = `...5`, `dom disapp` = `...6`,
    export = `...7`, `tot disappearance` = `...8`, consump = `...9`,
    `consump per capita` = `...10`, price = `...11`
)
df35 %<>% separate(year, into = c("year", "end_year"), sep = "/") %>%
    mutate(year = as.integer(year),
           end_year = as.integer(end_year)) %>%
    select(-end_year)
df35 %<>% select(year, `lard disapp` = consump)


# Table 36
# title: Edible tallow: U.S. supply, disappearance, and price, 1980--2022
df36  <-  read_excel(fpath, sheet = "tab36", range = "A6:K49")
df36 %<>% rename(
    year = `...1`, `begin stock` = `...2`, production = `...3`,
    import = `...4`, `tot supply` = `...5`, `dom disapp` = `...6`,
    export = `...7`, `tot disappearance` = `...8`, consump = `...9`,
    `consump per capita` = `...10`, price = `...11`
)
df36 %<>% separate(year, into = c("year", "end_year"), sep = "/") %>%
    mutate(year = as.integer(year),
           end_year = as.integer(end_year)) %>%
    select(-end_year)
df36 %<>% select(year, `tallow disapp` = consump)


# Table 32: all edible oils and fats (but only since 2006)
df32  <-  read_excel(fpath, sheet = "tab32", range = "A62:R77")
df32 %<>%
    pivot_longer(cols = `...2`:`...18`, names_to = "yr", values_to = "value") %>%
    pivot_wider(names_from = `Domestic disappearance`, values_from = value)
df32 %<>% mutate(year = seq(2006,2022)) %>% select(-yr)
df32 %<>% select(year, canola = "Canola", coconut = "Coconut", corn = "Corn", 
                 cottonseed = "Cottonseed", lard  = "Lard", olive = "Olive", 
                 `palm kernel` = "Palm kernel", palm = "Palm", 
                 peanut  = "Peanut 2/" , safflower = "Safflower", 
                 sesame = "Sesame", soybean  = "Soybean" , 
                 sunflower = "Sunflower", tallow = "Tallow, edible", 
                 `tot disapp` = "Total disappearance")
df32 %<>% left_join(., df50 %>% select(year,pop), by = "year")
df32 %<>% mutate(across(-c(year, pop), ~./pop))
df32 %<>% mutate(`all seed oils` = canola + corn + cottonseed + `palm kernel` + 
                                   palm + peanut + safflower + soybean + 
                                   sunflower)

#-------------------------------------------------------------------------------
# merge all together
#-------------------------------------------------------------------------------
df <- left_join(df50, df51, by = "year")
df <- left_join(df,   df52, by = "year")
df <- left_join(df,   df53, by = "year")
df <- left_join(df,   df5,  by = "year")
df <- left_join(df,   df20, by = "year")
df <- left_join(df,   df24, by = "year")
df <- left_join(df,   df26, by = "year")
df <- left_join(df,   df31, by = "year")
df <- left_join(df,   df33, by = "year")
df <- left_join(df,   df35, by = "year")
df <- left_join(df,   df36, by = "year")
dfcal <- left_join(dfcowid,   dfc , by = "year")


#-------------------------------------------------------------------------------
# express seed oil disappearance in terms of pounds per capita
# and create total across all seed oils
#-------------------------------------------------------------------------------
df %<>% mutate(across(contains("disapp"), ~as.numeric(.)/pop))
df %<>% mutate(`canola disapp 0` = case_when(is.na(`canola disapp`) ~ 0, 
                                             !is.na(`canola disapp`) ~ `canola disapp`,
                                             TRUE ~ NA_real_))
df %<>% mutate(`all seed` = `soybean disapp` + 
                            `cottonseed disapp` + 
                            `sunflowerseed disapp` + 
                            `canola disapp 0` + 
                            `linseed disapp` + 
                            `corn disapp`)

#-------------------------------------------------------------------------------
# save as cleaned CSVs
#-------------------------------------------------------------------------------
fpath <- "../../../data/usda-ers/cleaned/calories.csv"
write_csv(dfcal, file = fpath)

fpath <- "../../../data/usda-ers/cleaned/sweetener-oil-crops-consumption.csv"
write_csv(df, file = fpath)

fpath <- "../../../data/usda-ers/cleaned/oil-consumption-detailed.csv"
write_csv(df32, file = fpath)
