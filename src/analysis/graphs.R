library(tidyverse)


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# NHANES What We Eat in America graphs
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
fpath <- "../../data/wweia/cleaned/wweia.csv"
df <- read_csv(fpath)

# graph calories over time for adult men and women
ggplot(df %>% filter((gengrp %in% c("Male", "Female")) & agegrp=="20 and over"),
       aes(x = year, y = kcal)) +
    geom_line(aes(linetype = gengrp)) +
    labs(x = "Year", y = "Calories (kcal)", linetype = "Group") +
    scale_y_continuous(limits = c(0, 4000)) +  # Adjust y-axis limits
    scale_x_continuous(limits = c(2000, 2022)) +
    theme_minimal() +
    theme(legend.position = c(1, 0.1),  # Place legend in the bottom right corner
          legend.justification = c(1, 0),  # Anchor the legend at the bottom right corner
          legend.background = element_blank(),  # Optional: make legend background transparent
          legend.box.background = element_rect(color="white", size=0.5),  # Optional: add a white background to the legend box for better visibility
          legend.box.margin = margin(6, 6, 6, 6))  # Optional: adjust margin around the legend box

# Save the plot
figpath <- "../../exhibits/figures/"
ggsave(filename = paste0(figpath,"wweia_calories.eps"),
       width = 5, height = 5, dpi = 300)


# graph sugars over time for adult men and women
ggplot(df %>% filter((gengrp %in% c("Male", "Female")) & agegrp=="20 and over"),
       aes(x = year, y = sugars)) +
    geom_line(aes(linetype = gengrp)) +
    labs(x = "Year", y = "Sugars (g)", linetype = "Group") +
    scale_y_continuous(limits = c(0, 200)) +  # Adjust y-axis limits
    scale_x_continuous(limits = c(2000, 2022)) +
    theme_minimal() +
    theme(legend.position = c(1, 0.1),  # Place legend in the bottom right corner
          legend.justification = c(1, 0),  # Anchor the legend at the bottom right corner
          legend.background = element_blank(),  # Optional: make legend background transparent
          legend.box.background = element_rect(color="white", size=0.5),  # Optional: add a white background to the legend box for better visibility
          legend.box.margin = margin(6, 6, 6, 6))  # Optional: adjust margin around the legend box

# Save the plot
figpath <- "../../exhibits/figures/"
ggsave(filename = paste0(figpath,"wweia_sugars.eps"),
       width = 5, height = 5, dpi = 300)


# graph omega-6 over time for adult men and women
ggplot(df %>% filter((gengrp %in% c("Male", "Female")) & agegrp=="20 and over"),
       aes(x = year, y = omega6)) +
    geom_line(aes(linetype = gengrp)) +
    labs(x = "Year", y = "Omega-6 Polyunsaturated Fatty Acids (g)", linetype = "Group") +
    scale_y_continuous(limits = c(0, NA)) +  # Adjust y-axis limits
    scale_x_continuous(limits = c(2000, 2022)) +
    theme_minimal() +
    theme(legend.position = c(1, 0.1),  # Place legend in the bottom right corner
          legend.justification = c(1, 0),  # Anchor the legend at the bottom right corner
          legend.background = element_blank(),  # Optional: make legend background transparent
          legend.box.background = element_rect(color="white", size=0.5),  # Optional: add a white background to the legend box for better visibility
          legend.box.margin = margin(6, 6, 6, 6))  # Optional: adjust margin around the legend box

# Save the plot
figpath <- "../../exhibits/figures/"
ggsave(filename = paste0(figpath,"wweia_omega6.eps"),
       width = 5, height = 5, dpi = 300)


# graph omega-3 over time for adult men and women
ggplot(df %>% filter((gengrp %in% c("Male", "Female")) & agegrp=="20 and over"),
       aes(x = year, y = omega3)) +
    geom_line(aes(linetype = gengrp)) +
    labs(x = "Year", y = "Omega-3 Polyunsaturated Fatty Acids (g)", linetype = "Group") +
    scale_y_continuous(limits = c(0, NA)) +  # Adjust y-axis limits
    scale_x_continuous(limits = c(2000, 2022)) +
    theme_minimal() +
    theme(legend.position = c(1, 0.1),  # Place legend in the bottom right corner
          legend.justification = c(1, 0),  # Anchor the legend at the bottom right corner
          legend.background = element_blank(),  # Optional: make legend background transparent
          legend.box.background = element_rect(color="white", size=0.5),  # Optional: add a white background to the legend box for better visibility
          legend.box.margin = margin(6, 6, 6, 6))  # Optional: adjust margin around the legend box

# Save the plot
figpath <- "../../exhibits/figures/"
ggsave(filename = paste0(figpath,"wweia_omega3.eps"),
       width = 5, height = 5, dpi = 300)


# graph omega-6:omega-3 ratio over time for adult men and women
ggplot(df %>% filter((gengrp %in% c("Male", "Female")) & agegrp=="20 and over"),
       aes(x = year, y = omega6/omega3)) +
    geom_line(aes(linetype = gengrp)) +
    labs(x = "Year", y = "Ratio of Omega-6 to Omega-3", linetype = "Group") +
    scale_y_continuous(limits = c(0, NA)) +  # Adjust y-axis limits
    scale_x_continuous(limits = c(2000, 2022)) +
    theme_minimal() +
    theme(legend.position = c(1, 0.1),  # Place legend in the bottom right corner
          legend.justification = c(1, 0),  # Anchor the legend at the bottom right corner
          legend.background = element_blank(),  # Optional: make legend background transparent
          legend.box.background = element_rect(color="white", size=0.5),  # Optional: add a white background to the legend box for better visibility
          legend.box.margin = margin(6, 6, 6, 6))  # Optional: adjust margin around the legend box

# Save the plot
figpath <- "../../exhibits/figures/"
ggsave(filename = paste0(figpath,"wweia_omega63_ratio.eps"),
       width = 5, height = 5, dpi = 300)


# graph omega-6:omega-3 ratio over time for adult men and women
ggplot(df %>% filter((gengrp %in% c("Male", "Female")) & agegrp=="20 and over"),
       aes(x = year, y = 100*omega3/(omega3+omega6))) +
    geom_line(aes(linetype = gengrp)) +
    labs(x = "Year", y = "Omega-3 share of PUFAs (%)", linetype = "Group") +
    scale_y_continuous(limits = c(0, NA)) +  # Adjust y-axis limits
    scale_x_continuous(limits = c(2000, 2022)) +
    theme_minimal() +
    theme(legend.position = c(1, 0.1),  # Place legend in the bottom right corner
          legend.justification = c(1, 0),  # Anchor the legend at the bottom right corner
          legend.background = element_blank(),  # Optional: make legend background transparent
          legend.box.background = element_rect(color="white", size=0.5),  # Optional: add a white background to the legend box for better visibility
          legend.box.margin = margin(6, 6, 6, 6))  # Optional: adjust margin around the legend box

# Save the plot
figpath <- "../../exhibits/figures/"
ggsave(filename = paste0(figpath,"wweia_omega_balance.eps"),
       width = 5, height = 5, dpi = 300)


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Exercise graphs
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# load exercise data
fpath <- "../../data/nhcs-exercise/cleaned/exercise.csv"
df <- read_csv(fpath)

#-------------------------------------------------------------------------------
# Create a line chart for two different measures of caloric availability
#-------------------------------------------------------------------------------
ggplot(df %>% filter(Group %in% c("All", "Male", "Female"))) +
    geom_line(aes(x=year, y=`Pct meeting all exercise guidelines`, group = Group, linetype = Group)) +
    labs(x = "Year", 
         y = "Percentage of U.S. Adults",
         linetype = "Group") +
    scale_y_continuous(limits = c(0, NA)) +  # Adjust y-axis limits
    scale_x_continuous(limits = c(1970, 2025)) +  # Adjust x-axis limits
    theme_minimal() +
    theme(legend.position = c(1, 0.1),  # Place legend in the bottom right corner
          legend.justification = c(1, 0),  # Anchor the legend at the bottom right corner
          legend.background = element_blank(),  # Optional: make legend background transparent
          legend.box.background = element_rect(color="white", size=0.5),  # Optional: add a white background to the legend box for better visibility
          legend.box.margin = margin(6, 6, 6, 6))  # Optional: adjust margin around the legend box

# Save the plot
figpath <- "../../exhibits/figures/"
ggsave(filename = paste0(figpath,"exercise.eps"), 
       width = 5, height = 5, dpi = 300)


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Calories graphs
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# load calories data
fpath <- "../../data/usda-ers/cleaned/calories.csv"
df <- read_csv(fpath)

#-------------------------------------------------------------------------------
# Create a line chart for two different measures of caloric availability
#-------------------------------------------------------------------------------
ggplot(df) +
    geom_line(aes(x=year, y=calories, linetype = "USDA ERS")) +
    geom_line(aes(x=year, y=OWIDcalories, linetype = "Our World in Data")) +
    labs(x = "Year", 
         y = "Daily Caloric Availability Per Capita (kcal)",
         linetype = "Data Source") +
    scale_y_continuous(limits = c(0, NA)) +  # Adjust y-axis limits
    scale_x_continuous(limits = c(1970, 2025)) +  # Adjust x-axis limits
    theme_minimal() +
    theme(legend.position = c(1, 0.1),  # Place legend in the bottom right corner
          legend.justification = c(1, 0),  # Anchor the legend at the bottom right corner
          legend.background = element_blank(),  # Optional: make legend background transparent
          legend.box.background = element_rect(color="white", size=0.5),  # Optional: add a white background to the legend box for better visibility
          legend.box.margin = margin(6, 6, 6, 6))  # Optional: adjust margin around the legend box

# Save the plot
figpath <- "../../exhibits/figures/"
ggsave(filename = paste0(figpath,"calories.eps"), 
       width = 5, height = 5, dpi = 300)


#-------------------------------------------------------------------------------
# same chart, but since 2000
#-------------------------------------------------------------------------------
ggplot(df) +
    geom_line(aes(x=year, y=calories, linetype = "USDA ERS")) +
    geom_line(aes(x=year, y=OWIDcalories, linetype = "Our World in Data")) +
    labs(x = "Year", 
         y = "Daily Caloric Availability Per Capita (kcal)",
         linetype = "Data Source") +
    scale_y_continuous(limits = c(0, NA)) +  # Adjust y-axis limits
    scale_x_continuous(limits = c(2000, 2022)) +  # Adjust x-axis limits
    theme_minimal() +
    theme(legend.position = c(1, 0.1),  # Place legend in the bottom right corner
          legend.justification = c(1, 0),  # Anchor the legend at the bottom right corner
          legend.background = element_blank(),  # Optional: make legend background transparent
          legend.box.background = element_rect(color="white", size=0.5),  # Optional: add a white background to the legend box for better visibility
          legend.box.margin = margin(6, 6, 6, 6))  # Optional: adjust margin around the legend box

# Save the plot
figpath <- "../../exhibits/figures/"
ggsave(filename = paste0(figpath,"calories-since-2000.eps"), 
       width = 5, height = 5, dpi = 300)


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Agricultural data graphs
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# load sweetener and seed oil consumption availability data
fpath <- "../../data/usda-ers/cleaned/sweetener-oil-crops-consumption.csv"
df <- read_csv(fpath)

#-------------------------------------------------------------------------------
# Create a line chart for all sweeteners
#-------------------------------------------------------------------------------
ggplot(df, aes(x=year, y=`tot dry lbs`)) +
    geom_line() +
    labs(x = "Year", y = "Total Annual Dry Pounds Per Capita") +
    scale_y_continuous(limits = c(0, NA)) +  # Adjust y-axis limits
    scale_x_continuous(limits = c(1970, 2025)) +  # Adjust x-axis limits
    theme_minimal()

# Save the plot
figpath <- "../../exhibits/figures/"
ggsave(filename = paste0(figpath,"all-sweeteners-annual.eps"), 
       width = 5, height = 5, dpi = 300)


#-------------------------------------------------------------------------------
# all sweeteners since 2000 (in g per day)
#-------------------------------------------------------------------------------
ggplot(df %>% mutate(gperday = `tot dry lbs`*1.24186822724), aes(x=year, y=gperday)) +
    geom_line() +
    labs(x = "Year", y = "Per Capita Daily Sales (g)") +
    scale_y_continuous(limits = c(0, NA)) +  # Adjust y-axis limits
    scale_x_continuous(limits = c(2000, 2022)) +  # Adjust x-axis limits
    theme_minimal()

# Save the plot
figpath <- "../../exhibits/figures/"
ggsave(filename = paste0(figpath,"all-sweeteners-annual-since-2000.eps"), 
       width = 5, height = 5, dpi = 300)


#-------------------------------------------------------------------------------
# Create a line chart for cane and beet sugar
#-------------------------------------------------------------------------------
ggplot(df, aes(x=year, y=`cane beet g/day`)) +
    geom_line() +
    labs(x = "Year", y = "Total Daily Grams Per Capita") +
    scale_y_continuous(limits = c(0, NA)) +  # Adjust y-axis limits
    theme_minimal()

# Save the plot
figpath <- "../../exhibits/figures/"
ggsave(filename = paste0(figpath,"cane-beet-sugar-daily.eps"), 
       width = 5, height = 5, dpi = 300)


#-------------------------------------------------------------------------------
# Create a line chart for HFCS
#-------------------------------------------------------------------------------
ggplot(df, aes(x=year, y=`hfcs g/day`)) +
    geom_line() +
    labs(x = "Year", y = "Total Daily Grams Per Capita") +
    scale_y_continuous(limits = c(0, NA)) +  # Adjust y-axis limits
    theme_minimal()

# Save the plot
figpath <- "../../exhibits/figures/"
ggsave(filename = paste0(figpath,"hfcs-daily.eps"), 
       width = 5, height = 5, dpi = 300)


#-------------------------------------------------------------------------------
# Create a line chart for other sweeteners
#-------------------------------------------------------------------------------
ggplot(df, aes(x=year, y=`other g/day`)) +
    geom_line() +
    labs(x = "Year", y = "Total Daily Grams Per Capita") +
    scale_y_continuous(limits = c(0, NA)) +  # Adjust y-axis limits
    theme_minimal()

# Save the plot
figpath <- "../../exhibits/figures/"
ggsave(filename = paste0(figpath,"other-sweeteners-daily.eps"), 
       width = 5, height = 5, dpi = 300)


#-------------------------------------------------------------------------------
# Create a line chart for all seed oil
#-------------------------------------------------------------------------------
ggplot(df %>% filter(year>1979), aes(x=year, y=`all seed`)) +
    geom_line() +
    labs(x = "Year", y = "Total Annual Pounds Per Capita") +
    scale_y_continuous(limits = c(0, NA)) +  # Adjust y-axis limits
    scale_x_continuous(limits = c(1970, 2025)) +  # Adjust x-axis limits
    theme_minimal()

# Save the plot
figpath <- "../../exhibits/figures/"
ggsave(filename = paste0(figpath,"all-seed-oils-annual.eps"), 
       width = 5, height = 5, dpi = 300)


#-------------------------------------------------------------------------------
# All seed oil since 2000
#-------------------------------------------------------------------------------
ggplot(df %>% filter(year>1979) %>% mutate(gperday = `all seed`*1.24186822724), aes(x=year, y=gperday)) +
    geom_line() +
    labs(x = "Year", y = "Per Capita Daily Disappearance (g)") +
    scale_y_continuous(limits = c(0, NA)) +  # Adjust y-axis limits
    scale_x_continuous(limits = c(2000, 2022)) +  # Adjust x-axis limits
    theme_minimal()

# Save the plot
figpath <- "../../exhibits/figures/"
ggsave(filename = paste0(figpath,"all-seed-oils-annual-since-2000.eps"), 
       width = 5, height = 5, dpi = 300)


#-------------------------------------------------------------------------------
# Create a line chart for soybean oil
#-------------------------------------------------------------------------------
ggplot(df %>% filter(year>1979), aes(x=year, y=`soybean disapp`)) +
    geom_line() +
    labs(x = "Year", y = "Total Annual Pounds Per Capita") +
    scale_y_continuous(limits = c(0, NA)) +  # Adjust y-axis limits
    theme_minimal()

# Save the plot
figpath <- "../../exhibits/figures/"
ggsave(filename = paste0(figpath,"soybean-oil-annual.eps"), 
       width = 5, height = 5, dpi = 300)


#-------------------------------------------------------------------------------
# Create a line chart for sunflowerseed oil
#-------------------------------------------------------------------------------
ggplot(df %>% filter(year>1979), aes(x=year, y=`sunflowerseed disapp`)) +
    geom_line() +
    labs(x = "Year", y = "Total Annual Pounds Per Capita") +
    scale_y_continuous(limits = c(0, NA)) +  # Adjust y-axis limits
    theme_minimal()

# Save the plot
figpath <- "../../exhibits/figures/"
ggsave(filename = paste0(figpath,"sunflowerseed-oil-annual.eps"), 
       width = 5, height = 5, dpi = 300)


#-------------------------------------------------------------------------------
# Create a line chart for corn oil
#-------------------------------------------------------------------------------
ggplot(df %>% filter(year>1979), aes(x=year, y=`corn disapp`)) +
    geom_line() +
    labs(x = "Year", y = "Total Annual Pounds Per Capita") +
    scale_y_continuous(limits = c(0, NA)) +  # Adjust y-axis limits
    theme_minimal()

# Save the plot
figpath <- "../../exhibits/figures/"
ggsave(filename = paste0(figpath,"corn-oil-annual.eps"), 
       width = 5, height = 5, dpi = 300)


#-------------------------------------------------------------------------------
# Create a line chart for canola oil
#-------------------------------------------------------------------------------
ggplot(df %>% filter(year>1979), aes(x=year, y=`canola disapp`)) +
    geom_line() +
    labs(x = "Year", y = "Total Annual Pounds Per Capita") +
    scale_y_continuous(limits = c(0, NA)) +  # Adjust y-axis limits
    theme_minimal()

# Save the plot
figpath <- "../../exhibits/figures/"
ggsave(filename = paste0(figpath,"canola-oil-annual.eps"), 
       width = 5, height = 5, dpi = 300)


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Obesity graphs
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


# load sweetener and seed oil consumption availability data
fpath <- "../../data/WHO-GHO/cleaned/obesityAll.csv"
df <- read_csv(fpath)

#-------------------------------------------------------------------------------
# Create a line chart for USA
#-------------------------------------------------------------------------------
ggplot(df %>% filter(country=="USA"), aes(x=year, y=`obesity rate GHO`)) +
    geom_line() +
    labs(x = "Year", y = "Percentage of adults with BMI > 30") +
    scale_y_continuous(limits = c(0, NA)) +  # Adjust y-axis limits
    scale_x_continuous(limits = c(1970, 2025)) +  # Adjust x-axis limits
    theme_minimal()

# Save the plot
figpath <- "../../exhibits/figures/"
ggsave(filename = paste0(figpath,"obesity-usa.eps"), 
       width = 5, height = 5, dpi = 300)


