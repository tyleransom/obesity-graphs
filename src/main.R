#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Data Cleaning
#    (each script loads its own libraries)
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
setwd("data-cleaning/usda-ers")
source("cleaner.R")

setwd("../nhcs-exercise")
source("cleaner.R")

setwd("../WHO-GHO")
source("getObesityRates.R")

setwd("../wweia")
source("cleaner.R")


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Graphs
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
setwd("../../analysis")
source("graphs.R")

