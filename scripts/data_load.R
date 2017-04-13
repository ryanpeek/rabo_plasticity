# Pull Hydrotemp Data Together

# Wed Apr 12 15:32:38 2017 ------------------------------

library(tidyverse)

# READ in MASTER DATA -----------------------------------------------------

# get hrly2 dataset
load("./data/hydro/2011-2015_solinst_mainstem_hourly_compensated.rda") 

glimpse(hrly2)

# ADD OTHER DATA ----------------------------------------------------------

# read in various CSV's
source("./scripts/functions/f_format_solinst.R")

format_solinst(skip = 16)
format_solinst(skip = 13)

# read in NFA
nfa <- read_rds("data/2016_NFA_solinst_06_07_formatted.rds")
head(nfa)

# read in NFY
nfy <- read_rds("data/2016_NFY_solinst_08_19_formatted.rds")
head(nfy)

