# Pull Hydrotemp Data Together

# Wed Apr 12 15:32:38 2017 ------------------------------

library(tidyverse)

# READ in MASTER DATA -----------------------------------------------------

# get hrly2 dataset
load("./data/hydro/2011-2015_solinst_mainstem_hourly_compensated.rda") 

glimpse(hrly2)

# hobo dataset
load("./data/hydro/hobo_raw15min.rda")

# ADD OTHER DATA ----------------------------------------------------------

# read in various solinst CSV's
source("./scripts/functions/f_format_solinst.R")

# this function pulls in solinst and formats it
# format_solinst(skip = 16, site = "NFA", compensated = FALSE)
# format_solinst(skip = 13, site = "NFY", compensated = FALSE)
# format_solinst(skip = 16, site = "SFY", compensated = FALSE)
# format_solinst(skip = 11, site = "NFY", compensated = FALSE) # baro
# format_solinst(skip = 12, site = "NFA", compensated = FALSE) # baro
# format_solinst(skip = 11, site = "MFA", compensated = FALSE) 

# read in formatted NFA
nfa <- read_rds("data/hydro/2016_NFA_solinst_06_07_formatted.rds")
nfa$type <- "solinst"

nfa_baro <- read_rds("data/hydro/2016_NFA_BARO_06_07_full_formatted.rds")
nfa_baro$type <- "baro"

# read in formatted NFY
nfy <- read_rds("data/hydro/2016_NFY_solinst_08_19_formatted.rds")
nfy$type <- "solinst"

nfy_baro <- read_rds("data/hydro/2016_NFY_BARO_08_19_full_formatted.rds")
nfy_baro$type <- "baro"

# read in formatted SFY
sfy <- read_rds("data/hydro/2016_SFY_solinst_06_01_formatted.rds")
sfy$type <- "solinst"

# read in formatted MFA
mfa <- read_rds("data/hydro/2016_MFA_solinst_08_22_formatted.rds")
mfa$type <- "solinst"


# ADJ LEVELS (RESETS) -----------------------------------------------------


# fix MFA level first (it's 200 and needs adj)
summary(mfa)
mfa$level <- mfa$level - 206

# adj NFA level
summary(nfa)
nfa$level <- nfa$level - 0.41

# adj SFY (drop about 0.5)
summary(sfy)
sfy$level <- sfy$level - 0.62

# adj NFY (drop about 0.5)
summary(nfy)
nfy$level <- nfy$level - 0.71
