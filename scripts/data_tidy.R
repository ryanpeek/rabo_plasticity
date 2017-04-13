# Clean Data and Bind together in Master Dataset

# to add TIME Stamped section, type "ts" and hit "Shift + TAB"

# Wed Apr 12 15:31:42 2017 ------------------------------

# LOAD LIBRARIES ----------------------------------------------------------

library(tidyverse)
library(lubridate)

# RUN DATA_LOAD.R ---------------------------------------------------------

source("scripts/data_load.R")

# TIDY MASTER -------------------------------------------------------------

# Select sites, rm "grp" col, drop unused factors
hrly2 <- hrly2 %>% select(-grp, -WY, -wyd, -year, -yday) %>% filter(!site %in% c("TUO", "CLA")) %>% 
  mutate(site = factor(site))
levels(hrly2$site) # check factor levels

# make all cols lower case for ease of typing
colnames(hrly2) <- tolower(colnames(hrly2))

# add water year info with function
hrly2 <- add_WYD(hrly2, "datetime")
summary(hrly2)

# QUICK PLOTS -------------------------------------------------------------

ggplot() + 
  geom_path(data=hrly2, aes(x=datetime, y=level, color=site, group=WY)) +
  facet_grid(site~., scales = "free_y")

# ADD SITE UPDATES ---------------------------------------------------------

# combine and rm NA's
raw_updated<-bind_rows(nfa, mfa, sfy, nfy, nfy_baro, nfa_baro)
raw_updated <- raw_updated %>% filter(!is.na(level))

# make some cols factors
fxs <- c("site", "compensated", "type")
raw_updated[fxs] <- lapply(raw_updated[fxs], as.factor) 

summary(raw_updated)


