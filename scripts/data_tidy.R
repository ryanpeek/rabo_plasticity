# Clean Data and Bind together in Master Dataset

# to add TIME Stamped section, type "ts" and hit "Shift + TAB"

# Wed Apr 12 15:31:42 2017 ------------------------------

# LOAD LIBRARIES ----------------------------------------------------------

library(tidyverse)
library(lubridate)

# RUN DATA_LOAD.R ---------------------------------------------------------

source("scripts/data_load.R")

# TIDY --------------------------------------------------------------------

# Select sites, rm "grp" col, drop unused factors
hrly2 <- hrly2 %>% select(-grp) %>% filter(!site %in% c("TUO", "CLA")) %>% 
  mutate(site = factor(site))

levels(hrly2$site) # check factor levels

summary(hrly2)


# QUICK PLOTS -------------------------------------------------------------

ggplot() + 
  geom_line(data=hrly2, aes(x=Datetime, y=Level, color=site)) +
  facet_grid(site~., scales = "free_y")

