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

colnames(hrly2) <- tolower(colnames(hrly2)) # rename colnames

levels(hrly2$site) # check factor levels

# re add water year info with function

hrly2 <- add_WYD(hrly2, "datetime")


summary(hrly2)

# rm attr() from col
attr(x = hrly2$DOY, 'label') <- NULL
  attr(MyData[[deparse(as.name(var))]], "ATT_2") <- NULL
}


# QUICK PLOTS -------------------------------------------------------------

ggplot() + 
  geom_line(data=hrly2, aes(x=datetime, y=level, color=site)) +
  facet_grid(site~., scales = "free_y")


# ADD SITES ---------------------------------------------------------------


