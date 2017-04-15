# Analyze Data

# Fri Apr 14 15:24:23 2017 ------------------------------

# ERROR SOUNDS ------------------------------------------------------------

options(error = function(){    # Beep on error
  beepr::beep()
  Sys.sleep(1)
}
)

# .Last <- function() {          # Beep on exiting session
#   beepr::beep(2) # beep(8) is awesome but too long
#   Sys.sleep(1)
# }

options(error=NULL) # reset beeps to nothing 

# LOAD LIBRARIES ----------------------------------------------------------

library(tidyverse)
library(lubridate)
library(ggrepel)


# LOAD DATA ---------------------------------------------------------------

source('scripts/functions/f_doy.R') # for adding year date cols

# join the data together:
data1 <- left_join(hydroDat, frogBreed, by = c("Date"="estim_strt", "site"="Site")) %>% 
  mutate(site = as.factor(site))
data1 <- add_WYD(data1, "Date") %>% select(-WYT) %>% 
  mutate(DOY = as.integer(DOY),
         WY = as.integer(WY),
         DOWY = as.integer(DOWY))

# join with WY index data
data1 <- inner_join(data1, wys[,c(1,4:5)], by="WY")

names(data1)

# select only data between May-Jun 
nfdf <- data1 %>% #filter(site %in% sites) %>% 
  filter(DOY>120, DOY<182) %>% 
  select(Date, site, obs_strt, totalEM, DOY:DOWY, air.7:level.7dL, WYsum, Index, -ppt2_in, -days_wo_ppt2)

summary(nfdf)

# remove NAs:
nfdf2 <- nfdf %>% 
  filter(!is.na(air.7), 
         !is.na(level.avg))
summary(nfdf2)

# add binary 1 or 0 for breeding
nfdf2 <- nfdf2 %>% mutate(ovipos=as.integer(ifelse(is.na(totalEM), 0, 1)))
names(nfdf2)

# get data
d1 <- nfdf2 %>% dplyr::select(ovipos, site, WYsum, Index, DOWY, air.7:level.7dL) %>% 
  filter(!is.na(ppt_in))
summary(d1)

# refactor the sites to drop unused factors
d1$site <- factor(d1$site)

# drop factors
d2 <- dplyr::select(d1, -site) %>% setNames(., gsub("\\.", "_", colnames(.))) %>% as.data.frame
names(d2)
```
