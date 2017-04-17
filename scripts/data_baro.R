# MERGE BARO LOGGERS 

# Libraries & Functions ---------------------------------------------------

library(tidyverse)
library(lubridate)
library(purrr)  # for map(), reduce()

source("scripts/functions/f_doy.R") # for adding WY info


# NFY GAME BARO------------------------------------------------------------

# from the game baro: 
nfy_gb <- read_csv("data/baro/NFY_2011-2014_gamebaro.csv") %>% 
  mutate(datetime=mdy_hm(datetime)) %>% filter(!is.na(airtemp))
add1<-read_csv("data/baro/20150206_baro.csv") %>% select(site, datetime, baropressure, airtemp) %>% mutate(datetime=mdy_hm(datetime))
add2<-read_csv("data/baro/20150522_baro.csv") %>% select(site, datetime, baropressure, airtemp) %>% mutate(datetime=mdy_hm(datetime))

# read the EXIF files?
load("data/baro/NFY_20150811_photolist.rda")
add3<-photolist %>% select(timeround, baro_in) %>% rename(datetime=timeround,baropressure=baro_in)
load("data/baro/NFY_20160205_photolist.rda")
add4<-photolist %>% select(timeround, baro_in) %>% rename(datetime=timeround,baropressure=baro_in)


# this one is at 15 min so need to average:
load(file = "data/baro/NFY_20170211_photolist.rda")
add5.15<-photolist %>% select(timeround, baro_inHg, air_C) %>% rename(datetime=timeround,baropressure=baro_inHg, airtemp=air_C)
rm(photolist,photolist_sub)

head(add5.15)
add5.15 <- add5.15 %>% mutate(datetimeHR=floor_date(datetime, unit="hours"))
add5 <- add5.15 %>% select(datetimeHR, baropressure, airtemp) %>% 
  group_by(datetimeHR) %>% summarize_all(mean) %>% rename(datetime=datetimeHR)
head(add5)

# bind the rows
nfy_gb_all <- bind_rows(nfy_gb, add1, add2, add3, add4, add5) %>% mutate(site="NFY")
summary(nfy_gb_all)

save(nfy_gb_all, file = "data/NFY_gamebaro_hrly_2011-2017.rda")


# NFA ---------------------------------------------------------------------

load("data/baro/NFA_exif.rda")
nfa1 <- NFA_exif %>% mutate(datetime=floor_date(ymd_hms(datetime),unit = "hours")) %>% select(site, datetime, baro_inHg) %>% rename(baropressure=baro_inHg)
rm(NFA_exif)
summary(nfa1)

load("data/baro/NFA_photolist_2016-12-28.rda")
nfa3<- photolist %>% select(timeround, baro_inHg, air_C) %>% rename(datetime=timeround,baropressure=baro_inHg, airtemp=air_C)

rm(photolist,photolist_sub)

head(nfa3)
nfa3 <- nfa3 %>% mutate(datetime=floor_date(datetime, unit="hours"))
nfa3 <- nfa3 %>% select(datetime, baropressure, airtemp) %>% 
  group_by(datetime) %>% summarize_all(mean)
head(nfa3)

nfa_gb_all <- bind_rows(nfa1, nfa3) %>% mutate(site="NFA")
head(nfa_gb_all)

save(nfa_gb_all, file = "data/NFA_gamebaro_hrly_2011-2016.rda")
