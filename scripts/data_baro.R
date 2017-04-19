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
add1<-read_csv("data/baro/NFY_20150206_baro.csv") %>% select(site, datetime, baropressure, airtemp) %>% mutate(datetime=mdy_hm(datetime))
add2<-read_csv("data/baro/NFY_20150522_baro.csv") %>% select(site, datetime, baropressure, airtemp) %>% mutate(datetime=mdy_hm(datetime))

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
rm(list = ls(pattern = "add*"), nfy_gb)

# NFA GAME BARO ---------------------------------------------------------------------

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
rm(nfa1, nfa3)

#save(nfa_gb_all, file = "data/NFA_gamebaro_hrly_2011-2016.rda")


# WUNDER BARO -------------------------------------------------------------

# run the "scripts/data_wunder.R" for station wunderground data
#load("data/wunder/NFA_KCAGOLDR3_2012-03-01_2016-09-30.rda")
#load("data/wunder/NFA_KCAFORES6_2012-03-01_2016-09-30.rda")
load("data/wunder/RUB_KCAFORES14_2010-10-01_2016-09-30.rda")# seems best

rm(list = ls(pattern="*_15"))

# Plot and compare
ggplot() + geom_line(data=nfa_gb_all[year(nfa_gb_all$datetime)>2013 & year(nfa_gb_all$datetime)<2016,], 
                     aes(x=datetime, y=baropressure*0.3453-9)) +
  geom_line(data=nfy_gb_all[year(nfy_gb_all$datetime)>2013 & year(nfy_gb_all$datetime)<2016,], aes(x=datetime, y=baropressure*0.3453-8.6), color="darkgreen") +
  geom_line(data=KCAFORES14_hr[KCAFORES14_hr$year>2013 & KCAFORES14_hr$year<2016,], aes(x=datetime, y=PressureIn*0.3453-9.4), color="blue", alpha=0.5, lwd=1) 

# comparable...probably fine for compensation purposes at hourly levels


# MERGE WITH MASTER DATA AND COMPENSATE -----------------------------------

load("data/wunder/RUB_KCAFORES14_2010-10-01_2016-09-30.rda")
load("data/2011-2016_solinst_mainstem_hrly.rda")
load("data/2011-2016_solinst_mainstem_daily.rda")

# plot uncompensated levels
ggplot() + 
  geom_line(data=hr.df[hr.df$type=="solinst" & hr.df$compensated=="N",], 
            aes(x=datetime, y=level, color=site, group=WY)) +
  facet_grid(site~., scales = "free_y")

# so since 2014-Aug to current data

# Plot barologgers
ggplot() + geom_line(data=hr.df[hr.df$type=="baro",],
                     aes(x=datetime, y=level, color=site), alpha=0.8) # unfortunately not much overlap here...

# Plot and compare
ggplot() + geom_line(data=hr.df[hr.df$type=="baro" & year(hr.df$datetime)==2015 & month(hr.df$datetime)==5,],
                     aes(x=datetime, y=level, color=site), alpha=0.8) +
  geom_line(data=KCAFORES14_hr[KCAFORES14_hr$year==2015 & month(KCAFORES14_hr$datetime)==5,], aes(x=datetime, y=PressureIn*0.3453-9.475), color="blue")
# pretty good.

# and KCAFORES14_hr has plenty of data for period in need
ggplot() + geom_line(data=KCAFORES14_hr, aes(x=datetime, y=PressureIn*0.3453-9.475), color="blue")


# COMPENSATE --------------------------------------------------------------

# pull the approp col and prep from wunderdata:
wunder_baro_AMER <- KCAFORES14_hr %>% select(datetime, PressureIn, TemperatureF, Humidity, WindSpeedMPH) %>% 
  mutate(wunder_baro=PressureIn*0.3453-9.475, 
         airtempC=convertTemp(TemperatureF, unit = "F", convert="C")) %>% select(datetime, wunder_baro, airtempC, WindSpeedMPH, Humidity) %>% 
  rename(wind_mph = WindSpeedMPH, humidity_prcnt = Humidity) 
  #mutate(type="wunder")

# filter data to compensate:
hr.df2 <- filter(hr.df, compensated=="N", type=="solinst")

# now join wth other data set:
hr.df2 <- left_join(hr.df2, wunder_baro_AMER, by="datetime")

ggplot() + geom_line(data=hr.df2,
                     aes(x=datetime, y=level-wunder_baro+2,
                         color=site, group=WY)) +
  facet_grid(site~., scales = "free_y")

