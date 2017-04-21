# MERGE BARO LOGGERS 

# Libraries & Functions ---------------------------------------------------

library(tidyverse)
library(lubridate)
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

# and KCAFORES14_hr has plenty of data for period in need
ggplot() + geom_line(data=KCAFORES14_hr, aes(x=datetime, y=PressureIn*0.3453-9.475), color="blue")


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

# Plot barologger solinst data
ggplot() + geom_line(data=hr.df[hr.df$type=="baro",],
                     aes(x=datetime, y=level, color=site), alpha=0.8) # unfortunately not much overlap here...

# Plot and compare
ggplot() + geom_line(data=hr.df[hr.df$type=="baro" & year(hr.df$datetime)==2015 & month(hr.df$datetime)==5,],
                     aes(x=datetime, y=level, color=site), alpha=0.8) +
  geom_line(data=KCAFORES14_hr[KCAFORES14_hr$year==2015 & month(KCAFORES14_hr$datetime)==5,], aes(x=datetime, y=PressureIn*0.3453-9.475), color="blue")
# pretty good, should be fine for compensation

# quick check: 
nfa.tst<-filter(hr.df, site=="NFA", WY==2016,  month(datetime)>=1, month(datetime)<7, type=="solinst") %>%
  left_join(KCAFORES14_hr, by="datetime") %>% 
  mutate(wunder_baro=PressureIn*0.3453-9.475, 
         airtempC=convertTemp(TemperatureF, unit = "F", convert="C")) %>% select(datetime:DOWY, wunder_baro, airtempC, WindSpeedMPH, Humidity) %>% 
  rename(wind_mph = WindSpeedMPH, humidity_prcnt = Humidity)

# make solinst baro set
nfa.baro <- filter(hr.df, site=="NFA", WY==2016, month(datetime)>=1, month(datetime)<7, type=="baro") %>% mutate(sol_baro=level) %>% 
  select(datetime, sol_baro)
nfa.tst <- nfa.tst %>% right_join(nfa.baro, by="datetime") %>% filter(!is.na(wunder_baro))

ggplot() + geom_line(data=nfa.tst, aes(x=datetime, y=level), color="black", lty=3, lwd=1.3) + 
  geom_line(data=nfa.tst, aes(x=datetime, y=level-wunder_baro+0.93), color="red") + 
  geom_line(data=nfa.tst, aes(x=datetime, y=level-sol_baro+0.97), color="blue")

# Looks good!

# COMPENSATE --------------------------------------------------------------

# pull the approp col and prep from wunderdata:
wunder_baro_AMER <- KCAFORES14_hr %>% select(datetime, PressureIn, TemperatureF, Humidity, WindSpeedMPH) %>% 
  mutate(W_baro=PressureIn*0.3453-9.475, 
         W_airtempC=convertTemp(TemperatureF, unit = "F", convert="C")) %>% select(datetime, W_baro, W_airtempC, WindSpeedMPH, Humidity) %>% 
  rename(W_wind_mph = WindSpeedMPH, W_humidity_prcnt = Humidity) 
summary(wunder_baro_AMER)

# filter data to compensate:
hr.df.tst <- filter(hr.df, compensated=="N", type=="solinst")

# now join wth other data set:
hr.df.tst <- left_join(hr.df.tst, wunder_baro_AMER, by="datetime")

ggplot() + 
  geom_line(data=hr.df[hr.df$WY>=2014 & hr.df$type=="solinst",], aes(x=datetime, y=level, group=year(datetime), color=site), alpha=0.9)+
  geom_line(data=hr.df.tst[hr.df.tst$WY>=2014,],
            aes(x=datetime, y=level-W_baro+0.8, group=WY)) +
  facet_grid(site~., scales = "free_y")

hr.df2 <- left_join(hr.df, wunder_baro_AMER, by="datetime")
names(hr.df2)
summary(hr.df2)

# compensate all (for now...pull separate for NFY and SFY later)
hr.df2$level_comp <- ifelse(hr.df2$compensated=="N" & hr.df2$type=="solinst", hr.df2$level-hr.df2$W_baro+0.8, hr.df2$level)

summary(hr.df2)

ggplot() + 
  geom_line(data=hr.df2[hr.df2$type=="solinst",], 
            aes(x=datetime, y=level, color=site, group=WY)) +
  geom_line(data=hr.df2[hr.df2$type=="solinst",], aes(x=datetime, y=level_comp, group=WY), color="black") + 
  facet_grid(site~., scales = "free_y")

# remove old solinst BARO info...not useful here:
hr.df2 <- 
  hr.df2 %>% filter(!type=="baro") %>% select(-type)
summary(hr.df2)

save(hr.df2, file = "data/2011-2016_solinst_mainstem_hrly_compensated.rda")

