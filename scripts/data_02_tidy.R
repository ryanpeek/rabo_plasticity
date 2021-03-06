# Clean Data and Bind together in Master Dataset

# to add TIME Stamped section, type "ts" and hit "Shift + TAB"

# Wed Apr 12 15:31:42 2017 ------------------------------

# LOAD LIBRARIES ----------------------------------------------------------
library(tidyverse)
library(lubridate)

# RUN DATA_LOAD.R ---------------------------------------------------------

source("scripts/functions/f_doy.R") # for adding WY info
source("scripts/data_load.R")

# TIDY MASTER -------------------------------------------------------------

# Select sites, rm "grp" col, drop unused factors
hrly2 <- hrly2 %>% select(-grp, -WY, -wyd, -year, -yday, -mon, -hours) %>% filter(!site %in% c("TUO", "CLA")) %>% 
  mutate(site = factor(site),
         type = as.factor("solinst"))
levels(hrly2$site) # check factor levels

# make all cols lower case for ease of typing
colnames(hrly2) <- tolower(colnames(hrly2))
hrly2 <- hrly2 %>% rename(temp_C = temperature)

# add water year info with function
hrly2 <- add_WYD(hrly2, "datetime")
summary(hrly2)

# QUICK PLOTS -------------------------------------------------------------

# these are 2011-2014 (only compensated data)
ggplot() +
  geom_path(data=hrly2[hrly2$compensated=="Y",], 
            aes(x=datetime, y=level, color=site, group=WY)) + 
  scale_x_datetime(date_breaks = "4 months", date_labels = "%Y-%b")+
  xlab("") +ylab("Stage (m)")+ 
  facet_grid(site~., scales = "free_y")

# these are 2011-2015 (all data): need to compensated Aug-1-2014-current date
ggplot() +
  geom_path(data=hrly2, 
            aes(x=datetime, y=level, color=site, group=WY)) + 
  scale_x_datetime(date_breaks = "4 months", date_labels = "%Y-%b")+
  xlab("") +ylab("Stage (m)")+ 
  facet_grid(site~., scales = "free_y")

# ADD SITE UPDATES ---------------------------------------------------------

# combine and rm NA's
raw_updated<-bind_rows(nfa, mfa, sfy, nfy, nfy_baro, nfa_baro)
raw_updated <- raw_updated %>% filter(!is.na(level))

# make some cols factors
fxs <- c("site", "compensated", "type")
raw_updated[fxs] <- lapply(raw_updated[fxs], as.factor) 

# HOURLY: MAKE DATASET ---------------------------------------------------

## Make Hourly dataset: solinst
raw_hrly<-raw_updated %>% filter(type=="solinst") %>% 
  mutate(year = year(datetime),
         mon = month(datetime),
         hour = hour(datetime)) %>% 
    group_by(site, year, mon, DOY, hour)%>%
  dplyr::summarize(
    "level"=mean(level,na.rm=TRUE),
    "temp_C"=mean(temp_C,na.rm=TRUE))%>%
  mutate("datetime"=ymd_hms(strptime(paste0(year,"-", mon,"-", DOY, " ",
                                            hour,":00"),format = "%Y-%m-%j %H:%M")),
         "type" = "solinst") %>%
  add_WYD(., "datetime") %>% 
  as.data.frame() %>% 
  select(site, datetime, type, -year, -mon, -hour, level:temp_C, DOY, WY, DOWY)

## Make Hourly dataset: baro
raw_hrly_baro<-raw_updated %>% filter(type=="baro") %>% 
  mutate(year = year(datetime),
         mon = month(datetime),
         hour = hour(datetime)) %>% 
  group_by(site, year, mon, DOY, hour) %>%
  dplyr::summarize(
    "level"=mean(level,na.rm=TRUE),
    "temp_C"=mean(temp_C,na.rm=TRUE))

# some reason this same script breaks here and I don't know why...
# works if I run it separate of the previous pipe
raw_hrly_baro$datetime <- ymd_hms(strptime(paste0(raw_hrly_baro$year,"-",
                                                  raw_hrly_baro$mon,"-", 
                                                  raw_hrly_baro$DOY, " ",
                                                  raw_hrly_baro$hour,":00"),
                                           format = "%Y-%m-%j %H:%M"))

# now re-add the other stuff and select cols
raw_hrly_baro <- add_WYD(raw_hrly_baro, "datetime") %>% 
  mutate(type="baro") %>% 
  as.data.frame() %>% 
  select(site, datetime, type, 
         -year, -mon, -hour, level:temp_C, DOY, WY, DOWY) 

# bind the two hourly datasets
hrly_to_add <- bind_rows(raw_hrly, raw_hrly_baro) %>% 
  mutate(compensated="N")

# HOURLY: NEW DATA PLOTS -------------------------------------------------
# # Temp
# ggplot()+
#   geom_line(data=hrly_to_add[hrly_to_add$type=="solinst",], aes(datetime,temp_C, color=site, group=WY))+
#   facet_grid(site~., scales = "free_y") + 
#   theme_bw()+ggtitle("Avg Hourly Water Temperature (C)")
#         
# # Hourly Stage
# ggplot()+
#   geom_line(data=hrly_to_add[hrly_to_add$type=="solinst",],aes(datetime,level, color=site, group=WY))+
#   facet_grid(site~., scales = "free_y") + 
#   theme_bw()+ggtitle("Avg Hourly Stage (m)")
# 
# 
# HOURLY: JOIN WITH MASTER HOURLY DATA ------------------------------------

summary(hrly_to_add)
summary(hrly2)

names(hrly_to_add)
names(hrly2)
hr.df <- bind_rows(hrly2, hrly_to_add)
glimpse(hr.df)

# convert back to factors
fxs <- c("site", "compensated", "type")
hr.df[fxs] <- lapply(hr.df[fxs], as.factor) 
str(hr.df)
summary(hr.df)

# need to fix baro for NFA and NFY

# NFA
hr.df$level[which(hr.df$site=="NFA" & hr.df$type=="baro" & hr.df$level>4)] <- hr.df$level[which(hr.df$site=="NFA" & hr.df$type=="baro" & hr.df$level>4)]-9

# plot NFA
filter(hr.df, type=="baro", site=="NFA") %>%  ggplot()+geom_line(aes(x=datetime, y=level, color=site, group=WY))
hr.df$level[which(hr.df$site=="NFA" & hr.df$type=="baro" & hr.df$level>3)] <- hr.df$level[which(hr.df$site=="NFA" & hr.df$type=="baro" & hr.df$level>3)]-3
# plot NFA
filter(hr.df, type=="baro", site=="NFA") %>%  ggplot()+geom_line(aes(x=datetime, y=level, color=site, group=WY))

# NFY
hr.df$level[which(hr.df$site=="NFY" & hr.df$type=="baro")] <- hr.df$level[which(hr.df$site=="NFY" & hr.df$type=="baro")]-9

# plot NFY
filter(hr.df, type=="baro", site=="NFY") %>%  ggplot()+geom_line(aes(x=datetime, y=level, color=site, group=WY))

#save(hr.df, file="data/2011-2016_solinst_mainstem_hrly.rda")

# quick plot of hrly all
ggplot() + 
  geom_line(data=hr.df[hr.df$type=="solinst",], 
            aes(x=datetime, y=level, color=site, group=WY)) +
  facet_grid(site~., scales = "free_y")



# HOURLY: CLEAN ENVIRONMENT -----------------------------------------------

rm(hrly_to_add, hrly2, mfa, nfa, nfa_baro, nfy, nfy_baro, raw_hrly, raw_updated, raw_hrly_baro, sfy)

# DAILY: MAKE DATASET ----------------------------------------------

# For SOLINST-RIVER
require(caTools)
dy.sol.df<-hr.df %>%
  filter(type=="solinst") %>% 
  mutate(year = year(datetime),
         mon = month(datetime)) %>% 
  group_by(site, year, mon, DOY)%>%
  dplyr::summarize("lev_avg"=mean(level,na.rm=TRUE),
                   "lev_min"=min(level,na.rm=TRUE),
                   "lev_max"=max(level,na.rm=TRUE),
                   "temp_avg"=mean(temp_C,na.rm=TRUE),
                   "temp_min"=min(temp_C,na.rm=TRUE),
                   "temp_max"=max(temp_C,na.rm=TRUE)) %>%
  mutate("lev_7_avg"= runmean(lev_avg, k=7, endrule="mean",align="center"),
         "lev_7_min"= runmin(lev_min, k=7, align="center"),
         "lev_7_max"= runmax(lev_max, k=7, align="center"),
         "temp_7_avg"= runmean(temp_avg, k=7, endrule="mean",align="center"),
         "temp_7_min"= runmin(temp_min, k=7, align="center"),
         "temp_7_max"= runmax(temp_max, k=7, align="center")) %>%
  mutate("datetime"=ymd(strptime(paste0(year,"-", mon,"-", DOY),
                                 format = "%Y-%m-%j"))) %>%
  add_WYD(., "datetime") %>%
  mutate(type="solinst") %>% 
  as.data.frame() %>% 
  select(site, datetime, type, -year, -mon, lev_avg:temp_7_max, DOY, WY, DOWY)

# For SOLINST-BAROS

dy.baro.df<-hr.df %>%
  filter(type=="baro") %>% 
  mutate(year = year(datetime),
         mon = month(datetime)) %>% 
  group_by(site, year, mon, DOY)%>%
  dplyr::summarize("lev_avg"=mean(level,na.rm=TRUE),
                   "lev_min"=min(level,na.rm=TRUE),
                   "lev_max"=max(level,na.rm=TRUE),
                   "temp_avg"=mean(temp_C,na.rm=TRUE),
                   "temp_min"=min(temp_C,na.rm=TRUE),
                   "temp_max"=max(temp_C,na.rm=TRUE)) %>%
  mutate("lev_7_avg"= runmean(lev_avg, k=7, endrule="mean",align="center"),
         "lev_7_min"= runmin(lev_min, k=7, align="center"),
         "lev_7_max"= runmax(lev_max, k=7, align="center"),
         "temp_7_avg"= runmean(temp_avg, k=7, endrule="mean",align="center"),
         "temp_7_min"= runmin(temp_min, k=7, align="center"),
         "temp_7_max"= runmax(temp_max, k=7, align="center")) %>%
  mutate("datetime"=ymd(strptime(paste0(year,"-", mon,"-", DOY),
                                 format = "%Y-%m-%j"))) %>%
  add_WYD(., "datetime") %>%
  mutate(type="baro") %>% 
  as.data.frame() %>% 
  select(site, datetime, type, -year, -mon, lev_avg:temp_7_max, DOY, WY, DOWY)

# Combine
# bind the two hourly datasets
dy.df <- bind_rows(dy.sol.df, dy.baro.df)

# remove temp df
rm(dy.sol.df, dy.baro.df)

# write out data
#save(dy.df, file="data/2011-2016_solinst_mainstem_daily.rda")

# DAILY: PLOTS ---------------------------------------------------

# Stage: Daily Avg
ggplot() + 
  geom_line(data=dy.df[dy.df$type=="solinst",], 
            aes(x=datetime, y=lev_avg, color=site, group=WY)) +
  facet_grid(site~., scales = "free_y")

# Stage: 7-Day Avg
ggplot() + 
  geom_line(data=dy.df[dy.df$type=="solinst",], 
            aes(x=datetime, y=lev_7_avg, color=site, group=WY)) +
  facet_grid(site~., scales = "free_y")

# WTemp: Daily Avg
ggplot() + 
  geom_line(data=dy.df[dy.df$type=="solinst",], 
            aes(x=datetime, y=temp_avg, color=site, group=WY)) +
  facet_grid(site~., scales = "free_y")

# WTemp: 7-Day Avg
ggplot() + 
  geom_line(data=dy.df[dy.df$type=="solinst",], 
            aes(x=datetime, y=temp_7_avg, color=site, group=WY)) +
  geom_ribbon(data=dy.df, aes(x=datetime, ymin=10,ymax=12), fill="orange", alpha=0.4) +
  facet_grid(site~.)

# plot a single SITE
ggplot() + 
  geom_path(data=dy.df[dy.df$site=="NFA" & dy.df$type=="solinst",], aes(x=datetime, y=lev_7_avg, group=WY), color="maroon")


