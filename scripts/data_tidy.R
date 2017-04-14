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

ggplot() + 
  geom_path(data=hrly2, aes(x=datetime, y=level, color=site, group=WY)) +
  facet_grid(site~., scales = "free_y")

# SITE ONLY
ggplot() + 
  geom_path(data=hrly2[hrly2$site=="MFA",], aes(x=datetime, y=level, color=site, group=WY)) +
  facet_grid(site~., scales = "free_y")

# ADD SITE UPDATES ---------------------------------------------------------

# fix MFA level first (it's 200 and needs adj)
summary(mfa)
mfa$level <- mfa$level - 206

# adj NFA level
summary(nfa)
nfa$level <- nfa$level - 0.27

# adj SFY (drop about 0.5)
summary(sfy)
sfy$level <- sfy$level - 0.53

# adj NFY (drop about 0.5)
summary(nfy)
nfy$level <- nfy$level - 0.6

# test plot
ggplot() + 
  geom_path(data=nfy, aes(x=datetime, y=level, color=site, group=WY)) +
  facet_grid(site~., scales = "free_y")

# combine and rm NA's
raw_updated<-bind_rows(nfa, mfa, sfy, nfy, nfy_baro, nfa_baro)
raw_updated <- raw_updated %>% filter(!is.na(level))

# make some cols factors
fxs <- c("site", "compensated", "type")
raw_updated[fxs] <- lapply(raw_updated[fxs], as.factor) 

# MAKE HOURLY -------------------------------------------------------------

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

# Plot the Hourly Temperature
ggplot()+
  geom_line(data=hrly_to_add[hrly_to_add$type=="solinst",], aes(datetime,temp_C, color=site, group=WY))+
  facet_grid(site~., scales = "free_y") + 
  theme_bw()+ggtitle("Avg Hourly Water Temperature (C)")
        
# Plot the Hourly Stage
ggplot()+
  geom_line(data=hrly_to_add[hrly_to_add$type=="solinst",],aes(datetime,level, color=site, group=WY))+
  facet_grid(site~., scales = "free_y") + 
  theme_bw()+ggtitle("Avg Hourly Stage (m)")


# JOIN WITH MASTER DATA ---------------------------------------------------

summary(hrly_to_add)
summary(hrly2)

names(hrly_to_add)
names(hrly2)
fulldf <- bind_rows(hrly2, hrly_to_add)
glimpse(fulldf)

# convert back to factors
fxs <- c("site", "compensated", "type")
fulldf[fxs] <- lapply(fulldf[fxs], as.factor) 
str(fulldf)
summary(fulldf)

# quick plot of hrly all
ggplot() + 
  geom_line(data=fulldf[fulldf$type=="solinst",], 
            aes(x=datetime, y=level, color=site, group=WY)) +
  facet_grid(site~., scales = "free_y")


# MAKE A DAILY DATASET ----------------------------------------------------

## Make Daily dataset
require(caTools)
df.dy<-df %>%
  group_by(site, year, mon, yday)%>%
  dplyr::summarize("lev_avg"=mean(Level,na.rm=TRUE),
                   "lev_min"=min(Level,na.rm=TRUE),
                   "lev_max"=max(Level,na.rm=TRUE),
                   "temp_avg"=mean(Temperature,na.rm=TRUE),
                   "temp_min"=min(Temperature,na.rm=TRUE),
                   "temp_max"=max(Temperature,na.rm=TRUE)) %>%
  mutate("lev_7_avg"= runmean(lev.avg, k=7, endrule="mean",align="center"),
         "lev_7_min"= runmin(lev.min, k=7, align="center"),
         "lev_7_max"= runmax(lev.max, k=7, align="center"),
         "temp_7_avg"= runmean(temp.avg, k=7, endrule="mean",align="center"),
         "temp_7_min"= runmin(temp.min, k=7, align="center"),
         "temp_7_max"= runmax(temp.max, k=7, align="center")) %>%
  mutate("datetime"=ymd(strptime(paste0(year,"-", mon,"-", yday),
                                 format = "%Y-%m-%j"))) %>%
  select(datetime,year,mon,yday,lev_avg:temp_7_max) %>%  
  as.data.frame()

s(df.dy)