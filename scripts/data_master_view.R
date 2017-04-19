# master_data_view

# Tue Apr 18 16:34:55 2017 ------------------------------

# just load and look at most current master data set. Built off
# data_load.R & data_tidy.R
library(tidyverse)
library(lubridate)

# LOAD DATA ---------------------------------------------------------------

load("data/2011-2016_solinst_mainstem_hrly.rda")
load("data/2011-2016_solinst_mainstem_daily.rda")
load("data/wunder/RUB_KCAFORES14_2010-10-01_2016-09-30.rda")
load("data/NFA_gamebaro_hrly_2011-2016.rda")
load("data/NFY_gamebaro_hrly_2011-2017.rda")

# HOURLY ------------------------------------------------------------------

# quick plot of hrly all
ggplot() + 
  geom_line(data=hr.df[hr.df$type=="solinst",], 
            aes(x=datetime, y=level, color=site, group=WY)) +
  facet_grid(site~., scales = "free_y")

# plot all baro
ggplot() + 
  geom_line(data=hr.df[hr.df$type=="baro",], 
            aes(x=datetime, y=level, color=site, group=WY)) +
  facet_grid(site~., scales = "free_y")

# plot a single SITE
ggplot() + 
  geom_path(data=hr.df[hr.df$site=="SFY" & hr.df$type=="solinst" & hr.df$WY>2014,], aes(x=datetime, y=level, group=WY), color="maroon")

# just look at barologger data from NFY/NFA
filter(hr.df, type=="baro", site=="NFY", month(datetime)==4 ) %>%  ggplot()+geom_line(aes(x=datetime, y=level, color=site, group=WY)) + geom_line(data=KCAFORES14_hr[KCAFORES14_hr$year>2015 & KCAFORES14_hr$mon==4,], aes(x=datetime, y=PressureIn*0.3453-9.8),color="darkgreen", alpha=0.6)

filter(hr.df, type=="baro", site=="NFA") %>%  ggplot()+geom_line(aes(x=datetime, y=level, color=site, group=WY))

# or just game baro plot
ggplot() + geom_line(data=nfa_gb_all, 
                     aes(x=datetime, y=baropressure))

ggplot() + geom_line(data=nfy_gb_all, aes(x=datetime, y=baropressure*0.3453-9))+
  geom_line(data=hr.df[hr.df$type=="baro" & hr.df$site=="NFY",], aes(x=datetime, y=level), color="blue")


# DAILY -------------------------------------------------------------------

# Baro: 

filter(dy.df, type=="baro", site=="NFY") %>%  ggplot()+geom_line(aes(x=datetime, y=level, color=site, group=WY))

filter(dy.df, type=="baro", site=="NFA") %>%  ggplot()+geom_line(aes(x=datetime, y=level, color=site, group=WY))

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



# ADD WATER YEAR INFO -----------------------------------------------------

# add WY Index
wys<-read_csv(file = "data/cdec_wy_index.csv") %>% filter(Basin=="SAC") %>% as.data.frame()

# JOIN FROG DAT WITH HYDRO DAT --------------------------------------------

frogBreed <- read_csv("data/oviposition_start_mainstem_sites.csv") %>% 
  mutate(estim_strt=mdy(estim_strt),
         obs_strt=mdy(obs_strt)) %>%
  select(Site:WYT, REG:totalEM)


data1 <- left_join(hydroDat, frogBreed, by = c("Date"="estim_strt", "site"="Site")) %>% 
  mutate(site = as.factor(site))
data1 <- add_WYD(data1, "Date") %>% select(-WYT) %>% 
  mutate(DOY = as.integer(DOY),
         WY = as.integer(WY),
         DOWY = as.integer(DOWY))

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
