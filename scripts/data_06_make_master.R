# Make Master Data Set

# Sun Apr 23 22:13:11 2017 ------------------------------

library(tidyverse)
library(lubridate)
source("scripts/functions/f_doy.R") # for adding WY info

# LOAD DATA ---------------------------------------------------------------

load("data/2011-2016_solinst_mainstem_hrly_compensated.rda")
#load("data/wunder/RUB_KCAFORES14_2010-10-01_2016-09-30.rda")
#load("data/wunder/NFY_KCADOWNI2_2010-10-01_2017-04-19.rda")
load("data/cdec_sites_daily_ppt_air_2010_2017.rda")

# HOURLY ------------------------------------------------------------------

# quick plot of hrly all
ggplot() + 
  geom_line(data=hr.df2, 
            aes(x=datetime, y=level_comp, color=site, group=WY)) +
  facet_grid(site~., scales = "free_y")

# plot a single SITE
ggplot() + 
  geom_path(data=hr.df2[hr.df2$site=="NFY" & hr.df2$WY>2014,], aes(x=datetime, y=level, group=WY), color="dodgerblue") +
  geom_path(data=hr.df2[hr.df2$site=="NFY" & hr.df2$WY>2014,], aes(x=datetime, y=level_comp, group=WY), color="black")


# CREATE DAILY FROM THE ADJ HOURLY DATA -----------------------------------

library(RcppRoll) # rolling means/max/mins
#library(padr) # used to pad for continuous distrib of time/vector

# For SOLINST-RIVER: Make a Daily dataset
dy.sol.df<-hr.df2 %>% mutate(date=floor_date(datetime, unit = "day")) %>% 
  #pad(by = "date", group = "site") %>% 
  group_by(site, date)  %>% 
  dplyr::summarize("lev_avg"=mean(level_comp,na.rm=TRUE),
                   "lev_min"=min(level_comp,na.rm=TRUE),
                   "lev_max"=max(level_comp,na.rm=TRUE),
                   "temp_avg"=mean(temp_C,na.rm=TRUE),
                   "temp_min"=min(temp_C,na.rm=TRUE),
                   "temp_max"=max(temp_C,na.rm=TRUE),
                   "W_air_avg"=mean(W_airtempC, na.rm=TRUE),
                   "W_air_min"=min(W_airtempC, na.rm=TRUE),
                   "W_air_max"=max(W_airtempC, na.rm=TRUE),
                   "W_wind_avg"=mean(W_wind_mph, na.rm=TRUE),
                   "W_humidity_avg"=mean(W_humidity_prcnt, na.rm=TRUE)) %>%
  mutate("lev_7_avg"=roll_meanr(lev_avg, n=7, fill=NA),
         "lev_7_min"= roll_minr(lev_min, n=7, fill=NA),
         "lev_7_max"= roll_maxr(lev_max, n=7, fill=NA),
         "temp_7_avg"= roll_meanr(temp_avg, n=7, fill=NA),
         "temp_7_min"= roll_minr(temp_min, n=7, fill=NA),
         "temp_7_max"= roll_maxr(temp_max, n=7, fill=NA),
         "W_air_7_avg"=roll_meanr(W_air_avg, n=7, fill=NA),
         "W_air_7_min"=roll_minr(W_air_min, n=7, fill=NA),
         "W_air_7_max"=roll_maxr(W_air_max, n=7, fill=NA)) %>% 
  add_WYD(., "date") %>%
  arrange(site, date) %>% as.data.frame()

# CDEC AIR_PPT DAILY & 7-DAY ----------------------------------------------

### NOT WORKING
# Add 7-day averages:
cdec.dy <- cdec_ppt_air %>%
  filter(!is.na(CDEC_air_C),
         !is.na(CDEC_ppt_mm)) %>% 
  group_by(site, date) %>%
  mutate("ppt_7_avg_mm"= roll_meanr(CDEC_ppt_mm, n=7, fill=NA),
         "CDEC_air_7_avg"= roll_meanr(CDEC_air_C, n=7, fill=NA),
         "CDEC_air_7_min"= roll_minr(CDEC_air_C, n=7, fill=NA),
         "CDEC_air_7_max"= roll_maxr(CDEC_air_C, n=7, fill=NA))# %>%
  add_WYD(., "date") %>%
  arrange(site, date) %>% as.data.frame()

# DAILY -------------------------------------------------------------------

# Stage: Daily Avg
ggplot() + 
  geom_line(data=dy.sol.df, 
            aes(x=date, y=lev_avg, color=site, group=WY)) +
  facet_grid(site~., scales = "free_y")

# Stage: 7-Day Avg
ggplot() + 
  geom_line(data=dy.sol.df, 
            aes(x=date, y=lev_7_avg, color=site, group=WY)) +
  facet_grid(site~., scales = "free_y")

# WTemp: Daily Avg
ggplot() + 
  geom_line(data=dy.sol.df, 
            aes(x=date, y=temp_avg, color=site, group=WY)) +
  facet_grid(site~., scales = "free_y")

# WTemp: 7-Day Avg
ggplot() + 
  geom_line(data=dy.sol.df[dy.sol.df$WY==2011,], 
            aes(x=date, y=temp_7_avg, color=site, group=WY)) +
  geom_ribbon(data=dy.sol.df[dy.sol.df$WY==2011,], aes(x=date, ymin=10,ymax=12), fill="orange", alpha=0.4) +
  facet_grid(site~.)

# plot a single SITE
ggplot() + 
  geom_path(data=dy.sol.df[dy.sol.df$site=="NFA",], aes(x=date, y=lev_7_avg, group=WY), color="maroon")



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
