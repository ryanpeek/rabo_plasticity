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
  geom_path(data=hr.df2[hr.df2$site=="NFY" & hr.df2$WY>2014,], aes(x=datetime, y=level, group=WY), color="dodgerblue") + # uncompensated
  geom_path(data=hr.df2[hr.df2$site=="NFY" & hr.df2$WY>2014,], aes(x=datetime, y=level_comp, group=WY), color="black") # compensated



# DAILY AVG WITH caTools FROM THE ADJ HOURLY DATA ------------------------

library(caTools) # rolling means/max/mins

# For SOLINST-RIVER: Make a Daily dataset
dy.sol.df<-hr.df2 %>% mutate(date=floor_date(datetime, unit = "day")) %>% 
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
  mutate("lev_7_avg"=runmean(lev_avg, k=7, endrule="mean",align="right"),
         "lev_7_min"= runmean(lev_min, k=7, endrule="mean",align="right"),
         "lev_7_max"= runmean(lev_max, k=7, endrule="mean",align="right"),
         "temp_7_avg"= runmean(temp_avg, k=7, endrule="mean",align="right"),
         "temp_7_min"= runmean(temp_min, k=7, endrule="mean",align="right"),
         "temp_7_max"= runmean(temp_max, k=7, endrule="mean",align="right"),
         "W_air_7_avg"=runmean(W_air_avg, k=7, endrule="mean",align="right"),
         "W_air_7_min"=runmean(W_air_min, k=7, endrule="mean",align="right"),
         "W_air_7_max"=runmean(W_air_max, k=7, endrule="mean",align="right")) %>% 
  add_WYD(., "date") %>%
  mutate(date=ymd(date)) %>% 
  arrange(site, date) %>% as.data.frame()
summary(dy.sol.df)



# DAILY AVG WITH function FROM THE ADJ HOURLY DATA ------------------------

source("scripts/functions/f_moving_average.R")

# For SOLINST-RIVER: Make a Daily dataset
dy.sol.df<-hr.df2 %>% mutate(date=floor_date(datetime, unit = "day")) %>% 
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
  mutate("lev_7_avg"=movingAverage(lev_avg, n = 7, centered = F),
         "lev_7_min"= movingAverage(lev_min, n=7, centered=F),
         "lev_7_max"= movingAverage(lev_max, n=7, centered=F),
         "lev_30_avg"= movingAverage(lev_avg, n=30, centered=F),
         "temp_7_avg"= movingAverage(temp_avg, n=7, centered=F),
         "temp_7_min"= movingAverage(temp_min, n=7, centered=F),
         "temp_7_max"= movingAverage(temp_max, n=7, centered=F),
         "temp_30_avg"= movingAverage(temp_avg, n=30, centered=F),
         "temp_30_min"= movingAverage(temp_min, n=30, centered=F),
         "temp_30_max"= movingAverage(temp_max, n=30, centered=F),
         "W_air_7_avg"=movingAverage(W_air_avg, n=7, centered=F),
         "W_air_7_min"=movingAverage(W_air_min, n=7, centered=F),
         "W_air_7_max"=movingAverage(W_air_max, n=7, centered=F)) %>% 
  add_WYD(., "date") %>%
  mutate(date=ymd(date)) %>% 
  arrange(site, date) %>% as.data.frame()

# ADJ THE CDEC DATA -------------------------------------------------------

# this version doesn't fill gaps:
#mutate("CDEC_air_7_avg"= stats::filter(CDEC_air_C, rep(1/7, 7), sides=1))

# Cumulative zeros function
f7 <- function(x){ tmp<-cumsum(x);tmp-cummax((!x)*tmp)} # RUN AS f7(!x) # doesn't work with NAs

cdec<- cdec_ppt_air %>%  select(station, site, date:CDEC_ppt_mm) %>%
  mutate(CDEC_air_C = round(CDEC_air_C, 2),
         CDEC_ppt_mm = ifelse(is.na(CDEC_ppt_mm), 0, CDEC_ppt_mm)) %>% 
  group_by(site) %>%
  mutate("CDEC_air_7_avg"= movingAverage(CDEC_air_C, n=7, centered=F),
         "CDEC_air_30_avg"= movingAverage(CDEC_air_C, n=30, centered=F),
         "days_no_ppt" = f7(!CDEC_ppt_mm)) %>% # calc days without rain
  arrange(site,date) %>% as.data.frame() 

summary(cdec)
  
# quick plot
ggplot() + 
  geom_line(data=cdec[cdec$site=="NFA" & year(cdec$date)==2014,], 
            aes(x=date, y=CDEC_air_7_avg), col="black")+
  geom_line(data=cdec[cdec$site=="NFA" & year(cdec$date)==2014,], 
            aes(x=date, y=CDEC_air_30_avg), col="maroon", lty=4, lwd=1.2,
            alpha=0.8)+ xlab("") +  
  geom_bar(data=cdec[cdec$site=="NFA" & year(cdec$date)==2014,], 
                       aes(x=date, y=CDEC_ppt_mm/10), stat="identity", 
                       alpha=0.5, fill="blue") 

# MAKE MASTER DAT ---------------------------------------------------------

master_dat1 <- left_join(dy.sol.df, cdec, by = c("date", "site"))
summary(master_dat1)

# FILTER TO SPRING MONTHS -------------------------------------------------

# selected_mons <- c(4,5,6,7)
master_dat1 <- master_dat1 %>% filter(month(date) %in% selected_mons)

library(viridis)

# 7 Day Water Temps NFA
ggplot() + 
  geom_line(data=master_dat1[master_dat1$site=="NFA" & master_dat1$WY==2014,], aes(x=date, y=temp_7_min, group=WY), color="blue")+
  geom_line(data=master_dat1[master_dat1$site=="NFA" & master_dat1$WY==2014,], aes(x=date, y=temp_7_max, group=WY),color="red")+
  geom_line(data=master_dat1[master_dat1$site=="NFA" & master_dat1$WY==2014,], aes(x=date, y=temp_7_avg, group=WY),color="black", lty=2)+
  xlab("") + 
  geom_line(data=master_dat1[master_dat1$site=="NFA" & master_dat1$WY==2014,], aes(x=date, y=temp_30_avg, group=WY),color="forestgreen", lty=1, alpha=0.9)+
  xlab("") + geom_line(data=master_dat1[master_dat1$site=="NFA" & master_dat1$WY==2014,], aes(x=date, y=temp_30_max, group=WY),color="green2", lty=1, alpha=0.9)+
  xlab("") +
  geom_line(data=master_dat1[master_dat1$site=="NFA" & master_dat1$WY==2014,], aes(x=date, y=temp_30_min, group=WY),color="lightgreen", lty=1, alpha=0.9)+
  xlab("")

# DAILY PLOTS -------------------------------------------------------------

# Stage: Daily Avg
ggplot() + 
  geom_line(data=master_dat1, 
            aes(x=date, y=lev_avg, color=site, group=WY)) +
  facet_grid(site~WY, scales = "free")

# Stage: 7-Day Avg
ggplot() + 
  geom_line(data=master_dat1, 
            aes(x=date, y=lev_7_avg, color=site, group=WY)) +
  facet_grid(site~WY, scales = "free")

# WTemp: Daily Avg
ggplot() + 
  geom_line(data=master_dat1, 
            aes(x=date, y=temp_avg, color=site, group=WY)) +
  facet_grid(site~WY, scales = "free")

# WTemp: 7-Day Avg
ggplot() + 
  geom_line(data=master_dat1, 
            aes(x=date, y=temp_7_avg, color=site, group=WY)) +
  facet_grid(site~WY, scales = "free")

# WTemp: 7-Day Avg w threshold
ggplot() + 
  geom_line(data=master_dat1, 
            aes(x=date, y=temp_7_avg, color=site, group=WY)) +
  geom_ribbon(data=master_dat1, aes(x=date, ymin=10,ymax=12), fill="orange", alpha=0.4) +
  facet_grid(site~WY, scales="free")

# REMOVE OLD DFs ----------------------------------------------------------

rm(dy.sol.df, hr.df2, cdec_ppt_air)

# ADD WATER YEAR INFO -----------------------------------------------------

# add WY Index
wys<-read_csv(file = "data/cdec_wy_index.csv") %>% 
  filter(Basin=="SAC", WY>2010) %>% select(WY, `Apr-Jul`, WYsum, Index) %>% 
  rename(apr_jul = `Apr-Jul`) %>% as.data.frame

# JOIN FROG DAT WITH HYDRO DAT --------------------------------------------

frogBreed <- read_csv("data/oviposition_start_mainstem_sites.csv") %>% 
  mutate(estim_strt=mdy(estim_strt),
         obs_strt=mdy(obs_strt)) %>%
  rename(WY = Year, site=Site) %>% 
  select(site, WY, REG:totalEM)

# join with master dat
master_dat2 <- left_join(master_dat1, frogBreed, by = c("date"="estim_strt", "site", "WY")) %>% 
  mutate(site = as.factor(site))

# join with WY index data
master_df <- inner_join(master_dat2, wys, by="WY")

#master_df <- master_df %>% filter(site!="MFY", site!="MFA")

# quick plot:
# WTemp: 7-Day Avg w threshold
ggplot() + 
  geom_line(data=master_df, 
            aes(x=date, y=temp_7_avg, color=site, group=WY)) +
  geom_ribbon(data=master_df, aes(x=date, ymin=10,ymax=12), fill="orange", alpha=0.4) +
  geom_point(data=master_df[!is.na(master_df$totalEM),], aes(x=date, y=temp_7_avg, fill=site), 
             pch=21,color="gray20",size=4) + 
  facet_grid(site~WY, scales="free")


ggplot() + 
  geom_line(data=master_df, 
            aes(x=date, y=lev_7_avg, color=site, group=WY)) +
  #geom_ribbon(data=master_df, aes(x=date, ymin=10,ymax=12), fill="orange", alpha=0.4) +
  geom_point(data=master_df[!is.na(master_df$totalEM),], aes(x=date, y=lev_7_avg, fill=site), 
             pch=21,color="gray20",size=4) + 
  facet_grid(site~WY, scales="free")

# clean workspace
rm(master_dat1, master_dat2, frogBreed, wys, cdec)


# SAVE --------------------------------------------------------------------

save(master_df, file = "data/master_dat_2011-2016.rda")

