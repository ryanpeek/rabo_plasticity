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
dy.sol.df2<-hr.df2 %>% mutate(date=floor_date(datetime, unit = "day")) %>% 
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
         "temp_7_avg"= movingAverage(temp_avg, n=7, centered=F),
         "temp_7_min"= movingAverage(temp_min, n=7, centered=F),
         "temp_7_max"= movingAverage(temp_max, n=7, centered=F),
         "W_air_7_avg"=movingAverage(W_air_avg, n=7, centered=F),
         "W_air_7_min"=movingAverage(W_air_min, n=7, centered=F),
         "W_air_7_max"=movingAverage(W_air_max, n=7, centered=F)) %>% 
  add_WYD(., "date") %>%
  mutate(date=ymd(date)) %>% 
  arrange(site, date) %>% as.data.frame()

master_dat2 <- left_join(dy.sol.df, cdec_ppt_air[, c(1:4,8)], by = c("date", "site"))
summary(master_dat2)
summary(master_dat)

# ADD THE CDEC DATA -------------------------------------------------------

# this version doesn't fill gaps:
#mutate("CDEC_air_7_avg"= stats::filter(CDEC_air_C, rep(1/7, 7), sides=1))
#Cumulative zeros function
f7 <- function(x){ tmp<-cumsum(x);tmp-cummax((!x)*tmp)} # RUN AS f7(!x) # doesn't work with NAs


cdec<- cdec_ppt_air %>%  select(station, site, date:CDEC_ppt_mm) %>%
  mutate(CDEC_air_C = round(CDEC_air_C, 2),
         CDEC_ppt_mm = ifelse(is.na(CDEC_ppt_mm), 0, CDEC_ppt_mm)) %>% 
  group_by(site) %>%
  mutate("CDEC_air_7_avg"= movingAverage(CDEC_air_C, n=7, centered=F),
         "days_no_ppt" = f7(!CDEC_ppt_mm)) %>% 
  arrange(site,date) %>% as.data.frame() 

#cdec$days_no_ppt=(!cdec$CDEC_ppt_mm) * unlist(lapply(rle(cdec$CDEC_ppt_mm)$lengths, seq_len))
summary(cdec)
  
# quick plot
ggplot() + geom_line(data=cdec[cdec$site=="NFA" & year(cdec$date)==2014,], aes(x=date, y=CDEC_air_7_avg), col="black")+
  geom_line(data=cdec[cdec$site=="NFA" & year(cdec$date)==2014,], aes(x=date, y=CDEC_air_C), col="maroon", lty=2)+ xlab("") +   
#  geom_bar(data=cdec[cdec$site=="NFA" & year(cdec$date)==2014,], aes(x=date, y=days_no_ppt/10), stat="identity", alpha=0.5) + 
geom_bar(data=cdec[cdec$site=="NFA" & year(cdec$date)==2014,], aes(x=date, y=CDEC_ppt_mm/10), stat="identity", alpha=0.5, fill="blue") 

master_dat <- left_join(dy.sol.df, cdec_ppt_air[, c(1:4,8)], by = c("date", "site"))
summary(master_dat)

# FILTER TO SPRING MONTHS -------------------------------------------------
selected_mons <- c(5,6,7)
master_dat <- master_dat %>% filter(month(date) %in% selected_mons)
library(viridis)

ggplot() + 
  geom_line(data=master_dat[master_dat$site=="NFA" & master_dat$WY==2014,], aes(x=date, y=temp_7_min, group=WY), color="blue")+
  geom_line(data=master_dat[master_dat$site=="NFA" & master_dat$WY==2014,], aes(x=date, y=temp_7_max, group=WY),color="red")+
  geom_line(data=master_dat[master_dat$site=="NFA" & master_dat$WY==2014,], aes(x=date, y=temp_7_avg, group=WY),color="black", lty=2)+
  geom_line(data=master_dat[master_dat2$site=="NFA" & master_dat2$WY==2014,], aes(x=date, y=temp_7_max, group=WY),color="purple")+
  xlab("") + scale_color_viridis()

# ADD 7-Day to CDEC -------------------------------------------------------

# Add 7-day averages:
master_df <- master_dat %>%
  group_by(site, date) %>%
  mutate("ppt_7_avg_mm"= roll_meanr(CDEC_ppt_mm, n=7, fill=NA, na.rm = TRUE),
         "CDEC_air_7_avg"= roll_meanr(CDEC_air_C, n=7, fill=NA),
         "CDEC_air_7_min"= roll_minr(CDEC_air_C, n=7, fill=NA),
         "CDEC_air_7_max"= roll_maxr(CDEC_air_C, n=7, fill=NA)) %>%
  arrange(site, date) %>% as.data.frame()
summary(master_df)


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
