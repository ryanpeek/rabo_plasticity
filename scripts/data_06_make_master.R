# Make Master Data Set

# Sat May 13 16:42:07 2017 ------------------------------

library(tidyverse)
library(lubridate)
library(caTools)

# FUNCTIONS ---------------------------------------------------------------

source("scripts/functions/f_moving_average.R")

source("scripts/functions/f_doy.R") # for adding WY info

# C.Var(x) - calc coefficient of variation around the mean
C.Var <- function(x, rmNA=T) {
  if(rmNA){
    (100*sd(x, na.rm =T)/mean(x, na.rm=T))}
  else{
    (100*sd(x)/mean(x))
  }
}

cumul_zeros <- function(x)  {
  x <- !x
  rl <- rle(x)
  len <- rl$lengths
  v <- rl$values
  cumLen <- cumsum(len)
  z <- x
  # replace the 0 at the end of each zero-block in z by the 
  # negative of the length of the preceding 1-block....
  iDrops <- c(0, diff(v)) < 0
  z[ cumLen[ iDrops ] ] <- -len[ c(iDrops[-1],FALSE) ]
  # ... to ensure that the cumsum below does the right thing.
  # We zap the cumsum with x so only the cumsums for the 1-blocks survive:
  x*cumsum(z)
}


# LOAD DATA ---------------------------------------------------------------

load("data/2011-2016_solinst_mainstem_hrly_compensated.rda")
load("data/cdec_sites_daily_ppt_air_2010_2017.rda")
load("data/daily_flow_cfs_data_6sites.rda")

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

# library(caTools) # rolling means/max/mins

# For SOLINST-RIVER: Make a Daily dataset
dy.sol.df<-hr.df2 %>% mutate(date=floor_date(datetime, unit = "day")) %>% 
  group_by(site, date)  %>% 
  dplyr::summarize("lev_avg"=mean(level_comp,na.rm=TRUE),
                   "lev_CV" = C.Var(level_comp, rmNA = T),
                   "temp_avg"=mean(temp_C,na.rm=TRUE),
                   "temp_min"=min(temp_C,na.rm=TRUE),
                   "temp_max"=max(temp_C,na.rm=TRUE),
                   "temp_CV" = C.Var(abs(temp_C), rmNA=T),
                   "W_air_avg"=mean(W_airtempC, na.rm=TRUE),
                   "W_air_min"=min(W_airtempC, na.rm=TRUE),
                   "W_air_max"=max(W_airtempC, na.rm=TRUE),
                   "W_wind_avg"=mean(W_wind_mph, na.rm=TRUE),
                   "W_humidity_avg"=mean(W_humidity_prcnt, na.rm=TRUE)) %>%
  mutate("lev_7_avg"=runmean(lev_avg, k=7,align="right"),
         "lev_30_avg"=runmean(lev_avg, k=30, endrule = "NA", align="right"),
         "temp_7_avg"= runmean(temp_avg, k=7, align="right"),
         "temp_7_max"=runmax(temp_avg, k=7, align="right"),
         "temp_7_min"=runmin(temp_avg, k=7, align="right"),
         "temp_30_max"=runmax(temp_avg, k=30, endrule = "NA", align="right"),
         "temp_30_min"=runmin(temp_avg, k=30, endrule = "NA", align="right"),
         "W_air_7_avg"=runmean(W_air_avg, k=7, align="right"),
         "W_air_30_max"=runmax(W_air_avg, k=30, align="right")) %>%
  add_WYD(., "date") %>%
  mutate(date=ymd(date)) %>% 
  arrange(site, date) %>% as.data.frame()
summary(dy.sol.df)

# TEST PLOTS
# tst <-filter(dy.sol.df, site=="NFA")
# 
# ggplot() + geom_line(data=tst, aes(x=DOWY, y=temp_30_max), color="maroon") +
#   geom_line(data=tst, aes(x=DOWY, y=temp_30_min), color="blue") +
#   geom_line(data=tst, aes(x=DOWY, y=temp_7_avg), color="black", lty=2) +
#   geom_line(data=tst, aes(x=DOWY, y=temp_CV), color="orange", alpha=0.7) +
#  # ylim(0,50)+
#   facet_grid(WY~., scales = "free_x")
# 
# # lev_CV by WY
# ggplot() + geom_line(data=dy.sol.df, aes(x=DOWY, y=lev_CV, color=site)) +
#   facet_grid(WY~., scales = "free_x") + 
#   scale_x_continuous(breaks=c(1, 32, 62, 93, 121, 
#                               152, 182, 213, 243, 
#                               274, 305, 336),
#                      labels=c("Oct", "Nov", "Dec", "Jan", "Feb",
#                               "Mar", "Apr","May","Jun",
#                               "Jul","Aug","Sep")) +
#   geom_ribbon(data=dy.sol.df[dy.sol.df$DOWY>182 & dy.sol.df$DOWY<273,], aes(x=DOWY, ymin=0, ymax=60), fill="forestgreen", alpha=0.3)
# 
# # CV by site & WY
# ggplot() + geom_line(data=dy.sol.df, aes(x=DOWY, y=temp_CV, color=site)) +
#   facet_grid(WY~site, scales = "free_x") + 
#   scale_x_continuous(breaks=c(1, 32, 62, 93, 121, 
#                               152, 182, 213, 243, 
#                               274, 305, 336),
#                      labels=c("Oct", "Nov", "Dec", "Jan", "Feb",
#                               "Mar", "Apr","May","Jun",
#                               "Jul","Aug","Sep")) +
#   geom_ribbon(data=dy.sol.df[dy.sol.df$DOWY>182 & dy.sol.df$DOWY<273,], aes(x=DOWY, ymin=0, ymax=60), fill="forestgreen", alpha=0.3)
# 
# 
# ggplot() + geom_line(data=dy.sol.df, aes(x=date, y=lev_CV, color=site)) +
#   facet_grid(site~., scales = "free_x") + 
#   scale_x_date(date_breaks="3 months", date_labels = "%b")
# 
# ggplot() + geom_line(data=dy.sol.df, aes(x=date, y=temp_CV, color=site)) +
#   facet_grid(site~., scales = "free") + 
#   scale_x_date(date_breaks="3 months", date_labels = "%b")
# 


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

ggplot() + geom_line(data=tst, aes(x=DOWY, y=max30), color="maroon") +
  geom_line(data=tst, aes(x=DOWY, y=min30), color="blue") +
  geom_line(data=tst, aes(x=DOWY, y=mean30), color="black") +
  geom_line(data=tst, aes(x=DOWY, y=mad30), color="orange", lwd=1.1, lty=2) +
  facet_grid(WY~., scales = "free")



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
# ggplot() + 
#   geom_line(data=cdec[cdec$site=="NFA" & year(cdec$date)==2014,], 
#             aes(x=date, y=CDEC_air_7_avg), col="black")+
#   geom_line(data=cdec[cdec$site=="NFA" & year(cdec$date)==2014,], 
#             aes(x=date, y=CDEC_air_30_avg), col="maroon", lty=4, lwd=1.2,
#             alpha=0.8)+ xlab("") +  
#   geom_bar(data=cdec[cdec$site=="NFA" & year(cdec$date)==2014,], 
#                        aes(x=date, y=CDEC_ppt_mm/10), stat="identity", 
#                        alpha=0.5, fill="blue") 

# MAKE MASTER DAT ---------------------------------------------------------

master_dat1 <- left_join(dy.sol.df, cdec, by = c("date", "site"))
summary(master_dat1)

# FILTER TO SPRING MONTHS -------------------------------------------------

# selected_mons <- c(4,5,6,7)
# master_dat1 <- master_dat1 %>% filter(month(date) %in% selected_mons)

# library(viridis)
# 
# # 7 Day Water Temps NFA
# ggplot() + 
#   geom_line(data=master_dat1[master_dat1$site=="NFA" & master_dat1$WY==2014,], aes(x=date, y=temp_7_min, group=WY), color="blue")+
#   geom_line(data=master_dat1[master_dat1$site=="NFA" & master_dat1$WY==2014,], aes(x=date, y=temp_7_max, group=WY),color="red")+
#   geom_line(data=master_dat1[master_dat1$site=="NFA" & master_dat1$WY==2014,], aes(x=date, y=temp_7_avg, group=WY),color="black", lty=2)+
#   xlab("") + 
#   geom_line(data=master_dat1[master_dat1$site=="NFA" & master_dat1$WY==2014,], aes(x=date, y=temp_30_avg, group=WY),color="forestgreen", lty=1, alpha=0.9)+
#   xlab("") + geom_line(data=master_dat1[master_dat1$site=="NFA" & master_dat1$WY==2014,], aes(x=date, y=temp_30_max, group=WY),color="green2", lty=1, alpha=0.9)+
#   xlab("") +
#   geom_line(data=master_dat1[master_dat1$site=="NFA" & master_dat1$WY==2014,], aes(x=date, y=temp_30_min, group=WY),color="lightgreen", lty=1, alpha=0.9)+
#   xlab("")

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
  filter(Basin=="SAC", WY>2010) %>% 
  select(WY, `Apr-Jul`, WYsum, Index) %>% 
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
  geom_point(data=master_df[!is.na(master_df$missData),], aes(x=date, y=temp_7_avg, fill=missData), pch=21, size=4, col="gray30") + 
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



# ADD FLOW DATA -----------------------------------------------------------

# updated and merged flows:
#load("data/daily_flow_cfs_data_6sites.rda")

