# Combine the Flow Data into master Set
# Sun May 14 23:02:37 2017 ------------------------------

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

# LOAD DATA ---------------------------------------------------------------

# Hourly
load("data/MFA_hv_CDEC_OXB_1997-2017.rda")
load("data/MFY_hv_ORH_YC5_2003_2016.rda")
load("data/SFY_hv_CDEC_JBR_1998-2017.rda")

# IV (15 min)
load("data/NFY_iv_USGS_updated_2017-02-22.rda")
load("data/NFA_iv_USGS_updated_2017-02-22.rda")
load("data/RUB_iv_PCWA_2009-2016.rda")

#DAILY
# load("data/daily_flow_cfs_data_6sites.rda")


# FORMAT TO SAME VARS -----------------------------------------------------

# tidy
MFA_hv <- MFA_hv %>% select(datetime, flow_cfs, DOY:DOWY) %>%
  mutate(site="MFA")

MFY_hv <- mfy_hv %>% select(-flow_cms) %>% 
  mutate(site="MFY")
rm(mfy_hv)

SFY_hv <- SFY_hv %>% select(datetime, flow_cfs, DOY:DOWY) %>%
  mutate(site="SFY")

NFA_iv <- NFA_iv %>% select(datetime, flow_cfs) %>%
  add_WYD(., "datetime") %>% 
  mutate(site="NFA")

NFY_iv <- NFY_iv %>% select(datetime, flow_cfs) %>%
  add_WYD(., "datetime") %>% 
  mutate(site="NFY")

RUB_iv <- RUB_iv %>%
  add_WYD(., "datetime") %>% 
  mutate(site="RUB")

# bind all flow data:
flow_iv <- bind_rows(MFA_hv, NFA_iv, RUB_iv,
                     NFY_iv, MFY_hv, SFY_hv) %>%
  select(site, datetime, flow_cfs, DOY:DOWY)

flow_iv %>% group_by(site) %>% dplyr::count()


# MAKE ALL HOURLY ---------------------------------------------------------

flow_hv <- flow_iv %>% 
  mutate(datetime=floor_date(datetime, unit="hours")) %>% 
  group_by(site, datetime) %>% 
  summarize(flow_cfs=mean(flow_cfs, na.rm=TRUE)) %>% 
  add_WYD(., "datetime") %>% as.data.frame()

# check w plot
ggplot() + 
  geom_line(data=flow_hv, 
            aes(x=datetime, y=flow_cfs, 
                color=as.factor(site),
                group=WY)) +
  #scale_x_datetime(date_breaks="6 months", date_labels = "%b")+
  facet_grid(site~., scales = "free_y")

save(flow_hv, file = "data/flow_hv_cfs_6sites.rda")

# DAILY AVGS WITH caTools -------------------------------------------------

# Make a Daily dataset
flow_dv<-flow_hv %>% 
  mutate(date=floor_date(datetime, unit = "day")) %>% 
  group_by(site, date)  %>% 
  dplyr::summarize("Q_cfs"=mean(flow_cfs,na.rm=TRUE),
                   "Q_CV" =C.Var(flow_cfs, rmNA = T),
                   "Q_min"=min(flow_cfs,na.rm=TRUE),
                   "Q_max"=max(flow_cfs,na.rm=TRUE)) %>%
  mutate("Q_7_cfs"=runmean(Q_cfs, k=7, align="right"),
         "Q_7_CV"=runmean(Q_CV, k=7, align="right"),
         "deltQ" = (Q_cfs - lag(Q_cfs))/ Q_cfs) %>%   
  add_WYD(., "date") %>%
  mutate(date=as.Date(date)) %>% as.data.frame()
  #arrange(site, date) %>% as.data.frame()

summary(flow_dv)
flow_dv %>% group_by(site) %>% dplyr::count()

# check w plot
ggplot() + 
  geom_line(data=flow_dv, 
            aes(x=date, y=Q_CV, 
                color=as.factor(site),
                group=WY)) +
  scale_x_date(date_breaks="1 year", date_labels = "%Y")+
  facet_grid(site~., scales = "free_y")


# check w plot CV
ggplot() + 
  geom_line(data=flow_dv[flow_dv$WY>=2012,], 
            aes(x=date, y=Q_7_CV,
                color=as.factor(site),
                group=WY)) +
  #scale_x_date(date_breaks="1 year", date_labels = "%Y")+
  facet_grid(site~., scales = "free_y")


# check w plot deltQ
ggplot() + 
  geom_line(data=flow_dv[flow_dv$WY==2012,], 
            aes(x=date, y=deltQ,
                color=as.factor(site),
                group=WY)) +
  scale_x_date(date_breaks="2 months", date_labels = "%b")+
  facet_grid(site~.)

# CV by site
ggplot() + 
  geom_line(data=flow_dv, aes(x=DOWY, y=Q_CV, color=WY)) +
  facet_grid(site~., scales = "free_x") +
  scale_x_continuous(breaks=c(1, 32, 62, 93, 121,
                              152, 182, 213, 243,
                              274, 305, 336),
                     labels=c("Oct", "Nov", "Dec", "Jan", "Feb",
                              "Mar", "Apr","May","Jun",
                              "Jul","Aug","Sep")) +
  geom_ribbon(data=flow_dv[flow_dv$DOWY>182 & flow_dv$DOWY<273,], aes(x=DOWY, ymin=0, ymax=200), fill="forestgreen", alpha=0.3) + 
  ylim(0,100)

# deltQ by site
ggplot() + 
  geom_line(data=flow_dv, aes(x=DOWY, y=deltQ, color=WY)) +
  facet_grid(site~., scales = "free_y") +
  scale_x_continuous(breaks=c(1, 32, 62, 93, 121,
                              152, 182, 213, 243,
                              274, 305, 336),
                     labels=c("Oct", "Nov", "Dec", "Jan", "Feb",
                              "Mar", "Apr","May","Jun",
                              "Jul","Aug","Sep")) +
  geom_ribbon(data=flow_dv[flow_dv$DOWY>182 & flow_dv$DOWY<273,], aes(x=DOWY, ymin=0, ymax=200), fill="forestgreen", alpha=0.3) +
  ylim(-5,5)

save(flow_dv, file = "data/flow_dv_cfs_2011_6sites.rda")




