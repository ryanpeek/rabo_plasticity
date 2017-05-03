# GET CDEC & USGS flow

# Libraries & Functions ---------------------------------------------------

library(tidyverse)
library(lubridate)
library(purrr)  # for map(), reduce()
library(WaveletComp) # for wavelet analysis
library(hydrostats) # for seasonality colwell analysis
library(viridis)

# Load functions ----------------------------------------------------------

# CDEC FUNCTION TO DOWNLOAD CDEC and Add WY
source("scripts/functions/f_getCDEC.R")
source("scripts/functions/f_USGS_get_raw_dat.R")
source("scripts/functions/f_doy.R")

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


# GET USGS Data  ----------------------------------------------------------------

get.USGS.raw(gage=11413000,river = "NFY", sdate = "1920-01-01",saveRaw = F, daily = T)

summary(NFY_dv)
save(NFY_dv,file ="data/NFY_dv_USGS_1930_2017-04-28.rda")

get.USGS.raw(gage=11427000, river = "NFA", sdate = "1920-01-01",saveRaw = F, daily = T)

summary(NFA_dv)
save(NFA_dv,file ="data/NFA_dv_USGS_1941_2017-04-28.rda")

# NFA ----------------------------------------------------------------

# CDEC: North Fork Dam (NFD) daily flow (sensor=41)

# flow daily
get.CDEC(station = "NFD",duration = "D",sensor = 41, start = "1970-01-01",end = "2017-04-27",csv = F)

# tidy
nfa <- NFD %>% select(station, datetime, sensor_41) %>% 
  rename(flow_cfs = sensor_41, date=datetime) %>% 
  mutate(date=as.Date(date)) %>% 
  filter(!is.na(flow_cfs), flow_cfs>0) %>% 
  add_WYD(., "date") 

rm(NFD)
summary(nfa)

# USGS: NFA

get.USGS.raw(gage=11427000, river = "NFA", sdate = "1920-01-01",saveRaw = F, daily = T)

summary(NFA_dv)

# quick plots
ggplot() + geom_line(data=nfa, aes(x=DOWY, y=flow_cfs, color=as.factor(WY)), show.legend = F)

# load IV data
nfa_iv<-read_rds(path = "data/NFA_iv_USGS_updated_2017-02-22.rds")
nfa_iv <- add_WYD(nfa_iv, datecolumn = "datetime")

# plot
ggplot() + geom_line(data=nfa_iv, aes(x=datetime, y=flow_cfs), show.legend = F, color="darkblue") + ylab("log[Discharge] (cms)") +xlab("")+ scale_y_log10()+
  facet_zoom(x = WY == 2012, shrink=T)# + ylim(c(0,500))

ggsave(filename = "figs/NFA_log_flow_facet_zoom.png",width = 9, height = 7, units = "in")


# quick wavelet analysis:

nfa.w <- analyze.wavelet(NFA_dv, my.series = 4, dt = 1/30) # usgs
nfa.w <- analyze.wavelet(nfa, my.series = 3, dt = 1/30) # cdec

wt.image(nfa.w, main = "NFA Seasonality of Daily Flow",
         legend.params = list(lab = "cross-wavelet power levels"),
         timelab = "Time (days)", periodlab = "period (months)")

wt.avg(nfa.w)

plotNFA<-nfa.w[c("Power.avg","Period","Power.avg.pval")] %>% as.data.frame

ggplot() + geom_line(data=plotNFA, aes(x=Period, y=Power.avg))+geom_point(data=plotNFA, aes(x=Period,y=Power.avg), col=ifelse(plotNFA$Power.avg.pval<0.05, "red", "blue")) + scale_x_continuous(breaks=seq(0,64, 2), limits = c(0,64))

# quick colwell analysis of seasonality:
## From Tonkin et al 2017: M (Contingency) as metric of seasonality
## To standardize the role of seasonality in relation to overall predictability,
## we divided (M) by overall predictability (the sum of (M) and constancy (C)

# standardize
df.nfa <- nfa %>%  
  rename(Date=date, Q=flow_cfs)

df.nfa <- NFA_dv %>%  
  rename(Date=date, Q=flow_cfs)

# analyze
Col.nfa<-hydrostats::Colwells(df.nfa)
(seasonality <- tibble(site=c("NFA"), MP_metric=c(Col.nfa$MP)))

# save models
save(nfa.w, NFA_dv, plotNFA,  file = "models/nfa_usgs_wavelet.rda", compress = "xz")

# MFY ---------------------------------------------------------------
# avg daily flow = (41)
# avg daily Air = (30), daily ppt incr = (45)

# Our House Dam (ORH) (1/1/2000 to present)

# flow daily
get.CDEC(station = "ORH",duration = "D",sensor = 41, start = "2000-01-01",end = "2017-04-27",csv = F)

mfy_dv <- ORH %>% select(station, datetime, sensor_41) %>% 
  rename(flow_cfs = sensor_41, date=datetime) %>% 
  mutate(date=as.Date(date)) %>% 
  filter(!is.na(flow_cfs), flow_cfs>0) %>% 
  add_WYD(., "date") 

rm(ORH)

summary(mfy_dv)

# quick plot
ggplot() + geom_line(data=mfy_dv, aes(x=DOWY, y=flow_cfs, color=as.factor(WY)), show.legend = F) + scale_y_continuous(limits = c(0,8000))

# quick wavelet analysis:

mfy.w <- analyze.wavelet(mfy_dv, my.series = 3, dt = 1/30)

wt.image(mfy.w, main = "MFY Seasonality of Daily Flow",
         legend.params = list(lab = "cross-wavelet power levels"),
         timelab = "Time (days)", periodlab = "period (months)")

wt.avg(mfy.w)

plotMFY<-mfy.w[c("Power.avg","Period","Power.avg.pval")] %>% as.data.frame

ggplot() + geom_line(data=plotMFY, aes(x=Period, y=Power.avg))+geom_point(data=plotmfy, aes(x=Period,y=Power.avg), col=ifelse(plotmfy$Power.avg.pval<0.05, "red", "blue")) + scale_x_continuous(breaks=seq(0,64, 2), limits = c(0,64))

# quick colwell analysis of seasonality:
## From Tonkin et al 2017: M (Contingency) as metric of seasonality
## To standardize the role of seasonality in relation to overall predictability,
## we divided (M) by overall predictability (the sum of (M) and constancy (C)

# standardize
mfy.c <- mfy_dv %>%  
  rename(Date=date, Q=flow_cfs)

# analyze
Col.mfy<-hydrostats::Colwells(df.mfy)
(seasonality <- tibble(site=c("mfy"), MP_metric=c(Col.mfy$MP)))


# MFY_15min DATA ----------------------------------------------------------

mfy.lev <- read_csv("data/MY15min_data_csv.zip") %>% as.data.frame()
summary(mfy.lev)

mfy.lev <- mfy.lev %>% 
  replace_na(list(MYR_BLW_OHD_YC5_cfs=0, LOHMANRIDGE_YC4_cfs=0)) %>% 
  mutate(datetime=dmy_hm(Datetime),
         flow_cfs=LOHMANRIDGE_YC4_cfs + MYR_BLW_OHD_YC5_cfs) %>% 
  select(datetime, flow_cfs) %>% as.data.frame()

summary(mfy.lev)

## now make hourly to match timelapse photos
mfy.hv<-mfy.lev %>% 
  mutate(datetime = floor_date(datetime, unit = "hours")) %>% 
  group_by(datetime) %>% 
  summarize(
    "flow_cfs"=mean(flow_cfs,na.rm=TRUE),
    "flow_cms"=flow_cfs*0.028316847) %>% 
  add_WYD(., "datetime")

summary(mfy.hv)


mfy.dv<-mfy.lev %>% 
  mutate(date = floor_date(datetime, unit = "days")) %>% 
  group_by(date) %>% 
  summarize(
    "flow_cfs"=mean(flow_cfs,na.rm=TRUE),
    "flow_cms"=flow_cfs*0.028316847) %>% 
  add_WYD(., "date") %>% 
  select(date, flow_cfs, flow_cms, DOY, DOWY, WY)

summary(mfy.dv)
ggplot() + geom_line(data=mfy.dv, aes(x=date, y=flow_cfs, color=as.factor(WY)), show.legend = F) + scale_y_continuous(limits = c(0,8000))

mfy.w <- analyze.wavelet(mfy.dv, my.series = 3, dt = 1/30)

wt.image(mfy.w, main = "MFY Seasonality of Daily Flow",
         legend.params = list(lab = "cross-wavelet power levels"),
         timelab = "Time (days)", periodlab = "period (months)")

wt.avg(mfy.w)

plotMFY<-mfy.w[c("Power.avg","Period","Power.avg.pval")] %>% as.data.frame

ggplot() + geom_line(data=plotMFY, aes(x=Period, y=Power.avg))+geom_point(data=plotMFY, aes(x=Period,y=Power.avg), col=ifelse(plotMFY$Power.avg.pval<0.05, "red", "blue")) + scale_x_continuous(breaks=seq(0,64, 2), limits = c(0,64))


# SFY ---------------------------------------------------------------
# Jones Bar (JBR) (1/1/2000 to present)

# Hourly Flow: (another weird error, had to download 1998-2000, and 2001-2017)
get.CDEC(station = "JBR",duration = "H",sensor = 20, start = "1998-10-01",end = "2017-04-30",csv = F)


SFY_hv <- read_csv(file = "data/sfy_hourly_cdec_jbr_1998-2017.csv.zip", skip=1) %>% 
  rename(flow_cfs = `'FLOW`, date=`7528`, time=PST) %>%
  select(date, time, flow_cfs) %>%
  mutate(date = ymd(date),
         time = formatC(time, width = 4, format = "d", flag = "0"),
         flow_cfs = as.numeric(as.character(flow_cfs)),
         datetime = paste(date," ", time, sep=""),
         datetime = as.POSIXct(strptime(datetime,format="%Y-%m-%d %H%M"))) %>%
  filter(!is.na(datetime), !is.na(flow_cfs), flow_cfs>0) %>%
  add_WYD(., "datetime")

summary(SFY_hv)
save(SFY_hv, file = "data/SFY_hv_CDEC_JBR_1998-2017.rda") # save the hourly 

# make a daily version
SFY_dv <- SFY_hv %>% 
  group_by(date) %>% 
  arrange(date) %>% 
  summarize("flow_avg_cfs" = mean(flow_cfs, na.rm = T),
            "flow_CV" = 100*sd(flow_cfs)/mean(flow_cfs)) %>% 
  mutate("station"="JBR") %>%
  add_WYD(., "date") %>% 
  select(station, date, flow_avg_cfs, flow_CV, everything()) %>% 
  filter(!is.na(flow_CV))

summary(SFY_dv)
SFY_dv <- SFY_dv %>% as.data.frame(SFY_dv)
save(SFY_dv, file = "data/SFY_dv_CDEC_JBR_1998-2017.rda") # save data

# quick plot
ggplot() + geom_line(data=SFY_dv, aes(x=DOWY, y=flow_avg_cfs, color=as.factor(WY)), show.legend = F) + scale_y_continuous(limits = c(0,8000))
ggplot() + geom_line(data=SFY_dv, aes(x=date, y=flow_avg_cfs, color=as.factor(WY)), show.legend = F) + scale_y_continuous(limits = c(0,8000))

# quick wavelet analysis:
sfy.w <- analyze.wavelet(SFY_dv, my.series = 3, dt = 1/30)

wt.image(sfy.w, main = "SFY Seasonality of Daily Flow",
         legend.params = list(lab = "cross-wavelet power levels"),
         timelab = "Time (days)", periodlab = "period (months)")

wt.avg(sfy.w)

plotSFY<-sfy.w[c("Power.avg","Period","Power.avg.pval")] %>% as.data.frame

ggplot() + geom_line(data=plotSFY, aes(x=Period, y=Power.avg))+geom_point(data=plotSFY, aes(x=Period,y=Power.avg), col=ifelse(plotSFY$Power.avg.pval<0.05, "red", "blue")) + scale_x_continuous(breaks=seq(0,64, 2), limits = c(0,64))

# quick colwell analysis of seasonality:
## From Tonkin et al 2017: M (Contingency) as metric of seasonality
## To standardize the role of seasonality in relation to overall predictability,
## we divided (M) by overall predictability (the sum of (M) and constancy (C)

# standardize
sfy.c <- SFY_dv %>%  
  rename(Date=date, Q=flow_avg_cfs)

# analyze
Col.sfy<-hydrostats::Colwells(sfy.c)
(seasonality <- tibble(site=c("sfy"), MP_metric=c(Col.sfy$MP)))


# MFA ---------------------------------------------------------------------

# Oxbow/Ralston (OXB) (12/1/1997 to present)
# hourly/event flow = (20)

# get.CDEC(station = "OXB", duration = "H",sensor = 20, start = "1997-12-01",end = "2017-04-27",csv = F)
# weird error occuring from the page (html at the end throws off read.table) so "get.CDEC" function doesn't work
# used manual download: 
# "http://cdec.water.ca.gov/cgi-progs/queryCSV?station_id=OXB&sensor_num=20&dur_code=E&start_date=1998-12-01&end_date=2017-04-30&data_wish=View+CSV+Data" and then delete html at end

# mfa <- read.csv(file = "data/mfa_hourly_cdec_2017.csv", skip=1) %>% 
#   rename(flow_cfs = X.FLOW, date=X7710, time=PST) %>% 
#   select(date, time, flow_cfs) %>% 
#   mutate(date = ymd(date),
#          time = formatC(time, width = 4, format = "d", flag = "0"), # can also use sprintf
#          flow_cfs = as.numeric(as.character(flow_cfs)),
#          datetime = paste(date," ", time, sep=""),
#          datetime = as.POSIXct(strptime(datetime,format="%Y-%m-%d %H%M"))) %>% 
#   filter(!is.na(datetime), !is.na(flow_cfs), flow_cfs>0) %>% 
#   add_WYD(., "datetime") 
# 
# summary(mfa)
# MFA_hv <- mfa
# save(MFA_hv, file = "data/MFA_hv_CDEC_OXB_1997-2017.rda") # save the hourly data and make a daily data version:

load("data/MFA_hv_CDEC_OXB_1997-2017.rda")

MFA_dv <- MFA_hv %>% 
  group_by(date) %>% 
  arrange(date) %>% 
  summarize("flow_avg_cfs" = mean(flow_cfs, na.rm = T),
            "flow_CV" = 100*sd(flow_cfs)/mean(flow_cfs)) %>% 
  mutate("station"="OXB") %>%
  add_WYD(., "date") %>% 
  select(station, date, flow_avg_cfs, flow_CV, everything()) %>% 
  filter(!is.na(flow_CV))

summary(MFA_dv)
MFA_dv <- MFA_dv %>% as.data.frame(MFA_dv)
save(MFA_dv, file = "data/MFA_dv_CDEC_OXB_1997-2017.rda") # save data


# quick plot HV
ggplot() + geom_line(data=MFA_hv, aes(x=datetime, y=flow_cfs, color=WY), show.legend = F) + scale_color_viridis()

# ZOOM IN HERE ggforce: (cms= cfs*0.028316847)
library(ggforce)
ggplot() + geom_line(data=MFA_hv, aes(x=datetime, y=flow_cfs), show.legend = F, color="darkblue") + ylab("log[Discharge] (cms)") +xlab("")+ scale_y_log10()+
  facet_zoom(x = WY == 2012, shrink=T)# + ylim(c(0,500))
ggsave(filename = "figs/MFA_log_flow_facet_zoom.png",width = 9, height = 7, units = "in")

# quick plot DV
ggplot() + geom_line(data=MFA_dv, aes(x=date, y=flow_avg_cfs, color=as.factor(WY)), show.legend = F)

# quick plot CV
ggplot() + geom_line(data=MFA_dv, aes(x=date, y=flow_CV, color=as.factor(WY)), show.legend = F)


# quick wavelet analysis:
mfa.w <- analyze.wavelet(MFA_dv, my.series = 3, dt = 1/30)

wt.image(mfa.w, main = "MFA Seasonality of Daily Flow",
         legend.params = list(lab = "cross-wavelet power levels"),
         timelab = "Time (days)", periodlab = "period (months)")

wt.avg(mfa.w)

plotMFA<-mfa.w[c("Power.avg","Period","Power.avg.pval")] %>% as.data.frame

ggplot() + geom_line(data=plotMFA, aes(x=Period, y=Power.avg))+geom_point(data=plotMFA, aes(x=Period,y=Power.avg), col=ifelse(plotMFA$Power.avg.pval<0.05, "red", "blue")) + scale_x_continuous(breaks=seq(0,64, 2), limits = c(0,64))

# quick colwell analysis of seasonality:
## From Tonkin et al 2017: M (Contingency) as metric of seasonality
## To standardize the role of seasonality in relation to overall predictability,
## we divided (M) by overall predictability (the sum of (M) and constancy (C)

# standardize
df.mfa <- MFA_dv %>%  
  rename(Date=date, Q=flow_avg_cfs) %>% as.data.frame()

# analyze
Col.mfa<-hydrostats::Colwells(df.mfa)
(seasonality <- tibble(site=c("mfa"), MP_metric=c(Col.mfa$MP)))


# RUBICON -----------------------------------------------------------------

library(lubridate)
# no gage so using 15 min data from ellicott...need better data for longer term!
RUB_hv <- read_csv("data/hydro/RUB_ellicot_wy2011_wy2014_15min_corrected.csv") %>% 
  rename(flow_cfs = `Discharge (cfs)`) %>%
  mutate(datetime = mdy_hm(datetime), 
         datetime = floor_date(datetime, unit = "hours"),
         station = "Ellicot") %>% select(station, datetime, flow_cfs) %>% 
  add_WYD(., "datetime") %>% 
  as.data.frame()

ggplot() + geom_line(data=RUB_hv, aes(x=datetime, y=flow_cfs, color=as.factor(WY)), show.legend = F)


save(RUB_hv, file = "data/RUB_hv_PCWA_2011-2014.rda")

# make daily
RUB_dv <- RUB_hv %>%
  mutate(date = floor_date(datetime, unit="days")) %>% 
  group_by(date, station) %>% 
  arrange(date) %>% 
  summarize("flow_avg_cfs" = mean(flow_cfs, na.rm = T),
            "flow_CV" = 100*sd(flow_cfs)/mean(flow_cfs)) %>% 
  mutate("site"="RUB") %>%
  add_WYD(., "date") %>% 
  select(station, date, flow_avg_cfs, flow_CV, everything()) %>% 
  filter(!is.na(flow_CV)) %>% as.data.frame()

summary(RUB_dv)
save(RUB_dv, file = "data/RUB_dv_PCWA_2011-2014.rda") # save data

ggplot() + geom_line(data=RUB_dv, aes(x=date, y=flow_avg_cfs, color=as.factor(WY)), show.legend = F)
ggplot() + geom_line(data=RUB_dv, aes(x=date, y=flow_CV, color=as.factor(WY)), show.legend = F)


# wavelet analysis
rub.w <- analyze.wavelet(RUB_dv, my.series = 3, dt = 1/30)

wt.image(rub.w, main = "RUB Seasonality of Daily Flow",
         legend.params = list(lab = "cross-wavelet power levels"),
         timelab = "Time (days)", periodlab = "period (months)")

wt.avg(rub.w)

plotRUB<-rub.w[c("Power.avg","Period","Power.avg.pval")] %>% as.data.frame

ggplot() + geom_line(data=plotRUB, aes(x=Period, y=Power.avg))+geom_point(data=plotRUB, aes(x=Period,y=Power.avg), col=ifelse(plotRUB$Power.avg.pval<0.05, "red", "blue")) + scale_x_continuous(breaks=seq(0,64, 2), limits = c(0,64))

# quick colwell analysis of seasonality:
## From Tonkin et al 2017: M (Contingency) as metric of seasonality
## To standardize the role of seasonality in relation to overall predictability,
## we divided (M) by overall predictability (the sum of (M) and constancy (C)

# standardize
df.RUB <- RUB_dv %>%  
  rename(Date=date, Q=flow_avg_cfs) %>% as.data.frame()

# analyze
Col.RUB<-hydrostats::Colwells(df.RUB)
(seasonality <- tibble(site=c("RUB"), MP_metric=c(Col.RUB$MP)))


# Combined ----------------------------------------------------------------


# Power Plots:
plotNFY<-nfy.w[c("Power.avg","Period","Power.avg.pval")] %>% as.data.frame
ggplot() + geom_line(data=plotNFY, aes(x=Period, y=Power.avg))+geom_point(data=plotNFY, aes(x=Period,y=Power.avg), col=ifelse(plotNFY$Power.avg.pval<0.05, "red", "blue"))

plotNFA<-nfa.w[c("Power.avg","Period","Power.avg.pval")] %>% as.data.frame
ggplot() + geom_line(data=plotNFA, aes(x=Period, y=Power.avg))+geom_point(data=plotNFA, aes(x=Period,y=Power.avg), col=ifelse(plotNFA$Power.avg.pval<0.05, "red", "blue")) 

plotMFA<-mfa.w[c("Power.avg","Period","Power.avg.pval")] %>% as.data.frame

# all 3
ggplot() + geom_line(data=plotNFA, aes(x=Period, y=Power.avg), col="black")+
  geom_line(data=plotNFY, aes(x=Period, y=Power.avg), col="blue",lty=2) +
  geom_line(data=plotMFA, aes(x=Period, y=Power.avg), col="red",lty=3, lwd=2) +
  scale_x_continuous(breaks=c(seq(0,24,3)))



