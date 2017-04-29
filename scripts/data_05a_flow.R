# GET CDEC & USGS flow

# Libraries & Functions ---------------------------------------------------

library(tidyverse)
library(purrr)  # for map(), reduce()
library(WaveletComp) # for wavelet analysis
library(hydrostats) # for seasonality colwell analysis


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

# analyze
Col.nfa<-hydrostats::Colwells(df.nfa)
(seasonality <- tibble(site=c("NFA"), MP_metric=c(Col.nfa$MP)))

# MFY ---------------------------------------------------------------
# avg daily flow = (41)
# avg daily Air = (30), daily ppt incr = (45)

# Our House Dam (ORH) (1/1/2000 to present)

# flow daily
get.CDEC(station = "ORH",duration = "D",sensor = 41, start = "2000-01-01",end = "2017-04-27",csv = F)

mfy <- ORH %>% select(station, datetime, sensor_41) %>% 
  rename(flow_cfs = sensor_41, date=datetime) %>% 
  mutate(date=as.Date(date)) %>% 
  filter(!is.na(flow_cfs), flow_cfs>0) %>% 
  add_WYD(., "date") 

rm(ORH)

summary(mfy)

# quick plot
ggplot() + geom_line(data=mfy, aes(x=DOWY, y=flow_cfs, color=as.factor(WY)), show.legend = F) + scale_y_continuous(limits = c(0,8000))

# quick wavelet analysis:

mfy.w <- analyze.wavelet(mfy, my.series = 3, dt = 1/30)

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
df.mfy <- mfy %>%  
  rename(Date=date, Q=flow_cfs)

# analyze
Col.mfy<-hydrostats::Colwells(df.mfy)
(seasonality <- tibble(site=c("mfy"), MP_metric=c(Col.mfy$MP)))




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



