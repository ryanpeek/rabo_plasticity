# GET CDEC PRECIP & AIR

# Libraries & Functions ---------------------------------------------------

library(tidyverse)
library(lubridate)
library(purrr)  # for map(), reduce()

# CDEC FUNCTION TO DOWNLOAD CDEC and Add WY
source("scripts/functions/f_getCDEC.R")
source("scripts/functions/f_doy.R")

# Single one liner to calc consec days without ppt
#days_no_ppt <- (!cdec$CDEC_ppt_mm) * unlist(lapply(rle(cdec$CDEC_ppt_mm)$lengths, seq_len))

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


# GET NFY  ----------------------------------------------------------------

# avg daily Air = (30), daily ppt incr = (45)

# Downieville (DNV)

# ppt
get.CDEC(station = "DNV",duration = "D",sensor = 45, start = "2010-10-01",end = "2017-04-01",csv = F) 
dnv_ppt <- DNV %>% select(station, datetime, sensor_45) %>% 
  rename(ppt_in = sensor_45)
rm(DNV)

# air
get.CDEC(station = "DNV",duration = "D",sensor = 30, start = "2010-10-01",end = "2017-04-01",csv = F)
dnv_air <- DNV %>% select(station, datetime, sensor_30) %>% 
  rename(air_F = sensor_30)
rm(DNV)

DNV_air_ppt<- select(dnv_ppt, -station) %>% inner_join(dnv_air, by="datetime") %>% select(station, everything())

rm(dnv_ppt, dnv_air)

# MFA & RUB ---------------------------------------------------------------

# avg daily Air = (30), daily ppt incr = (45)

# Georgetown (GTW)

# ppt
get.CDEC(station = "GTW",duration = "D",sensor = 45, start = "2010-10-01",end = "2017-04-01",csv = F) 
ppt <- GTW %>% select(station, datetime, sensor_45) %>% 
  rename(ppt_in = sensor_45)
rm(GTW)

# air
get.CDEC(station = "GTW",duration = "D",sensor = 30, start = "2010-10-01",end = "2017-04-01",csv = F)
air <- GTW %>% select(station, datetime, sensor_30) %>% 
  rename(air_F = sensor_30)
rm(GTW)

GTW_air_ppt<- select(ppt, -station) %>% inner_join(air, by="datetime") %>% select(station, everything())

rm(ppt, air)

cdec_ppt_air <- bind_rows(DNV_air_ppt, GTW_air_ppt)
rm(GTW_air_ppt, DNV_air_ppt)

# NFA ---------------------------------------------------------------

# avg daily Air = (30), daily ppt incr = (45)

# Sugar Pine (SGP)

# ppt
get.CDEC(station = "SGP",duration = "D",sensor = 45, start = "2010-10-01",end = "2017-04-01",csv = F) 
ppt <- SGP %>% select(station, datetime, sensor_45) %>% 
  rename(ppt_in = sensor_45)
rm(SGP)

# air
get.CDEC(station = "SGP",duration = "D",sensor = 30, start = "2010-10-01",end = "2017-04-01",csv = F)
air <- SGP %>% select(station, datetime, sensor_30) %>% 
  rename(air_F = sensor_30)
rm(SGP)

air_ppt<- select(ppt, -station) %>% inner_join(air, by="datetime") %>% select(station, everything())

rm(ppt, air)


# MFY/SFY -----------------------------------------------------------------


# Our House Dam (OHD):  avg daily Air = (30), daily ppt incr = (45) (only since 2015), raintip (precip event) = 16 since 2006

# GET PPT (15 min) convert to DAILY
get.CDEC(station = "OHD",duration = "E",sensor = 16, start = "2010-10-01",end = "2017-04-01",csv = F) 

ppt <- OHD %>% select(station, datetime, sensor_16) %>% 
  add_WYD(., "datetime") %>%
  rename(ppt_in_accum = sensor_16) %>% 
  group_by(WY, DOWY) %>% 
  mutate(ppt_in_accum = ifelse(DOWY==1, 0, ppt_in_accum))

# needs to be filtered and fixed
ppt$ppt_in_accum <- ifelse(ppt$ppt_in_accum>500, NA, ppt$ppt_in_accum)
ppt$ppt_in_accum <- ifelse(ppt$ppt_in_accum<0, NA, ppt$ppt_in_accum)
#ppt <- ppt %>% filter(!is.na(ppt_in_accum)) %>% as.data.frame()

# make 15 min into DAILY
ppt2 <- ppt %>% 
  mutate(date=as.Date(floor_date(datetime, unit = "day"))) %>% 
  group_by(date, station) %>% 
  arrange(date) %>% 
  summarize("ppt_increm" = max(ppt_in_accum, na.rm = T)) %>% 
  add_WYD(., "date")

# fix errors in data
ppt2$ppt_increm[24]<-3.43
ppt2$ppt_increm[2309:2317]<-70
ppt2$ppt_increm[730]<-43.70

# get daily incremental and not cumulative
ppt2$ppt_in <- abs(ppt2$ppt_increm - lag(x=ppt2$ppt_increm, n=1, default=0))
ppt2$ppt_in <- ifelse(ppt2$DOWY == 1, 0, ppt2$ppt_in)

# quick plot
ggplot() + geom_line(data=ppt2, aes(x=date, y=ppt_increm))+
  geom_line(data=ppt2, aes(x=date, y=ppt_in), col="blue")

ggplot() + geom_line(data=ppt2, aes(x=date, y=ppt_in))

rm(OHD)

# air
get.CDEC(station = "OHD",duration = "D",sensor = 30, start = "2010-10-01",end = "2017-04-01",csv = F)
air <- OHD %>% select(station, datetime, sensor_30) %>% 
  rename(air_F = sensor_30,
         date = datetime) %>% 
  mutate(date = as.Date(date))
rm(OHD)


air_ppt<- select(ppt2, -station, -ppt_increm) %>% right_join(air, by="date") %>% select(station, everything())

air_ppt <- air_ppt %>% 
  mutate(CDEC_air_C = convertTemp(air_F, unit = "F", convert="C"),
         CDEC_ppt_mm = ppt_in*25.4) %>% 
  select(station, date, CDEC_air_C, CDEC_ppt_mm) %>% 
  add_WYD(., datecolumn = "date") %>% 
  mutate(site=as.factor("MFY"))
summary(air_ppt)


rm(ppt2, air)

# remove old MFY
cdec_ppt_air <- filter(cdec_ppt_air, !site=="MFY")
table(cdec_ppt_air$site)

# bind with master
cdec_ppt_air <- cdec_ppt_air %>% 
  mutate(date = as.Date(date)) %>% 
  bind_rows(., air_ppt) # add MFY


# SFY ---------------------------------------------------------------------


# Lake Spaulding (LSP) at 5,146ft :  avg daily Air = (30), daily ppt incr = (45), rel humidity (EVENT) = 12

# ppt
get.CDEC(station = "LSP",duration = "D",sensor = 45, start = "2010-10-01",end = "2017-04-01",csv = F) 
ppt <- LSP %>% select(station, datetime, sensor_45) %>% 
  rename(ppt_in = sensor_45)
rm(LSP)

# air
get.CDEC(station = "LSP",duration = "D",sensor = 30, start = "2010-10-01",end = "2017-04-01",csv = F)
air <- LSP %>% select(station, datetime, sensor_30) %>% 
  rename(air_F = sensor_30)
rm(LSP)

air_ppt<- select(ppt, -station) %>% inner_join(air, by="datetime") %>% select(station, everything())

rm(ppt, air)

air_ppt2 <- air_ppt %>% 
  mutate(CDEC_air_C = convertTemp(air_F, unit = "F", convert="C"),
         CDEC_ppt_mm = ppt_in*25.4) %>% rename(date=datetime) %>% 
  select(station, date, CDEC_air_C, CDEC_ppt_mm) %>% 
  add_WYD(., datecolumn = "date") %>% 
  mutate(site=as.factor("SFY"))
summary(air_ppt2)

air_ppt_MFY <- air_ppt %>% 
  mutate(CDEC_air_C = convertTemp(air_F, unit = "F", convert="C"),
         CDEC_ppt_mm = ppt_in*25.4) %>% rename(date=datetime) %>% 
  select(station, date, CDEC_air_C, CDEC_ppt_mm) %>% 
  add_WYD(., datecolumn = "date") %>% 
  mutate(site=as.factor("MFY"))

air_ppt_YUB <- bind_rows(air_ppt2, air_ppt_MFY)
air_ppt_YUB$site <- as.factor(air_ppt_YUB$site)

# SFY ---------------------------------------------------------------

# hourly Air = (4), hourly ppt acc = (2)

# White Cloud (WTC)

# ppt
get.CDEC(station = "WTC",duration = "H",sensor = 2, start = "2010-10-01",end = "2017-04-01",csv = F) 
ppt <- WTC %>% select(station, datetime, sensor_2) %>% 
  rename(ppt_in_accum = sensor_2)
rm(WTC)

# air
get.CDEC(station = "WTC",duration = "H",sensor = 4, start = "2010-10-01",end = "2017-04-01",csv = F)
air <- WTC %>% select(station, datetime, sensor_4) %>% 
  rename(air_F = sensor_4)
rm(WTC)

WTC_air_ppt<- select(ppt, -station) %>% inner_join(air, by="datetime") %>% select(station, everything())

rm(ppt, air)


# SAVE MASTER CDEC AIR_PPT ------------------------------------------------

# existing file is here:
load("data/cdec_sites_daily_ppt_air_2010_2017.rda")

cdec_ppt_air <- bind_rows(cdec_ppt_air, air_ppt_YUB) # add YUB

# add RUB (duplicate Georgetown data)
RUB <- filter(cdec_ppt_air, site=="MFA") %>% mutate(site="RUB")

cdec_ppt_air <- bind_rows(cdec_ppt_air, RUB)


#rm(air_ppt)

# cdec_ppt_air <- cdec_ppt_air %>% 
#   mutate(CDEC_air_C = convertTemp(air_F, unit = "F", convert="C"),
#          CDEC_ppt_mm = ppt_in*25.4) %>% rename(date=datetime) %>% 
#   select(station, date, CDEC_air_C, CDEC_ppt_mm) %>% 
#   add_WYD(., datecolumn = "date")

# add sites
# cdec_ppt_air$site<-ifelse(cdec_ppt_air$station=="DNV","NFY",NA) # NFY
# cdec_ppt_air$site<-ifelse(cdec_ppt_air$station=="GTW","MFA",cdec_ppt_air$site) # MFA/RUB
# cdec_ppt_air$site<-as.factor(ifelse(cdec_ppt_air$station=="SGP","NFA",cdec_ppt_air$site)) # NFA
# cdec_ppt_air$site<-as.factor(ifelse(cdec_ppt_air$station=="LSP","SFY",cdec_ppt_air$site)) # SFY


save(cdec_ppt_air, file = "data/cdec_sites_daily_ppt_air_2010_2017.rda")


# CDEC STATIONS -----------------------------------------------------

sta.cdec <- read.csv("data/cdec_all_stations.csv")

# PG&E Grouped: Central Hydro 1 (PP3): http://cdec.water.ca.gov/cgi-progs/queryDgroups?s=PP3 (PPT INC = 45)
# includes: LSP, BRE, DPH, DRC, HLH, FBS

# NFA
# Blue Canyon (BLC) 
# Sugar Pine (SGP)
# Blue Canyon DWR-2 (BYM)
# Colfax (CLF)

# MFA
# Foresthill R S (FRH)
# Georgetown USBR (GTW)
# Georgetown USFS (GRG)
# Pilot Hill (PIH) (has relative humidity and temp)

# NFY
# Downieville DWR (DNV)
# Downieville NWS (DWV)
# Sierra City DWR (SRC)

# M/S YUBA
# Bowman Lake DWR (BOL)
# Our House Dam DWR (OHD)
# Deer Ck Forebay PG&E (DRC)

# RUB
# Hell Hole USFS (HLH)



# SEASONALITY COLWELL ANALYSIS --------------------------------------------

load(file = "data/cdec_sites_daily_ppt_air_2010_2017.rda")

summary(cdec_ppt_air)
ggplot() + geom_line(data=cdec_ppt_air, aes(x=date, y=CDEC_ppt_mm, color=site)) + facet_grid(.~site)

df.NFY <- cdec_ppt_air %>% filter(site=="NFY") %>% 
  select(date, CDEC_ppt_mm) %>% rename(Date=date, Q=CDEC_ppt_mm)

df.NFA <- cdec_ppt_air %>% filter(site=="NFA") %>% 
  select(date, CDEC_ppt_mm) %>% rename(Date=date, Q=CDEC_ppt_mm)

df.MFA <- cdec_ppt_air %>% filter(site=="MFA") %>% 
  select(date, CDEC_ppt_mm) %>% rename(Date=date, Q=CDEC_ppt_mm)

## SEASONALITY WITH COLWELL
# From Tonkin et al 2017: M (Contingency) as metric of seasonality. 
# To standardize the role of seasonality in relation to overall predictability,
# we divided (M) by overall predictability (the sum of (M) and constancy (C)

library(hydrostats)
Col.NFY<-hydrostats::Colwells(df.NFY)
Col.NFA<-hydrostats::Colwells(df.NFA)
Col.MFA<-hydrostats::Colwells(df.MFA)
(seasonality <- tibble(site=c("NFY","NFA","MFA"), MP_metric=c(Col.NFY$MP, Col.NFA$MP, Col.MFA$MP)))

# WAVELET ANALYSIS --------------------------------------------------------

library(WaveletComp)

# NFY
dfNFY <- cdec_ppt_air %>% filter(site=="NFY") %>%
  mutate(mon=lubridate::month(date)) %>% 
  filter(!is.na(CDEC_ppt_mm)) %>% group_by(WY, mon) %>% 
  summarize(mon_ppt_mm=sum(CDEC_ppt_mm)) %>% as.data.frame()

nfy.w <- analyze.wavelet(dfNFY, my.series = 2)

wt.image(nfy.w, n.levels = 74, main="NFY Seasonality of Precip.",
         legend.params = list(lab = "cross-wavelet power levels"),
         timelab = "Time (months)", periodlab = "period (months)")

# NFA
dfNFA <- cdec_ppt_air %>% filter(site=="NFA") %>%
  mutate(mon=lubridate::month(date)) %>% 
  filter(!is.na(CDEC_ppt_mm)) %>% group_by(WY, mon) %>% 
  summarize(mon_ppt_mm=sum(CDEC_ppt_mm)) %>% as.data.frame()

nfa.w <- analyze.wavelet(dfNFA, my.series = 2)

wt.image(nfa.w, n.levels = 74,main = "NFA Seasonality of Precip.",
         legend.params = list(lab = "cross-wavelet power levels"),
         timelab = "Time (months)", periodlab = "period (months)")

# MFA
dfMFA <- cdec_ppt_air %>% filter(site=="MFA") %>%
  mutate(mon=lubridate::month(date)) %>% 
  filter(!is.na(CDEC_ppt_mm)) %>% group_by(WY, mon) %>% 
  summarize(mon_ppt_mm=sum(CDEC_ppt_mm)) %>% as.data.frame()

mfa.w <- analyze.wavelet(dfMFA, my.series = 2)

wt.image(mfa.w, n.levels = 74,main = "MFA Seasonality of Precip.",
         legend.params = list(lab = "cross-wavelet power levels"),
         timelab = "Time (months)", periodlab = "period (months)")

wt.avg(mfa.w)
wt.avg(nfa.w)
wt.avg(nfy.w)

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




# PURRR MANUAL DOWNLOADED PPT CSVs --------------------------------------

folder <- "data/cdec_ppt"
files_list <- dir(path = folder, pattern = "*.csv") # list files

# Read in files & add column with the filename then combine
data <- data_frame(filename = files_list) %>% # create df of files
  mutate(file_contents = map(filename, # read files in
                             ~read_csv(file.path(folder, .), skip=2, col_names = c("date", "pst", "ppt_in"), col_types = "ccd"))) %>% 
  unnest %>% # unlist all to df
  select(-pst) %>% 
  mutate(date=as.Date(data$date,"%Y%m%d"),
         ppt_mm=ppt_in*25.4) %>% 
  add_WYD(., "date")

dim(data)

write_rds(data, path = "data/cdec_ppt_daily_2005_2017.rds", compress = "gz")

ggplot() + geom_point(data=data[data$WY>=2011 & data$filename=="SGP_2005-2017.csv",], aes(x=DOWY, y=ppt_in, group=WY, color=as.factor(WY)), alpha=0.5) + scale_y_continuous(limits = c(0,8)) + 
  geom_area(data=data[data$WY>=2011 & data$filename=="SGP_2005-2017.csv",], aes(x=DOWY, y=ppt_in, group=WY, color=as.factor(WY)), alpha=0.5)

