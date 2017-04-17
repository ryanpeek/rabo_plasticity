# GET CDEC PRECIP & AIR

# Libraries & Functions ---------------------------------------------------

library(tidyverse)
library(lubridate)
library(purrr)  # for map(), reduce()

# CDEC FUNCTION TO DOWNLOAD CDEC and Add WY
source("scripts/functions/f_getCDEC.R")
source("scripts/functions/f_doy.R")

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


# SAVE MASTER CDEC AIR_PPT ------------------------------------------------

cdec_ppt_air <- bind_rows(cdec_ppt_air, air_ppt)
rm(air_ppt)

# add sites
cdec_ppt_air$site<-ifelse(cdec_ppt_air$station=="DNV","NFY",NA)
cdec_ppt_air$site<-ifelse(cdec_ppt_air$station=="GTW","MFA",cdec_ppt_air$site)
cdec_ppt_air$site<-ifelse(cdec_ppt_air$station=="SGP","NFA",cdec_ppt_air$site)

save(cdec_ppt_air, file = "data/sites_cdec_daily_ppt_air_2010_2017.rda")

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
write_rds(air_ppt, path = "data/WTC_cdec_hourly_2010_2017.rds", compress = "gz")
save(WTC_air_ppt, file = "data/WTC_cdec_hourly_2010_2017.rda")

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

# READ IN MANUAL DOWNLOADED PPT CSVs --------------------------------------

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

# BARO PRESSURE -----------------------------------------------------------
# Wunder
# KCADOWNI2 (downieville) back to Apr 1 2013
# KCAALTA3 (alta) back to Apr 1 2004

