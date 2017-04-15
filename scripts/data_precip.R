# GET CDEC PRECIP & AIR

# Libraries & Functions ---------------------------------------------------

library(tidyverse)
library(lubridate)
library(purrr)  # for map(), reduce()

# CDEC FUNCTION TO DOWNLOAD CDEC
source("scripts/functions/f_getCDEC.R")

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


# GROUPED (PGE) -----------------------------------------------------------

# Central Hydro 1 (PP3): http://cdec.water.ca.gov/cgi-progs/queryDgroups?s=PP3 (PPT INC = 45)
# includes: LSP, BRE, DPH, DRC, HLH, FBS

# INDIVIDUAL STATIONS -----------------------------------------------------

# NFA
# Blue Canyon (BLC) : http://cdec.water.ca.gov/cdecstation2/?sta=BLC
# Sugar Pine (SGP): http://cdec.water.ca.gov/cdecstation2/?sta=SGP
# Blue Canyon DWR-2 (BYM): http://cdec.water.ca.gov/cdecstation2/?sta=BYM
# Colfax (CLF): http://cdec.water.ca.gov/cdecstation2/?sta=CLF

# MFA
# Foresthill R S (FRH): http://cdec.water.ca.gov/cdecstation2/?sta=FRH
# Georgetown USBR (GTW): http://cdec.water.ca.gov/cdecstation2/?sta=GTW
# Georgetown USFS (GRG): http://cdec.water.ca.gov/cdecstation2/?sta=GRG
# Pilot Hill (PIH) (has relative humidity and temp)
# NFY
# Downieville DWR (DNV): http://cdec.water.ca.gov/cdecstation2/?sta=DNV
# Downieville NWS (DWV): http://cdec.water.ca.gov/cdecstation2/?sta=DWV
# Sierra City DWR (SRC): http://cdec.water.ca.gov/cdecstation2/?sta=SRC

# M/S YUBA
# Bowman Lake DWR (BOL): http://cdec.water.ca.gov/cdecstation2/?sta=BOL
# Our House Dam DWR (OHD): http://cdec.water.ca.gov/cdecstation2/?sta=OHD
# Deer Ck Forebay PG&E (DRC): http://cdec.water.ca.gov/cdecstation2/?sta=DRC

# RUB
# Hell Hole USFS (HLH): http://cdec.water.ca.gov/cdecstation2/?sta=HLH


# STATION METADATA --------------------------------------------------------

sta.cdec <- read.csv("data/cdec_ppt_stations_metadata.csv")


# READ IN PPT CSVs --------------------------------------------------------

folder <- "data/precip"
files_list <- dir(path = folder, pattern = "*.csv") # list files

#Read in files & add column with the filename then combine
data <- data_frame(filename = files_list) %>% # create df of files
  mutate(file_contents = map(filename, # read files in
                             ~read_csv(file.path(folder, .), skip=2, col_names = c("date", "pst", "ppt_in"), col_types = "ccd"))) %>% 
  unnest # this unlists all the list of dataframe

# add column names
colnames(data)<-c("filename", "siteID", "year", "month", "day", "hour24", "min", "windDir",   "windSpeed_m_s", "windSteady", "baro_hPa", "temp_C_2m", "temp_C_10m", "temp_C_towertop", "rel_humid", "precip_intens_mm_hr")

dim(data)

# save this as an rds file and an rda file!
mloa_2001 <- data # remember with rda we can't rename the dataframe when we "load()". 

dplyr::write_rds(mloa_2001, path = "data_output/mauna_loa_met_2001_minute.rds", compress = "gz")


# BARO PRESSURE -----------------------------------------------------------
# Wunder
# KCADOWNI2 (downieville) back to Apr 1 2013
# KCAALTA3 (alta) back to Apr 1 2004

