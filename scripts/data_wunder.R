# Data Wunderground
# get wunderground data

# Tue Apr 18 15:38:02 2017 ------------------------------

# Set up function to pull data from wunderground at a sub hourly timestamp
# aggregate to hourly or daily and plot/save data

# LOAD FUNCTIONS ----------------------------------------------------------

source("scripts/functions/f_wunderground_daily.R") # scrape data
source("scripts/functions/f_wunderground_clean.R") # clean data

#   TUO: KCAGROVE6,KCAGROVE11[2013,2015], KCAGROVE19[2015], MTS026, MD2740
#   NFA: 
    # KCAGOLDR3: Feb 2012
#   MFA: KCACOOL6: Jul 2011
#   MFY: KCAALLEG2
#   SFY: KCANEVAD78: Feb 2017
#   RUB/RALSTON: KCAFORES14 Oct 2010
#   FORESTHILL: 
      # KCAFORES6: Apr 2010
      # KCAFORES21: Mar 2011
      # KCAFORES22: Jan 2012
#   MALAKOFF: MNVYC1
#   NFY: Saddleback, CA Headwaters of Goodyear Ck: MSLEC1
#   STAN: MSPWC1  
#   DAVIS: KCADAVIS17 (slide hill park)

# SCRAPE WUNDERGROUND DATA ------------------------------------------------

site <- "NFA"
station<-'KCAFORES6' # station name here
start<-'2012-03-01' 
end<-'2016-09-30'

# create vector of dates
date.range <- seq.Date(from=as.Date(start), to=as.Date(end), by='1 day')

l <- vector(mode='list', length=length(date.range)) # pre-allocate list

# use wunderground_daily function to loop through dates
for(i in seq_along(date.range)) {s
  print(date.range[i])
  l[[i]] <- wunder_daily(station, date.range[i])
  l[[i]]$station <- station # add station
}

### output is a list of dataframes, each frame is daily data
### if you get "Error in `*tmp*`[[i]] : subscript out of bounds"
### it just means there is not data available for a given day(s), script will
### still run.

# WUNDER CLEAN & PLOT FUNCTION --------------------------------------------

# use wunderground_clean function to combine list and clean data
wunder_clean(data = l, # data
             interval = 60) # minutes


# SAVE DATA ---------------------------------------------------------------

# save as RData file (can combine all three in one .rda file)
save(list = ls(pattern = station), file = paste0("./data/wunder/", site, "_", station,"_",start,"_",end,".rda"))

# remove values/files
rm(l,i, start, end, date.range) 
