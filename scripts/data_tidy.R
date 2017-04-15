# Clean Data and Bind together in Master Dataset

# to add TIME Stamped section, type "ts" and hit "Shift + TAB"

# Wed Apr 12 15:31:42 2017 ------------------------------

# LOAD LIBRARIES ----------------------------------------------------------
library(tidyverse)
library(lubridate)

# RUN DATA_LOAD.R ---------------------------------------------------------

source("scripts/data_load.R")

# TIDY MASTER -------------------------------------------------------------

# Select sites, rm "grp" col, drop unused factors
hrly2 <- hrly2 %>% select(-grp, -WY, -wyd, -year, -yday, -mon, -hours) %>% filter(!site %in% c("TUO", "CLA")) %>% 
  mutate(site = factor(site),
         type = as.factor("solinst"))
levels(hrly2$site) # check factor levels

# make all cols lower case for ease of typing
colnames(hrly2) <- tolower(colnames(hrly2))
hrly2 <- hrly2 %>% rename(temp_C = temperature)

# add water year info with function
hrly2 <- add_WYD(hrly2, "datetime")
summary(hrly2)

# # QUICK PLOTS -------------------------------------------------------------
# 
# ggplot() + 
#   geom_path(data=hrly2, aes(x=datetime, y=level, color=site, group=WY)) +
#   facet_grid(site~., scales = "free_y")
# 
# # SITE ONLY
# ggplot() + 
#   geom_path(data=hrly2[hrly2$site=="MFA",], aes(x=datetime, y=level, color=site, group=WY)) +
#   facet_grid(site~., scales = "free_y")

# ADD SITE UPDATES ---------------------------------------------------------

# combine and rm NA's
raw_updated<-bind_rows(nfa, mfa, sfy, nfy, nfy_baro, nfa_baro)
raw_updated <- raw_updated %>% filter(!is.na(level))

# make some cols factors
fxs <- c("site", "compensated", "type")
raw_updated[fxs] <- lapply(raw_updated[fxs], as.factor) 

# MAKE HOURLY -------------------------------------------------------------

## Make Hourly dataset: solinst
raw_hrly<-raw_updated %>% filter(type=="solinst") %>% 
  mutate(year = year(datetime),
         mon = month(datetime),
         hour = hour(datetime)) %>% 
    group_by(site, year, mon, DOY, hour)%>%
  dplyr::summarize(
    "level"=mean(level,na.rm=TRUE),
    "temp_C"=mean(temp_C,na.rm=TRUE))%>%
  mutate("datetime"=ymd_hms(strptime(paste0(year,"-", mon,"-", DOY, " ",
                                            hour,":00"),format = "%Y-%m-%j %H:%M")),
         "type" = "solinst") %>%
  add_WYD(., "datetime") %>% 
  as.data.frame() %>% 
  select(site, datetime, type, -year, -mon, -hour, level:temp_C, DOY, WY, DOWY)

## Make Hourly dataset: baro
raw_hrly_baro<-raw_updated %>% filter(type=="baro") %>% 
  mutate(year = year(datetime),
         mon = month(datetime),
         hour = hour(datetime)) %>% 
  group_by(site, year, mon, DOY, hour) %>%
  dplyr::summarize(
    "level"=mean(level,na.rm=TRUE),
    "temp_C"=mean(temp_C,na.rm=TRUE))

# some reason this same script breaks here and I don't know why...
# works if I run it separate of the previous pipe
raw_hrly_baro$datetime <- ymd_hms(strptime(paste0(raw_hrly_baro$year,"-",
                                                  raw_hrly_baro$mon,"-", 
                                                  raw_hrly_baro$DOY, " ",
                                                  raw_hrly_baro$hour,":00"),
                                           format = "%Y-%m-%j %H:%M"))

# now re-add the other stuff and select cols
raw_hrly_baro <- add_WYD(raw_hrly_baro, "datetime") %>% 
  mutate(type="baro") %>% 
  as.data.frame() %>% 
  select(site, datetime, type, 
         -year, -mon, -hour, level:temp_C, DOY, WY, DOWY) 

# bind the two hourly datasets
hrly_to_add <- bind_rows(raw_hrly, raw_hrly_baro) %>% 
  mutate(compensated="N")

# # Hrly New Data Plots ----------------------------------------------------
# 
# # Temp
# ggplot()+
#   geom_line(data=hrly_to_add[hrly_to_add$type=="solinst",], aes(datetime,temp_C, color=site, group=WY))+
#   facet_grid(site~., scales = "free_y") + 
#   theme_bw()+ggtitle("Avg Hourly Water Temperature (C)")
#         
# # Hourly Stage
# ggplot()+
#   geom_line(data=hrly_to_add[hrly_to_add$type=="solinst",],aes(datetime,level, color=site, group=WY))+
#   facet_grid(site~., scales = "free_y") + 
#   theme_bw()+ggtitle("Avg Hourly Stage (m)")
# 
# 
# JOIN WITH MASTER DATA ---------------------------------------------------

summary(hrly_to_add)
summary(hrly2)

names(hrly_to_add)
names(hrly2)
hr.df <- bind_rows(hrly2, hrly_to_add)
glimpse(hr.df)

# convert back to factors
fxs <- c("site", "compensated", "type")
hr.df[fxs] <- lapply(hr.df[fxs], as.factor) 
str(hr.df)
summary(hr.df)


# MASTER HOURLY PLOTS -----------------------------------------------------

# quick plot of hrly all
ggplot() + 
  geom_line(data=hr.df[hr.df$type=="solinst",], 
            aes(x=datetime, y=level, color=site, group=WY)) +
  facet_grid(site~., scales = "free_y")

# plot a single SITE
ggplot() + 
  geom_path(data=hr.df[hr.df$site=="SFY" & hr.df$type=="solinst" & hr.df$WY>2014,], aes(x=datetime, y=level, group=WY), color="maroon")

write_rds(hr.df, path = "data/2011-2016_solinst_mainstem_hrly.rds", compress = "gz")
save(hr.df, file="data/2011-2016_solinst_mainstem_hrly.rda")

# CLEAN ENVIRONMENT -------------------------------------------------------

rm(hrly_to_add, hrly2, mfa, nfa, nfa_baro, nfy, nfy_baro, raw_hrly, raw_updated, raw_hrly_baro, sfy)

# MAKE A DAILY DATASET ----------------------------------------------------

# For SOLINST-RIVER
require(caTools)
dy.sol.df<-hr.df %>%
  filter(type=="solinst") %>% 
  mutate(year = year(datetime),
         mon = month(datetime)) %>% 
  group_by(site, year, mon, DOY)%>%
  dplyr::summarize("lev_avg"=mean(level,na.rm=TRUE),
                   "lev_min"=min(level,na.rm=TRUE),
                   "lev_max"=max(level,na.rm=TRUE),
                   "temp_avg"=mean(temp_C,na.rm=TRUE),
                   "temp_min"=min(temp_C,na.rm=TRUE),
                   "temp_max"=max(temp_C,na.rm=TRUE)) %>%
  mutate("lev_7_avg"= runmean(lev_avg, k=7, endrule="mean",align="center"),
         "lev_7_min"= runmin(lev_min, k=7, align="center"),
         "lev_7_max"= runmax(lev_max, k=7, align="center"),
         "temp_7_avg"= runmean(temp_avg, k=7, endrule="mean",align="center"),
         "temp_7_min"= runmin(temp_min, k=7, align="center"),
         "temp_7_max"= runmax(temp_max, k=7, align="center")) %>%
  mutate("datetime"=ymd(strptime(paste0(year,"-", mon,"-", DOY),
                                 format = "%Y-%m-%j"))) %>%
  add_WYD(., "datetime") %>%
  mutate(type="solinst") %>% 
  as.data.frame() %>% 
  select(site, datetime, type, -year, -mon, lev_avg:temp_7_max, DOY, WY, DOWY)

# For SOLINST-BAROS

dy.baro.df<-hr.df %>%
  filter(type=="baro") %>% 
  mutate(year = year(datetime),
         mon = month(datetime)) %>% 
  group_by(site, year, mon, DOY)%>%
  dplyr::summarize("lev_avg"=mean(level,na.rm=TRUE),
                   "lev_min"=min(level,na.rm=TRUE),
                   "lev_max"=max(level,na.rm=TRUE),
                   "temp_avg"=mean(temp_C,na.rm=TRUE),
                   "temp_min"=min(temp_C,na.rm=TRUE),
                   "temp_max"=max(temp_C,na.rm=TRUE)) %>%
  mutate("lev_7_avg"= runmean(lev_avg, k=7, endrule="mean",align="center"),
         "lev_7_min"= runmin(lev_min, k=7, align="center"),
         "lev_7_max"= runmax(lev_max, k=7, align="center"),
         "temp_7_avg"= runmean(temp_avg, k=7, endrule="mean",align="center"),
         "temp_7_min"= runmin(temp_min, k=7, align="center"),
         "temp_7_max"= runmax(temp_max, k=7, align="center")) %>%
  mutate("datetime"=ymd(strptime(paste0(year,"-", mon,"-", DOY),
                                 format = "%Y-%m-%j"))) %>%
  add_WYD(., "datetime") %>%
  mutate(type="baro") %>% 
  as.data.frame() %>% 
  select(site, datetime, type, -year, -mon, lev_avg:temp_7_max, DOY, WY, DOWY)

# Combine
# bind the two hourly datasets
dy.df <- bind_rows(dy.sol.df, dy.baro.df)

# remove temp df
rm(dy.sol.df, dy.baro.df)

# MASTER DAILY PLOTS -------------------------------------------------------

# Stage: Daily Avg
ggplot() + 
  geom_line(data=dy.df[dy.df$type=="solinst",], 
            aes(x=datetime, y=lev_avg, color=site, group=WY)) +
  facet_grid(site~., scales = "free_y")

# Stage: 7-Day Avg
ggplot() + 
  geom_line(data=dy.df[dy.df$type=="solinst",], 
            aes(x=datetime, y=lev_7_avg, color=site, group=WY)) +
  facet_grid(site~., scales = "free_y")

# WTemp: Daily Avg
ggplot() + 
  geom_line(data=dy.df[dy.df$type=="solinst",], 
            aes(x=datetime, y=temp_avg, color=site, group=WY)) +
  facet_grid(site~., scales = "free_y")

# WTemp: 7-Day Avg
ggplot() + 
  geom_line(data=dy.df[dy.df$type=="solinst",], 
            aes(x=datetime, y=temp_7_avg, color=site, group=WY)) +
  geom_ribbon(data=dy.df, aes(x=datetime, ymin=10,ymax=12), fill="orange", alpha=0.4) +
  facet_grid(site~.)

# plot a single SITE
ggplot() + 
  geom_path(data=dy.df[dy.df$site=="NFA" & dy.df$type=="solinst",], aes(x=datetime, y=lev_7_avg, group=WY), color="maroon")

# write out data
write_rds(dy.df, path = "data/2011-2016_solinst_mainstem_daily.rds", compress = "gz")
save(dy.df, file="data/2011-2016_solinst_mainstem_daily.rda")




# JOIN FROG DAT WITH HYDRO DAT --------------------------------------------


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
```
