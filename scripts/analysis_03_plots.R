# Analyze Data: Plots

# LOAD LIBRARIES ----------------------------------------------------------

library(beepr)
library(tidyverse)
library(lubridate)
library(ggrepel)
library(viridis)
library(caTools)
source("scripts/functions/f_doy.R")

# LOAD DATA ---------------------------------------------------------------

load("data/master_dat_2011-2016.rda") 
load("data/flow_dv_cfs_2011_6sites.rda")
load("data/flow_dv_cfs_full_6sites.rda")

# DATA PREP ---------------------------------------------------------------

df <- master_df
# df <- master_df %>% filter(site!="MFY")
#df <- master_df %>% filter(month(date)>3 & month(date)<8)

dowy_labs<-c("Oct","Nov","Dec","Jan", "Feb","Mar", "Mar-15", "Apr-01", "Apr-15", "May-01", "May-15", "Jun-01", "Jun-15", "Jul-01", "Jul-15", "Aug", "Aug-15", "Sep")
dowy_breaks<-c(1, 32, 62, 93, 124, 152, 167, 183, 198, 213, 228, 244, 259, 274, 289, 305, 320, 336)
dowys<-data.frame("mon"=dowy_labs, "dowy"=dowy_breaks)


# PLOTS: WTEMP vs DOWY ------------------------------------------------

# WTemp: 7-Day Avg w threshold
ggplot() + 
  geom_line(data=df, aes(x=DOY, y=temp_7_avg, color=as.factor(WY),
                         group=WY), show.legend = F) + 
  geom_point(data=df[!is.na(df$missData),], aes(x=DOY, y=temp_7_avg,
                                               fill=as.factor(WY)),
             show.legend = T, pch=21, color="gray20", size=4) + 
  scale_color_viridis(discrete = T, 
                      guide = guide_legend(title = "Water Year")) +
  scale_fill_viridis(discrete = T, 
                     guide = guide_legend(title = "Water Year")) +
  
  scale_x_continuous(breaks=c(105,120,135,150,165,180,195,210),
                     labels=c("Apr-15","May-1","May-15","Jun-1",
                              "Jun-15","Jul-1","Jul-15","Aug-1")) +
  scale_y_continuous(limits=c(0,27), breaks=seq(0,27,3)) + 
  geom_hline(yintercept = 10, color="maroon", lty=2)+
  #geom_hline(yintercept = 11, color="orange", lty=2)+
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1),
                     legend.key=element_blank()) + 
  labs(y=expression(paste("Water Temperature (",degree,"C)")), 
       title="7 Day Average Water Temperature", x="") +
  facet_grid(site~.)#, scales="free_x")

ggsave(filename = "figs/watertemp7_breeding_10C.png", width = 9, height = 6, units = "in")


# SUMMARIZE DATA ----------------------------------------------------------


filter(df, !is.na(missData)) %>% n_distinct()
filter(df, !is.na(missData) & temp_7_avg>10) %>% n_distinct()/25 # nearly 90% fall into temps above 7 day avg of 10 degrees C

# span of 64 days! (DOY 118 to 182)
# W_air_7_max range: 14.8-30.4, mean=24
# W_air_7_avg range: 9.3-22.4, mean=17.1
# temp_30_min range: 4.5, 11.0, mean=9.2
# temp_30_avg range: 5, 12.4, mean=10.3
# temp_7_max range: 7.5, 16.9, mean=13.1
# temp_7_avg range: 6.9, 14.7, mean=11.8
# Q_cfs: 78 to 2260



# PLOTS: LOG-Q vs DOY ---------------------------------------------------------


# Log Flows cfs
ggplot() + 
  geom_line(data=df, aes(x=DOY, y=Q_cfs, color=as.factor(WY)),
            show.legend = T) + 
  geom_point(data=df[!is.na(df$missData),], aes(x=DOY, y=Q_cfs, 
                                               fill=as.factor(WY)), 
             show.legend = T, pch=21, color="gray20", size=4) + 
  scale_color_viridis(discrete = T, 
                      guide = guide_legend(title = "Water Year & \n Oviposition Date")) +
  scale_fill_viridis(discrete = T, 
                     guide = guide_legend(title = "Water Year & \n Oviposition Date")) +
  scale_y_log10()+
  scale_x_continuous(breaks=c(105,120,135,150,165,180,195,210),
                     labels=c("Apr-15","May-1","May-15","Jun-1",
                              "Jun-15","Jul-1","Jul-15","Aug-1")) +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1),
                     legend.key=element_blank()) + 
  labs(y="log(Discharge) (cfs)", x="", 
       title="Log of Daily Flow (cfs)")+
  facet_grid(site~.)#, scales="free_y")

ggsave(filename = "figs/logflow_breeding.png", width = 9, height = 6, units = "in")



# PLOTS: Q vs. DOY -------------------------------------------------------

ggplot() + 
  geom_line(data=df, 
            aes(x=DOY, y=Q_cfs, color=as.factor(WY), group=WY), show.legend = F) + 
  
  geom_point(data=df[!is.na(df$missData),], aes(x=DOY, y=Q_cfs, fill=as.factor(WY)), 
             show.legend = T, pch=21, color="gray20", size=4) +
  facet_grid(site~., scales="free_x") + 
  scale_color_viridis(discrete = T, guide = guide_legend(title = "Water Year & \n Oviposition Date")) +
  scale_fill_viridis(discrete = T, guide = guide_legend(title = "Water Year & \n Oviposition Date")) + guides(color=FALSE)+
  scale_x_continuous(breaks=c(105,120,135,150,165,180,195,210),
                     labels=c("Apr-15","May-1","May-15","Jun-1",
                              "Jun-15","Jul-1","Jul-15","Aug-1")) +
  #scale_y_continuous(limits=c(0,27), breaks=seq(0,27,3)) + 
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1),
                     legend.key=element_blank()) + 
  #panel.grid.major = element_line(colour = 'gray80', linetype = 2)) +
  labs(y="Avg Daily Flow (cfs)", x="",
       title="Average Daily Flow (cfs)")


ggsave(filename = "figs/cfs_DOY_by_breeding.png", width = 9, height = 6, units = "in")

# PLOTS: LOG FLOW ALL YEARS ---------------------------------------------------

ggplot() + 
  geom_line(data=master_df, aes(x=date, y=Q_cfs+2, 
                color=as.factor(WY)), show.legend = T) + 
  
  geom_point(data=master_df[!is.na(master_df$missData),], 
             aes(x=date, y=Q_cfs+2, fill=as.factor(WY)), 
             show.legend = T, pch=21,  size=4) + 

  scale_color_viridis(discrete = T, guide = guide_legend(title = "Water Year & \n Oviposition Date")) +
  scale_fill_viridis(discrete = T, guide = guide_legend(title = "Water Year & \n Oviposition Date")) +
  scale_y_log10()+ guides(color=FALSE)+
  scale_x_date(date_breaks = "6 months", date_labels = "%b-%y")+
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1),
                     legend.key=element_blank()) + 
  labs(y="Avg Daily [log]Flow (cfs)", x="",
       title="Average Daily log Flow (cfs)")+
  facet_grid(site~., scales="free_y")

ggsave(filename = "figs/cfs_log_by_WY_breeding.png", width = 9, height = 6, units = "in")


# CV by Site & WY -------------------------------------------------------------

ggplot() + geom_line(data=master_df, aes(x=DOWY, y=temp_CV, color=site)) +
  facet_grid(site~., scales = "free_x") +
 scale_x_continuous(breaks=c(1, 32, 62, 93, 121,
                            152, 182, 213, 243,
                             274, 305, 336),
                    labels=c("Oct", "Nov", "Dec", "Jan", "Feb",
                             "Mar", "Apr","May","Jun",
                             "Jul","Aug","Sep")) +
  geom_ribbon(data=df[df$DOWY>182 & df<273,], aes(x=DOWY, ymin=0, ymax=60), fill="forestgreen", alpha=0.3)


# Q-7_cfs -----------------------------------------------------------------

ggplot() + 
  geom_line(data=master_df, 
            aes(x=DOWY, y=Q_7_cfs, color=as.factor(WY)), show.legend = F) +
  scale_color_viridis("WY",discrete = T) +
  geom_point(data=master_df[!is.na(master_df$missData),], 
             aes(x=DOWY, y=Q_7_cfs, fill=as.factor(WY)), 
             pch=21,color="gray20",size=4) + 
  scale_fill_viridis("WY",discrete = T)+
  scale_x_continuous(breaks=c(1, 32, 62, 93, 121,
                              152, 182, 213, 243,
                              274, 305, 336),
                     labels=c("Oct", "Nov", "Dec", "Jan", "Feb",
                              "Mar", "Apr","May","Jun",
                              "Jul","Aug","Sep")) +
  facet_grid(site~., scales="free")

# Q_7_CV
ggplot() + 
  geom_line(data=master_df, 
            aes(x=DOWY, y=Q_7_CV, color=as.factor(WY)), show.legend = F) +
  scale_color_viridis("WY",discrete = T) +
  geom_point(data=master_df[!is.na(master_df$missData),], 
             aes(x=DOWY, y=Q_7_CV, fill=as.factor(WY)), 
             pch=21,color="gray20",size=4) + 
  scale_fill_viridis("WY",discrete = T)+
  scale_x_continuous(breaks=c(1, 32, 62, 93, 121,
                              152, 182, 213, 243,
                              274, 305, 336),
                     labels=c("Oct", "Nov", "Dec", "Jan", "Feb",
                              "Mar", "Apr","May","Jun",
                              "Jul","Aug","Sep")) +
  facet_grid(site~., scales="free")

# deltQ
ggplot() + 
  geom_line(data=master_df, 
            aes(x=DOWY, y=deltQ, color=as.factor(WY)), show.legend = F) +
  scale_color_viridis("WY",discrete = T) +
  geom_point(data=master_df[!is.na(master_df$missData),], 
             aes(x=DOWY, y=deltQ, fill=as.factor(WY)), 
             pch=21,color="gray20",size=4) + xlab("")+ 
  scale_fill_viridis("WY",discrete = T)+ ylim(-2,1)+
  scale_x_continuous(breaks=c(1, 32, 62, 93, 121,
                              152, 182, 213, 243,
                              274, 305, 336),
                     labels=c("Oct", "Nov", "Dec", "Jan", "Feb",
                              "Mar", "Apr","May","Jun",
                              "Jul","Aug","Sep")) +
  facet_grid(site~., scales="free")



# OLD PLOTS ---------------------------------------------------------------

# WTemp: 7-Day Avg w threshold
ggplot() + 
  geom_line(data=df, 
            aes(x=date, y=temp_7_avg, color=site, group=WY), show.legend = F) +
  geom_ribbon(data=df, aes(x=date, ymin=10,ymax=12), fill="orange", alpha=0.4) + xlab("")+ ylab("7-day Avg Water Temp (C)")+
  geom_point(data=df[!is.na(df$missData),], aes(x=date, y=temp_7_avg, fill=site), show.legend = F, pch=21,color="gray20",size=4) + 
  facet_grid(site~WY, scales="free_x")

ggsave(filename = "figs/watertemp7_breeding.png", width = 9, height = 6, units = "in")


# Stage: 7-day avg
ggplot() + 
  geom_line(data=df, 
            aes(x=date, y=lev_7_avg, color=site, group=WY), show.legend = F) +
  geom_point(data=df[!is.na(df$missData),], aes(x=date, y=lev_7_avg, fill=site), show.legend = F,
             pch=21,color="gray20",size=4) + 
  xlab("")+ ylab("7-day Avg Stage (m)")+
  facet_grid(site~WY, scales="free")

ggsave(filename = "figs/stage7_breeding.png", width = 9, height = 6, units = "in")



