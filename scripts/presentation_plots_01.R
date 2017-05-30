# presentation Plots

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
dfv<-flowdf %>% filter(site=="NFA") %>% filter(month(date)>3 & month(date)<8)

#df <- master_df
df <- master_df %>% filter(site=="NFA") %>% filter(month(date)>3 & month(date)<8)

dowy_labs1<-c("Oct","Nov","Dec","Jan", "Feb","Mar", "Mar-15", "Apr-01", "Apr-15", "May-01", "May-15", "Jun-01", "Jun-15", "Jul-01", "Jul-15", "Aug", "Aug-15", "Sep")
dowy_breaks<-c(1, 32, 62, 93, 124, 152, 167, 183, 198, 213, 228, 244, 259, 274, 289, 305, 320, 336)
dowy_labs2<-c("Oct","Nov","Dec","Jan", "Feb","Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep")
dowy_breaks2<-c(1, 32, 62, 93, 124, 152, 183, 213, 244, 274, 305, 336)
#dowys<-data.frame("mon"=dowy_labs, "dowy"=dowy_breaks)


#ggsave(filename = "figs/watertemp7_DOWY_spawn_2011.png", width = 9, height = 6, units = "in")

# PLOTS: WTEMP vs DOWY ------------------------------------------------

# WTemp: 7-Day Avg w threshold
ggplot() + 
  geom_line(data=df, aes(x=DOY, y=temp_7_max, color=as.factor(WY),
                         group=WY), show.legend = F) + 
  geom_point(data=df[!is.na(df$missData),], aes(x=DOY, y=temp_7_max,
                                                fill=as.factor(WY)),
             show.legend = T, pch=21, color="gray20", size=4) + 
  scale_color_viridis(discrete = T, 
                      guide = guide_legend(title = "Water Year")) +
  scale_fill_viridis(discrete = T, 
                     guide = guide_legend(title = "Water Year")) +
  
  scale_x_continuous(breaks=c(90, 105,120,135,150,165,180,195,210),
                     labels=c("Apr-1", "Apr-15","May-1","May-15","Jun-1",
                              "Jun-15","Jul-1","Jul-15","Aug-1")) +
  scale_y_continuous(limits=c(0,27), breaks=seq(0,27,3)) + 
  geom_hline(yintercept = 12, color="maroon", lty=2)+
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1),
                     legend.key=element_blank()) + 
  labs(y=expression(paste("Water Temperature (",degree,"C)")), 
       title="7 Day Max Water Temperature", x="") +
  facet_grid(site~.)#, scales="free_x")

ggsave(filename = "figs/watertemp7mx_spawn_12C.png", width = 9, height = 6, units = "in")

# Q vs. DOWY --------------------------------------------------------------


(gQ <- ggplot() + 
   # add all WYs
   geom_line(data=dfv, 
             aes(x=DOY, y=flow_cfs, group=WY), 
             color="gray60", alpha=0.7, show.legend = F) +
   ylim(c(0,10000))+
   geom_line(data=df, 
             aes(x=DOY, y=Q_cfs, color=as.factor(WY), group=WY), show.legend = F) + 
   
   geom_point(data=df[!is.na(df$missData),], aes(x=DOY, y=Q_cfs, fill=as.factor(WY)), 
              show.legend = T, pch=21, color="gray20", size=5) +
   #facet_grid(site~., scales="free_x") + 
   scale_color_viridis(discrete = T, guide = guide_legend(title = "Water Year & \n Spawn Date")) +
   scale_fill_viridis(discrete = T, guide = guide_legend(title = "Water Year & \n Spawn Date")) + guides(color=FALSE)+
   scale_x_continuous(breaks=c(90, 105,120,135,150,165,180,195,210),
                      labels=c("Apr-1", "Apr-15","May-1","May-15","Jun-1",
                               "Jun-15","Jul-1","Jul-15","Aug-1")) +
   #scale_y_continuous(limits=c(0,27), breaks=seq(0,27,3)) + 
   theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1, size=12),
                      legend.key=element_blank(),
                      axis.text.y = element_text(hjust = 1, size=12)) + 
   #panel.grid.major = element_line(colour = 'gray80', linetype = 2)) +
   labs(y="Avg Daily Flow (cfs)", x="",
        title="Average Daily Flow (cfs) & Spawn Date (NFA)")) +
  annotate(geom = "text", 180, y = 8000, col="gray50",
           label="Daily Flows \n USGS 11427000 (1942-2017)", 
          cex=3)
  

ggsave(filename = "figs/Q_cfs_spawndate_NFA.png", width = 9, height = 6, units = "in")



# Delta Q -----------------------------------------------------------------

# deltQ

(gQ <- ggplot() + 
    
   # specific data
   geom_line(data=df, 
              aes(x=DOY, y=deltQ, color=as.factor(WY), group=WY), show.legend = F) + 
    
    geom_point(data=df[!is.na(df$missData),], aes(x=DOY, y=deltQ, fill=as.factor(WY)), 
               show.legend = T, pch=21, color="gray20", size=5) +
    #facet_grid(site~., scales="free_x") + 
    scale_color_viridis(discrete = T, guide = guide_legend(title = "Water Year & \n Spawn Date")) +
    scale_fill_viridis(discrete = T, guide = guide_legend(title = "Water Year & \n Spawn Date")) + guides(color=FALSE)+
    scale_x_continuous(breaks=c(90, 105,120,135,150,165,180,195,210),
                       labels=c("Apr-1", "Apr-15","May-1","May-15","Jun-1",
                                "Jun-15","Jul-1","Jul-15","Aug-1")) +
    #scale_y_continuous(limits=c(0,27), breaks=seq(0,27,3)) + 
    theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1, size=12),
                       legend.key=element_blank(),
                       axis.text.y = element_text(hjust = 1, size=12)) + 
    #panel.grid.major = element_line(colour = 'gray80', linetype = 2)) +
    labs(y="Avg Daily Flow (cfs)", x="",
         title="Average Daily Flow (cfs) & Spawn Date (NFA)"))
ggsave(filename = "figs/Q_cfs_spawndate_NFA.png", width = 9, height = 6, units = "in")


