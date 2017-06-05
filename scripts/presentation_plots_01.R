# presentation Plots

# LOAD LIBRARIES ----------------------------------------------------------

#library(beepr)
library(tidyverse)
library(lubridate)
library(ggrepel)
library(viridis)
library(caTools)
source("scripts/functions/f_doy.R")
source("scripts/functions/f_USGS_get_raw_dat.R")

# LOAD DATA ---------------------------------------------------------------

load("data/master_dat_2011-2016.rda") 
load("data/flow_dv_cfs_2011_6sites.rda")
load("data/flow_dv_cfs_full_6sites.rda")

# GET NFA USGS flow
#load("data/NFA_dv_USGS_1941_2017-05-30.rda") 
#NFA_dv <- NFA_dv %>% dplyr::filter(month(date)>3 & month(date)<8)
#get.USGS.raw(gage=11427000, river = "NFA", sdate = "1920-01-01",saveRaw = F, daily = T) 
#save(NFA_dv,file =paste0("data/NFA_dv_USGS_1941_",Sys.Date(), ".rda"))

cfs_cms <- function(cfs){
  cfs * 0.028316847 # cfs * cms
}

#cms=0.028316847*cfs

# DATA PREP ---------------------------------------------------------------

#dfv<-flowdf %>% filter(site=="NFA") %>% filter(month(date)>3 & month(date)<8)

#df <- flow_dv %>% filter(!site=="MFA", !site=="MFY") #%>% filter(mont

df <- master_df %>% filter(!site=="MFA", !site=="MFY") #%>% filter(month(date)>3 & month(date)<8)
#table(df$site)

#df <- master_df %>% filter(site=="NFA") %>% filter(month(date)>3 & month(date)<8)

dowy_labs1<-c("Oct","Nov","Dec","Jan", "Feb","Mar", "Mar-15", "Apr-01", "Apr-15", "May-01", "May-15", "Jun-01", "Jun-15", "Jul-01", "Jul-15", "Aug", "Aug-15", "Sep")
dowy_breaks<-c(1, 32, 62, 93, 124, 152, 167, 183, 198, 213, 228, 244, 259, 274, 289, 305, 320, 336)
dowy_labs2<-c("Oct","Nov","Dec","Jan", "Feb","Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep")
dowy_breaks2<-c(1, 32, 62, 93, 124, 152, 183, 213, 244, 274, 305, 336)
#dowys<-data.frame("mon"=dowy_labs, "dowy"=dowy_breaks)


# RUBICONvNFA HYDRO+Breeding --------------------------------------------------
load("data/flow_hv_cfs_6sites.rda")

# for hourly
rub <- flow_hv %>% filter(site=="RUB" & WY==2011 & month(datetime)>3 & month(datetime)<8)

nfa <- flow_hv %>% filter(site=="NFA" & WY==2011 & month(datetime)>3 & month(datetime)<8)

# for daily (and breeding points only)
rubBreed <- df %>% filter(site=="RUB" & WY==2011 & month(date)>3 & month(date)<8 & !is.na(missData)) # 5/26, and 6/28

with(rub, plot(datetime, flow_cfs, typ="l"))
# temp7max exceeds 9 on 5/20
first(rubBreed$date[which(rubBreed$temp_7_max>9)])

# rub plot
ggplot() + 
  geom_line(data=rub, aes(x=datetime, y=flow_cfs* 0.028316847), 
            show.legend = F, size=0.9, alpha=0.9) + 
  # early breeding RUB
  geom_point(aes(x=ymd_hms("2011-05-26 12:00:00"), y=485.8* 0.028316847), fill=c("darkblue"), pch=21, show.legend = T, size=7) +
  # late breeding RUB
  geom_point(aes(x=ymd_hms("2011-06-28 12:00:00"), y=1252.6* 0.028316847), fill=c("red4"), pch=21, show.legend = T, size=7)+
  scale_x_datetime("", date_breaks = "2 weeks", date_labels = "%b-%d")+
  #geom_vline(aes(xintercept = ymd_hms("2011-05-20 12:00:00")), color="maroon", lty=2, lwd=1, alpha=0.8)+
  theme_classic(base_size = 12) +
  labs(y="Mean Daily Flow (cms)", 
       title="Rubicon Hourly Discharge", x="") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=12),
        legend.key=element_blank(),
        #legend.key.height = unit(0.5,"mm"),
        #legend.key.size = unit(1,"mm"),
        legend.position=c(0.1,.8),
        axis.text.y = element_text(hjust = 1, size=12))

ggsave(filename = "figs/rubicon_flow_spawn_2011.png", width = 9.6, height=3.7, units = "in")

## NFA SPAWN 2011
# for hourly:
nfa <- flow_hv %>% filter(site=="NFA" & WY==2011 & month(datetime)>3 & month(datetime)<8)

# temp7max permanently exceeds 9 on 5/1 for NFA
#first(nfa$date[which(nfa$temp_7_max>9)])
# for daily (and breeding points only)
nfaBreed <- df %>% filter(site=="NFA" & WY==2011 & month(date)>3 & month(date)<8 & !is.na(missData)) # 6/30 

# hourly

ggplot() + 
  geom_line(data=nfa, aes(x=datetime, y=flow_cfs* 0.028316847), 
            show.legend = F, size=0.9, alpha=0.9) + 
  # Eggs
  geom_point(aes(x=ymd_hms("2011-06-29 12:00:00"), y=2617* 0.028316847), fill=c("red4"), pch=21, show.legend = T, size=7)+
  scale_x_datetime("", date_breaks = "2 weeks", date_labels = "%b-%d")+
  theme_classic(base_size = 12) +
  labs(y="Mean Daily Flow (cms)", 
       title="NFA Hourly Discharge", x="") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=12),
        legend.key=element_blank(),
        #legend.key.height = unit(0.5,"mm"),
        #legend.key.size = unit(1,"mm"),
        #legend.position=c(0.1,.8),
        axis.text.y = element_text(hjust = 1, size=12))

ggsave(filename = "figs/nfa_flow_spawn_2011.png", width = 9.6, height=3.4, units = "in")



# RUB BIOSCI PLOT --------------------------------------------------

# RUB version
load("data/RUB_dv_PCWA_2009-2016.rda") 
rub_sum <- RUB_dv %>% 
  group_by(DOWY) %>% 
  summarize(meanAnnQ=mean(flow_avg_cfs, na.rm = T),
            q95=quantile(flow_avg_cfs, probs=c(0.95)),
            q05=quantile(flow_avg_cfs, probs=c(0.05))) %>% 
  mutate(meanAnnQ_cms = cfs_cms(meanAnnQ),
         q95_cms = cfs_cms(q95),
         q05_cms = cfs_cms(q05))

# using RUB data
(gBiosciRUB <- ggplot() +
    # add all WYs
    geom_ribbon(data=rub_sum, aes(x=DOWY, ymax=q95_cms, ymin=q05_cms),
                fill="gray60", lwd=0.5, alpha=0.8, show.legend = F) +
    geom_line(data=rub_sum,
              aes(x=DOWY, y=meanAnnQ_cms),
              color="black", lwd=1.5, show.legend = F) +
    scale_x_continuous(breaks=dowy_breaks2, labels=dowy_labs2) +
    theme_classic(base_size = 14) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size=12),
          legend.key=element_blank(),
          axis.text.y = element_text(hjust = 1, size=12))+
    labs(y="Mean Annual Discharge (cms)", x="",
         title="Rubicon River (1996-2017)"))

ggsave(filename = "figs/Q_RUB_mean_ann_discharge_plain.png", width = 9, height=6, units="in", dpi=300)

# MFA BIOSCI PLOT --------------------------------------------------

# MFA version
load("data/MFA_dv_CDEC_OXB_1997-2017.rda") 
mfa_sum <- MFA_dv %>% 
  group_by(DOWY) %>% 
  summarize(meanAnnQ=mean(flow_avg_cfs, na.rm = F),
            q95=quantile(flow_avg_cfs, probs=c(0.95)),
            q05=quantile(flow_avg_cfs, probs=c(0.05))) %>% 
  mutate(meanAnnQ_cms = cfs_cms(meanAnnQ),
         q95_cms = cfs_cms(q95),
         q05_cms = cfs_cms(q05))

# using MFA data
(gBiosciMFA <- ggplot() +
    # add all WYs
    geom_ribbon(data=mfa_sum, aes(x=DOWY, ymax=q95_cms, ymin=q05_cms),
                fill="gray60", lwd=0.5, alpha=0.8, show.legend = F) +
    geom_line(data=mfa_sum,
              aes(x=DOWY, y=meanAnnQ_cms),
              color="black", lwd=1.5, show.legend = F) +
    scale_x_continuous(breaks=dowy_breaks2, labels=dowy_labs2) +
    theme_classic(base_size = 14) + #ylim(c(0,300))+
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size=12),
          legend.key=element_blank(),
          axis.text.y = element_text(hjust = 1, size=12))+
        labs(y="Mean Annual Discharge (cms)", x="",
         title="Middle Fork American River (1996-2017)") +
    annotate(geom = "text", 300, y = 300, col="gray50",
            label="*5th & 95th percentiles shaded",
            cex=3))

ggsave(filename = "figs/Q_MFA_mean_ann_discharge_plain.png", width = 9, height=6, units="in", dpi=300)

# NFA BIOSCI PLOT --------------------------------------------------

load("data/NFA_dv_USGS_1941_2017-05-30.rda") 

# summarize data with 5/95
nfa_sum <- NFA_dv %>% 
  group_by(DOWY) %>% 
  summarize(meanAnnQ=mean(flow_cfs, na.rm = T),
            q95=quantile(flow_cfs, probs=c(0.95)),
            q05=quantile(flow_cfs, probs=c(0.05))) %>% 
  mutate(meanAnnQ_cms = cfs_cms(meanAnnQ),
         q95_cms = cfs_cms(q95),
         q05_cms = cfs_cms(q05))

head(nfa_sum)

# using NFA data
(gBiosci <- ggplot() +
   # add all WYs
    geom_ribbon(data=nfa_sum, aes(x=DOWY, ymax=q95_cms, ymin=q05_cms),
              fill="gray60", lwd=0.5, alpha=0.8, show.legend = F) +
    geom_line(data=nfa_sum,
              aes(x=DOWY, y=meanAnnQ_cms),
              color="black", lwd=1.5, show.legend = F) +
    scale_x_continuous(breaks=dowy_breaks2, labels=dowy_labs2) +
    theme_classic(base_size = 14) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size=12),
          legend.key=element_blank(),
          axis.text.y = element_text(hjust = 1, size=12))+
    labs(y="Mean Annual Discharge (cms)", x="",
         title="North Fork American River (1942-2017)") +
    annotate(geom = "text", 300, y = 300, col="gray50",
             label="Gage data: USGS 11427000 \n *5th & 95th percentiles shaded",
             cex=3))
ggsave(filename = "figs/Q_NFA_mean_ann_discharge_plain.png", width = 9, height=6, units="in", dpi=300)

# RUB-NFA-MFA AnnFlow --------------------------------------------------------

mfa_sum$site<-"MF American"
nfa_sum$site<-"NF American"
rub_sum$site<-"Rubicon"
site_sum<-bind_rows(mfa_sum,rub_sum, nfa_sum)

(gBiosciAll <- ggplot() +
    # geom_line(data=mfa_sum,
    #           aes(x=DOWY, y=meanAnnQ_cms),
    #           color="#440154FF", lwd=1.5, show.legend = T) +
    # geom_line(data=nfa_sum,
    #           aes(x=DOWY, y=meanAnnQ_cms),
    #           color="#21908CFF", lwd=1.5, show.legend = T) +
    # geom_line(data=rub_sum,
    #           aes(x=DOWY, y=meanAnnQ_cms),
    #           color="#FDE725FF", lwd=1.5, show.legend = T) +
    # limit to specific sites with: [!site_sum$site=="MF American",]
    geom_line(data=site_sum[!site_sum$site=="MF American",], aes(x=DOWY, y=meanAnnQ_cms,color=as.factor(site)),
              lwd=1.5, show.legend = T) + 
    scale_color_manual("River", values=c("NF American"="#21908CFF", "MF American"= "#440154FF", "Rubicon"="#F1605DFF"))+
    scale_x_continuous(breaks=dowy_breaks2, labels=dowy_labs2) +
    theme_classic(base_size = 14) + ylim(c(0,100))+
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size=12),
          legend.key=element_blank(),
          legend.position=c(0.8,.8),
          axis.text.y = element_text(hjust = 1, size=12))+
    labs(y="Mean Annual Discharge (cms)", x=""))

ggsave(filename = "figs/Q_AMR_mean_ann_discharge_combined.png", width = 9, height=6, units="in", dpi=300)

ggsave(filename = "figs/Q_AMR_mean_ann_discharge_combined-RUB-NFA.png", width = 9, height=6, units="in", dpi=300)

# PLOTS: LogFLOW_DOWY -----------------------------------------------------

# Log Flows cfs
ggplot() + 
  geom_line(data=df, aes(x=date, y=Q_cfs),
            show.legend = F) + 
  geom_point(data=df[!is.na(df$missData),], aes(x=date, y=Q_cfs, 
                                                fill=as.factor(WY)), 
             show.legend = T, pch=21, color="gray20", size=6) + 
  #scale_color_viridis(discrete = T, 
   #                   guide = guide_legend(title = "Water Year & \n Oviposition Date")) +
  scale_fill_viridis(discrete = T, 
                     guide = guide_legend(title = "Water Year & \n Oviposition Date")) +
  #ylim(c(0,50))+
  scale_y_log10()+
  scale_x_date(date_breaks = "1 year",date_labels = "%Y") +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1),
                     legend.key=element_blank()) + 
  labs(y="log(Discharge) (cfs)", x="", 
       title="Log of Daily Flow (cfs)")+
  facet_grid(site~.)#, scales="free_y")

ggsave(filename = "figs/logflow_breeding.png", width = 9, height = 6, units = "in")

# PLOTS: WTEMP vs DOWY ------------------------------------------------

# WTemp: 7-Day max w threshold
ggplot() + 
  geom_point(data=df, aes(x=DOY, y=temp_7_max, color=as.factor(WY),
                         group=WY), show.legend = F, pch=16, size=0.7, alpha=0.5) + 
  geom_point(data=df[!is.na(df$missData),], aes(x=DOY, y=temp_7_max,
                                                fill=as.factor(WY), shape=as.factor(site)), color="gray70",
             show.legend = T, size=5, alpha=0.9) +
  guides(fill=FALSE, color=F)+
  scale_shape_manual("Site", values =c("NFY"=22,"NFA"=21, "RUB"=23, "SFY"=24, "MFY"=25), breaks=c("NFY","NFA","RUB","SFY","MFY"), labels=c("NFY","NFA","RUB","SFY","MYF")) +
  scale_fill_viridis("Water Year", discrete = T, option = "A")+
  scale_color_viridis("Water Year", discrete = T, option = "A")+
  #scale_fill_manual("Water Year", values =c("2011"="#000004FF","2012"="#3B0F70FF", "2013"="#8C2981FF", "2014"="#DE4968FF", "2015"="#FE9F6DFF", "2016"="#FCFDBFFF",labels=c("2011", "2012","2013","2014","2015","2016"))) +
  
  scale_x_continuous(breaks=c(90, 105,120,135,150,165,180,195,210),
                     labels=c("Apr-1", "Apr-15","May-1","May-15","Jun-1",
                              "Jun-15","Jul-1","Jul-15","Aug-1")) +
  scale_y_continuous(limits=c(0,27), breaks=seq(0,27,3)) + 
  geom_hline(yintercept = 9, color="maroon", lty=2, lwd=1, alpha=0.8)+
  theme_classic(base_size = 12) +
  labs(y=expression(paste("Water Temperature (",degree,"C)")), 
       title="7 Day Max Water Temperature", x="") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=12),
        legend.key=element_blank(),
        #legend.key.height = unit(0.5,"mm"),
        #legend.key.size = unit(1,"mm"),
        legend.position=c(0.1,.8),
        axis.text.y = element_text(hjust = 1, size=12))
  #facet_grid(site~.)#, scales="free_x")

ggsave(filename = "figs/water7mx_spawn_9C.png", width = 9, height = 6, units = "in")

# PLOTS: Q_CV vs DOWY ------------------------------------------------

# WTemp: 7-Day max w threshold
ggplot() + 
  geom_point(data=df, aes(x=DOY, y=Q_CV, color=as.factor(WY),
                          group=WY), show.legend = F, pch=16, size=0.7, alpha=0.5) + 
  geom_point(data=df[!is.na(df$missData),], aes(x=DOY, y=Q_CV,
                                                fill=as.factor(WY), shape=as.factor(site)), color="gray30",
             show.legend = T, size=5, alpha=0.9) +
  guides(color=F, fill=F)+
  scale_shape_manual("Site", values =c("NFY"=22,"NFA"=21, "RUB"=23, "SFY"=24, "MFY"=25), breaks=c("NFY","NFA","RUB","SFY","MFY"), labels=c("NFY","NFA","RUB","SFY","MYF")) +
  scale_fill_viridis("Water Year", discrete = T, option = "A")+
  scale_color_viridis("Water Year", discrete = T, option = "A")+
  scale_x_continuous(breaks=c(90, 105,120,135,150,165,180,195,210),
                     labels=c("Apr-1", "Apr-15","May-1","May-15","Jun-1",
                              "Jun-15","Jul-1","Jul-15","Aug-1")) +
  scale_y_continuous(limits=c(0,26), breaks=seq(0,26,2)) + 
  geom_hline(yintercept = 8, color="gray", lty=2, lwd=1)+
  theme_classic(base_size = 12) +
  labs(y="% Coefficient Variation of Daily Discharge", 
       title="CV Q", x="") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=12),
        legend.key=element_blank(),
        legend.position=c(0.1,.8),
        axis.text.y = element_text(hjust = 1, size=12))

ggsave(filename = "figs/CV_Q_w_spawn.png", width = 9, height = 6, units = "in")



# PLOTS: FlowShade vs DOWY ------------------------------------------------
df1 <- df %>% filter(WY<2017) # for one by one

ggplot() + 
  geom_ribbon(data=df1[df1$site=="NFY",], aes(x=DOY,ymin=0, ymax=Q_cfs, fill=as.factor(WY),
                                              group=WY), show.legend = F, lwd=0.5, alpha=0.5, color="skyblue") +
  geom_ribbon(data=df1[df1$site=="NFA",], aes(x=DOY,ymin=0, ymax=Q_cfs, fill=as.factor(WY),
                          group=WY), show.legend = F, lwd=0.5, alpha=0.5, color="blue2") + 
  geom_ribbon(data=df1[df1$site=="SFY",], aes(x=DOY,ymin=0, ymax=Q_cfs, fill=as.factor(WY),
                                            group=WY), show.legend = F, lwd=0.5, alpha=0.5, color="purple") + 
  geom_ribbon(data=df1[df1$site=="RUB",], aes(x=DOY,ymin=0, ymax=Q_cfs, fill=as.factor(WY),
                                              group=WY), show.legend = F, lwd=0.5, alpha=0.5, color="maroon") + 
  
  #geom_ribbon(data=df1[df1$site=="MFY",], aes(x=DOY,ymin=0, ymax=Q_cfs, fill=as.factor(WY),
  #                                            group=WY), show.legend = F, lwd=0.5, alpha=0.5, color="orange") + 
  
  geom_point(data=df1[!is.na(df1$missData),], aes(x=DOY, y=Q_cfs,
                                                shape=as.factor(site)), fill="gray90",color="red",
             show.legend = T, size=5, alpha=0.9) +
  guides(fill=FALSE, color=F)+
  scale_shape_manual("Site", values =c("NFY"=22,"NFA"=21, "RUB"=23, "SFY"=24, "MFY"=25), breaks=c("NFY","NFA","RUB","SFY","MFY"), labels=c("NFY","NFA","RUB","SFY","MFY")) +
  #scale_color_manual("Site", values =c("NFY"="#000004FF","NFA"="#3B0F70FF", "RUB"="#8C2981FF", "SFY"="#DE4968FF", "MFY"="#FE9F6DFF"))+
  
  #scale_fill_viridis("Site", discrete = T, option = "A")+
  #scale_color_viridis("Water Year", discrete = T, option = "A")+
  scale_fill_manual("Water Year", values =c("2011"="#000004FF","2012"="#3B0F70FF", "2013"="#8C2981FF", "2014"="#DE4968FF", "2015"="#FE9F6DFF", "2016"="#FCFDBFFF"),labels=c("2011", "2012","2013","2014","2015","2016")) +
  
  scale_x_continuous(breaks=c(90, 105,120,135,150,165,180,195,210),
                     labels=c("Apr-1", "Apr-15","May-1","May-15","Jun-1",
                              "Jun-15","Jul-1","Jul-15","Aug-1")) +
  theme_bw() +
  labs(y="Q Daily (cfs)", 
       title="Mean Daily Flow (cfs)", x="") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=12),
        legend.key=element_blank(),
        #legend.position=c(0.85,.8), # for single years
        legend.position=c(0.95,.2), # for facetted
        axis.text.y = element_text(hjust = 1, size=12))+
  facet_grid(WY~., scales="free_y")

ggsave(filename = "figs/Q_shade_allyrs_spawn_freeY.png", width = 9, height = 6, units = "in")
ggsave(filename = "figs/Q_shade_2016_spawn.png", width = 9, height = 6, units = "in")

# PLOTS: WTEMP vs Q_CV ------------------------------------------------

# WTemp: 7-Day max vs CV
ggplot() + 
  geom_point(data=df[!df$site=="MFY",], 
             aes(y=Q_CV, x=temp_7_max, group=WY), 
             show.legend = F, pch=16, size=0.7, alpha=0.5) + 
  ylim(c(0,30)) +
  geom_point(data=df[!is.na(df$missData) & !df$site=="MFY",], 
             aes(y=Q_CV, x=temp_7_max, fill=as.factor(site)), 
             color="gray70", pch=21, show.legend = T, size=6, alpha=1) +
  guides(color=F)+
  scale_fill_viridis("Spawn Site", discrete = T, option = "B")+
  scale_color_viridis("Spawn Site", discrete = T, option = "B")+
  scale_x_continuous(limits=c(6,27), breaks=seq(6,27,3)) + 
  theme_classic(base_size = 13) +
  labs(x=expression(paste("Max 7-day Mean Water Temperature (",degree,"C)")), 
        y="%CV of Daily Flow") + 
  theme(legend.key=element_blank(),
        legend.position=c(0.9,.7))
  #geom_vline(xintercept = 9, color="gray50", lty=2, lwd=1.4, alpha=0.8)
#facet_grid(site~.)#, scales="free_x")

ggsave(filename = "figs/water7mx_Q_CV_spawn.png", width = 9, height = 3.7, units = "in")

# PLOTS: oldCV vs. DOWY: ---------------------------------------------------------

(gCV <- ggplot() +
   # add all WYs
   geom_line(data=df[!df$site=="MFY",],
             aes(x=DOY, y=Q_CV, color=as.factor(WY), group=WY), show.legend = F, lwd=0.8, alpha=0.8) +
   geom_point(data=df[!is.na(df$missData) & !df$site=="MFY",], aes(x=DOY, y=lev_CV, fill=as.factor(WY)), shape=21, show.legend = T, size=6) +
   scale_color_viridis(discrete = T, option = "A", guide = guide_legend(title = "Water Year & \n Spawn Date")) +
   #guides(shape=guide_legend(title="Site"))+
   scale_fill_viridis(discrete = T, option = "A", guide = guide_legend(title = "Water Year & \n Spawn Date")) + guides(color=F)+
   scale_x_continuous(breaks=c(90, 105,120,135,150,165,180,195,210),
                      labels=c("Apr-1", "Apr-15","May-1","May-15","Jun-1",
                               "Jun-15","Jul-1","Jul-15","Aug-1")) +
   theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1, size=12),
                      legend.key=element_blank(),
                      #legend.key.height = unit(0.5,"mm"),
                      #legend.key.size = unit(1,"mm"),
                      #legend.position=c(.9,.55),
                      axis.text.y = element_text(hjust = 1, size=12))+
   labs(y="Daily Coefficient of Variation in Stage", x=""))# +
   #facet_grid(site~.))


# save
#ggsave(filename = "figs/Q_cfs_NFA_01_historical.png", width = 9, height = 6, units = "in")
#ggsave(filename = "figs/Q_cfs_NFA_02_historical-w2017.png", width = 9, height = 6, units = "in")
#ggsave(filename = "figs/Q_cfs_NFA_03_hist_2011.png", width = 9, height = 6, units = "in")
#ggsave(filename = "figs/Q_cfs_NFA_04_hist_2012.png", width = 9, height = 6, units = "in")
#ggsave(filename = "figs/Q_cfs_NFA_05_hist_2013.png", width = 9, height = 6, units = "in")
#ggsave(filename = "figs/Q_cfs_NFA_06_hist_2014.png", width = 9, height = 6, units = "in")
#ggsave(filename = "figs/Q_cfs_NFA_07_hist_2015.png", width = 9, height = 6, units = "in")
#ggsave(filename = "figs/Q_cfs_NFA_08_hist_2016.png", width = 9, height = 6, units = "in")
ggsave(filename = "figs/Q_cfs_NFA_09_spawn_all.png", width = 9, height = 6, units = "in")



# PLOTS: Q vs. DOWY: cfs ---------------------------------------------------------

(gQ <- ggplot() +
   # add all WYs
   geom_line(data=NFA_dv[!NFA_dv$WY==2017,],
             aes(x=DOY, y=flow_cfs, group=WY),
             color="gray60", alpha=0.7, show.legend = F) +
   ylim(c(0,10000))+
   #geom_line(data=NFA_dv[NFA_dv$WY==2017,],
  #           aes(x=DOY, y=flow_cfs, group=WY),
   #          color="black", alpha=0.8, lwd=1, lty=2, show.legend = F) +
   geom_line(data=df[df$WY>=2011 & df$WY<=2016,],
             aes(x=DOY, y=Q_cfs, color=as.factor(WY), group=WY), show.legend = F, lwd=2) +
   # geom_line(data=df,
   #           aes(x=DOY, y=Q_cfs, color=as.factor(WY), group=WY), show.legend = F, lwd=2) +
   geom_point(data=df[!is.na(df$missData),], aes(x=DOY, y=Q_cfs, fill=as.factor(WY)),
              show.legend = T, pch=21, color="black", size=7.5) +
   scale_color_viridis(discrete = T, option = "A", guide = guide_legend(title = "Water Year & \n Spawn Date")) +
   scale_fill_viridis(discrete = T, option = "A", guide = guide_legend(title = "Water Year & \n Spawn Date")) + guides(color=FALSE)+
   scale_x_continuous(breaks=c(90, 105,120,135,150,165,180,195,210),
                      labels=c("Apr-1", "Apr-15","May-1","May-15","Jun-1",
                               "Jun-15","Jul-1","Jul-15","Aug-1")) +
   theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1, size=12),
                      legend.key=element_blank(),
                      #legend.key.height = unit(0.5,"mm"),
                      #legend.key.size = unit(1,"mm"),
                      legend.position=c(.9,.55),
                      axis.text.y = element_text(hjust = 1, size=12))+

                      #panel.grid.major = element_line(colour = 'gray80', linetype = 2)) +
   labs(y="Mean Daily Flow (cfs)", x="",
        title="NF American River") +
   annotate(geom = "text", 170, y = 7800, col="gray50",
            label="Mean Daily Flow \n USGS 11427000 (1942-2017)",
            cex=4))

# save
#ggsave(filename = "figs/Q_cfs_NFA_01_historical.png", width = 9, height = 6, units = "in")
#ggsave(filename = "figs/Q_cfs_NFA_02_historical-w2017.png", width = 9, height = 6, units = "in")
#ggsave(filename = "figs/Q_cfs_NFA_03_hist_2011.png", width = 9, height = 6, units = "in")
#ggsave(filename = "figs/Q_cfs_NFA_04_hist_2012.png", width = 9, height = 6, units = "in")
#ggsave(filename = "figs/Q_cfs_NFA_05_hist_2013.png", width = 9, height = 6, units = "in")
#ggsave(filename = "figs/Q_cfs_NFA_06_hist_2014.png", width = 9, height = 6, units = "in")
#ggsave(filename = "figs/Q_cfs_NFA_07_hist_2015.png", width = 9, height = 6, units = "in")
#ggsave(filename = "figs/Q_cfs_NFA_08_hist_2016.png", width = 9, height = 6, units = "in")
ggsave(filename = "figs/Q_cfs_NFA_09_spawn_all.png", width = 9, height = 6, units = "in")


# PLOTS: Q vs. DOWY: cms ----------------------------------------------------------

#cms=0.028316847*cfs

(gQ <- ggplot() + 
   # add all WYs
   geom_line(data=NFA_dv[!NFA_dv$WY==2017,], 
             aes(x=DOY, y=flow_cfs*0.028316847, group=WY), 
             color="gray60", alpha=0.7, show.legend = F) +
   ylim(c(0,250))+
   geom_line(data=NFA_dv[NFA_dv$WY==2017,],
             aes(x=DOY, y=flow_cfs*0.028316847, group=WY),
             color="black", alpha=0.7, lwd=1, show.legend = F) +
   geom_line(data=df,
             aes(x=DOY, y=Q_cfs*0.028316847, color=as.factor(WY), group=WY), show.legend = F, lwd=2) +

   geom_point(data=df[!is.na(df$missData),], aes(x=DOY, y=Q_cfs*0.028316847, fill=as.factor(WY)),
              show.legend = T, pch=21, color="gray20", size=6) +
   facet_grid(site~., scales="free_x") +
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
   labs(y="Mean Daily Flow (cms)", x="",
        title="NF American: Mean Daily Flow (cms) & Spawn Date") +
  annotate(geom = "text", x=180, y = 400, col="gray50", # for cfs use 180 and 8000
           label="Mean Daily Flow \n USGS 11427000 (1942-2017)", 
          cex=4))
  

ggsave(filename = "figs/Q_cms_NFA_w2017.png", width = 9, height = 6, units = "in")



# PLOTS: Delta Q -----------------------------------------------------------------

# deltQ

(gQ <- ggplot() + 
    
   # specific data
   geom_line(data=df, 
              aes(x=DOY, y=deltQ, color=as.factor(WY), group=WY), show.legend = T) + ylim(c(-2,1))+
    
    geom_point(data=df[!is.na(df$missData),], aes(x=DOY, y=deltQ, fill=as.factor(REG)), 
               show.legend = T, pch=21, color="gray20", size=5) +
    #facet_grid(site~., scales="free_x") + 
    scale_color_viridis(discrete = T, option = "A",guide = guide_legend(title = "Water Year")) +
    #scale_fill_viridis(discrete = T, option="A", guide = guide_legend(title = "Water Year & \n Spawn Date")) + guides(color=FALSE)+
    scale_x_continuous(breaks=c(90, 105,120,135,150,165,180,195,210),
                       labels=c("Apr-1", "Apr-15","May-1","May-15","Jun-1",
                                "Jun-15","Jul-1","Jul-15","Aug-1")) +
    theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1, size=12),
                       legend.key=element_blank(),
                       axis.text.y = element_text(hjust = 1, size=12)) + 
    labs(y="Avg Daily Flow (cfs)", x="",
         title="Average Daily Flow (cfs) & Spawn Date (NFA)"))


#ggsave(filename = "figs/Q_cfs_spawndate_NFA.png", width = 9, height = 6, units = "in")







# COMBINED COLWELL --------------------------------------------------------

load("models/colwell_variables.rda")
#colwell_dat <- final_bind %>% filter(!site=="MFY", !site=="SFY")
colwell_dat <- final_bind %>% filter(!site=="MFA" , !site=="MFY")

# add reg field
regs <- c("MFY", "MFA", "SFY", "RUB")
colwell_dat$REG <- ifelse(colwell_dat$site %in% regs, "R", "U")

reg1<- filter(colwell_dat, var=="deltQ")
# select variables:
t.test(MP_metric ~ REG, data = reg1)

# lev.avg is sig at 0.05 (P=0.041), mean R=0.6806723 mean U=0.9061462 

#colwell_dat$site <- factor(colwell_dat$site, levels = c("NFA", "NFY","RUB", "MFA"))
colwell_dat$site <- factor(colwell_dat$site, levels = c("NFA","RUB","NFY","SFY"))
levels(colwell_dat$site)
#colwell_dat$site <- factor(colwell_dat$site, levels = c("NFA","RUB","MFA", "NFY", "MFY", "SFY"))
levels(colwell_dat$site)

ggplot() + 
  geom_col(data=colwell_dat, aes(x=var, y=MP_metric, group=site), 
           show.legend = F, position="dodge") + coord_flip()+  
  scale_fill_viridis(discrete = T,option = "A") + 
  geom_hline(yintercept = 0.5, color="white", lty=2, lwd=0.9, alpha=0.9)+ ylab("Colwell's Seasonality (MP)") + xlab("") + labs(title="Seasonality Metrics in 6 Sierran Rivers") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=12),
        axis.text.y = element_text(hjust = 1, size=12))+
  facet_grid(.~site)

ggsave("./figs/colwell_metrics_6rivs_barplot.png", width = 7, height=4, units = "in")

# COMBINE WAVELETS --------------------------------------------------------
load("models/wavelet_plotpower.rda")


# all sites (except MFA)
ggplot() + 
  geom_line(data=plotNFA, aes(x=Period, y=Power.avg), col="#31688EFF", lwd=1.3)+
  geom_line(data=plotNFY, aes(x=Period, y=Power.avg), col="blue",lty=2, lwd=1.3) +
  #geom_line(data=plotMFA, aes(x=Period, y=Power.avg), col="red",lty=4, lwd=1) + 
  geom_line(data=plotMFY, aes(x=Period, y=Power.avg), col="#DD513AFF", lwd=1) + 
  geom_line(data=plotSFY, aes(x=Period, y=Power.avg), col="#932667FF", lwd=2, alpha=0.8) + 
  geom_line(data=plotRUB, aes(x=Period, y=Power.avg), col="#420A68FF", lty=4, lwd=1.5) +
  scale_x_continuous(breaks=c(seq(0,24,3)), limits = c(0,27)) +
  annotate("text", label="NFA", color="#31688EFF", x=15, y=8, fontface =2)+
  annotate("text", label="NFY", color="blue", x=15, y=10, fontface =2)+
  annotate("text", label="Rubicon", color="#420A68FF", x=3, y=5.8,fontface =2)+
  #annotate("text", label="MFA", color="red", x=11.8, y=4.8) + 
  annotate("text", label="MFY", color="#DD513AFF", x=1, y=6.5, fontface =2)+
  annotate("text", label="MFY", color="#DD513AFF", x=11.6, y=1.5, fontface =2)+
  annotate("text", label="SFY", color="#932667FF", x=11.8, y=4, fontface =2)+theme_bw() + xlab("Seasonality (Months)")


ggsave("./figs/wavelet_powerplot_5rivs_9v6.png", width = 9, height=6, units = "in")
ggsave("./figs/wavelet_powerplot_5rivs_10v5.png", width = 10, height=5.5, units = "in")


# all unreg sites
ggplot() + 
  geom_line(data=plotNFA, aes(x=Period, y=Power.avg), col="black")+
  geom_line(data=plotNFY, aes(x=Period, y=Power.avg), col="blue",lty=2)+
  scale_x_continuous(breaks=c(seq(0,300,24)))


# SEASONALITY VS PREDICTABILIITY -------------------------------------------

# seasonality (colwell)
load("models/colwell_daily_variables.rda")
colwell_dat <- final_bind %>% #filter(!site=="MFY") %>% 
  filter(var=="Q_hr_cfs") 

# predictability (wavelet)
load("models/wavelet_plotpower.rda")

# add a site col and combine:
plotMFY$site <- "MFY"; plotSFY$site <- "SFY"; plotNFY$site <- "NFY"; plotNFA$site <- "NFA"; plotMFA$site <- "MFA"; plotRUB$site <- "RUB"

waveletsALL<-rbind(plotMFY, plotSFY,plotNFY,plotNFA,plotMFA,plotRUB)

ggplot() + 
  geom_line(data=waveletsALL, aes(x=Period, y=Power.avg, color=site), lwd=1.3)+
  scale_x_continuous(breaks=c(seq(0,24,3)), limits = c(0,27)) +
  scale_y_continuous(breaks=c(seq(0,12,2))) +
  scale_color_viridis(discrete = T, option="A")+
  theme_bw() + xlab("Predictability Period (Months)")

# identify peaks:
maxWave <- waveletsALL %>% 
  group_by(site) %>% 
  summarize(maxPower=max(Power.avg),
            period = .$Period[which.max(Power.avg)],
            sumPowerP=sum(Power.avg.pval))

maxWave
colwell_dat
colWave_combine<-inner_join(maxWave, colwell_dat, by="site")

colWave_combine$REG <- ifelse(colWave_combine$site=="NFY"|colWave_combine$site=="NFA", "Unregulated", "Regulated")
#save(colWave_combine, file = "data/seasonality_v_predictable.rda")

load("data/seasonality_v_predictable.rda")

# seasonality vs. predictability
ggplot() + 
  geom_point(data=colWave_combine, aes(x=MP_metric, y=maxPower, fill=as.factor(site)), pch=21, size=7.5, show.legend = F)+
  ylim(c(5,11))+xlim(c(0.4,1))+
  geom_label_repel(data=colWave_combine, aes(x=MP_metric, y=maxPower, label=site), nudge_x = 0.07, size=7)+
  scale_fill_manual("Site", values=c("NFA"="#21908CFF", "NFY"="#21908CFF", "MFA"= "#440154FF", "RUB"="#F1605DFF","MFY"="#F1605DFF","SFY"="#F1605DFF"))+
  #scale_fill_viridis("", discrete = T, option="A")+
  theme_classic(base_size = 14) + xlab("Seasonality (M/P)") + ylab("Predictability (Avg. Power at 12 Months)")+
  theme(legend.position=c(0.2,.8),
        legend.text = element_text(size=14))
#axis.text.x = element_text(angle = 0, size=14, color = "black"),        
#axis.title.y = element_text(size=14, face="bold"),
#axis.title.x = element_text(size=14, face="bold"),
#axis.text.y = element_text(hjust = 1, size=14, color="black"))

ggsave("./figs/seasonality_predictability_dots_v2.png")#, width = 10, height=8, units = "in")


