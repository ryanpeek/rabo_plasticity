# Analyze Data

# Wed Apr 26 16:06:59 2017 ------------------------------

# ERROR SOUNDS ------------------------------------------------------------

options(error = function(){    # Beep on error
  beepr::beep()
  Sys.sleep(1)
}
)

# .Last <- function() {          # Beep on exiting session
#   beepr::beep(2) # beep(8) is awesome but too long
#   Sys.sleep(1)
# }

options(error=NULL) # reset beeps to nothing 

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
df <- master_df %>% filter(month(date)>3 & month(date)<8)

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



# COLWELL SEASONALITY INDEX -----------------------------------------------

library(hydrostats) # for seasonality 
## From Tonkin et al 2017: M (Contingency) as metric of seasonality
## To standardize seasonality in relation to overall predictability:
## divide (M) by overall predictability (the sum of (M) and constancy (C)

# standardize each site
nfa <- df %>% filter(site=="NFA")
mfa <- df %>% filter(site=="MFA")
rub <- df %>% filter(site=="RUB")
nfy <- df %>% filter(site=="NFY")
mfy <- df %>% filter(site=="MFY")
sfy <- df %>% filter(site=="SFY")

# calc colwell function:
get.colwell <- function(data, datecol, y, site){
  data <- data
  datecol <- datecol
  y_col <- y
  data <- data %>% filter(!is.na(y_col))
  site <- site
  df <- select(data, datecol, y_col) %>% set_names(c("Date", "Q"))
  colwells <- Colwells(df)$MP
  season <- tibble(site=c(site), MP_metric=c(colwells))
  return(season)
}

# lev_avg
nfa.c <- get.colwell(data = nfa, 2, 3, site = "NFA")
mfa.c <- get.colwell(data = mfa, 2, 3, site = "MFA")
rub.c <- get.colwell(data = rub, 2, 3, site = "RUB")
nfy.c <- get.colwell(data = nfy, 2, 3, site = "NFY")
sfy.c <- get.colwell(data = sfy, 2, 3, site = "SFY")

(S_var1 <- bind_rows(nfa.c, mfa.c, rub.c, nfy.c, sfy.c) %>% mutate("var"="lev_avg"))

# lev_CV
nfa.c <- get.colwell(data = nfa, 2, 4, site = "NFA")
mfa.c <- get.colwell(data = mfa, 2, 4, site = "MFA")
rub.c <- get.colwell(data = rub, 2, 4, site = "RUB")
nfy.c <- get.colwell(data = nfy, 2, 4, site = "NFY")
sfy.c <- get.colwell(data = sfy, 2, 4, site = "SFY")

(S_var2 <- bind_rows(nfa.c, mfa.c, rub.c, nfy.c, sfy.c) %>% mutate("var"="lev_CV") %>% bind_rows(S_var1))

# temp_7_avg
nfa.c <- get.colwell(data = nfa, 2, 16, site = "NFA")
mfa.c <- get.colwell(data = mfa, 2, 16, site = "MFA")
rub.c <- get.colwell(data = rub, 2, 16, site = "RUB")
nfy.c <- get.colwell(data = nfy, 2, 16, site = "NFY")
sfy.c <- get.colwell(data = sfy, 2, 16, site = "SFY")

(S_var3 <- bind_rows(nfa.c, mfa.c, rub.c, nfy.c, sfy.c) %>% mutate("var"="temp_7_avg") %>% bind_rows(S_var2))

# CDEC_ppt_mm
nfa.c <- get.colwell(data = nfa, 2, 25, site = "NFA")
mfa.c <- get.colwell(data = mfa, 2, 25, site = "MFA")
rub.c <- get.colwell(data = rub, 2, 25, site = "RUB")
nfy.c <- get.colwell(data = nfy, 2, 25, site = "NFY")
sfy.c <- get.colwell(data = sfy, 2, 25, site = "SFY")

(S_var4 <- bind_rows(nfa.c, mfa.c, rub.c, nfy.c, sfy.c) %>% mutate("var"="CDEC_ppt_mm") %>% bind_rows(S_var3))

# W_air_7_avg
nfa.c <- get.colwell(data = nfa, 2, 21, site = "NFA")
mfa.c <- get.colwell(data = mfa, 2, 21, site = "MFA")
rub.c <- get.colwell(data = rub, 2, 21, site = "RUB")
nfy.c <- get.colwell(data = nfy, 2, 21, site = "NFY")
sfy.c <- get.colwell(data = sfy, 2, 21, site = "SFY")

(S_var5 <- bind_rows(nfa.c, mfa.c, rub.c, nfy.c, sfy.c) %>% mutate("var"="W_air_7_avg") %>% bind_rows(S_var4))

# W_humidity_avg
nfa.c <- get.colwell(data = nfa, 2, 13, site = "NFA")
mfa.c <- get.colwell(data = mfa, 2, 13, site = "MFA")
rub.c <- get.colwell(data = rub, 2, 13, site = "RUB")
nfy.c <- get.colwell(data = nfy, 2, 13, site = "NFY")
sfy.c <- get.colwell(data = sfy, 2, 13, site = "SFY")

(S_var6 <- bind_rows(nfa.c, mfa.c, rub.c, nfy.c, sfy.c) %>% mutate("var"="W_humidity_avg") %>% bind_rows(S_var5))

# FLOW

# Q_cfs
nfa.c <- get.colwell(data = nfa, 2, 36, site = "NFA")
nfy.c <- get.colwell(data = nfy, 2, 36, site = "NFY")
mfy.c <- get.colwell(data = mfy, 2, 36, site = "MFY")
mfa.c <- get.colwell(data = mfa, 2, 36, site = "MFA")
rub.c <- get.colwell(data = rub, 2, 36, site = "RUB")
sfy.c <- get.colwell(data = sfy, 2, 36, site = "SFY")

(S_var7 <- bind_rows(nfa.c, mfa.c, rub.c, nfy.c, sfy.c) %>% mutate("var"="Q_cfs") %>% bind_rows(S_var6))


# Q_CV
nfa.c <- get.colwell(data = nfa, 2, 37, site = "NFA")
nfy.c <- get.colwell(data = nfy, 2, 37, site = "NFY")
mfy.c <- get.colwell(data = mfy, 2, 37, site = "MFY")
mfa.c <- get.colwell(data = mfa, 2, 37, site = "MFA")
rub.c <- get.colwell(data = rub, 2, 37, site = "RUB")
sfy.c <- get.colwell(data = sfy, 2, 37, site = "SFY")

(S_var8 <- bind_rows(nfa.c, mfa.c, rub.c, nfy.c, sfy.c) %>% mutate("var"="Q_CV") %>% bind_rows(S_var7))


# deltQ
nfa.c <- get.colwell(data = nfa, 2, 42, site = "NFA")
nfy.c <- get.colwell(data = nfy, 2, 42, site = "NFY")
mfy.c <- get.colwell(data = mfy, 2, 42, site = "MFY")
mfa.c <- get.colwell(data = mfa, 2, 42, site = "MFA")
rub.c <- get.colwell(data = rub, 2, 42, site = "RUB")
sfy.c <- get.colwell(data = sfy, 2, 42, site = "SFY")

(S_var9 <- bind_rows(nfa.c, mfa.c, rub.c, nfy.c, sfy.c) %>% mutate("var"="deltQ") %>% bind_rows(S_var8))

(final_bind <- S_var9%>% arrange(var))

final_bind$site <- factor(final_bind$site, ordered = T,levels = c("NFA","RUB","MFA", "NFY","MFY","SFY"))
levels(final_bind$site)

final_bind %>% filter(!site=="MFY") %>% filter(var=="lev_avg" | var=="Q_cfs" | var=="Q_CV" |var=="deltQ") %>% 
  ggplot(.) + 
  geom_bar(aes(x=site, y=MP_metric, fill=var), stat="identity", show.legend = F) + theme_bw() + #scale_fill_viridis(discrete = T)+
  geom_hline(yintercept = 0.75, color="#440154FF", lty=2, alpha=0.9, lwd=0.9) + #coord_flip() + facet_grid(.~var)
  facet_grid(var~.) 

save(final_bind, file = "models/colwell_variables.rda")

rm(list = ls(pattern = "S_var*"))
rm(list = ls(pattern = ".c$"))
rm(mfa, mfy, nfa, nfy, rub, sfy)

# WAVELET ANALYSIS --------------------------------------------------------

library(WaveletComp) # for wavelet analysis

# RUB
rub.w <- analyze.wavelet(RUB_dv, my.series = 3, dt = 1/30)

pdf(file = "./figs/wavelet_RUB_dailyflow_decade.pdf", width = 9, height = 6.6)
wt.image(rub.w, main = "RUB Seasonality of Daily Flow",
         legend.params = list(lab = "cross-wavelet power levels"),
         timelab = "Time (days)", periodlab = "period (months)")
dev.off()

wt.avg(rub.w)

plotRUB<-rub.w[c("Power.avg","Period","Power.avg.pval")] %>% as.data.frame

ggplot() + geom_line(data=plotRUB, aes(x=Period, y=Power.avg))+geom_point(data=plotRUB, aes(x=Period,y=Power.avg), col=ifelse(plotRUB$Power.avg.pval<0.05, "red", "blue")) + scale_x_continuous(breaks=seq(0,32, 2), limits = c(0,32))

# MFA
mfa.w <- analyze.wavelet(MFA_dv, my.series = 3, dt = 1/30)

pdf(file = "./figs/wavelet_MFA_dailyflow_decade.pdf", width = 9, height = 6.6)
wt.image(mfa.w, main = "MFA Seasonality of Daily Flow",
         legend.params = list(lab = "cross-wavelet power levels"),
         timelab = "Time (days)", periodlab = "period (months)")
dev.off()


wt.avg(mfa.w)

plotMFA<-mfa.w[c("Power.avg","Period","Power.avg.pval")] %>% as.data.frame

ggplot() + geom_line(data=plotMFA, aes(x=Period, y=Power.avg))+geom_point(data=plotMFA, aes(x=Period,y=Power.avg), col=ifelse(plotMFA$Power.avg.pval<0.05, "red", "blue")) + scale_x_continuous(breaks=seq(0,64, 2), limits = c(0,64))

# NFA
nfa.w <- analyze.wavelet(NFA_dv, my.series = 4, dt = 1/30) # usgs

pdf(file = "./figs/wavelet_NFA_dailyflow_decade.pdf", width = 9, height = 6.6)
wt.image(nfa.w, main = "NFA Seasonality of Daily Flow",
         legend.params = list(lab = "cross-wavelet power levels"),
         timelab = "Time (days)", periodlab = "period (months)")
dev.off()

wt.avg(nfa.w)

plotNFA<-nfa.w[c("Power.avg","Period","Power.avg.pval")] %>% as.data.frame

ggplot() + geom_line(data=plotNFA, aes(x=Period, y=Power.avg))+geom_point(data=plotNFA, aes(x=Period,y=Power.avg), col=ifelse(plotNFA$Power.avg.pval<0.05, "red", "blue")) + scale_x_continuous(breaks=seq(0,64, 2), limits = c(0,64))


# NFY
nfy.w <- analyze.wavelet(NFY_dv, my.series = 4, dt = 1/30) # usgs

pdf(file = "./figs/wavelet_NFY_dailyflow_decade.pdf", width = 9, height = 6.6)
wt.image(nfy.w, main = "NFY Seasonality of Daily Flow",
         legend.params = list(lab = "cross-wavelet power levels"),
         timelab = "Time (days)", periodlab = "period (months)")
dev.off()

wt.avg(nfy.w)

plotNFY<-nfy.w[c("Power.avg","Period","Power.avg.pval")] %>% as.data.frame

ggplot() + geom_line(data=plotNFY, aes(x=Period, y=Power.avg))+geom_point(data=plotNFY, aes(x=Period,y=Power.avg), col=ifelse(plotNFY$Power.avg.pval<0.05, "red", "blue")) + scale_x_continuous(breaks=seq(0,64, 2), limits = c(0,64))


# SFY
sfy.w <- analyze.wavelet(SFY_dv, my.series = 3, dt = 1/30) # usgs

pdf(file = "./figs/wavelet_SFY_dailyflow.pdf", width = 9, height = 6.6)
wt.image(sfy.w, main = "SFY Seasonality of Daily Flow",
         legend.params = list(lab = "cross-wavelet power levels"),
         timelab = "Time (days)", periodlab = "period (months)")
dev.off()

wt.avg(sfy.w)

plotSFY<-sfy.w[c("Power.avg","Period","Power.avg.pval")] %>% as.data.frame

ggplot() + geom_line(data=plotSFY, aes(x=Period, y=Power.avg))+geom_point(data=plotSFY, aes(x=Period,y=Power.avg), col=ifelse(plotSFY$Power.avg.pval<0.05, "red", "blue")) + scale_x_continuous(breaks=seq(0,64, 2), limits = c(0,64))



# MFY
mfy.w <- analyze.wavelet(MFY_dv, my.series = 3, dt = 1/30)

pdf(file = "./figs/wavelet_MFY_dailyflow.pdf", width = 9, height = 6.6)
wt.image(mfy.w, main = "MFY Seasonality of Daily Flow",
         legend.params = list(lab = "cross-wavelet power levels"),
         timelab = "Time (days)", periodlab = "period (months)")
dev.off()


wt.avg(mfy.w)

plotMFY<-mfy.w[c("Power.avg","Period","Power.avg.pval")] %>% as.data.frame

ggplot() + geom_line(data=plotMFY, aes(x=Period, y=Power.avg))+geom_point(data=plotMFY, aes(x=Period,y=Power.avg), col=ifelse(plotMFY$Power.avg.pval<0.05, "red", "blue")) + scale_x_continuous(breaks=seq(0,32, 2), limits = c(0,32))

# save multiple files:
save(plotNFA, plotRUB, plotMFA, plotNFY,  plotMFY,  file = "models/wavelet_decade_plotpower.rda")

# COMBINED COLWELL --------------------------------------------------------

load("models/colwell_variables.rda")
colwell_dat <- final_bind %>% filter(!site=="MFY", !site=="SFY")
colwell_dat <- final_bind %>% filter(!site=="SFY")
colwell_dat <- final_bind 

#colwell_dat$site <- factor(colwell_dat$site, levels = c("NFA", "NFY","RUB", "MFA"))
colwell_dat$site <- factor(colwell_dat$site, levels = c("NFA","RUB","MFA", "NFY", "MFY", "SFY"))
levels(colwell_dat$site)

ggplot() + 
  geom_col(data=colwell_dat, aes(x=var, y=MP_metric, fill=site, group=site), 
           show.legend = F, position="dodge") + coord_flip()+ 
  scale_fill_viridis(discrete = T,option = "D") + geom_hline(yintercept = 0.7, color="maroon", lty=2, lwd=0.9, alpha=0.9)+ ylab("Colwell's Seasonality (MP)") + xlab("") + labs(title="Seasonality Metrics in 6 Sierran Rivers") +
  facet_grid(.~site)
ggsave("./figs/colwell_metrics_6rivs_barplot.png", width = 7, height=4, units = "in")



# COMBINE WAVELETS --------------------------------------------------------

save(plotNFA, plotRUB, plotMFA, plotNFY,  plotMFY, plotSFY,  file = "models/wavelet_plotpower.rda")

load("models/wavelet_plotpower.rda")


# all sites (except RUB)
ggplot() + 
  geom_line(data=plotNFA, aes(x=Period, y=Power.avg), col="black")+
  geom_line(data=plotNFY, aes(x=Period, y=Power.avg), col="blue",lty=2) +
  geom_line(data=plotMFA, aes(x=Period, y=Power.avg), col="red",lty=4, lwd=1) + 
  geom_line(data=plotMFY, aes(x=Period, y=Power.avg), col="purple") + 
  #geom_line(data=plotSFY, aes(x=Period, y=Power.avg), col="forestgreen") + 
  geom_line(data=plotRUB, aes(x=Period, y=Power.avg), col="orange") +
  scale_x_continuous(breaks=c(seq(0,24,3)), limits = c(0,27)) +
  annotate("text", label="NFA", color="black", x=15, y=8)+
  annotate("text", label="NFY", color="blue", x=15, y=10)+
  annotate("text", label="Rubicon", color="orange", x=12, y=6.2)+
  annotate("text", label="MFA", color="red", x=11.8, y=4.8) + 
  annotate("text", label="MFY", color="purple", x=1, y=6.5)

ggsave("./figs/wavelet_powerplot_5rivs.png", width = 9, height=6, units = "in")



# all unreg sites
ggplot() + 
  geom_line(data=plotNFA, aes(x=Period, y=Power.avg), col="black")+
  geom_line(data=plotNFY, aes(x=Period, y=Power.avg), col="blue",lty=2)+
  scale_x_continuous(breaks=c(seq(0,300,24)))

# PCA ---------------------------------------------------------------------

# 2 NAs in dataset from 30 day avgs
df[!is.na(df$missData) & df$site=="RUB" & df$date==ymd("2011-05-26"),]$temp_30_max<-10.99
df[!is.na(df$missData) & df$site=="RUB" & df$date==ymd("2011-05-26"),]$temp_30_min<-9.7

# get only the breeding points drop redundant types
master_breed <- filter(df, !is.na(df$missData)) %>% 
  select(-WYsum, -DOY, -DOWY, -station, -missData, -obs_strt, -CDEC_air_C, -CDEC_air_7_avg, -CDEC_air_30_avg, -totalEM) %>% as.data.frame

# PCA
prin_comp <- master_breed %>% select(-site, -date, -WY, -REG) %>% prcomp(., center = T, scale. = T)
prin_comp$center

# rotation: The rotation measure provides the principal component loading. Each column of rotation matrix contains the principal component loading vector. This is the most important measure we should be interested in
prin_comp$rotation

# summary of components
summary(prin_comp) # so PC1 : PC3 explain 83% of the total variance in the data

# loadings per variable PC1
prin_comp$rotation[,1:3] %>% as.data.frame %>%
  mutate(vars = row.names(prin_comp$rotation)) %>% 
  select(vars, PC1) %>% arrange(PC1)

# loadings per variable PC2 & PC3
prin_comp$rotation[,2:3] %>% as.data.frame %>%
  mutate(vars = row.names(prin_comp$rotation)) %>% 
  select(vars, PC2) %>% arrange(PC2)

# loadings per variable PC3
prin_comp$rotation[,1:3] %>% as.data.frame %>%
  mutate(vars = row.names(prin_comp$rotation)) %>% 
  select(vars, PC3) %>% arrange(PC3)

# plot
prin_comp %>% plot

# predict to existing data
mapped_breed <- prin_comp %>% predict(master_breed)
mapped_breed %>% head

# GGPLOT
mapped_breed %>%
  as.data.frame %>%
  cbind(site = master_breed$site,
        REG = master_breed$REG) %>%
  ggplot() +
  geom_point(aes(x = PC4, y = PC5, fill = REG), pch=21, size=4)

# plot
biplot(prin_comp, choices = 1:2, scale = 0, pc.biplot = F, 
       xlabs=rep(".", nrow(master_breed)), col="blue")

# a quad plot of results

# THE FUNCTION:
pcaCharts <- function(x) {
  x.var <- x$sdev ^ 2
  x.pvar <- x.var/sum(x.var)
  print("proportions of variance:")
  print(x.pvar)
  
  par(mfrow=c(2,2))
  plot(x.pvar,xlab="Principal component", ylab="Proportion of variance explained", ylim=c(0,1), type='b')
  plot(cumsum(x.pvar),xlab="Principal component", ylab="Cumulative Proportion of variance explained", ylim=c(0,1), type='b')
  screeplot(x)
  screeplot(x,type="l")
  par(mfrow=c(1,1))
}

# plot
pcaCharts(prin_comp)

# plot first 5 prcomp
pc <- master_breed %>% select(-site, -date, -REG) %>% prcomp
comp <- data.frame(pc$x[,1:5])
row.names(comp) <- paste0(master_breed$site, "_", row.names(master_breed))

plot(comp, pch=16, col=rgb(0,0,0,0.5))


# K MEANS -----------------------------------------------------------------
dat1 <- master_breed %>% select(-site, -date, -REG, -WY)
row.names(dat1) <- paste0(master_breed$site, "_", row.names(master_breed))

wss <- (nrow(dat1)-1)*sum(apply(dat1,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(dat1,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

k <- kmeans(comp, 4, nstart=25, iter.max=1000)
library(RColorBrewer)
library(scales)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot(comp, col=k$clust, pch=16)

# Cluster sizes
sort(table(k$clust))
clust <- names(sort(table(k$clust)))
clust
# First cluster
row.names(dat1[k$clust==clust[1],])
# Second Cluster
row.names(dat1[k$clust==clust[2],])
# Third Cluster
row.names(dat1[k$clust==clust[3],])
# Fourth Cluster
row.names(dat1[k$clust==clust[4],])


# PLOTTING PCA ------------------------------------------------------------

#devtools::install_github("vqv/ggbiplot")

library(ggbiplot)

g <- ggbiplot(prin_comp, obs.scale = 1, var.scale = 1,
              ellipse = TRUE, 
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)

ggsave("figs/biplot_pca_v2.png", width = 7, height = 6, units="in")


# now switch off scaling factors (var.scale)
g <- ggbiplot(prin_comp, choices = 2:3, scale = 0, var.scale = 0, labels= prin_comp$site, groups = prin_comp$site,
              ellipse = TRUE, 
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)

ggsave("figs/biplot_pca_noscale_v2.png", width = 7, height = 6, units="in")

