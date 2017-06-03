# Analyze Data: Wavelet

# LOAD LIBRARIES ----------------------------------------------------------

library(tidyverse)
library(lubridate)
library(ggrepel)
library(viridis)
library(caTools)
source("scripts/functions/f_doy.R")

# LOAD DATA ---------------------------------------------------------------

load("data/master_dat_2011-2016.rda") 
#load("data/flow_dv_cfs_2011_6sites.rda")
load("data/flow_dv_cfs_full_6sites.rda")

# DATA PREP ---------------------------------------------------------------

dowy_labs<-c("Oct","Nov","Dec","Jan", "Feb","Mar", "Mar-15", "Apr-01", "Apr-15", "May-01", "May-15", "Jun-01", "Jun-15", "Jul-01", "Jul-15", "Aug", "Aug-15", "Sep")
dowy_breaks<-c(1, 32, 62, 93, 124, 152, 167, 183, 198, 213, 228, 244, 259, 274, 289, 305, 320, 336)
dowys<-data.frame("mon"=dowy_labs, "dowy"=dowy_breaks)

RUB_dv <- filter(flowdf, site=="RUB")
NFA_dv <- filter(flowdf, site=="NFA")
MFA_dv <- filter(flowdf, site=="MFA")
NFY_dv <- filter(flowdf, site=="NFY")
MFY_dv <- filter(flowdf, site=="MFY")
SFY_dv <- filter(flowdf, site=="SFY")

# WAVELET ANALYSIS --------------------------------------------------------

library(WaveletComp) # for wavelet analysis

# RUB
rub.w <- analyze.wavelet(RUB_dv, my.series = 4, dt = 1/30)

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

# COMBINE WAVELETS --------------------------------------------------------

#save(plotNFA, plotRUB, plotMFA, plotNFY,  plotMFY, plotSFY,  file = "models/wavelet_plotpower.rda")

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


# PLOTS -------------------------------------------------------------------

# V1
# all sites 
ggplot() + 
  geom_line(data=plotNFA, aes(x=Period, y=Power.avg), col="black")+
  geom_line(data=plotNFY, aes(x=Period, y=Power.avg), col="blue",lty=2) +
  geom_line(data=plotMFA, aes(x=Period, y=Power.avg), col="red",lty=4, lwd=1) + 
  geom_line(data=plotMFY, aes(x=Period, y=Power.avg), col="purple") + 
  geom_line(data=plotSFY, aes(x=Period, y=Power.avg), col="forestgreen") + 
  geom_line(data=plotRUB, aes(x=Period, y=Power.avg), col="orange") +
  scale_x_continuous(breaks=c(seq(0,24,3)), limits = c(0,27)) +
  annotate("text", label="NFA", color="black", x=15, y=8)+
  annotate("text", label="NFY", color="blue", x=15, y=10)+
  annotate("text", label="Rubicon", color="orange", x=12, y=6.2)+
  annotate("text", label="MFA", color="red", x=11.8, y=4.8) + 
  annotate("text", label="MFY", color="purple", x=1, y=6.5)

#ggsave("./figs/wavelet_powerplot_5rivs.png", width = 9, height=6, units = "in")

#V2
# all sites (except MFA)
ggplot() + 
  geom_line(data=plotNFA, aes(x=Period, y=Power.avg), col="#31688EFF", lwd=1.3)+
  geom_line(data=plotNFY, aes(x=Period, y=Power.avg), col="blue",lty=2, lwd=1.3) +
  geom_line(data=plotMFA, aes(x=Period, y=Power.avg), col="red",lty=4, lwd=1) + 
  geom_line(data=plotMFY, aes(x=Period, y=Power.avg), col="#DD513AFF", lwd=1) + 
  geom_line(data=plotSFY, aes(x=Period, y=Power.avg), col="#932667FF", lwd=2, alpha=0.8) + 
  geom_line(data=plotRUB, aes(x=Period, y=Power.avg), col="#420A68FF", lty=4, lwd=1.5) +
  scale_x_continuous(breaks=c(seq(0,24,3)), limits = c(0,27)) +
  annotate("text", label="NFA", color="#31688EFF", x=15, y=8, fontface =2)+
  annotate("text", label="NFY", color="blue", x=15, y=10, fontface =2)+
  annotate("text", label="Rubicon", color="#420A68FF", x=3, y=5.8,fontface =2)+
  annotate("text", label="MFA", color="red", x=11.8, y=4.8) + 
  annotate("text", label="MFY", color="#DD513AFF", x=1, y=6.5, fontface =2)+
  annotate("text", label="MFY", color="#DD513AFF", x=11.6, y=1.5, fontface =2)+
  annotate("text", label="SFY", color="#932667FF", x=11.8, y=4, fontface =2)+theme_bw() + xlab("Seasonality (Months)")


#ggsave("./figs/wavelet_powerplot_5rivs_9v6.png", width = 9, height=6, units = "in")
#ggsave("./figs/wavelet_powerplot_5rivs_10v5.png", width = 10, height=5.5, units = "in")

# all unreg sites
ggplot() + 
  geom_line(data=plotNFA, aes(x=Period, y=Power.avg), col="black")+
  geom_line(data=plotNFY, aes(x=Period, y=Power.avg), col="blue",lty=2)+
  scale_x_continuous(breaks=c(seq(0,300,24)))

#V3
# all sites (except MFA)
ggplot() + 
  geom_line(data=waveletsALL, aes(x=Period, y=Power.avg, color=site), lwd=1.3)+
  scale_x_continuous(breaks=c(seq(0,24,3)), limits = c(0,27)) +
  scale_color_viridis(discrete = T, option="A")+
  theme_bw() + xlab("Seasonality (Months)")
#facet_grid(site~.)
