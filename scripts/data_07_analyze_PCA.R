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

# LOAD DATA ---------------------------------------------------------------

load("data/master_dat_2011-2016.rda") 

load("data/NFA_dv_USGS_1941_2017-04-28.rda")
load("data/NFY_dv_USGS_1930_2017-04-28.rda")
load("data/MFY_dv_CDEC_ORH_2000_2017.rda")
MFY_dv <- mfy_dv
load("data/MFA_dv_CDEC_OXB_1997-2017.rda") 
load("data/RUB_dv_PCWA_2009-2016.rda")
load("data/SFY_dv_CDEC_JBR_1998-2017.rda")


# DATA PREP ---------------------------------------------------------------

df <- master_df %>% filter(site!="MFY")
#df <- master_df %>% filter(site!="MFY", site!="MFA")


# NEW PLOTS: WTEMP vs DOWY ------------------------------------------------

# WTemp: 7-Day Avg w threshold
df <- df %>% filter(DOY>105 & DOY<210)

ggplot() + 
  geom_line(data=df, 
            aes(x=DOY, y=temp_7_avg, color=as.factor(WY), group=WY), show.legend = F) + 
  #geom_ribbon(data=df, aes(x=DOY, ymin=10,ymax=12), fill="orange", alpha=0.4) + 
  geom_point(data=df[!is.na(df$totalEM),], aes(x=DOY, y=temp_7_avg, fill=as.factor(WY)), 
             show.legend = T, pch=21, color="gray20", size=4) + 
  facet_grid(site~., scales="free_x") + scale_color_viridis(discrete = T, guide = guide_legend(title = "Water Year")) +
  scale_fill_viridis(discrete = T, guide = guide_legend(title = "Water Year")) +
  scale_x_continuous(breaks=c(105,120,135,150,165,180,195,210),labels=c("Apr-15","May-1","May-15","Jun-1","Jun-15","Jul-1","Jul-15","Aug-1")) +
  scale_y_continuous(limits=c(0,27), breaks=seq(0,27,3)) + 
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1),
                     legend.key=element_blank()) + 
                     #panel.grid.major = element_line(colour = 'gray80', linetype = 2)) +
  labs(y=expression(paste("Water Temperature (",degree,"C)")), 
       title="7 Day Average Water Temperature")

#ggsave(filename = "figs/watertemp7_breeding.png", width = 9, height = 6, units = "in")


# OLD PLOTS ---------------------------------------------------------------

# WTemp: 7-Day Avg w threshold
ggplot() + 
  geom_line(data=df, 
            aes(x=date, y=temp_7_avg, color=site, group=WY), show.legend = F) +
  geom_ribbon(data=df, aes(x=date, ymin=10,ymax=12), fill="orange", alpha=0.4) + xlab("")+ ylab("7-day Avg Water Temp (C)")+
  geom_point(data=df[!is.na(df$totalEM),], aes(x=date, y=temp_7_avg, fill=site), show.legend = F, pch=21,color="gray20",size=4) + 
  facet_grid(site~WY, scales="free_x")

ggsave(filename = "figs/watertemp7_breeding.png", width = 9, height = 6, units = "in")


# Stage: 7-day avg
ggplot() + 
  geom_line(data=df, 
            aes(x=date, y=lev_7_avg, color=site, group=WY), show.legend = F) +
  geom_point(data=df[!is.na(df$totalEM),], aes(x=date, y=lev_7_avg, fill=site), show.legend = F,
             pch=21,color="gray20",size=4) + 
  xlab("")+ ylab("7-day Avg Stage (m)")+
  facet_grid(site~WY, scales="free")

ggsave(filename = "figs/stage7_breeding.png", width = 9, height = 6, units = "in")


# Stage vs. Temp: 7-day avg
ggplot() + 
  geom_point(data=df[!is.na(df$totalEM),],
            aes(x=temp_7_avg, y=W_air_7_min, shape=site),
            size=4.1)  +
  geom_point(data=df[!is.na(df$totalEM),], 
             aes(x=temp_7_avg, y=W_air_7_min, shape=site, color=as.factor(WY)),
             size=4)  + 
  scale_color_viridis(discrete = T, option = "D")

ggsave(filename = "figs/air7_temp7_breeding.png", width = 9, height = 6, units = "in")
  

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

# temp_avg
nfa.c <- get.colwell(data = nfa, 2, 6, site = "NFA")
mfa.c <- get.colwell(data = mfa, 2, 6, site = "MFA")
rub.c <- get.colwell(data = rub, 2, 6, site = "RUB")
nfy.c <- get.colwell(data = nfy, 2, 6, site = "NFY")
sfy.c <- get.colwell(data = sfy, 2, 6, site = "SFY")

(S_var2 <- bind_rows(nfa.c, mfa.c, rub.c, nfy.c, sfy.c) %>% mutate("var"="temp_avg") %>% bind_rows(S_var1))

# CDEC_ppt_mm
nfa.c <- get.colwell(data = nfa, 2, 32, site = "NFA")
mfa.c <- get.colwell(data = mfa, 2, 32, site = "MFA")
rub.c <- get.colwell(data = rub, 2, 32, site = "RUB")
nfy.c <- get.colwell(data = nfy, 2, 32, site = "NFY")
sfy.c <- get.colwell(data = sfy, 2, 32, site = "SFY")

(S_var3 <- bind_rows(nfa.c, mfa.c, rub.c, nfy.c, sfy.c) %>% mutate("var"="CDEC_ppt_mm") %>% bind_rows(S_var2))

# CDEC_air_7
nfa.c <- get.colwell(data = nfa, 2, 33, site = "NFA")
mfa.c <- get.colwell(data = mfa, 2, 33, site = "MFA")
rub.c <- get.colwell(data = rub, 2, 33, site = "RUB")
nfy.c <- get.colwell(data = nfy, 2, 33, site = "NFY")
sfy.c <- get.colwell(data = sfy, 2, 33, site = "SFY")

(S_var4 <- bind_rows(nfa.c, mfa.c, rub.c, nfy.c, sfy.c) %>% mutate("var"="CDEC_air_7") %>% bind_rows(S_var3))

# CDEC_air_30
nfa.c <- get.colwell(data = nfa, 2, 34, site = "NFA")
mfa.c <- get.colwell(data = mfa, 2, 34, site = "MFA")
rub.c <- get.colwell(data = rub, 2, 34, site = "RUB")
nfy.c <- get.colwell(data = nfy, 2, 34, site = "NFY")
sfy.c <- get.colwell(data = sfy, 2, 34, site = "SFY")

(S_var5 <- bind_rows(nfa.c, mfa.c, rub.c, nfy.c, sfy.c) %>% mutate("var"="CDEC_air_30") %>% bind_rows(S_var4))

# W_humidity_avg
nfa.c <- get.colwell(data = nfa, 2, 13, site = "NFA")
mfa.c <- get.colwell(data = mfa, 2, 13, site = "MFA")
rub.c <- get.colwell(data = rub, 2, 13, site = "RUB")
nfy.c <- get.colwell(data = nfy, 2, 13, site = "NFY")
sfy.c <- get.colwell(data = sfy, 2, 13, site = "SFY")

(S_var6 <- bind_rows(nfa.c, mfa.c, rub.c, nfy.c, sfy.c) %>% mutate("var"="W_humidity_avg") %>% bind_rows(S_var5))

# FLOW

# flow_cfs
nfa.c <- get.colwell(data = NFA_dv, 3, 4, site = "NFA")
nfy.c <- get.colwell(data = NFY_dv, 3, 4, site = "NFY")
mfy.c <- get.colwell(data = MFY_dv, 2, 3, site = "MFY")
mfa.c <- get.colwell(data = MFA_dv, 2, 3, site = "MFA")
rub.c <- get.colwell(data = RUB_dv, 2, 3, site = "RUB")
sfy.c <- get.colwell(data = SFY_dv, 2, 3, site = "SFY")

#flow_bind <- bind_rows(nfa.c, mfa.c, rub.c, nfy.c, mfy.c, sfy.c) %>% mutate("var" = "flow_cfs") %>% arrange(MP_metric)

final_bind <- bind_rows(nfa.c, mfa.c, rub.c, nfy.c, mfy.c, sfy.c) %>% mutate("var" = "flow_cfs") %>% 
  bind_rows(S_var6)

final_bind

ggplot() + 
  geom_bar(data=final_bind, aes(x=site, y=MP_metric, fill=var), stat="identity", show.legend = F) + 
  geom_hline(yintercept = 0.75, color="green", lty=2, alpha=0.9, lwd=0.9)+
  facet_grid(var~.)

save(final_bind, file = "models/colwell_variables.rda")

# WAVELET ANALYSIS --------------------------------------------------------

library(WaveletComp) # for wavelet analysis

# RUB
rub.w <- analyze.wavelet(RUB_dv, my.series = 3, dt = 1/30)

pdf(file = "./figs/wavelet_RUB_dailyflow.pdf", width = 9, height = 6.6)
wt.image(rub.w, main = "RUB Seasonality of Daily Flow",
         legend.params = list(lab = "cross-wavelet power levels"),
         timelab = "Time (days)", periodlab = "period (months)")
dev.off()

wt.avg(rub.w)

plotRUB<-rub.w[c("Power.avg","Period","Power.avg.pval")] %>% as.data.frame

ggplot() + geom_line(data=plotRUB, aes(x=Period, y=Power.avg))+geom_point(data=plotRUB, aes(x=Period,y=Power.avg), col=ifelse(plotRUB$Power.avg.pval<0.05, "red", "blue")) + scale_x_continuous(breaks=seq(0,32, 2), limits = c(0,32))

# MFA
mfa.w <- analyze.wavelet(MFA_dv, my.series = 3, dt = 1/30)

pdf(file = "./figs/wavelet_MFA_dailyflow.pdf", width = 9, height = 6.6)
wt.image(mfa.w, main = "MFA Seasonality of Daily Flow",
         legend.params = list(lab = "cross-wavelet power levels"),
         timelab = "Time (days)", periodlab = "period (months)")
dev.off()


wt.avg(mfa.w)

plotMFA<-mfa.w[c("Power.avg","Period","Power.avg.pval")] %>% as.data.frame

ggplot() + geom_line(data=plotMFA, aes(x=Period, y=Power.avg))+geom_point(data=plotMFA, aes(x=Period,y=Power.avg), col=ifelse(plotMFA$Power.avg.pval<0.05, "red", "blue")) + scale_x_continuous(breaks=seq(0,64, 2), limits = c(0,64))

# NFA
nfa.w <- analyze.wavelet(NFA_dv, my.series = 4, dt = 1/30) # usgs

pdf(file = "./figs/wavelet_NFA_dailyflow.pdf", width = 9, height = 6.6)
wt.image(nfa.w, main = "NFA Seasonality of Daily Flow",
         legend.params = list(lab = "cross-wavelet power levels"),
         timelab = "Time (days)", periodlab = "period (months)")
dev.off()

wt.avg(nfa.w)

plotNFA<-nfa.w[c("Power.avg","Period","Power.avg.pval")] %>% as.data.frame

ggplot() + geom_line(data=plotNFA, aes(x=Period, y=Power.avg))+geom_point(data=plotNFA, aes(x=Period,y=Power.avg), col=ifelse(plotNFA$Power.avg.pval<0.05, "red", "blue")) + scale_x_continuous(breaks=seq(0,64, 2), limits = c(0,64))


# NFY
nfy.w <- analyze.wavelet(NFY_dv, my.series = 4, dt = 1/30) # usgs

pdf(file = "./figs/wavelet_NFY_dailyflow.pdf", width = 9, height = 6.6)
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

# get only the breeding points
master_breed <- filter(df, !is.na(df$totalEM), !is.na(df$CDEC_air_30_avg)) %>% 
  select(-site, -WY, -WYsum, -DOY, -DOWY, -date, -station, -missData, -REG, -obs_strt, -CDEC_air_C, -CDEC_air_7_avg, -totalEM) %>% as.data.frame

# remove as much datetime information (DOWY, days without ppt, etc)
master_breed <- master_breed %>% select(-W_humidity_avg)


# REMOVE ALL BUT A FEW TO SEE WHAT HAPPENS:
#master_breed <- master_breed %>% select(lev_avg, temp_avg, W_air_min, temp_7_avg, W_air_7_avg, CDEC_air_30_avg, CDEC_ppt_mm, Index)

# PCA
prin_comp <- prcomp(master_breed)
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
biplot(prin_comp, choices = 1:2, scale = 0, pc.biplot = F, 
       xlabs=rep(".", nrow(master_breed)), col="blue")

biplot(prin_comp, choices = 2:3, scale = 0, pc.biplot = F, xlabs=rep(".", nrow(master_breed)), 
       col="black")

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
g <- ggbiplot(prin_comp, choices = 1:2, scale = 0, var.scale = 0, labels= prin_comp$site, groups = prin_comp$site,
              ellipse = TRUE, 
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)
ggsave("figs/biplot_pca_noscale_v2.png", width = 7, height = 6, units="in")

