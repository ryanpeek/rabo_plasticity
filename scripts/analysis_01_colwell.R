# Analyze Data: Colwell Analysis (Seasonality)

# LOAD LIBRARIES ----------------------------------------------------------

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
load("data/flow_hv_cfs_6sites.rda")

# DATA PREP ---------------------------------------------------------------

df <- master_df
# df <- master_df %>% filter(site!="MFY")
#df <- master_df %>% filter(month(date)>3 & month(date)<8)

dowy_labs<-c("Oct","Nov","Dec","Jan", "Feb","Mar", "Mar-15", "Apr-01", "Apr-15", "May-01", "May-15", "Jun-01", "Jun-15", "Jul-01", "Jul-15", "Aug", "Aug-15", "Sep")
dowy_breaks<-c(1, 32, 62, 93, 124, 152, 167, 183, 198, 213, 228, 244, 259, 274, 289, 305, 320, 336)
dowys<-data.frame("mon"=dowy_labs, "dowy"=dowy_breaks)


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

# deltlev
nfa.c <- get.colwell(data = nfa, 2, 15, site = "NFA")
mfa.c <- get.colwell(data = mfa, 2, 15, site = "MFA")
rub.c <- get.colwell(data = rub, 2, 15, site = "RUB")
nfy.c <- get.colwell(data = nfy, 2, 15, site = "NFY")
sfy.c <- get.colwell(data = sfy, 2, 15, site = "SFY")

(S_var1 <- bind_rows(nfa.c, mfa.c, rub.c, nfy.c, sfy.c) %>% mutate("var"="deltLev"))

# lev_CV
nfa.c <- get.colwell(data = nfa, 2, 4, site = "NFA")
mfa.c <- get.colwell(data = mfa, 2, 4, site = "MFA")
rub.c <- get.colwell(data = rub, 2, 4, site = "RUB")
nfy.c <- get.colwell(data = nfy, 2, 4, site = "NFY")
sfy.c <- get.colwell(data = sfy, 2, 4, site = "SFY")

(S_var2 <- bind_rows(nfa.c, mfa.c, rub.c, nfy.c, sfy.c) %>% mutate("var"="lev_CV") %>% bind_rows(S_var1))

# temp_7_mx
nfa.c <- get.colwell(data = nfa, 2, 17, site = "NFA")
mfa.c <- get.colwell(data = mfa, 2, 17, site = "MFA")
rub.c <- get.colwell(data = rub, 2, 17, site = "RUB")
nfy.c <- get.colwell(data = nfy, 2, 17, site = "NFY")
sfy.c <- get.colwell(data = sfy, 2, 17, site = "SFY")

(S_var3a <- bind_rows(nfa.c, mfa.c, rub.c, nfy.c, sfy.c) %>% mutate("var"="temp_7_max") %>% bind_rows(S_var2))

# temp_CV
nfa.c <- get.colwell(data = nfa, 2, 8, site = "NFA")
mfa.c <- get.colwell(data = mfa, 2, 8, site = "MFA")
rub.c <- get.colwell(data = rub, 2, 8, site = "RUB")
nfy.c <- get.colwell(data = nfy, 2, 8, site = "NFY")
sfy.c <- get.colwell(data = sfy, 2, 8, site = "SFY")

(S_var3b <- bind_rows(nfa.c, mfa.c, rub.c, nfy.c, sfy.c) %>% mutate("var"="temp_CV") %>% bind_rows(S_var3a))

# CDEC_ppt_mm
nfa.c <- get.colwell(data = nfa, 2, 25, site = "NFA")
mfa.c <- get.colwell(data = mfa, 2, 25, site = "MFA")
rub.c <- get.colwell(data = rub, 2, 25, site = "RUB")
nfy.c <- get.colwell(data = nfy, 2, 25, site = "NFY")
sfy.c <- get.colwell(data = sfy, 2, 25, site = "SFY")

(S_var4 <- bind_rows(nfa.c, mfa.c, rub.c, nfy.c, sfy.c) %>% mutate("var"="CDEC_ppt_mm") %>% bind_rows(S_var3b))

# temp_30_min
nfa.c <- get.colwell(data = nfa, 2, 20, site = "NFA")
mfa.c <- get.colwell(data = mfa, 2, 20, site = "MFA")
rub.c <- get.colwell(data = rub, 2, 20, site = "RUB")
nfy.c <- get.colwell(data = nfy, 2, 20, site = "NFY")
sfy.c <- get.colwell(data = sfy, 2, 20, site = "SFY")

(S_var5 <- bind_rows(nfa.c, mfa.c, rub.c, nfy.c, sfy.c) %>% mutate("var"="temp_30_min") %>% bind_rows(S_var4))

# W_humidity_avg
nfa.c <- get.colwell(data = nfa, 2, 13, site = "NFA")
mfa.c <- get.colwell(data = mfa, 2, 13, site = "MFA")
rub.c <- get.colwell(data = rub, 2, 13, site = "RUB")
nfy.c <- get.colwell(data = nfy, 2, 13, site = "NFY")
sfy.c <- get.colwell(data = sfy, 2, 13, site = "SFY")

(S_var6 <- bind_rows(nfa.c, mfa.c, rub.c, nfy.c, sfy.c) %>% mutate("var"="W_humidity_avg") %>% bind_rows(S_var5))

# FLOW

# hourly flow
nfa.c <- get.colwell(data = flow_hv[flow_hv$site=="NFA",], 2, 3, site = "NFA")
nfy.c <- get.colwell(data = flow_hv[flow_hv$site=="NFY",], 2, 3, site = "NFY")
mfy.c <- get.colwell(data = flow_hv[flow_hv$site=="MFY",], 2, 3, site = "MFY")
mfa.c <- get.colwell(data = flow_hv[flow_hv$site=="MFA",], 2, 3, site = "MFA")
rub.c <- get.colwell(data = flow_hv[flow_hv$site=="RUB",], 2, 3, site = "RUB")
sfy.c <- get.colwell(data = flow_hv[flow_hv$site=="SFY",], 2, 3, site = "SFY")
hr_flow_colwell <- bind_rows(nfa.c, mfa.c, rub.c, nfy.c, mfy.c, sfy.c) %>% mutate("var"="Q_hr_cfs")
(S_var7a <- bind_rows(nfa.c, mfa.c, rub.c, nfy.c, mfy.c, sfy.c) %>% mutate("var"="Q_hr_cfs") %>% bind_rows(S_var6))

# Q_cfs
nfa.c <- get.colwell(data = nfa, 2, 36, site = "NFA")
nfy.c <- get.colwell(data = nfy, 2, 36, site = "NFY")
mfy.c <- get.colwell(data = mfy, 2, 36, site = "MFY")
mfa.c <- get.colwell(data = mfa, 2, 36, site = "MFA")
rub.c <- get.colwell(data = rub, 2, 36, site = "RUB")
sfy.c <- get.colwell(data = sfy, 2, 36, site = "SFY")

(S_var7 <- bind_rows(nfa.c, mfa.c, rub.c, nfy.c, mfy.c, sfy.c) %>% mutate("var"="Q_dy_cfs") %>% bind_rows(S_var7a))


# Q_CV
nfa.c <- get.colwell(data = nfa, 2, 37, site = "NFA")
nfy.c <- get.colwell(data = nfy, 2, 37, site = "NFY")
mfy.c <- get.colwell(data = mfy, 2, 37, site = "MFY")
mfa.c <- get.colwell(data = mfa, 2, 37, site = "MFA")
rub.c <- get.colwell(data = rub, 2, 37, site = "RUB")
sfy.c <- get.colwell(data = sfy, 2, 37, site = "SFY")

(S_var8 <- bind_rows(nfa.c, mfa.c, rub.c, nfy.c, mfy.c, sfy.c) %>% mutate("var"="Q_CV") %>% bind_rows(S_var7))


# deltQ
nfa.c <- get.colwell(data = nfa, 2, 42, site = "NFA")
nfy.c <- get.colwell(data = nfy, 2, 42, site = "NFY")
mfy.c <- get.colwell(data = mfy, 2, 42, site = "MFY")
mfa.c <- get.colwell(data = mfa, 2, 42, site = "MFA")
rub.c <- get.colwell(data = rub, 2, 42, site = "RUB")
sfy.c <- get.colwell(data = sfy, 2, 42, site = "SFY")

(S_var9 <- bind_rows(nfa.c, mfa.c, rub.c, nfy.c,  mfy.c, sfy.c) %>% mutate("var"="deltQ") %>% bind_rows(S_var8))

(final_bind <- S_var9%>% arrange(var))

final_bind$site <- factor(final_bind$site, ordered = T,levels = c("NFA","RUB","MFA", "NFY","SFY","MFY"))
levels(final_bind$site)

final_bind %>% #filter(!site=="MFY") %>% 
  filter(var=="Q_hr_cfs") %>% 
  #filter(var=="lev_avg" | var=="Q_cfs" | var=="Q_CV" |var=="deltQ") %>% 
  ggplot(.) + 
  geom_bar(aes(x=site, y=MP_metric, fill=var), stat="identity", show.legend = F, position="dodge", color="black") + theme_bw() + 
  #scale_fill_viridis(discrete = T, option="C")+
  scale_fill_grey("Variable") + labs(title="Colwell's M/P of Hourly Flow (cfs)")+
  ylab("Seasonality (Colwell's M/P)") + xlab("") + ylim(c(0,1)) + 
  theme(axis.text.x = element_text(angle = 0, size=12, face = "italic", color = "black"),
        #legend.position=c(0.1,.8),
        axis.title.y = element_text(size=12, face="bold"),
        axis.text.y = element_text(hjust = 1, size=12, color="black", face="italic"))
ggsave("./figs/colwell_Q_hr_cfs_6rivs_barplot.png", width = 7, height=4, units = "in")

save(final_bind, file = "models/colwell_daily_variables.rda")

rm(list = ls(pattern = "S_var*"))
rm(list = ls(pattern = ".c$"))
rm(mfa, mfy, nfa, nfy, rub, sfy)


# COMBINED COLWELL --------------------------------------------------------

load("models/colwell_daily_variables.rda")
colwell_dat <- final_bind %>% filter(!site=="MFY") %>% 
  filter(var=="Q_hr_cfs") 

#colwell_dat$site <- factor(colwell_dat$site, levels = c("NFA", "NFY","RUB", "MFA"))
colwell_dat$site <- factor(colwell_dat$site, levels = c("NFA","RUB","MFA", "NFY", "MFY", "SFY"))
levels(colwell_dat$site)

ggplot() + 
  geom_col(data=colwell_dat, aes(x=var, y=MP_metric, fill=site, group=site), 
           show.legend = F, position="dodge") + coord_flip()+ 
  scale_fill_viridis(discrete = T,option = "D") + geom_hline(yintercept = 0.7, color="maroon", lty=2, lwd=0.9, alpha=0.9)+ ylab("Colwell's Seasonality (MP)") + xlab("") + labs(title="Seasonality Metrics in 6 Sierran Rivers") +
  facet_grid(.~site)

#ggsave("./figs/colwell_metrics_6rivs_barplot.png", width = 7, height=4, units = "in")


# BIPLOT ------------------------------------------------------------------

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
save()

# seasonality vs. predictability
ggplot() + 
  geom_point(data=colWave_combine, aes(x=MP_metric, y=maxPower, fill=REG), pch=21, size=7.5)+
  ylim(c(5,11))+xlim(c(0.4,1))+
  geom_label_repel(data=colWave_combine, aes(x=MP_metric, y=maxPower, label=site), nudge_x = 0.07, size=7)+
  scale_fill_viridis("", discrete = T, option="A")+
  theme_classic(base_size = 14) + xlab("Seasonality (M/P)") + ylab("Predictability (Avg. Power at 12 Months)")+
  theme(legend.position=c(0.2,.8),
        legend.text = element_text(size=14))
        #axis.text.x = element_text(angle = 0, size=14, color = "black"),        
        #axis.title.y = element_text(size=14, face="bold"),
        #axis.title.x = element_text(size=14, face="bold"),
        #axis.text.y = element_text(hjust = 1, size=14, color="black"))

ggsave("./figs/seasonality_predictability_dots.png")#, width = 10, height=8, units = "in")


    
    
    