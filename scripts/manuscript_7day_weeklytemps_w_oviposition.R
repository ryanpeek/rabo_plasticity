## 7 Day Weekly Temps with Oviposition (RABO)
## PEEK 02-NOV-2012

library(tidyverse)
library(RColorBrewer)
library(viridis)

# GET FILE ----------------------------------------------------------------
load(file="./data/2011-2016_solinst_mainstem_daily.rda")
load("data/master_dat_2011-2016.rda")

daily <- master_df %>% filter(!site=="MFY")

#daily<-dy.df %>% filter(type=="solinst")

## FOR HOURLY/DAILY DATA use one, otherwise change idx to 15
# idx<-c(1,diff(daily$datetime)) # make a diff column
# i2<-c(1,which(idx!=1),nrow(daily)+1) # compare which rows are are not diff by 15 min
# daily$grp<-rep(1:length(diff(i2)),diff(i2)) #use group to assign each portion of plotted line

# 7 Day AVERAGE with Breeding Pts -----------------------------------------

#8dd3c7 green
#ffffb3 yellow
#bebada purple
#fb8072 red

#ca0020
#f4a582
#92c5de
#0571b0

## Added day of breeding initiation 
# colors<-scale_color_manual(name="Site", breaks=c("NFY","SFY","NFA","RUB"),
#                            values=c("NFY"="#e41a1c","SFY"="#377eb8","NFA"="#4daf4a","RUB"="#984ea3"))

colors<-scale_color_manual(name="Site", breaks=c("NFY","SFY","NFA","RUB", "MFA"),
                           values=c("NFY"="#f4a582","SFY"="#ca0020","NFA"="#0571b0","RUB"="#92c5de", "MFA"="gray40"))


fillcolors<-scale_fill_manual(name="Oviposition", breaks=c("NFY","SFY","NFA","RUB", "MFA"),
                              values=c("NFY"="gray20","SFY"="gray20","NFA"="gray20","RUB"="gray20", "MFA"="gray20"))

# fillcolors<-scale_fill_manual(name="Oviposition", breaks=c("NFY","SFY","NFA","RUB","MFA"),
#                   values=c("NFY"="darkgreen","SFY"="brown3","NFA"="cornflowerblue","RUB"="darkorange","MFA"="black"))



d7avg<-ggplot() +
  geom_ribbon(data=daily[daily$DOY>105 & daily$DOY<210,],aes(ymin=10, ymax=12, x=DOY),fill="gray80",color="gray80",alpha=0.5)+
  geom_line(aes(x=DOY, y=temp_7_avg, color="NFA",linetype="NFA"),data=daily[daily$site=="NFA" & daily$DOY>105 & daily$DOY<210,],alpha=1, lwd=1.2) +
  geom_line(aes(x=DOY, y=temp_7_avg, color="NFY",linetype="NFY"),data=daily[daily$site=="NFY" & daily$DOY>105 & daily$DOY<210,],alpha=1, lwd=0.7) +
  geom_line(aes(x=DOY, y=temp_7_avg, color="RUB",linetype="RUB"),data=daily[daily$site=="RUB" & daily$DOY>105 & daily$DOY<210,],alpha=1, lwd=1.1) +
  geom_line(aes(x=DOY, y=temp_7_avg, color="SFY",linetype="SFY"),data=daily[daily$site=="SFY" & daily$DOY>105 & daily$DOY<210,], alpha=1, lwd=0.9) +
  geom_line(aes(x=DOY, y=temp_7_avg, color="MFA",linetype="MFA"),data=daily[daily$site=="MFA" & daily$DOY>105 & daily$DOY<210,], alpha=1, lwd=0.7) +
 
   # NFA
  geom_point(aes(x=DOY, y=temp_7_avg, fill="NFA",shape="NFA"),daily[daily$DOY==179 & daily$WY==2011 & daily$site=="NFA",],size=4.5)+ # NFA 2011
  geom_point(aes(x=DOY, y=temp_7_avg),daily[daily$DOY==138 & daily$WY==2012 & daily$site=="NFA",], pch=21,fill="gray20",size=4.5)+ # NFA 2012
  geom_point(aes(x=DOY, y=temp_7_avg),daily[daily$DOY==132 & daily$WY==2013 & daily$site=="NFA",], pch=21,fill="gray20",size=4.5)+ # NFA 2013
  # NFY
  geom_point(aes(x=DOY, y=temp_7_avg,fill="NFY",shape="NFY"),daily[daily$DOY==137 & daily$WY==2012 & daily$site=="NFY",],size=4.5)+
  geom_point(aes(x=DOY, y=temp_7_avg),daily[daily$DOY==134 & daily$WY==2013 & daily$site=="NFY",], pch=22,fill="gray20",size=4.5)+
  
  # RUB
  geom_point(aes(x=DOY, y=temp_7_avg,fill="RUB",shape="RUB"),daily[daily$DOY==146 & daily$WY==2011 & daily$site=="RUB",],color="black",size=4.5)+ # prespill "2011-05-26"
  geom_point(aes(x=DOY, y=temp_7_avg),daily[daily$DOY==182 & daily$WY==2011 & daily$site=="RUB",], pch=24,fill="gray20",size=4.5)+ # postspill "2011-07-01"
  geom_point(aes(x=DOY, y=temp_7_avg),daily[daily$DOY==135 & daily$WY==2012 & daily$site=="RUB",], pch=24,fill="gray20",size=4.5)+ # RUB 2012
  geom_point(aes(x=DOY, y=temp_7_avg),daily[daily$DOY==120 & daily$WY==2013 & daily$site=="RUB",], pch=24,fill="gray20",size=4.5)+ # RUB 2013
  # SFY
  geom_point(aes(x=DOY, y=temp_7_avg,fill="SFY",shape="SFY"),daily[daily$DOY==120 & daily$WY==2013 & daily$site=="SFY",],size=5)+
  
  scale_x_continuous(breaks=c(105,120,135,150,165,180,195,210),labels=c("Apr-15","May-1","May-15","Jun-1","Jun-15","Jul-1","Jul-15","Aug-1")) +
  scale_y_continuous(limits=c(0,27), breaks=seq(0,27,3)) + colors + fillcolors +
  scale_shape_manual(name="Oviposition", breaks=c("NFY","SFY","NFA","RUB"),
                                values=c("NFY"=22,"SFY"=23,"NFA"=21,"RUB"=24))+
  scale_linetype_manual(name="Site", breaks=c("NFY","SFY","NFA","RUB","MFA"),
                     values=c("NFY"=1,"SFY"=2,"NFA"=1,"RUB"=4, "MFA"=1))+
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1),
                     legend.key=element_blank(),
                     #legend.background=element_rect(fill="white",colour="gray30"),
                     panel.grid.major = element_line(colour = 'gray80', linetype = 2)) +
  #guides(fill=guide_legend(keywidth=0.9,keyheight=1))+
  labs(y=expression(paste("Water Temperature (",degree,"C)")),x="Date", title="7 Day Average Water Temperature 2011-2013")

(d7out<-d7avg+facet_grid(.~WY,scales="fixed",drop=T))


# d7avg+facet_grid(WY~.,scales="fixed",drop=T)
# ggsave(filename="./output/figs/7day_watertemps_w_oviposition_2011-2013.png", width=8, height=6, dpi=300) ## for full slide
# ggsave(filename="./output/figs/7day_watertemps_w_oviposition_2011-2013.pdf", width=8, height=6) ## for full slide

# 7 Day MINIMUM with Breeding Pts -----------------------------------------

# d7min<-ggplot() +
#   geom_ribbon(data=daily[daily$DOY>105 & daily$DOY<210,],aes(ymin=10,ymax=12, x=DOY),fill="gray80",color="gray80",alpha=0.5)+
#   geom_line(aes(x=DOY, y=temp.min.7,color="NFA"),data=daily[daily$site=="NFA" & daily$DOY>105 & daily$DOY<210,], type=2,size=0.6,alpha=1) +
#   geom_line(aes(x=DOY, y=temp.min.7,color="RUB"),data=daily[daily$site=="RUB" & daily$DOY>105 & daily$DOY<210,],size=0.6,alpha=1) +
#   geom_line(aes(x=DOY, y=temp.min.7,color="MFA"),data=daily[daily$site=="MFA" & daily$DOY>105 & daily$DOY<210,], size=0.6,alpha=1) +
#   geom_line(aes(x=DOY, y=temp.min.7,color="NFY"),data=daily[daily$site=="NFY" & daily$DOY>105 & daily$DOY<210,], size=0.6,alpha=1) +
#   geom_line(aes(x=DOY, y=temp.min.7,color="SFY"),data=daily[daily$site=="SFY" & daily$DOY>105 & daily$DOY<210,], size=0.6,alpha=1) +
#   # NFA
#   geom_point(aes(x=DOY, y=temp.min.7,fill="NFA"),daily[daily$DOY==179 & daily$WY==2011 & daily$site=="NFA",], pch=21,color="black",size=4)+ # NFA 2011
#   geom_point(aes(x=DOY, y=temp.min.7,fill="NFA"),daily[daily$DOY==138 & daily$WY==2012 & daily$site=="NFA",], pch=21,color="black",size=4)+# NFA 2012
#   geom_point(aes(x=DOY, y=temp.min.7,fill="NFA"),daily[daily$DOY==132 & daily$WY==2013 & daily$site=="NFA",], pch=21,color="black",size=4)+# NFA 2013
#   # NFY
#   geom_point(aes(x=DOY, y=temp.min.7,fill="NFY"),daily[daily$DOY==137 & daily$WY==2012 & daily$site=="NFY",], pch=21,color="black",size=4)+
#   geom_point(aes(x=DOY, y=temp.min.7,fill="NFY"),daily[daily$DOY==134 & daily$WY==2013 & daily$site=="NFY",], pch=21,color="black",size=4)+
#   # RUB
#   geom_point(aes(x=DOY, y=temp.min.7,fill="RUB"),daily[daily$DOY==146 & daily$WY==2011 & daily$site=="RUB",], pch=21,color="black",size=4)+ # prespill "2011-05-26"
#   geom_point(aes(x=DOY, y=temp.min.7,fill="RUB"),daily[daily$DOY==182 & daily$WY==2011 & daily$site=="RUB",], pch=21,color="black",size=4)+ # postspill "2011-07-01"
#   geom_point(aes(x=DOY, y=temp.min.7,fill="RUB"),daily[daily$DOY==135 & daily$WY==2012 & daily$site=="RUB",], pch=21,color="black",size=4)+# RUB 2012
#   geom_point(aes(x=DOY, y=temp.min.7,fill="RUB"),daily[daily$DOY==120 & daily$WY==2013 & daily$site=="RUB",], pch=21,color="black",size=4)+# RUB 2013
#   # SFY
#   geom_point(aes(x=DOY, y=temp.min.7,fill="SFY"),daily[daily$DOY==120 & daily$WY==2013 & daily$site=="SFY",], pch=21,color="black",size=4)+
#   
#   scale_x_continuous(breaks=c(105,120,135,150,165,180,195,210),labels=c("Apr-15","May-1","May-15","Jun-1","Jun-15","Jul-1","Jul-15","Aug-1")) +
#   scale_y_continuous(limits=c(0,27), breaks=seq(0,27,3))+ colors + fillcolors +
#   theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1),
#                      panel.grid.major = element_line(colour = 'gray60', linetype = 2)) +
#   labs(y=expression(paste("Minimum Water Temperature (",degree,"C)")),x="Date", 
#        title="7 Day Minimum Water Temperature 2011-2013")
# d7min+facet_grid(.~WY,scales="fixed",drop=T)
# d7min+facet_grid(WY~.,scales="fixed",drop=T)
# ggsave(filename="./output/figs/7day_mintemps_w_oviposition_2011-2013.png", width=8, height=6, dpi=300) ## for full slide
# ggsave(filename="./output/figs/7day_mintemps_w_oviposition_2011-2013.pdf", width=8, height=6) ## for full slide

# rm(list=ls())

# NEW PLOT WITH ONE YEAR ONLY ---------------------------------------------

daily1 <- daily %>% filter(WY==2012)

d7avg<-ggplot() +
  geom_ribbon(data=daily1[daily1$DOY>105 & daily1$DOY<210,],aes(ymin=10, ymax=12, x=DOY),fill="gray80",color="gray80",alpha=0.5)+
  geom_line(aes(x=DOY, y=temp_7_avg, color="NFA",linetype="NFA"),data=daily1[daily1$site=="NFA" & daily1$DOY>105 & daily1$DOY<210,],alpha=1, lwd=1.2) +
  geom_line(aes(x=DOY, y=temp_7_avg, color="NFY",linetype="NFY"),data=daily1[daily1$site=="NFY" & daily1$DOY>105 & daily1$DOY<210,],alpha=1, lwd=0.7) +
  geom_line(aes(x=DOY, y=temp_7_avg, color="RUB",linetype="RUB"),data=daily1[daily1$site=="RUB" & daily1$DOY>105 & daily1$DOY<210,],alpha=1, lwd=1.1) +
  geom_line(aes(x=DOY, y=temp_7_avg, color="SFY",linetype="SFY"),data=daily1[daily1$site=="SFY" & daily1$DOY>105 & daily1$DOY<210,], alpha=1, lwd=0.9) +
  geom_line(aes(x=DOY, y=temp_7_avg, color="MFA",linetype="MFA"),data=daily1[daily1$site=="MFA" & daily1$DOY>105 & daily1$DOY<210,], alpha=1, lwd=0.7) +
  

  scale_x_continuous(breaks=c(105,120,135,150,165,180,195,210),labels=c("Apr-15","May-1","May-15","Jun-1","Jun-15","Jul-1","Jul-15","Aug-1")) +
  scale_y_continuous(limits=c(0,27), breaks=seq(0,27,3)) + colors + fillcolors +

  scale_linetype_manual(name="Site", breaks=c("NFY","SFY","NFA","RUB","MFA"),
                        values=c("NFY"=1,"SFY"=2,"NFA"=1,"RUB"=4, "MFA"=1))+
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1),
                     legend.key=element_blank(),
                     panel.grid.major = element_line(colour = 'gray80', linetype = 2)) +
  labs(y=expression(paste("Water Temperature (",degree,"C)")),x="Date", title="7 Day Average Water Temperature 2012")

(d7out<-d7avg+facet_grid(.~WY,scales="fixed",drop=T))

ggsave(filename = "figs/watertemp_7day_2012.png", width = 9, height = 6.6, units="in")
