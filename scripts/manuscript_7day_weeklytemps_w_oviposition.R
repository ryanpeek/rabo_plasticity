## 7 Day Weekly Temps with Oviposition (RABO)
## PEEK 02-NOV-2012

library(ggplot2)
library(RColorBrewer)

# GET FILE ----------------------------------------------------------------
load(file="C:/Users/rapeek/Dropbox/R/PROJECTS/Loggers/data/processed/2011-2014_solinst_mainstem_daily_compensated.RData")

## FOR HOURLY/DAILY DATA use one, otherwise change idx to 15
idx<-c(1,diff(daily$Datetime)) # make a diff column
i2<-c(1,which(idx!=1),nrow(daily)+1) # compare which rows are are not diff by 15 min
daily$grp<-rep(1:length(diff(i2)),diff(i2)) #use group to assign each portion of plotted line

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

colors<-scale_color_manual(name="Site", breaks=c("NFY","SFY","NFA","RUB"),
                           values=c("NFY"="#f4a582","SFY"="#ca0020","NFA"="#0571b0","RUB"="#92c5de"))


fillcolors<-scale_fill_manual(name="Oviposition", breaks=c("NFY","SFY","NFA","RUB"),
                              values=c("NFY"="gray20","SFY"="gray20","NFA"="gray20","RUB"="gray20"))

# fillcolors<-scale_fill_manual(name="Oviposition", breaks=c("NFY","SFY","NFA","RUB","MFA"),
#                   values=c("NFY"="darkgreen","SFY"="brown3","NFA"="cornflowerblue","RUB"="darkorange","MFA"="black"))

d7avg<-ggplot() +
  geom_ribbon(data=daily[daily$yday>105 & daily$yday<210,],aes(ymin=10,ymax=12, x=yday),fill="gray80",color="gray80",alpha=0.5,expand=c(0.5,0.5))+
  geom_line(aes(x=yday, y=Temperature.7,color="NFA",size="NFA"),data=daily[daily$site=="NFA" & daily$yday>105 & daily$yday<210,],alpha=1) +
  geom_line(aes(x=yday, y=Temperature.7,color="RUB",size="RUB"),data=daily[daily$site=="RUB" & daily$yday>105 & daily$yday<210,],alpha=1) +
  #geom_line(aes(x=yday, y=Temperature.7,color="MFA"),data=daily[daily$site=="MFA" & daily$yday>105 & daily$yday<210,], size=0.6,alpha=1) +
  geom_line(aes(x=yday, y=Temperature.7,color="NFY",size="NFY"),data=daily[daily$site=="NFY" & daily$yday>105 & daily$yday<210,],alpha=1) +
  geom_line(aes(x=yday, y=Temperature.7,color="SFY",size="SFY"),data=daily[daily$site=="SFY" & daily$yday>105 & daily$yday<210,],lty=5,alpha=1) +
  # NFA
  geom_point(aes(x=yday, y=Temperature.7, fill="NFA",shape="NFA"),daily[daily$yday==179 & daily$year==2011 & daily$site=="NFA",],size=4.5)+ # NFA 2011
  geom_point(aes(x=yday, y=Temperature.7),daily[daily$yday==138 & daily$year==2012 & daily$site=="NFA",], pch=21,fill="gray20",size=4.5)+ # NFA 2012
  geom_point(aes(x=yday, y=Temperature.7),daily[daily$yday==132 & daily$year==2013 & daily$site=="NFA",], pch=21,fill="gray20",size=4.5)+ # NFA 2013
  # NFY
  geom_point(aes(x=yday, y=Temperature.7,fill="NFY",shape="NFY"),daily[daily$yday==137 & daily$year==2012 & daily$site=="NFY",],size=4.5)+
  geom_point(aes(x=yday, y=Temperature.7),daily[daily$yday==134 & daily$year==2013 & daily$site=="NFY",], pch=22,fill="gray20",size=4.5)+
  # RUB
  geom_point(aes(x=yday, y=Temperature.7,fill="RUB",shape="RUB"),daily[daily$yday==146 & daily$year==2011 & daily$site=="RUB",],color="black",size=4.5)+ # prespill "2011-05-26"
  geom_point(aes(x=yday, y=Temperature.7),daily[daily$yday==182 & daily$year==2011 & daily$site=="RUB",], pch=24,fill="gray20",size=4.5)+ # postspill "2011-07-01"
  geom_point(aes(x=yday, y=Temperature.7),daily[daily$yday==135 & daily$year==2012 & daily$site=="RUB",], pch=24,fill="gray20",size=4.5)+ # RUB 2012
  geom_point(aes(x=yday, y=Temperature.7),daily[daily$yday==120 & daily$year==2013 & daily$site=="RUB",], pch=24,fill="gray20",size=4.5)+ # RUB 2013
  # SFY
  geom_point(aes(x=yday, y=Temperature.7,fill="SFY",shape="SFY"),daily[daily$yday==120 & daily$year==2013 & daily$site=="SFY",],size=5)+
  
  scale_x_continuous(breaks=c(105,120,135,150,165,180,195,210),labels=c("Apr-15","May-1","May-15","Jun-1","Jun-15","Jul-1","Jul-15","Aug-1")) +
  scale_y_continuous(limits=c(0,27), breaks=seq(0,27,3)) + colors + fillcolors+
  scale_shape_manual(name="Oviposition", breaks=c("NFY","SFY","NFA","RUB"),
                                values=c("NFY"=22,"SFY"=23,"NFA"=21,"RUB"=24))+
  scale_size_manual(name="Site", breaks=c("NFY","SFY","NFA","RUB"),
                     values=c("NFY"=0.7,"SFY"=0.9,"NFA"=1.3,"RUB"=1.1))+
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1),
                     legend.key=element_blank(),
                     #legend.background=element_rect(fill="white",colour="gray30"),
                     panel.grid.major = element_line(colour = 'gray80', linetype = 2)) +
  #guides(fill=guide_legend(keywidth=0.9,keyheight=1))+
  labs(y=expression(paste("Water Temperature (",degree,"C)")),x="Date", title="7 Day Average Water Temperature 2011-2013")
d7out<-d7avg+facet_grid(.~year,scales="fixed",drop=T)

d7out

# d7avg+facet_grid(year~.,scales="fixed",drop=T)
# ggsave(filename="./output/figs/7day_watertemps_w_oviposition_2011-2013.png", width=8, height=6, dpi=300) ## for full slide
# ggsave(filename="./output/figs/7day_watertemps_w_oviposition_2011-2013.pdf", width=8, height=6) ## for full slide

# 7 Day MINIMUM with Breeding Pts -----------------------------------------

# d7min<-ggplot() +
#   geom_ribbon(data=daily[daily$yday>105 & daily$yday<210,],aes(ymin=10,ymax=12, x=yday),fill="gray80",color="gray80",alpha=0.5)+
#   geom_line(aes(x=yday, y=temp.min.7,color="NFA"),data=daily[daily$site=="NFA" & daily$yday>105 & daily$yday<210,], type=2,size=0.6,alpha=1) +
#   geom_line(aes(x=yday, y=temp.min.7,color="RUB"),data=daily[daily$site=="RUB" & daily$yday>105 & daily$yday<210,],size=0.6,alpha=1) +
#   geom_line(aes(x=yday, y=temp.min.7,color="MFA"),data=daily[daily$site=="MFA" & daily$yday>105 & daily$yday<210,], size=0.6,alpha=1) +
#   geom_line(aes(x=yday, y=temp.min.7,color="NFY"),data=daily[daily$site=="NFY" & daily$yday>105 & daily$yday<210,], size=0.6,alpha=1) +
#   geom_line(aes(x=yday, y=temp.min.7,color="SFY"),data=daily[daily$site=="SFY" & daily$yday>105 & daily$yday<210,], size=0.6,alpha=1) +
#   # NFA
#   geom_point(aes(x=yday, y=temp.min.7,fill="NFA"),daily[daily$yday==179 & daily$year==2011 & daily$site=="NFA",], pch=21,color="black",size=4)+ # NFA 2011
#   geom_point(aes(x=yday, y=temp.min.7,fill="NFA"),daily[daily$yday==138 & daily$year==2012 & daily$site=="NFA",], pch=21,color="black",size=4)+# NFA 2012
#   geom_point(aes(x=yday, y=temp.min.7,fill="NFA"),daily[daily$yday==132 & daily$year==2013 & daily$site=="NFA",], pch=21,color="black",size=4)+# NFA 2013
#   # NFY
#   geom_point(aes(x=yday, y=temp.min.7,fill="NFY"),daily[daily$yday==137 & daily$year==2012 & daily$site=="NFY",], pch=21,color="black",size=4)+
#   geom_point(aes(x=yday, y=temp.min.7,fill="NFY"),daily[daily$yday==134 & daily$year==2013 & daily$site=="NFY",], pch=21,color="black",size=4)+
#   # RUB
#   geom_point(aes(x=yday, y=temp.min.7,fill="RUB"),daily[daily$yday==146 & daily$year==2011 & daily$site=="RUB",], pch=21,color="black",size=4)+ # prespill "2011-05-26"
#   geom_point(aes(x=yday, y=temp.min.7,fill="RUB"),daily[daily$yday==182 & daily$year==2011 & daily$site=="RUB",], pch=21,color="black",size=4)+ # postspill "2011-07-01"
#   geom_point(aes(x=yday, y=temp.min.7,fill="RUB"),daily[daily$yday==135 & daily$year==2012 & daily$site=="RUB",], pch=21,color="black",size=4)+# RUB 2012
#   geom_point(aes(x=yday, y=temp.min.7,fill="RUB"),daily[daily$yday==120 & daily$year==2013 & daily$site=="RUB",], pch=21,color="black",size=4)+# RUB 2013
#   # SFY
#   geom_point(aes(x=yday, y=temp.min.7,fill="SFY"),daily[daily$yday==120 & daily$year==2013 & daily$site=="SFY",], pch=21,color="black",size=4)+
#   
#   scale_x_continuous(breaks=c(105,120,135,150,165,180,195,210),labels=c("Apr-15","May-1","May-15","Jun-1","Jun-15","Jul-1","Jul-15","Aug-1")) +
#   scale_y_continuous(limits=c(0,27), breaks=seq(0,27,3))+ colors + fillcolors +
#   theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1),
#                      panel.grid.major = element_line(colour = 'gray60', linetype = 2)) +
#   labs(y=expression(paste("Minimum Water Temperature (",degree,"C)")),x="Date", 
#        title="7 Day Minimum Water Temperature 2011-2013")
# d7min+facet_grid(.~year,scales="fixed",drop=T)
# d7min+facet_grid(year~.,scales="fixed",drop=T)
# ggsave(filename="./output/figs/7day_mintemps_w_oviposition_2011-2013.png", width=8, height=6, dpi=300) ## for full slide
# ggsave(filename="./output/figs/7day_mintemps_w_oviposition_2011-2013.pdf", width=8, height=6) ## for full slide

# rm(list=ls())
