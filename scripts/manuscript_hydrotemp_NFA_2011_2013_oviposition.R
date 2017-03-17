# Match Flow and Temp 
## PEEK 08-Oct-2012

# LIBRARIES ---------------------------------------------------------------
library(lubridate)
library(grid)
library(scales)
library(ggplot2)

# GET DATA ----------------------------------------------------------------
site="NFA"
inputfile<-load(paste(root,"PROJECTS/Loggers/data//processed//2011-2013_solinst_mainstem_hourly_compensated.RData",sep=""))
h(hourly)
stg<-hourly[hourly$site=="NFA",]

# PLOT DISCONTINUOUS DATA -------------------------------------------------

# ## Filter the breaks as groups and add to aes() (DON"T CONNECT THE NAs)
# idx<-c(15,diff(stg$Datetime)) # make a diff column
# i2<-c(1,which(idx!=15),nrow(stg)+1) # compare which rows are are not diff by 15 min
# stg$grp<-rep(1:length(diff(i2)),diff(i2)) #use group to assign each portion of plotted line

# STACKPLOT ---------------------------------------------------------------

year="2011-2013"

breaks<-(c(0,4,8,12,16,20,24,28)) # for color scale
palette<-c("dark blue","blue","light blue","green","yellow","orange","orangered","brown4")

hrly11<-ggplot() + geom_line(mapping=aes(x=Datetime, y=Level+.5, colour=Temperature), data=stg, size=0.65,alpha=1) +
  ylab("Stage (m)") + xlab("") + scale_y_continuous(limits=c(0.5,3),breaks=seq(0.5,3,0.5))+
  scale_colour_gradientn("Water \nTemp (C)",colours=palette(c("dark blue","blue","light blue","green","yellow","orange","orangered","brown4")), 
                         values=(c(0,4,8,12,16,20,24,28)), rescaler = function(x, ...) x, oob = identity,limits=c(0,28), breaks=breaks, space="Lab") +
  scale_x_datetime(limits = c(as.POSIXct("2011-04-01 12:00"),as.POSIXct("2011-08-01 12:00")), breaks=date_breaks("2 weeks"), minor_breaks=date_breaks("1 week"),labels = date_format("%m/%d")) +
  theme_bw() + labs(title=paste("Compensated 15-min Stage & Water Temp. for ",site," 2011",sep=""))+theme(plot.title=element_text(size=11),axis.text.x = element_text(angle = 0, hjust = 0.5))+ 
  geom_point(aes(x = Datetime, y = Level+.5),subset(stg,Datetime==as.POSIXct("2011-06-28 12:00:00")), pch=21,color="black",size=5,fill="maroon1") +
  theme(legend.position = "right",plot.margin = unit(c(0.2,0.5,0,0.5), "cm"))
hrly11

hrly12<-ggplot() + geom_line(mapping=aes(x=Datetime, y=Level+0.5, colour=Temperature), data=stg, size=0.65,alpha=1) +
  ylab("Stage (m)") + xlab("") + scale_y_continuous(limits=c(0.5,4),breaks=seq(0.5,4.0,0.5))+
  scale_colour_gradientn("Water \nTemp (C)",colours=palette(palette), values=breaks, rescaler = function(x, ...) x, oob = identity,limits=c(0,28), breaks=breaks, space="Lab") +
  scale_x_datetime(limits = c(as.POSIXct("2012-04-01 12:00"),as.POSIXct("2012-08-01 12:00")), breaks=date_breaks("2 weeks"), minor_breaks=date_breaks("1 week"),labels = date_format("%m/%d")) +
  theme_bw() + labs(title=paste("Compensated 15-min Stage & Water Temp. for ",site," 2012",sep=""))+theme(plot.title=element_text(size=11),axis.text.x = element_text(angle = 0, hjust = 0.5))+
  geom_point(aes(x = Datetime, y = Level+0.5),subset(stg,Datetime==as.POSIXct("2012-05-17 12:00:00")), pch=21,color="black",size=5,fill="maroon1")+
  theme(legend.position = "right",plot.margin = unit(c(0.2,0.5,0,0.5), "cm"))
# hrly12

hrly13<-ggplot() + geom_line(mapping=aes(x=Datetime, y=Level+0.6, colour=Temperature), data=stg, size=0.65,alpha=1) +
  ylab("Stage (m)") + xlab("") + scale_y_continuous(limits=c(0.5,3),breaks=seq(0.0,3,0.5))+
  scale_colour_gradientn("Water \nTemp (C)",colours=palette(palette), values=breaks, rescaler = function(x, ...) x, oob = identity,limits=c(0,28), breaks=breaks, space="Lab") +
  scale_x_datetime(limits = c(as.POSIXct("2013-04-01 12:00"),as.POSIXct("2013-08-01 12:00")), breaks=date_breaks("2 weeks"), minor_breaks=date_breaks("1 week"),labels = date_format("%m/%d")) +
  theme_bw() + labs(title=paste("Compensated 15-min Stage & Water Temp. for ",site," 2013",sep=""))+theme(plot.title=element_text(size=11),axis.text.x = element_text(angle = 0, hjust = 0.5))+ 
  geom_point(aes(x = Datetime, y = Level+0.6),subset(stg,Datetime==as.POSIXct("2013-05-12 12:00:00")), pch=21,color="black",size=5,fill="maroon1") +
  theme(legend.position = "right",plot.margin = unit(c(0.2,0.5,0,0.5), "cm"),legend.margin=unit(0.1, "cm"))
# hrly13

source(paste(root,"functions/multiplot.R",sep=""))
multiNFA<-multiplot(hrly11,hrly12,hrly13)
