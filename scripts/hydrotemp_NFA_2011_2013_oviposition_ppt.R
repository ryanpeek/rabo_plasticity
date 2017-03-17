# Match Flow and Temp 
## PEEK 08-Oct-2012

# LIBRARIES ---------------------------------------------------------------
library(lubridate)
library(grid)
library(scales)

# GET DATA ----------------------------------------------------------------
site="NFA"

## this is the stage/temp data
# inputfile<-choose.files()
#inputfile<-paste(root,"PROJECTS/Recsn_Limb/Hydrotemp/data//processed//merged/2011-2013_NFA_compensated_solinst.csv",sep="") # compensated data
inputfile<-paste(root,"PROJECTS/Recsn_Limb/Hydrotemp/data//processed//merged/2011-2013_NFA_solinst_adj_hourly.csv",sep="") # uncompensated data

inputfile3<-paste(root,"PROJECTS/Loggers/data//processed//2011-2013_NFA_solinst_daily.csv",sep="") # uncompensated data


## import stage/temp data and format 
stg<-read.csv(inputfile3,stringsAsFactors=FALSE)
dim(stg) #lists total columns of dataset
head(stg)
stg$Datetime<-mdy_hm(stg$Datetime)
str(stg)
summary(stg)

# dim(stg)
stg<-na.omit(stg) # remove N/As
summary(stg)
dim(stg)

## THIS IS BREEDING DATA:
inputfile2<-paste(root,"PROJECTS/Recsn_Limb/Frogs/data//processed//oviposition_daily_studysites.csv",sep="") # uncompensated data

## import stage/temp data and format 
ovi<-read.csv(inputfile2,stringsAsFactors=F)
str(ovi)
ovi$obs.s<-paste0(ovi$obs_strt," ",ovi$Time)
ovi$est.s<-paste0(ovi$estim_strt," ",ovi$Time)
ovi$obs.s<-mdy_hm(ovi$obs.s)
ovi$est.s<-mdy_hm(ovi$est.s)
h(ovi)

inputfile3<-paste(root,"PROJECTS/Loggers/data//processed//2011-2013_NFA_solinst_daily.csv",sep="") # uncompensated data


# PLOT DISCONTINUOUS DATA -------------------------------------------------

## Filter the breaks as groups and add to aes() (DON"T CONNECT THE NAs)
idx<-c(15,diff(stg$Datetime)) # make a diff column
i2<-c(1,which(idx!=15),nrow(stg)+1) # compare which rows are are not diff by 15 min
stg$grp<-rep(1:length(diff(i2)),diff(i2)) #use group to assign each portion of plotted line

# STACKPLOT ---------------------------------------------------------------

source("C:/Users/rapeek/Dropbox/R/PROJECTS/Recsn_Limb/Hydrotemp/R/functions/ggplot_3yr.R")
# fig.hydrotmp3yr(with.all=T)

breaks<-(c(0,4,8,12,16,20,24,28)) # for color scale
palette<-c("dark blue","blue","light blue","green","yellow","orange","orangered","brown4")

year="2011-2013"
#hrly11<-ggplot() + geom_line(mapping=aes(group=grp,x=Datetime, y=Level, colour=Temperature), data=stg, size=0.65,alpha=1) +
hrly11<-ggplot() + geom_line(mapping=aes(x=Datetime, y=Level, colour=Temperature), data=stg, size=0.65,alpha=1) +
  ylab("Stage (m)") + xlab("") + scale_y_continuous(limits=c(0.5,3.5),breaks=seq(0.5,3.5,0.5))+
  scale_colour_gradientn("Water \nTemp (C)",colours=palette(palette), values=breaks, rescaler = function(x, ...) x, oob = identity,limits=c(0,28), breaks=breaks, space="Lab") +
  scale_x_datetime(limits = c(as.POSIXct("2011-04-01 12:00"),as.POSIXct("2011-08-01 12:00")), breaks=date_breaks("2 weeks"), minor_breaks=date_breaks("1 week"),labels = date_format("%m/%d")) +
  theme_bw() + labs(title=paste("Compensated 15-min Stage & Water Temp. for ",site," 2011",sep=""))+theme(plot.title=element_text(size=11),axis.text.x = element_text(angle = 0, hjust = 0.5))+ 
  geom_point(aes(x = Datetime, y = Level),subset(stg,Datetime==as.POSIXct("2011-06-28 12:00:00")), pch=21,color="black",size=5,fill="maroon1") +
  geom_rect(aes(xmin = as.POSIXct("2011-06-27"),xmax = as.POSIXct("2011-06-30"), ymin = 0.5,ymax=1.0),stat="identity",fill="gray30",alpha=0.5)+
  geom_rect(aes(xmin = as.POSIXct("2011-06-15"),xmax = as.POSIXct("2011-06-17"), ymin = 0.5,ymax=1.0),stat="identity",fill="gray30",alpha=0.5)+
  geom_rect(aes(xmin = as.POSIXct("2011-06-03"),xmax = as.POSIXct("2011-06-07"), ymin = 0.5,ymax=1.0),stat="identity",fill="gray30",alpha=0.5)+
  geom_rect(aes(xmin = as.POSIXct("2011-05-31"),xmax = as.POSIXct("2011-06-02"), ymin = 0.5,ymax=1.0),stat="identity",fill="gray30",alpha=0.5)+
  geom_rect(aes(xmin = as.POSIXct("2011-05-23"),xmax = as.POSIXct("2011-05-25"), ymin = 0.5,ymax=1.0),stat="identity",fill="gray30",alpha=0.5)+
  theme(legend.position = "right",plot.margin = unit(c(0.2,0.5,0,0.5), "cm"))
hrly11

hrly12<-ggplot() + geom_line(mapping=aes(x=Datetime, y=Level, colour=Temperature), data=stg, size=0.65,alpha=1) +
  ylab("Stage (m)") + xlab("") + scale_y_continuous(limits=c(0.5,3.5),breaks=seq(0.5,3.5,0.5))+
  scale_colour_gradientn("Water \nTemp (C)",colours=palette(palette), values=breaks, rescaler = function(x, ...) x, oob = identity,limits=c(0,28), breaks=breaks, space="Lab") +
  scale_x_datetime(limits = c(as.POSIXct("2012-04-01 12:00"),as.POSIXct("2012-08-01 12:00")), breaks=date_breaks("2 weeks"), minor_breaks=date_breaks("1 week"),labels = date_format("%m/%d")) +
  theme_bw() + labs(title=paste("Compensated 15-min Stage & Water Temp. for ",site," 2012",sep=""))+theme(plot.title=element_text(size=11),axis.text.x = element_text(angle = 0, hjust = 0.5))+
  geom_point(aes(x = Datetime, y = Level),subset(stg,Datetime==as.POSIXct("2012-05-17 12:00:00")), pch=21,color="black",size=5,fill="maroon1")+
  geom_rect(aes(xmin = as.POSIXct("2012-04-25"),xmax = as.POSIXct("2012-04-29"), ymin = 0.5,ymax=1.0),stat="identity",fill="gray30",alpha=0.5)+
  geom_rect(aes(xmin = as.POSIXct("2012-05-03"),xmax = as.POSIXct("2012-05-05"), ymin = 0.5,ymax=1.0),stat="identity",fill="gray30",alpha=0.5)+
  geom_rect(aes(xmin = as.POSIXct("2012-05-25"),xmax = as.POSIXct("2012-05-27"), ymin = 0.5,ymax=1.0),stat="identity",fill="gray30",alpha=0.5)+
  geom_rect(aes(xmin = as.POSIXct("2012-06-04"),xmax = as.POSIXct("2012-06-05"), ymin = 0.5,ymax=1.0),stat="identity",fill="gray30",alpha=0.5)+
  geom_rect(aes(xmin = as.POSIXct("2012-06-22"),xmax = as.POSIXct("2012-06-23"), ymin = 0.5,ymax=1.0),stat="identity",fill="gray30",alpha=0.5,alpha=0.5)+
  theme(legend.position = "right",plot.margin = unit(c(0.2,0.5,0,0.5), "cm"))
hrly12

hrly13<-ggplot() + geom_line(mapping=aes(x=Datetime, y=Level, colour=Temperature), data=stg, size=0.65,alpha=1) +
  ylab("Stage (m)") + xlab("") + scale_y_continuous(limits=c(0.5,3.5),breaks=seq(0.5,3.5,0.5))+
  scale_colour_gradientn("Water \nTemp (C)",colours=palette(palette), values=breaks, rescaler = function(x, ...) x, oob = identity,limits=c(0,28), breaks=breaks, space="Lab") +
  scale_x_datetime(limits = c(as.POSIXct("2013-04-01 12:00"),as.POSIXct("2013-08-01 12:00")), breaks=date_breaks("2 weeks"), minor_breaks=date_breaks("1 week"),labels = date_format("%m/%d")) +
  theme_bw() + labs(title=paste("Compensated 15-min Stage & Water Temp. for ",site," 2013",sep=""))+theme(plot.title=element_text(size=11),axis.text.x = element_text(angle = 0, hjust = 0.5))+ 
  geom_rect(aes(xmin = as.POSIXct("2013-04-06"),xmax = as.POSIXct("2013-04-09"), ymin = 0.5,ymax=0.7),stat="identity",fill="gray30",alpha=0.5)+
  geom_rect(aes(xmin = as.POSIXct("2013-05-04"),xmax = as.POSIXct("2013-05-09"), ymin = 0.5,ymax=0.7),stat="identity",fill="gray30",alpha=0.5)+
  geom_rect(aes(xmin = as.POSIXct("2013-06-23"),xmax = as.POSIXct("2013-06-26"), ymin = 0.5,ymax=0.7),stat="identity",fill="gray30",alpha=0.5)+
  geom_point(aes(x = Datetime, y = Level),subset(stg,Datetime==as.POSIXct("2013-05-12 12:00:00")), pch=21,color="black",size=5,fill="maroon1") +
  theme(legend.position = "right",plot.margin = unit(c(0.2,0.5,0,0.5), "cm"),legend.margin=unit(0.1, "cm"))
hrly13

# source("C:/Users/rapeek/Dropbox/R/PROJECTS/Recsn_Limb/Hydrotemp/R/functions/multiplot.R")
source(paste(root,"functions/multiplot.R",sep=""))
multiplot(hrly11,hrly12,hrly13)

# ggsave("breeding_NFA_hydrotemp_2011_2013_9x3_7.png", width=9.5, height=3.7, dpi=300) ## for ppt slides stacked
# ggsave("breeding_NFA_hydrotemp_2011_2013.png", width=8, height=6, dpi=300) ## for ppt slides stacked

png("./output/figs/breedingtiming/breeding_NFA_hydrotemp_2011_2013_200dpi.png",width=8,height=6,units="in",res=200)
#png("./output/figs/breedingtiming/breeding_NFA_hydrotemp_2011_2013_9x3_7.png",width=9.5,height=4,units="in",res=200)
multiplot(hrly11,hrly12,hrly13)
dev.off()



### NEED TO GET PPT STATION DATA FROM 
## YUBA: LSP
## AMR: CLF (C)

year<-2011
begin<-"2010-10-01"
ends<-"2013-09-30"
sensor<-2 # 45=PPT INC (precip incremental), 16=RAINTIP (tipping bucket), 2=RAIN (precip accumulated) 

sgp<-get.CDEC("lsp" ,duration="D", paste(sensor,sep=""),paste(begin,sep=""),paste(ends,sep=""))

names(sgp)[3] <- "PPT_incremental_in" # rename to shorter column

cdec.dat$ppt_diff<-NA
cdec.dat$ppt_diff<-c(NA,with(cdec.dat,diff(sensor_2)))

df<-cdec.dat
df$year<-year(df$datetime)
df$mon<-month(df$datetime)
summary(df)
plot(df$datetime, df$sensor_2, col="red",type="h")
with(subset(df, df$mon<10 & df$year==2013), plot(datetime, ppt_diff, col="blue",type="h"))
with(subset(df, df$mon<10 & df$year==2011), plot(datetime, ppt_diff, col="blue",type="h"))

subset(df, df$mon<10 & df$year==2011)



