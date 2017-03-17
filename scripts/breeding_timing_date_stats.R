## STATS ON FROG BREEDING
## FOR PLASTIcITY PAPER

library(lubridate)

inputfile<-paste(root, "PROJECTS//Recsn_Limb//Frogs//data/processed/Plasticity_breeding_summary_CA.csv", sep="")


## import frog/breeding and format
frog<-read.csv(inputfile)
dim(frog)
names(frog)
summary(frog)
str(frog)
h(frog)

frog$date_obs<-mdy(as.character(frog[[10]])) #  obs start of breeding
frog$date_end<-mdy(as.character(frog[[11]])) #  obs end of breeding
frog$date_strt_est<-mdy(as.character(frog[[12]])) #  est start of breeding
frog$date_end_est<-mdy(as.character(frog[[13]])) #  est end of breeding

frog$yday_obs<-yday(frog$date_strt) # use lubridate
frog$yday_strt<-yday(frog$date_strt_est)
frog$yday_end<-yday(frog$date_end_est)
frog$duration_est<-as.numeric(round(frog$date_end_est-frog$date_strt_est))
str(frog)

summary(frog)
names(frog)
frog


## subset columns
Frog<-frog[,c(1:4,6:7,9,14,16:22,15)]
coast<-subset(Frog,Sierra_Coast=="C" & Year>2001)
coast_all<-subset(Frog,Sierra_Coast=="C")
dim(coast)

sierra<-subset(Frog,Sierra_Coast=="S" & Year>2001)
sierra_all<-subset(Frog,Sierra_Coast=="S")
dim(sierra)
summary(sierra)

rcsn11<-subset(Frog,Sierra_Coast=="S" & Year==2011)
rcsn11<-subset(Frog,Sierra_Coast=="S" & Year==2011 & !Trib=="Y")
dim(rcsn11)
rcsn11
av_11<-mean(rcsn11$yday_strt)
as.Date(as.character(av_11), "%j")

rcsn12<-subset(Frog,Sierra_Coast=="S" & Year==2012)
rcsn12<-subset(Frog,Sierra_Coast=="S" & Year==2012 & !Trib=="Y" )
dim(rcsn12)
rcsn12
av_12<-mean(rcsn12$yday_strt)
as.Date(as.character(av_12), "%j")

rcsn13<-subset(Frog,Sierra_Coast=="S" & Year==2013)
rcsn13<-subset(Frog,Sierra_Coast=="S" & Year==2013 & !Trib=="Y" )
dim(rcsn13)
rcsn13
av_13<-mean(rcsn13$yday_strt)
as.Date(as.character(av_13), "%j")


sierra<-data.frame(sierra[,c(1,2,3,13,14,15)])
coast<-data.frame(coast[,c(1,2,3,13,14,15)])
summary(sierra)

dim(sierra)
dim(coast)


# STATS -------------------------------------------------------------------

frogStats <- function(y, data){
  arguments <- as.list(match.call())
  y = eval(arguments$y, data)
  result<-data.frame("mean.date"=as.Date(as.character(mean(y)),"%j"),
                     "mean.yday"=(mean(y)),
                     "sd"=sd(y),
                     "avg.Date"=as.Date(as.character(mean(y)),"%j"),
                     "min.Date"=as.Date(as.character(min(y)),"%j"),
                     "max.Date"=as.Date(as.character(max(y)),"%j",),
                     "range"= max(y)-min(y),
                     "cv"= (sd(y)/mean(y))*100)
  print(result)
}

frogStats(yday_strt,sierra) # 2002-2013
frogStats(yday_strt,sierra_all) # all years
frogStats(yday_strt,coast) # 2002-2013
frogStats(yday_strt,coast_all) # all years


# ## COASTAL STATS
# av_coast<-mean(coast$yday_strt)
# as.Date(as.character(av_coast), "%j")
# ## CALC CV
# sd(coast$yday_strt)
# (sd(coast$yday_strt)/mean(coast$yday_strt))*100
# max(coast$yday_strt)-min(coast$yday_strt)
# ## SIERRA STATS
# av_sierra<-mean(sierra$yday_strt)
# as.Date(as.character(av_sierra), "%j")
# ## CALC CV
# (sd(sierra$yday_strt)/mean(sierra$yday_strt))*100
# sd(sierra$yday_strt)
# max(sierra$yday_strt)-min(sierra$yday_strt)


wilcox.test(sierra$yday_obs, coast$yday_obs)
kruskal.test(list(sierra$yday_obs, coast$yday_obs))
