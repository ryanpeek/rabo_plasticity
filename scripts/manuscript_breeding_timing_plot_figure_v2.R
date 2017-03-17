## PLOT RANA BOYLII OVIPOSITION TIMING OVER 30+ YEARS
## R. Peek

library(lubridate)
library(ggplot2)
library(scales)
library(grid)
library(ggthemes)
library(viridis)

# GET DATA ----------------------------------------------------------------
inputfile<-"C://Users//rapeek//Dropbox//R//PROJECTS//RbPlasticity/data/processed/Plasticity_breeding_summary_CA.csv"

inputfile<-"data/processed/Plasticity_breeding_summary_CA.csv"
frog<-read.csv(inputfile,stringsAsFactors=TRUE)

# FORMAT DATES ------------------------------------------------------------
frog[,c(10)]<-mdy(as.character(frog[,c(10)]))
frog[,c(11)]<-mdy(as.character(frog[,c(11)]))
frog[,c(12)]<-mdy(as.character(frog[,c(12)]))
frog[,c(13)]<-mdy(as.character(frog[,c(13)]))

Frogs<-frog[frog$Trib=="N",]# remove all tribs

## Subset to years above 1980 or 2000
rcsn80<-Frogs[Frogs$estim_strt >= as.POSIXct("1980-01-01"),]
rcsn02<-Frogs[Frogs$estim_strt >= as.POSIXct("2002-01-01"),]

# reorder the factor levels for water year type so they appear correctly in legend
rcsn80$WYT<-factor(rcsn80$WYT,levels=c("W","AN","BN","D","C"))
rcsn02$WYT<-factor(rcsn02$WYT,levels=c("W","AN","BN","D","C"))

# SHADED BOXES AND 2002-2013 ----------------------------------------------

sp <- ggplot(aes(x=yday(obs_strt), y=Year, color=Sierra_Coast),data=rcsn02) + 
  geom_rect(aes(ymin=2002.5,ymax=2003.5, xmin=55,xmax=215),fill="gray90",color="gray90",alpha=0.7,expand=c(0.5,0.5))+
  geom_rect(aes(ymin=2004.5,ymax=2005.5, xmin=55,xmax=215),fill="gray90",color="gray90",alpha=0.7,expand=c(0.5,0.5))+
  geom_rect(aes(ymin=2006.5,ymax=2007.5, xmin=55,xmax=215),fill="gray90",color="gray90",alpha=0.7,expand=c(0.5,0.5))+
  geom_rect(aes(ymin=2008.5,ymax=2009.5, xmin=55,xmax=215),fill="gray90",color="gray90",alpha=0.7,expand=c(0.5,0.5))+
  geom_rect(aes(ymin=2010.5,ymax=2011.5, xmin=55,xmax=215),fill="gray90",color="gray90",alpha=0.7,expand=c(0.5,0.5))+
  geom_rect(aes(ymin=2012.5,ymax=2013.5, xmin=55,xmax=215),fill="gray90",color="gray90",alpha=0.7,expand=c(0.5,0.5))+
  geom_point(pch=16,size=3.7) +
  geom_point(pch=21,bg="gray",size=2.6,show.legend=FALSE) +
  geom_text(aes(label=Site),size=3,hjust=-0.2,vjust=-1,fontface=3,show.legend=FALSE)+ # position=position_jitter(width = 0.4,height=0.4)
  scale_y_continuous(breaks=seq(2002,2013,1)) +
  ylab("Year") + xlab("")+
  scale_x_continuous(breaks=c(60,75,90,105,120,135,150,165,180,195,210),expand=c(0,0),labels=c("Mar-1","Mar-15","Apr-1","Apr-15","May-1","May-15","Jun-1","Jun-15","Jul-1","Jul-15","Aug-1")) +
  scale_colour_manual(labels=c("Coastal", "Sierra"),values=c("red", "black"))+ theme_bw() +theme(plot.title=element_text(family="sans",size=14))+
  labs(title=expression(paste(italic("R. boylii")," Start of Oviposition", sep="")),color="Sierra/Coastal")+
  annotate("text", label = "n = 47 records",y = 2002, x = 190,size=5, fontface="italic")
sp 

# different colors

sp2 <- ggplot(aes(x=yday(obs_strt), y=Year),data=rcsn02) + 
  geom_rect(aes(ymin=2002.5,ymax=2003.5, xmin=55,xmax=215),fill="gray90",color="gray90",alpha=0.7,expand=c(0.5,0.5))+
  geom_rect(aes(ymin=2004.5,ymax=2005.5, xmin=55,xmax=215),fill="gray90",color="gray90",alpha=0.7,expand=c(0.5,0.5))+
  geom_rect(aes(ymin=2006.5,ymax=2007.5, xmin=55,xmax=215),fill="gray90",color="gray90",alpha=0.7,expand=c(0.5,0.5))+
  geom_rect(aes(ymin=2008.5,ymax=2009.5, xmin=55,xmax=215),fill="gray90",color="gray90",alpha=0.7,expand=c(0.5,0.5))+
  geom_rect(aes(ymin=2010.5,ymax=2011.5, xmin=55,xmax=215),fill="gray90",color="gray90",alpha=0.7,expand=c(0.5,0.5))+
  geom_rect(aes(ymin=2012.5,ymax=2013.5, xmin=55,xmax=215),fill="gray90",color="gray90",alpha=0.7,expand=c(0.5,0.5))+
  geom_point(aes(x=yday(obs_strt), y=Year, fill=Sierra_Coast),data=rcsn02, pch=21,size=5.5,color="gray50") +
#   geom_point(pch=16,size=3.7) +
#   geom_point(pch=21,bg="gray",size=2.6,show.legend=FALSE) +
  geom_text(aes(label=Site),size=4,hjust=-0.2,vjust=-1,fontface=3,show.legend=FALSE, 
            position=position_jitter(width = 0.4,height=0.4))+
  scale_y_continuous(breaks=seq(2002,2013,1)) +
  ylab("Year") + xlab("")+
  scale_x_continuous(breaks=c(60,75,90,105,120,135,150,165,180,195,210),expand=c(0,0),labels=c("Mar-1","Mar-15","Apr-1","Apr-15","May-1","May-15","Jun-1","Jun-15","Jul-1","Jul-15","Aug-1")) +
  scale_fill_manual(labels=c("Coastal", "Sierra"),values=c("C"="white","S"="black"))+
  theme_bw() +theme(plot.title=element_text(family="sans",size=14))+
  labs(title=expression(paste(italic("R. boylii")," Start of Oviposition", sep="")),color="Sierra/Coastal")+
  annotate("text", label = "n = 47 records",y = 2002, x = 190,size=5, fontface="italic")

sp2 

png("./output/figs/Breeding_notribs_Sierra_Coastal_2002-2013_300dpi.png",width=8,height=6,units="in",res=300)
pdf("./output/figs/Breeding_notribs_Sierra_Coastal_2002-2013.pdf",width=10,height=8,useDingbats=FALSE)
sp2
dev.off()



# 2011-2013 BARPLOT -------------------------------------------------------

rcsn02$YearFac<-as.factor(rcsn02$Year)
rcsnUCD<-rcsn02[rcsn02$obs_strt>as.POSIXct("2011-01-01") & rcsn02$Site=="MFY"|rcsn02$Site=="NFA"|rcsn02$Site=="NFY"|rcsn02$Site=="RUB"|rcsn02$Site=="SFY",]
rcsnUCD<-rcsnUCD[-1,] # remove record from 2009 that is somehow appearing
h(rcsnUCD)

horiz <- ggplot(aes(x=yday(estim_strt), y=Site,color=YearFac,shape=YearFac),data=rcsnUCD) + 
  geom_point(size=5) + 
  # geom_text(aes(label=WYT),color="black",size=4,hjust=-0.2, vjust=-1,fontface=3,show.legend=FALSE)+
  ylab("Site") + xlab("Date")+
  annotate("text",label="(pre-spill)",y="RUB",x=145, color="black", size=4,vjust=-1.5)+
  annotate("text",label="(post-spill)",y="RUB", x=179, color="black", size=4,vjust=-1.5)+
  scale_x_continuous(breaks=c(60,75,90,105,120,135,150,165,180,195,210),labels=c("Mar-1","Mar-15","Apr-1","Apr-15","May-1","May-15","Jun-1","Jun-15","Jul-1","Jul-15","Aug-1")) +
  scale_color_manual(labels=c("2011", "2012","2013"),values=c("blue", "forestgreen","orange"))+ 
  scale_shape_manual(name="Year",labels=c("2011", "2012","2013"),values=c(15,16,17))+
  theme_bw() +theme(plot.title=element_text(family="sans",size=14),legend.key = element_blank())+
  labs(title=expression(paste(italic("R. boylii")," Start of Oviposition", sep="")),color="Year")

horiz #+annotate("text", label = "n = 53 records",y = 2011, x = 190,size=5, fontface="italic")
# ggsave(path="./output/figs/",filename="Rb_Breeding_Init_2011-2013_bySite_Shape.png", width=9, height=6, dpi=200)

