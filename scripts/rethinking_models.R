#library(tidyverse)
library(lubridate)
library(dplyr)

load("data/master_dat_2011-2016.rda") 
load("data/flow_dv_cfs_2011_6sites.rda") # updated and merged flows:

df <- master_df
# df <- master_df %>% filter(site!="MFY")
# df <- master_df %>% filter(month(date)>2 & month(date)<8 & !site=="MFA")
# df <- master_df %>% filter(month(date)>3 & month(date)<8 & site=="NFA" | site=="NFY")


# 2 NAs in dataset from 30 day avgs
df[!is.na(df$missData) & df$site=="RUB" & df$date==ymd("2011-05-26"),]$temp_30_max<-10.99
df[!is.na(df$missData) & df$site=="RUB" & df$date==ymd("2011-05-26"),]$temp_30_min<-9.7

rabo <- df[!is.na(df$missData),] %>% arrange(date)

# add unique rownames based on siteID
rabo <- rabo %>%
  mutate("siteID" = paste0(site, "-", row.names(.))) %>% 
  column_to_rownames(var = "siteID")  %>% as.data.frame
#names(rabo)
#rownames(rabo)

# create column with date for 15 and 30 day lags (for binomial logistic)
rabo_lag <- rabo %>% 
  mutate(d07 = date - 7, 
         d14 = date - 14,
         d21 = date - 21,
         d30 = date - 30) %>% 
  select(site, date, WY, d07:d30)

# rejoin / filter form orig dataset

rabo_bin7<- inner_join(df, rabo_lag, by=c("site"="site", "WY"="WY", "date"="d07")) %>% select(-c(date.y, d14:d30)) %>% mutate(lagEM="d07")
rabo_bin14 <- inner_join(df, rabo_lag, by=c("site"="site", "WY"="WY", "date"="d14")) %>% select(-c(date.y, d07:d30)) %>% mutate(lagEM="d14")
rabo_bin21 <- inner_join(df, rabo_lag, by=c("site"="site", "WY"="WY", "date"="d21")) %>% select(-c(date.y, d07:d30)) %>% mutate(lagEM="d21")
rabo_bin30 <- inner_join(df, rabo_lag, by=c("site"="site", "WY"="WY", "date"="d30")) %>% select(-c(date.y, d07:d21)) %>% mutate(lagEM="d30")

# bind up for one dataset of lags, add 1/0 col for breeding
rabo_LAGS<-rbind(rabo_bin7, rabo_bin14, rabo_bin21, rabo_bin30) %>% 
  mutate(breed = 0)
rm(list=ls(pattern = "rabo_bin*"))

# add breed col to orig dataset:
rabo <- rabo %>% mutate(breed = 1, lagEM="d00")

# make final logistic dataset, add 1/0 column
rabo_logist <- rbind(rabo_LAGS, rabo)




# RYANS MODELS ------------------------------------------------------------
library(rethinking)

# Filter down data, use only unreg sites
d1 <- dplyr::select(rabo_logist, site, breed, lagEM, DOWY, everything(), -date, -EM_per_km, -REG,
                    -(obs_strt:apr_jul), -len_km, -starts_with("CDEC"), 
                    -station, -WYsum, -lev_7_avg, -Q_cfs, 
                    -Q_min, -Q_7_cfs) %>% 
  filter(!site=="MFA") %>% 
  #    filter(site=="NFY" | site=="NFA") %>%
  as.data.frame

# RM NAs, add id and site to rownames:
d1 <- d1 %>% filter(!is.na(temp_30_min))
d1_rownames<- paste0(d1$site, "-",d1$WY, "-", seq(1:nrow(d1)))
d1$site <- as.factor(d1$site)
d1$lagEM <- as.integer(as.factor(d1$lagEM))

# custom scale function
cusScale <- function(x){
  (x - mean(x))/sd(x)
}

# scale all data and rebind site and breed info:
d1s <- d1
d1s[,c(4:31)] <- apply(d1[,c(4:31)], 2, cusScale)
d1s$site <- as.integer(d1s$site)





# varying intercepts
d1.2 <- select(d1s, breed, site, lagEM, temp_7_max, Index, deltLev, Q_CV, deltQ,
               temp_CV, DOY, W_air_7_avg) %>% rename(wy_index=Index)

#bdeltLev*deltLev + bQCV*Q_CV + bdeltQ*deltQ + btCV*temp_CV +bDOY*DOY + bAir7*W_air_7_avg,
#      c(bLAG, bt7, bwyi, bdeltLev, bQCV, bdeltQ, btCV, bDOY, bAir7) ~ dnorm(0,5)

# now add other stuff
m1.2 <- map2stan(
  alist(
    breed ~ dbinom( 1 , p ) ,
    logit(p) <- a + a_site[site] + bLAG*lagEM + bt7mx*temp_7_max + bwyi*wy_index + 
      a ~ dnorm(0,5),
    a_site[site] ~ dnorm(0, sigma_site),
    sigma_site ~ dcauchy(0,1),
    c(bLAG, bt7, bwyi) ~ dnorm(0,5)
  ),
  data=d1.2, warmup=1000, iter=5000, chains=1, cores=1)

precis(m10.2)
logistic(c(1.63, 0.96,-.94, -1.01, 0.80)) # about a 20% chance of breeding based 

m10.2stan <- map2stan( m10.2 , data=d.1s , iter=1e4 , warmup=1000 )
precis(m10.2stan)
plot(m10.2stan)
plot(precis(m10.2stan))


# BOOK MODS ---------------------------------------------------------------

data(chimpanzees)
d <- chimpanzees
d$recipient <- NULL


m12.4 <- map2stan(
  alist(
    # get rid of NAs
    pulled_left ~ dbinom( 1 , p ) ,
    logit(p) <- a + a_actor[actor] + (bp + bpC*condition)*prosoc_left ,
    a_actor[actor] ~ dnorm( 0 , sigma_actor ),
    a ~ dnorm(0,10),
    bp ~ dnorm(0,10),
    bpC ~ dnorm(0,10),
    sigma_actor ~ dcauchy(0,1)
  ),
  data=d , warmup=1000 , iter=5000 , chains=4 , cores=3 )
precis(m12.4)
plot(m12.4)


post <- extract.samples(m12.4)
total_a_actor <- sapply( 1:7 , function(actor) post$a + post$a_actor[,actor] )
round( apply(total_a_actor,2,mean) , 2 )


d$block_id <- d$block  # name 'block' is reserved by Stan


m12.5 <- map2stan(
  alist(
    pulled_left ~ dbinom( 1 , p ),
    logit(p) <- a + a_actor[actor] + a_blockID[block_id] +
      (bp + bpc*condition)*prosoc_left,
    a_actor[actor] ~ dnorm( 0 , sigma_actor ),
    a_blockID[block_id] ~ dnorm( 0 , sigma_blockID ),
    c(a,bp,bpc) ~ dnorm(0,10),
    sigma_actor ~ dcauchy(0,1),
    sigma_blockID ~ dcauchy(0,1)
  ),
  data=d, warmup=1000 , iter=6000 , chains=4 , cores=3 )

plot(m12.5)

precis(m12.5,depth=2) # depth=2 displays varying effects
plot(precis(m12.5,depth=2)) # also plot

post <- extract.samples(m12.5)
dens( post$sigma_block , xlab="sigma" , xlim=c(0,4) )
dens( post$sigma_actor , col=rangi2 , lwd=2 , add=TRUE )
text( 2 , 0.85 , "actor" , col=rangi2 )
text( 0.75 , 2 , "block" )
