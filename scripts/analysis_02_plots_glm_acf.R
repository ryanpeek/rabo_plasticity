# Analyze Data for acf and linear trends

# see here:
# https://rpubs.com/markpayne/164550

# Mon May 15 16:52:31 2017 ------------------------------

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
library(caTools)
source("scripts/functions/f_doy.R")

# LOAD DATA ---------------------------------------------------------------

load("data/master_dat_2011-2016.rda") 
load("data/flow_dv_cfs_2011_6sites.rda")
load("data/flow_dv_cfs_full_6sites.rda")

# DATA PREP ---------------------------------------------------------------

# site lengths for EM/km adj
sites<- tibble("site"=c("RUB","NFA"," NFA-RR", "MFA","MFA-AMC", "SFY", "NFY", "MFY"), "len_km"=c(0.5, 0.57, 0.11, 0.5, 0.2, 0.5, 0.8, 0.8))


df <- master_df
# df <- master_df %>% filter(site!="MFY")
df <- master_df %>% filter(month(date)>3 & month(date)<8)

# 2 NAs in dataset from 30 day avgs
df[!is.na(df$missData) & df$site=="RUB" & df$date==ymd("2011-05-26"),]$temp_30_max<-10.99
df[!is.na(df$missData) & df$site=="RUB" & df$date==ymd("2011-05-26"),]$temp_30_min<-9.7

rabo <- df[!is.na(df$missData),] %>% arrange(date)



# PLOTS: WTEMP vs DOWY ------------------------------------------------

# WTemp: 7-Day Avg w threshold
ggplot() + 
  geom_line(data=df, aes(x=DOY, y=temp_7_avg, color=as.factor(WY),
                         group=WY), show.legend = F) + 
  geom_point(data=df[!is.na(df$missData),], aes(x=DOY, y=temp_7_avg,
                                               fill=as.factor(WY)),
             show.legend = T, pch=21, color="gray20", size=4) + 
  scale_color_viridis(discrete = T, 
                      guide = guide_legend(title = "Water Year")) +
  scale_fill_viridis(discrete = T, 
                     guide = guide_legend(title = "Water Year")) +
  
  scale_x_continuous(breaks=c(105,120,135,150,165,180,195,210),
                     labels=c("Apr-15","May-1","May-15","Jun-1",
                              "Jun-15","Jul-1","Jul-15","Aug-1")) +
  scale_y_continuous(limits=c(0,27), breaks=seq(0,27,3)) + 
  geom_hline(yintercept = 10, color="maroon", lty=2)+
  #geom_hline(yintercept = 11, color="orange", lty=2)+
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1),
                     legend.key=element_blank()) + 
  labs(y=expression(paste("Water Temperature (",degree,"C)")), 
       title="7 Day Average Water Temperature", x="") +
  facet_grid(site~.)#, scales="free_x")

#ggsave(filename = "figs/watertemp7_breeding_10C.png", width = 9, height = 6, units = "in")


# LINEAR TREND ANALYSIS ---------------------------------------------------

ggplot() + geom_point(data=df, aes(x=date, y=totalEM, fill=site), pch=21, size=4) + 
  scale_color_viridis(discrete = T)

rabo$missData <- as.factor(rabo$missData)

mod1 <- lm(totalEM ~ Q_CV, data=rabo)
summary(mod1)

par(mfrow=c(2,2))
plot(mod1)

# The Tukey-Anscombe plot (top-left) suggests that the variance of the residuals is "relatively" , as does the scale-location plot (bottom-left). The QQ plot looks ok-ish. There don’t appear to be any bad outliers (bottom-right) How does the time series of residuals look?

par(mfrow=c(1,1))
plot(residuals(mod1), type="b", pch=21, bg="gray")
abline(h=0, lty=3)

acf(residuals(mod1))


# LINEAR TREND ANALYSIS W AUTOCORR ----------------------------------------

library(nlme)
mdl.ac <- gls(totalEM ~ Q_CV, data=rabo, 
              correlation = corAR1(form=~Q_CV),
              na.action=na.omit)
summary(mdl.ac)
coef(mdl.ac)
coef(mod1)

# tukey anscombe
plot(fitted(mdl.ac),residuals(mdl.ac))
abline(h=0,lty=3)

# qqplot
qqnorm(mdl.ac)

acf(residuals(mdl.ac,type="p"))


# PICK BEST MOD -----------------------------------------------------------

library(MuMIn)
model.sel(mod1,mdl.ac)



# LASSO glm ---------------------------------------------------------------
library(glmnet)


# add id and site to rownames:
row.names(rabo) <- paste0(rabo$site, "-", row.names(rabo))
rabo.df <- 

x <- model.matrix(site~., data=rabo)
x <- x[,-1]

fit = glmnet(x=x, y=mtcars$mpg)
plot(fit, label=T)
print(fit)
coef(fit,s=0.1)



data(mtcars)

x<-model.matrix(mpg~.,data=mtcars)
x=x[,-1]

fit = glmnet(x=x, y=mtcars$mpg)
plot(fit, label=T)
print(fit)
coef(fit,s=0.1)

nx = matrix(rnorm(32*10),32,10)
predict(fit,newx=nx,s=c(0.1,0.05))

cvfit <- cv.glmnet(x=x, y=mtcars$mpg)
plot(cvfit)
cvfit$lambda.min

# lamda.min is the value of λλ that gives minimum mean cross-validated error. The other λλ saved is lambda.1se, which gives the most regularized model such that error is within one standard error of the minimum. To use that, we only need to replace lambda.min with lambda.1se above


coef(cvfit, s = "lambda.min")

predict(cvfit, newx = x[1:5,], s = "lambda.min")




