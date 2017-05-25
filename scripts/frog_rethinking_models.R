library(rethinking)
library(lubridate)
library(tidyverse)


# LOAD DAT ----------------------------------------------------------------

# this is unregulated rivers only (NFA and NFY)
load("models/NF_w_lags_data_scaled.rda")
load("models/NF_w_lags_data_unscaled.rda")


# SIMPLE FUNCTIONS --------------------------------------------------------

# custom scale function
cusScale <- function(x){
  (x - mean(x))/sd(x)
}

# unscale the data
unScale <- function(x, y){
  (x * sd(y) + mean(y))
}


# SIMPLE MODEL ---------------------------------------------------------------

# basic static logistic regression model
m1.1b <- map2stan(
  alist(
    breed ~ dbinom( 1 , p ) ,
    logit(p) <- a + bt7mx*temp_7_max + bt30mn*temp_30_min + btCV*temp_CV ,
    a ~ dnorm(0,1),
    c(bt7mx, bt30mn, btCV) ~ dnorm(0,1)
  ), data=d1s, warmup=1000, iter=5000, chains=4, cores=2)

plot(m1.1b)
dev.off()
precis(m1.1b)
plot(precis(m1.1b))

# show density of log-odds
post1b <- extract.samples(m1.1b)
dens( post1b$bt7mx , ylim=c(0,1.5), xlim=c(-1.4,3.4))
text(1.4, 1, "7 day max H20")
dens( post1b$bt30mn, col=rangi2 , lwd=2 , add=TRUE )
text(1.2, 0.9, "30 day min H20", col=rangi2)
dens( post1b$btCV , col="red", add=TRUE)
text(-0.8, 0.9, "CV of daily temp H20", col="red")

# diff between distributions of 30dmn and 7dmx (all really close)
mu.1b.7mx <- logistic(post1b$a + post1b$bt7mx)
mu.1b.30mn <- logistic(post1b$a + post1b$bt30mn)
mu.1b.CV <- logistic(post1b$a + post1b$btCV)
precis( data.frame(mu.1b.CV,mu.1b.7mx, mu.1b.30mn) )

# differences in prob
diff_CV_7mx <- mu.1b.7mx - mu.1b.CV
diff_30mn_7mx <- mu.1b.30mn - mu.1b.7mx
quantile( diff_30mn_7mx , probs=c(0.025,0.5,0.975) )
quantile( diff_CV_7mx , probs=c(0.025,0.5,0.975) )



# VARYING INTERCEPTS ------------------------------------------------------

# now temp + flow, varying Water Year
m2.1a <- map2stan(
  alist(
    breed ~ dbinom( 1 , theta ) ,
    logit(theta) <- a + a_wy[WY] + bt7mx*temp_7_max +
      bt30mn*temp_30_min + bdQ*deltQ + bQCV*Q_CV + blevCV*lev_CV,
    a ~ dnorm(0, 1),
    a_wy[WY] ~ dnorm( 0, sigma_WY ),
    c(a,bt7mx, bt30mn, bdQ, bQCV, blevCV) ~ dnorm(0,1),
    sigma_WY ~ dcauchy(0,1)
  ), 
  data=d1s, warmup=1000, iter=5000, chains=4, cores=2)

# getting divergence warnings here, but trace plots look good and rhat==1

# view output
plot(m2.1a)
dev.off()
pairs(m2.1a)
dev.off()
precis(m2.1a, depth=2, warn = F)
plot(precis(m2.1a, depth=2))

# compare models
compare(m2.1a, m1.1b)

# show density of log-odds
post2a <- extract.samples(m2.1a)
dens( post2a$bt7mx, col="maroon", lty=2, ylim=c(0,1), xlim=c(-4,6)) # running avg 7 day max
dens( post2a$bdQ, col=rangi2 , lwd=2 , add=TRUE ) # delta Flow (daily)
dens( post2a$blevCV, col="darkblue", lty=4, lwd=2 , add=TRUE ) # CV of stage
dens( post2a$bt30mn , col="red", add=TRUE) # running 30 day min



# WHERE TO NOW? -----------------------------------------------------------

# getting residual plots out...how do I do that with binomial data 
# what about all the other potential variables...include more in initial models?
# predictions....how to predict an "event" as 1/0 (on a given date), versus predicting a day of water year?


