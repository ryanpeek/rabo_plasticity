# Analyze Data

# Wed Apr 26 16:06:59 2017 ------------------------------

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

library(tidyverse)
library(ggrepel)
library(viridis)

# LOAD DATA ---------------------------------------------------------------

load("data/master_dat_2011-2016.rda")


# PLOTS -------------------------------------------------------------------

master_df <- master_df %>% filter(site!="MFY", site!="MFA")

# WTemp: 7-Day Avg w threshold
ggplot() + 
  geom_line(data=master_df, 
            aes(x=date, y=temp_7_avg, color=site, group=WY)) +
  geom_ribbon(data=master_df, aes(x=date, ymin=10,ymax=12), fill="orange", alpha=0.4) +
  geom_point(data=master_df[!is.na(master_df$totalEM),], aes(x=date, y=temp_7_avg, fill=site), 
             pch=21,color="gray20",size=4) + 
  facet_grid(site~WY, scales="free_x")

# Stage: 7-day avg
ggplot() + 
  geom_line(data=master_df, 
            aes(x=date, y=lev_7_avg, color=site, group=WY)) +
  geom_point(data=master_df[!is.na(master_df$totalEM),], aes(x=date, y=lev_7_avg, fill=site), 
             pch=21,color="gray20",size=4) + 
  facet_grid(site~WY, scales="free")

# Stage vs. Temp: 7-day avg
ggplot() + 
  geom_point(data=master_df[!is.na(master_df$totalEM),],
            aes(x=temp_7_avg, y=lev_7_avg, shape=site),
            size=4.1)  +
  geom_point(data=master_df[!is.na(master_df$totalEM),], 
             aes(x=temp_7_avg, y=lev_7_avg, shape=site, color=as.factor(WY)),
             size=4)  + 
  scale_color_viridis(discrete = T, option = "D")
  


# PCA ---------------------------------------------------------------------

# get only the breeding points
master_breed <- filter(master_df, !is.na(master_df$totalEM)) %>% 
  select(-site, -DOY, -DOWY, -date, -station, -missData, -REG, -obs_strt, -CDEC_air_C, -CDEC_air_7_avg, -totalEM) %>% as.data.frame

# PCA
prin_comp <- prcomp(master_breed)
prin_comp$center

# rotation: The rotation measure provides the principal component loading. Each column of rotation matrix contains the principal component loading vector. This is the most important measure we should be interested in
prin_comp$rotation

# summary of components
summary(prin_comp) # so PC1 : PC3 explain 83% of the total variance in the data

# loadings per variable PC1
prin_comp$rotation[,1:3] %>% as.data.frame %>%
  mutate(vars = row.names(prin_comp$rotation)) %>% 
  select(vars, PC1) %>% arrange(PC1)

# loadings per variable PC2 & PC3
prin_comp$rotation[,2:3] %>% as.data.frame %>%
  mutate(vars = row.names(prin_comp$rotation)) %>% 
  select(vars, PC2) %>% arrange(PC2)

# loadings per variable PC3
prin_comp$rotation[,1:3] %>% as.data.frame %>%
  mutate(vars = row.names(prin_comp$rotation)) %>% 
  select(vars, PC3) %>% arrange(PC3)


# plot
biplot(prin_comp, choices = 1:2, scale = 0, pc.biplot = F, 
       xlabs=rep(".", nrow(master_breed)), col="blue")
biplot(prin_comp, choices = 2:3, scale = 0, pc.biplot = F, xlabs=rep(".", nrow(master_breed)), 
       col="black")

# a quad plot of results

# THE FUNCTION:
pcaCharts <- function(x) {
  x.var <- x$sdev ^ 2
  x.pvar <- x.var/sum(x.var)
  print("proportions of variance:")
  print(x.pvar)
  
  par(mfrow=c(2,2))
  plot(x.pvar,xlab="Principal component", ylab="Proportion of variance explained", ylim=c(0,1), type='b')
  plot(cumsum(x.pvar),xlab="Principal component", ylab="Cumulative Proportion of variance explained", ylim=c(0,1), type='b')
  screeplot(x)
  screeplot(x,type="l")
  par(mfrow=c(1,1))
}

# plot
pcaCharts(prin_comp)


# PLOTTING PCA ------------------------------------------------------------

library(ggbiplot)

g <- ggbiplot(prin_comp, obs.scale = 1, var.scale = 1,
              ellipse = TRUE, 
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)


pcaCharts(prin_comp)

# now switch off scaling factors (var.scale)
g <- ggbiplot(prin_comp, choices = 1:2, scale = 0, var.scale = 0, labels= prin_comp$site, groups = prin_comp$site,
              ellipse = TRUE, 
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)


