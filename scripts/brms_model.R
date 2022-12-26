# BRMS model of breeding timing:


# Packages ----------------------------------------------------------------

library(tidyverse)
library(ggthemes)
library(lubridate)
library(here)
library(modelr)
library(brms)
library(rstanarm)
library(broom)
library(tidybayes)


# Load Data ---------------------------------------------------------------

load("data/master_dat_2011-2016.rda") 
load("data/flow_dv_cfs_2011_6sites.rda") # updated and merged flows:

# filter to period of interest: breeding and rearing period for only unreg sites:
df <- master_df %>% filter(month(date)>2 & month(date)<9) %>% 
  #filter(site %in% c("NFA", "NFY")) %>% 
  mutate(M_DAY = mday(date),
         WK = week(date),
         OVIPOSITION = ifelse(is.na(missData), 0, 1),
         "rowID" = as.integer(row.names(.))) %>% 
  select(site, date, REG, DOY, WY, DOWY, M_DAY, everything())

summary(df)


# visualize:
ggplot() + 
  geom_line(data=df, aes(x=DOWY, y=lev_avg, color=site, group=site), alpha=0.9) +
  facet_grid(WY~.) + 
  #scale_fill_colorblind("Site", labels=c("NFA", "NFY"))+
  #scale_color_colorblind("Site", labels=c("NFA", "NFY"))+
  scale_x_continuous(breaks=c(152,183,213,244,274,305, 335),
                     labels=c("Mar-01","Apr-01","May-01","Jun-01",
                              "Jul-01","Aug-01","Sep-01")) +
  theme_bw() + xlab("")

library(viridis)

# quick ribbon plot:
ggplot() + 
  geom_ribbon(data=df, aes(x=DOWY, ymin=0, ymax=Q_cfs, 
                           fill=site, group=site), alpha=0.9) +
  #scale_fill_viridis_d("Site", labels=c("NFA", "NFY"), option = "E")+
  scale_x_continuous(breaks=c(152,183,213,244,274,305, 335),
                     labels=c("Mar-01","Apr-01","May-01",
                              "Jun-01","Jul-01","Aug-01","Sep-01")) +
  theme_bw() + xlab("DOWY") + ylab("Flow (cfs)") + 
  geom_point(data=df, 
             aes(x=DOWY, y=ifelse(OVIPOSITION==0, NA, Q_cfs), 
                 group=site), pch=21, fill="#7C7B78FF", 
             alpha=0.9, size=3.5) + 
  facet_grid(WY~., scales = "free_y")

# ggsave(filename = "figs/rabo_spawning_nfa-nfy_2011-2016.pdf", 
       # width = 11, height = 8.5, dpi = 300, units = "in")  

# lineplot
ggplot() + 
  geom_line(data=df, aes(x=DOWY, y=Q_cfs, color=site, group=site), alpha=0.9) +
  #scale_color_colorblind("Site", labels=c("NFA", "NFY"))+
  scale_x_continuous(breaks=c(152,183,213,244,274,305, 335),
                     labels=c("Mar-01","Apr-01","May-01",
                              "Jun-01","Jul-01","Aug-01","Sep-01")) +
  theme_bw() + xlab("DOWY") + ylab("Flow (cms)") + 
  geom_point(data=df, 
             aes(x=DOWY, y=ifelse(OVIPOSITION==0, NA, Q_cfs), 
                 group=site), pch=21, fill="#7C7B78FF", 
             alpha=0.9, size=3.5) + 
  facet_grid(WY~., scales = "free_y")

# ggsave(filename = "figs/rabo_spawning_nfa-nfy_2011-2016_lines.pdf", 
#        width = 11, height = 8.5, dpi = 300, units = "in")  

# thermohydrograph
ggplot() + 
  geom_linerange(data=df[df$site=="NFA",], aes(x=DOWY, ymin=0, ymax=Q_cfs, 
                           color=temp_avg, group=site), size=1.5, alpha=0.8) +
  geom_line(data=df[df$site=="NFA",], aes(x=DOWY, y=Q_cfs), size=0.5, alpha=1) +
  scale_colour_gradientn("Water \nTemp (C)",
                         colours=viridis(33, option="A"),
                         breaks=seq(0,33,3), limits=c(0,33)) + 
  scale_x_continuous(breaks=c(152,183,213,244,274,305, 335),
                     labels=c("Mar-01","Apr-01","May-01",
                              "Jun-01","Jul-01","Aug-01","Sep-01")) +
  theme_bw() + xlab("DOWY") + ylab("Flow (cfs)") + 
  geom_point(data=df[df$site=="NFA",], 
             aes(x=DOWY, y=ifelse(OVIPOSITION==0, NA, Q_cfs), 
                 group=site), pch=21, fill="yellow2",#fill="#7C7B78FF", 
             alpha=0.9, size=3.5) + 
  facet_grid(WY~., scales = "free_y")


# For Interpolations of Data ----------------------------------------------

# Fix NFA
nfa14 <- filter(df, site=="NFA", WY==2014)
nfa14$temp_avg_interp <- zoo::na.spline(nfa14$temp_avg)
plot(nfa14$temp_avg_interp, type="l")
nfa14 <- filter(nfa14, is.na(temp_avg))

# get only the NFY for 2013 to check:
nfy13 <- filter(df, site=="NFY", WY==2013) %>% select(site, date, WY, DOWY, Q_cfs, temp_avg)
nfy14 <- filter(df, site=="NFY", WY==2014) %>% select(site, date, WY, DOWY, Q_cfs, temp_avg)

# missing values, use simple avg to interpolate
nfy13$Q_cfs_approx <- zoo::na.approx(nfy13$Q_cfs)
# or NA spline
nfy13$Q_cfs_spline <- zoo::na.spline(nfy13$Q_cfs)
nfy13$temp_avg_interp <- zoo::na.spline(nfy13$temp_avg)
nfy14$temp_avg_interp <- zoo::na.spline(nfy14$temp_avg)

# plot to check
ggplot(data=nfy13) + 
  geom_line(aes(x=date, y=Q_cfs, color=temp_avg), lwd=2) +
  geom_line(aes(x=date, y=Q_cfs_approx, color=temp_avg_interp), alpha=0.7, lwd=1.2) + 
  #geom_line(aes(x=date, y=Q_cfs_spline), col="orange", alpha=0.7)+
  scale_color_viridis()

# add back to df:
df2 <- df %>% 
  mutate(
    temp_avg=ifelse((site=="NFA" & WY==2014 & is.na(temp_avg)), 
                    nfa14$temp_avg_interp, df$temp_avg),
    Q_cfs = ifelse((site=="NFY" & WY==2013), nfy13$Q_cfs_spline, df$Q_cfs),
    temp_avg2=ifelse((site=="NFY" & WY==2013), nfy13$temp_avg_interp, df$temp_avg),
    temp_avg3=ifelse((site=="NFY" & WY==2014), nfy14$temp_avg_interp, df$temp_avg))


# PLOTS -------------------------------------------------------------------

# NFA
ggplot() + 
  geom_linerange(data=df2[df2$site=="NFA",], aes(x=DOWY, ymin=0, ymax=Q_cfs, 
                                                 color=temp_avg, group=site), size=1.5, alpha=0.8) +
  geom_line(data=df2[df2$site=="NFA",], aes(x=DOWY, y=Q_cfs), size=0.5, alpha=1) +
  scale_colour_gradientn("Water \nTemp (C)",
                         colours=viridis(33, option="A"),
                         breaks=seq(0,33,3), limits=c(0,33)) + 
  scale_x_continuous(breaks=c(152,183,213,244,274,305, 335),
                     labels=c("Mar-01","Apr-01","May-01",
                              "Jun-01","Jul-01","Aug-01","Sep-01")) +
  theme_bw() + xlab("") + ylab("Flow (cms)") + labs(subtitle="NF American: Foothill Yellow-legged Frog Spawning Initiation") +
  geom_point(data=df2[df2$site=="NFA",], 
             aes(x=DOWY, y=ifelse(OVIPOSITION==0, NA, Q_cfs), 
                 group=site), pch=21, fill="yellow2",#fill="#7C7B78FF", 
             alpha=0.9, size=5) + 
  facet_grid(WY~., scales = "free_y")

ggsave(filename = "figs/rabo_spawning_nfa_2011-2016_thermohydro.png", width = 11, height = 8.5, dpi = 300, units = "in")

# NFY
ggplot() + 
  geom_linerange(data=df2[df2$site=="NFY",], aes(x=DOWY, ymin=0, ymax=Q_cfs, 
                                                 color=temp_avg3, group=site), size=1.5, alpha=0.8) +
  geom_line(data=df2[df2$site=="NFY",], aes(x=DOWY, y=Q_cfs), size=0.5, alpha=1) +
  scale_colour_gradientn("Water \nTemp (C)",
                         colours=viridis(33, option="A"),
                         breaks=seq(0,33,3), limits=c(0,33)) + 
  scale_x_continuous(breaks=c(152,183,213,244,274,305, 335),
                     labels=c("Mar-01","Apr-01","May-01",
                              "Jun-01","Jul-01","Aug-01","Sep-01")) +
  theme_bw() + xlab("") + ylab("Flow (cms)") + labs(subtitle="NF Yuba: Foothill Yellow-legged Frog Spawning Initiation") +
  geom_point(data=df2[df2$site=="NFY",], 
             aes(x=DOWY, y=ifelse(OVIPOSITION==0, NA, Q_cfs), 
                 group=site), pch=21, fill="yellow2",#fill="#7C7B78FF", 
             alpha=0.9, size=5) + 
  facet_grid(WY~., scales = "free_y")
ggsave(filename = "figs/rabo_spawning_nfy_2011-2016_thermohydro.png", width = 11, height = 8.5, dpi = 300, units = "in")


# Summary and Export ------------------------------------------------------
df_out <- df2 %>% select(site:temp_CV, lev_7_avg, temp_7_avg:temp_30_min, days_no_ppt:totalEM, Q_cfs:Q_7_CV, len_km, EM_per_km, OVIPOSITION)
naniar::gg_miss_var(df_out)

#xport
library(fs)
fs::dir_create("data_clean")
write_csv(df_out, file = "data_clean/breeding_temp_flow_2011-2016_sierras.csv")


# TRIM DATA FOR MOD -------------------------------------------------------

# test with one site across multiple years
mod1 <- df %>% filter(site=="NFA") %>% 
  select(DOWY, WY, site, Q_cfs, OVIPOSITION, WK, temp_avg, temp_7_avg, temp_7_max, temp_7_min, W_air_avg, W_air_min, W_humidity_avg, Index, Q_CV, deltQ)

# check for NAs
summary(mod1)

# filter
mod1 <- filter(mod1, !is.na(W_air_avg)) %>% 
  select(site, OVIPOSITION, WY, everything()) %>% 
  mutate_at(c("WY","site"), as.factor)

summary(mod1)


# standardize: (var - mean(var))/(sd(var))
scale2 <- function(x, na.rm = FALSE) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)

mod1_s <- mod1 %>%  
  mutate_at(c(4:16), scale2) %>% 
  select(-site)
summary(mod1_s)


# BRMS Model --------------------------------------------------------------

# with time spline

bm1 <- brm(formula = WK ~ OVIPOSITION + Q_cfs + temp_7_avg + Index + Q_CV + 
             W_humidity_avg + (1|WY) + s(WK, bs="cc", k=26),
           data = mod1_s,
           family = gaussian(),
           chains = 2,
           cores = 2,
           iter = 4000)

summary(bm1)
plot(bm1)

library(sjPlot)
library(sjstats)
library(bayesplot)
library(ggstance)
library(ggeffects)

# neff ratio
neff_ratio(bm1) %>% 
  mcmc_neff() + labs(title="Breeding: scaled")+
  theme_fivethirtyeight()

# check
pp_check(bm1, nsamples = 100) + labs(title="Model 1")

# coeff plot
sjPlot::plot_model(bm1, type = "est") + labs(title="Model 1")

# ROPE test
sjstats::equi_test(bm1, out="plot")

# RSTANARM ----------------------------------------------------------------

# set prior to student T (similar to cauchy)
t_prior <- student_t(df = 7, location = 0, scale = 2.5)


# varying intercept only
fit1 <- stan_glmer(formula = OVIPOSITION ~ 
                     DOWY + Q_cfs + WK + temp_avg + temp_7_avg + 
                     temp_7_max + temp_7_min + W_air_avg + W_air_min +
                     W_humidity_avg + Index + Q_CV + deltQ + (1|WY),
                   data = mod1_s,
                   family = binomial(link = "logit"), 
                   # QR = TRUE,
                   prior = t_prior, prior_intercept = t_prior,
                   chains=4, cores=2, seed=3232)
#adapt_delta=0.99 if divergent

summary(fit1)
plot(fit1)

model.results <- tidy(fit1) %>% 
  mutate_if(is.numeric, list(~round(., 2))) %>% 
  arrange(estimate)

model.results

# extract posterior
post1 <- as.array(fit1)

# traceplot
bayesplot::mcmc_trace(post1, facet_args = list(ncol = 2, strip.position = "left"))

library(ggeffects)

fit1_dat <- ggpredict(fit1, terms = c("temp_7_avg", "W_air_avg"))
plot(fit1_dat)
