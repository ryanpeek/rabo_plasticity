# oviposition plot

library(tidyverse)
library(tidylog)
library(googledrive)
library(googlesheets4)
library(janitor)
library(lubridate)

# Set Up Authorization ----------------------------------------------------

# drive auth
options(gargle_oauth_cache = here::here(".secrets")) 

# if using googledrive to look up file: set drive auth for rapeek
drive_auth(cache = here::here(".secrets"), email="rapeek@ucdavis.edu",
           scopes = c(
             "https://www.googleapis.com/auth/drive.readonly"
             # only if editing/modifying/creating
             #"https://www.googleapis.com/auth/spreadsheets"
           ))

# then use same token (read only)
gs4_auth(token=drive_token())
#gs4_deauth() # to death the token


# GET GOOGLESHEET BY GDRIVE -----------------------------------------------

# TO USE DRIVE FIRST, CHECK FOR NAME AND GET ID
# drive_find(pattern="RABO_oviposition_initiation_summary_CA_2020", type="spreadsheet")
# 
# # Then read in by ID
# wb <- drive_get(id = "1n7Cpf6J7wvGhX4sEuZ7MFOZZ7jh7HrwZC9N8tdu6ZG0") 
# 
# dat_clean <- wb %>% 
#   read_sheet() %>% 
#   janitor::clean_names() %>% 
#   select(-time)

# GET GOOGLESHEET BY GOOGLESHEET DIRECT -----------------------------------

# to read a sheet by URL from webpage
wb <- gs4_get("https://docs.google.com/spreadsheets/d/1n7Cpf6J7wvGhX4sEuZ7MFOZZ7jh7HrwZC9N8tdu6ZG0/edit#gid=1335161501")

dat_clean <- wb %>% 
  read_sheet() %>% 
  janitor::clean_names() %>% 
  select(-time)

# GET NFA ESTIM -----------------------------------------------------------

dat_nfa <- dat_clean %>% filter(site == "NFA", !is.na(obs_strt)) %>% 
  mutate(obs_doy = yday(obs_strt),
         estim_doy = yday(estim_strt), 
         wyt=forcats::fct_relevel(wyt, c("W","BN","D","C")))

# plot
ggplot(data=dat_nfa, aes(y=estim_doy, x=year, shape=trib)) + 
  geom_point(aes(color=wyt),size=4) +
  scale_color_viridis_d() +
  scale_y_continuous(breaks=c(seq(115,180,10)), labels = c("May 1", "May 10", "May 20", "May 30", "Jun 9", "Jun 19", "Jun 29"))+
  geom_smooth(method = "glm") +
  facet_grid(.~trib)

# avg start date:
mean(dat_nfa$estim_doy)
