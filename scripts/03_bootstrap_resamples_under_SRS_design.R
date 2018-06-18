# 03_bootstrap_resamples_under_SRS_design

rm(list = ls())

options(scipen = 999999)

library(foreach)
library(doParallel)
library(sjlabelled)
library(here)
library(magrittr)
library(tidyverse)

n_bootstrap_resamples <- 10000

# Register parallel backend ----

cl <- makeCluster(4)
registerDoParallel(cl)

getDoParWorkers()
getDoParName()

set.seed(4)


# Import objects ----

## datasets

data_863_labelled <- read_rds(here("data", "survey_data_863_recoded_01.rds")) %>%
  filter(province == "Barcelona")

# Data management ----

data_for_bootstrap_resamples <- data_863_labelled %$% 
  ORDRE_CINE %>%
  as.numeric()

# Resample ----

if(!file.exists(here("interim_outputs", "resamples_863_bcn", str_c("resamples_", n_bootstrap_resamples, "_SRSdesign",".rds") ))){

resamples <- list()

resamples <- foreach(icount(n_bootstrap_resamples))%dopar%{
  
  sample(data_for_bootstrap_resamples, replace = T)
  
}

write_rds(here("interim_outputs", "resamples_863_bcn", str_c("resamples_", n_bootstrap_resamples, "_SRSdesign",".rds") ))

}else{
  
  resamples <- read_rds(here("interim_outputs", "resamples_863_bcn", str_c("resamples_", n_bootstrap_resamples, "_SRSdesign",".rds") ))
  
}

# Checks ---- 

## Explore the number of times each individual appeared in resamples
### Doesn't look like there are anomalies at first glance.    

resamples %>%
  unlist %>%
  table() %>%
  sort(decreasing = T) %>%
  head(20)

data_frame(resamples_ = resamples %>%
             unlist() %>%
             table() %>%
             as.numeric()) %>%
  ggplot(aes(x = resamples_)) +
  geom_density()  

