#05_compute_bootstrap_estimates

rm(list = ls())

options(scipen = 999999)

library(furrr)
library(survey)
library(sjlabelled)
library(here)
library(magrittr)
library(tidyverse)

plan(multiprocess)


# Import data ----

# ** main survey ----

## main survey data

data_863_labelled <- read_rds(here("data", "survey_data_863_recoded_01.rds")) %>%
  filter(province == "Barcelona")

## main survey weights 

data_863_weights <- read_rds(here("interim_outputs", "calibration", "main_survey_weights_03.csv"))

# ** bootstrap resamples -----

## bootstrap resamples

bootstrap_resamples <- read_rds(here("interim_outputs", "resamples_863_bcn", str_c("resamples_", 10000, ".rds")))

### untrimmed weights

resamples_calibration_weights <- read_rds(here("interim_outputs", "calibration", "weights_resamples_untrimmed_03.rds"))

## first trimmed weights

resamples_calibration_weights_trimmed <- read_rds(here("interim_outputs", "calibration", "weights_resamples_trimmed_1_03.rds"))

## final trimmed weights

resamples_calibration_weights_trimmed_2 <- read_rds(here("interim_outputs", "calibration", "weights_resamples_trimmed_2_03.rds"))

# ** jackknife resamples ----

# jackknife resamples data




# referendum_participation = recode(as_factor(P82A),
#                                   "No hi vaig participar ni votar perquè no vaig poder (feina, malaltia)" = "couldn't participate",
#                                   "No hi vaig participar ni votar perquè no vaig voler" = "didn't want to participate",
#                                   "No hi vaig participar ni votar perquè m’ho van impedir" = "they prevented resp to participate",
#                                   "Estic segur/a que hi vaig participar i votar" = "voted",
#                                   .default = "DK/NA" )

# note: compute estimates with trimmed and untrimmed weights



# compute actual estimates ----
# plain estimate from weighted survey




# bootstrap percentiles estimates ----
# authors compute: BS Mean - mean of resamples
# BS Median - median of resamples
# BS se - SD of resamples

# confidence intervals: 
# Normal.L95: Actual est. - qnorm(0.975) * SD from bootstrap resamples
# Normal.U95: Actual est. + qnorm(0.975) * SD from bootstrap resamples
# Percentile.L95: 0.025 percentile of bootstrap resamples 
# Percentile.U95: 0.975 percentile of bootstrap resamples 

# bootstrap bca ----

# inputs: 
# * point estimates from jacknife resamples (proportion of vote in jacknife resamples)       
# * point estimates from resamples (proportion of vote in resample)       

