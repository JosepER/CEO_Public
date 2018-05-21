# 04_jackknife estimations

# Delete note:
# the output should be the 968 jackknife resamples and their 968 weights.

# TO DO: go back to 03 and trim weights of main survey? I'm gonna need estimates from that survey.

rm(list = ls())

options(scipen = 999999)

library(survey)
library(sjlabelled)
library(here)
library(magrittr)
library(tidyverse)

# Import data ----

## survey dataset

data_863_labelled <- read_rds(here("data", "survey_data_863_recoded_01.rds")) %>%
  filter(province == "Barcelona")

## population proportions for calibration

first_language_province <- read_csv(here("interim_outputs", "calibration", "first_language_calibration_proportions_03.csv"))

place_of_birth <- read_csv(here("interim_outputs", "calibration", "place_of_birth_proportions_03.csv"))

# Data management ----

# ** subset main survey dataset with calibration variables ----

data_863_for_calibration <- data_863_labelled %>%
  select(ORDRE_CINE, first_language_calibration, age_place_of_birth_calibration)

# ** create jackknife resamples 

data_863_jackknife_resamples_list <- list()

for(i in 1:nrow(data_863_for_calibration)){

  data_863_jackknife_resamples_list[[i]] <- data_863_for_calibration[-i,]
  
}

# ** compute frequencies for calibration ----

first_language_calibration_survey_863 <- first_language_province %>%
  mutate(Freq = prop * (nrow(data_863_for_calibration)-1)) %>%
  select(-prop)

place_of_birth_survey_863 <- place_of_birth %>%
  mutate(Freq = prop * (nrow(data_863_for_calibration)-1)) %>%
  select(-prop)

rm(first_language_province, place_of_birth)


# Calibrate jackknife resamples ----

# ** compute survey designs

data_863_jackknife_survey_designs_list <-  data_863_jackknife_resamples_list %>%
  map(~ svydesign(ids = ~ 0, data = .x))



# ** apply calibration

raked_data_863 <- rake(survey_design_data_863, 
                       sample.margins = list(~first_language_calibration_survey_863, ~place_of_birth_survey_863),
                       population = list(first_language_calibration_survey_863, place_of_birth_survey_863),
                       control = list(maxit = 30, epsilon = 1))

interim_outputs/resample_survey_desings_863_bcn/survey_designs_03.rds
interim_outputs/resample_survey_designs_863_bcn/survey_designs_03.rds
