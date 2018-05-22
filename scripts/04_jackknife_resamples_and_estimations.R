# 04_jackknife estimations

# Delete note:
# the output should be the 968 jackknife resamples and their 968 weights.

rm(list = ls())

options(scipen = 999999)

library(tictoc)
library(furrr)
library(survey)
library(sjlabelled)
library(here)
library(magrittr)
library(tidyverse)

plan(multiprocess)


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

first_language_calibration_jackknife_resamples <- first_language_province %>%
  mutate(Freq = prop/100 * (nrow(data_863_for_calibration)-1)) %>%
  select(-prop)

place_of_birth_jackknife_resamples <- place_of_birth %>%
  mutate(Freq = prop * (nrow(data_863_for_calibration)-1)) %>%
  select(-prop)

rm(first_language_province, place_of_birth)

# test sum of calibration frequencies

test_freq_1 <- isTRUE(all.equal(first_language_calibration_jackknife_resamples$Freq %>% sum(), (nrow(data_863_for_calibration)-1)))

test_freq_2 <- isTRUE(all.equal(place_of_birth_jackknife_resamples$Freq %>% sum(), (nrow(data_863_for_calibration)-1)))

if(!all(test_freq_1, test_freq_2)){
  stop("Failed test: frequencies should of calibration objects should add to rows in jackknife resamples.")
}

rm(test_freq_1, test_freq_2, i)

# Calibrate jackknife resamples ----

# ** compute survey designs

if(!file.exists(here("interim_outputs", "jackknife", "jackknife_survey_designs.rds"))){

data_863_jackknife_survey_designs_list <-  data_863_jackknife_resamples_list %>%
  map(~ svydesign(ids = ~ 0, data = .x))

data_863_jackknife_survey_designs_list %>%
  write_rds(here("interim_outputs", "jackknife", "jackknife_survey_designs.rds"))

}else{
  
  data_863_jackknife_survey_designs_list <- read_rds(here("interim_outputs", "jackknife", "jackknife_survey_designs.rds"))
  
}

# ** apply calibration

if(!file.exists(here("interim_outputs", "jackknife", "jackknife_raked_resamples.rds"))){

data_863_jackknife_raked_list <- data_863_jackknife_survey_designs_list %>%
  future_map(~ rake(.x,
             sample.margins = list(~first_language_calibration, ~age_place_of_birth_calibration),
             population = list(first_language_calibration_jackknife_resamples, place_of_birth_jackknife_resamples),
             control = list(maxit = 30, epsilon = 1)))

data_863_jackknife_raked_list %>%
  write_rds(here("interim_outputs", "jackknife", "jackknife_raked_resamples.rds"))

}else{data_863_jackknife_raked_list <- read_rds(here("interim_outputs", "jackknife", "jackknife_raked_resamples.rds"))}

rm(data_863_jackknife_survey_designs_list)

## ** retrieve weights

data_863_jackknife_weights_list <- data_863_jackknife_raked_list %>%
  map(~weights(.x))

rm(data_863_jackknife_raked_list)

## ** check weighted jackknife resamples ----

## first language

first_language_calibrated_resamples <- map2(.x = data_863_jackknife_resamples_list, 
                                                   .y = data_863_jackknife_weights_list,
                                                   ~ .x %>%
                                                     bind_cols(weights = .y) %>%
                                                     count(first_language_calibration, wt = weights))

test_first_language_calibrated_resamples <- map_lgl(first_language_calibrated_resamples, 
                                                ~ isTRUE(all.equal(.x$n, first_language_calibration_jackknife_resamples$Freq,
                                                                   tolerance = 0.001)))

if(!all(test_first_language_calibrated_resamples)){
  stop("Failed test: not all jackknife resamples seem to be correctly calibrated to first language population proportions. 
       \n *check 'first_language_calibrated_resamples' object.")
}

## age by place of birth

place_of_birth_calibrated_resamples <- map2(.x = data_863_jackknife_resamples_list, 
                                            .y = data_863_jackknife_weights_list,
                                            ~ .x %>%
                                              bind_cols(weights = .y) %>%
                                              count(age_place_of_birth_calibration, wt = weights) %>%
                                              arrange(age_place_of_birth_calibration))

test_place_of_birth_calibrated_resamples <- map_lgl(place_of_birth_calibrated_resamples, 
                                                    ~ isTRUE(all.equal(.x$n, place_of_birth_jackknife_resamples %>% 
                                                                         arrange(age_place_of_birth_calibration) %$% 
                                                                         Freq,
                                                                       tolerance = 0.001)))

if(!all(test_place_of_birth_calibrated_resamples)){
  stop("Failed test: not all jackknife resamples seem to be correctly calibrated to age by place of birth population proportions. 
       \n *check 'first_language_calibrated_resamples' object.")}

rm(first_language_calibrated_resamples, test_first_language_calibrated_resamples,
   place_of_birth_calibrated_resamples, test_place_of_birth_calibrated_resamples,
   first_language_calibration_jackknife_resamples,
   place_of_birth_jackknife_resamples)

#** summary of weights for jackknife resamples ----
# I would expect the weights to be similar to the main survey ones.
# Probably with the exception of resamples missing the already scarce groups.

# looks ok. no need to trim.
summary_jackknife_resamples_weights <- data_863_jackknife_weights_list %>%
  map(~ data_frame(wt = .x) %>%
        summarise(min = min(wt),
                  median = median(wt),
                  max = max(wt),
                  q95 = quantile(wt, 0.95),
                  q975 = quantile(wt, 0.975),
                  q99 = quantile(wt, 0.99),
                  q995 = quantile(wt, 0.995),
                  ratio = max/min)) %>%
  bind_rows(.id = "jackknife_resample")

summary_jackknife_resamples_weights %>%
  select(-jackknife_resample) %>%
  summarise_all(.funs = funs(min, mean, max)) %>%
  gather()

summary_jackknife_resamples_weights %>%
  write_csv(here("interim_outputs", "jackknife", "summary_jackknife_resamples_weights_03.csv"))

rm(summary_jackknife_resamples_weights)






