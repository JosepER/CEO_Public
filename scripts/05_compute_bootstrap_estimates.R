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

source(here("scripts", "00_functions.R"))

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

## jackknife resamples data

data_863_jackknife_resamples_list <- read_rds(here("interim_outputs", "jackknife", "jackknife_resamples_04.rds"))

## jackknife resamples weights

data_863_jackknife_weights_list <- read_rds(here("interim_outputs", "jackknife", "weights_jackknife_resamples_04.rds"))




# referendum_participation = recode(as_factor(P82A),
#                                   "No hi vaig participar ni votar perquè no vaig poder (feina, malaltia)" = "couldn't participate",
#                                   "No hi vaig participar ni votar perquè no vaig voler" = "didn't want to participate",
#                                   "No hi vaig participar ni votar perquè m’ho van impedir" = "they prevented resp to participate",
#                                   "Estic segur/a que hi vaig participar i votar" = "voted",
#                                   .default = "DK/NA" )

# note: compute estimates with trimmed and untrimmed weights


# for estimates, delete DK/NA.

valid_responses <- c("couldn't participate", "didn't want to participate", 
                     "they prevented resp to participate", "voted")

# compute actual estimates ----
# plain estimate from weighted survey

estimate_survey_plain <- data_863_labelled %>% 
  compute_participation(weighted = F)

estimate_survey_plain_wt <- data_863_labelled %>%
  left_join(data_863_weights, by = "ORDRE_CINE") %>% 
  compute_participation(weighted = T)

estimate_survey_plain_wt_referendum_participation <-  estimate_survey_plain_wt$clean_ %>% filter(referendum_participation == "voted") %$% prop

# bootstrap percentiles estimates ----
# authors compute: BS Mean - mean of resamples
# BS Median - median of resamples
# BS se - SD of resamples

# confidence intervals: 
# Normal.L95: Actual est. - qnorm(0.975) * SD from bootstrap resamples
# Normal.U95: Actual est. + qnorm(0.975) * SD from bootstrap resamples
# Percentile.L95: 0.025 percentile of bootstrap resamples 
# Percentile.U95: 0.975 percentile of bootstrap resamples 


#** Prepare data from bootstrap resamples ----

  ### map respondent ID (ORDRE_CINE) to response categories in referendum participation (referendum_participation)
data_863_referendum <- data_863_labelled %>%
  select(ORDRE_CINE, referendum_participation)

rm(data_863_labelled)

  ### subset respondent ID (ORDRE_CINE) variable from bootstrap resamples 
bootstrap_resamples_analysis <- bootstrap_resamples %>%
  map("ORDRE_CINE")
  
  ### merge respondent ID from bootstrap resamples with survey response categories in ref participation
bootstrap_resamples_analysis %<>%
  map(~ .x %>%
        data_frame(ORDRE_CINE = .) %>%
        left_join(data_863_referendum, by = "ORDRE_CINE"))
             
  ### merge bootstrap resamples (ID and ref participation) with calibration weights untrimmed
bootstrap_resamples_analysis_with_weights_untrimmed <- bootstrap_resamples_analysis %>%
  map2(resamples_calibration_weights, ~.x %>%
        bind_cols(weights = .y))

  ### merge bootstrap resamples (ID and ref participation) with calibration weights slighly trimmed
bootstrap_resamples_analysis_with_weights_trimmed1 <- bootstrap_resamples_analysis %>%
  map2(resamples_calibration_weights_trimmed, ~.x %>%
         bind_cols(weights = .y))

  ### merge bootstrap resamples (ID and ref participation) with calibration weights 'properly' trimmed
bootstrap_resamples_analysis_with_weights_trimmed2 <- bootstrap_resamples_analysis %>%
  map2(resamples_calibration_weights_trimmed_2, ~.x %>%
         bind_cols(weights = .y))

# TO DO: SOME CHECKS TO MAKE SURE MERGING OF RESPONSES WITH WEIGHTS IS CORRECT
# WOULD NEED TO CHECK WEIGHTED FREQUENCIES OF CALIBRATION VARIABLES (AGAIN)

##** compute bootstrap estimates for all resamples -----

  ### with untrimmed weights
if(!file.exists(here("interim_outputs", "estimates", "bootstrap_estimates_untrimmed_weights_05.rds"))){

estimate_bootstrap_resamples_untrimmed_weights <-  bootstrap_resamples_analysis_with_weights_untrimmed %>%
  map(~ .x %>% compute_participation(weighted = T) %>% bind_rows(.id = "type"))

estimate_bootstrap_resamples_untrimmed_weights %>%
  write_rds(here("interim_outputs", "estimates", "bootstrap_estimates_untrimmed_weights_05.rds"))

}else{
  estimate_bootstrap_resamples_untrimmed_weights <- read_rds(here("interim_outputs", "estimates", "bootstrap_estimates_untrimmed_weights_05.rds"))
}

  ### with trimmed weights (first trimm method)
if(!file.exists(here("interim_outputs", "estimates", "bootstrap_estimates_trimmed1_weights_05.rds"))){
  
estimate_bootstrap_resamples_trimmed_weights1 <-  bootstrap_resamples_analysis_with_weights_trimmed1 %>%
  map(~ .x %>% compute_participation(weighted = T) %>% bind_rows(.id = "type"))

estimate_bootstrap_resamples_trimmed_weights1 %>%
  write_rds(here("interim_outputs", "estimates", "bootstrap_estimates_trimmed1_weights_05.rds"))

}else{
  estimate_bootstrap_resamples_trimmed_weights1 <- read_rds(here("interim_outputs", "estimates", "bootstrap_estimates_trimmed1_weights_05.rds"))
}


  ### with trimmed weights (second trimm method)
if(!file.exists(here("interim_outputs", "estimates", "bootstrap_estimates_trimmed2_weights_05.rds"))){

estimate_bootstrap_resamples_trimmed_weights2 <- bootstrap_resamples_analysis_with_weights_trimmed2 %>%
  map(~ .x %>% compute_participation(weighted = T) %>% bind_rows(.id = "type"))

estimate_bootstrap_resamples_trimmed_weights2 %>%
  write_rds(here("interim_outputs", "estimates", "bootstrap_estimates_trimmed2_weights_05.rds"))

}else{
  estimate_bootstrap_resamples_trimmed_weights2 <- read_rds(here("interim_outputs", "estimates", "bootstrap_estimates_trimmed2_weights_05.rds"))
}

  ### make a list with all estimates from resamples
estimates_all_bootstrap_resamples <- list(resamples_with_untrimmed_weights = estimate_bootstrap_resamples_untrimmed_weights,
     resamples_with_trimmed_weights_1 = estimate_bootstrap_resamples_trimmed_weights1,
     resamples_with_trimmed_weights_2 = estimate_bootstrap_resamples_trimmed_weights2)

##** mean, median, SD and quantiles of all bootstrap resamples -----


    ### first, retrieve the proportions of vote to referendum in each resample
estimate_proportions_referendum_vote_all_resamples <- estimates_all_bootstrap_resamples %>%
  map(~ .x %>% map_dbl(~ .x %>% filter(type == "clean_", referendum_participation == "voted") %$% prop ) )

    #good! estimates don't change much using trimmed or untrimmed weights
    #I will keep using only the trimmed2 weights.
estimate_bootstrap_resamples_mean <- estimate_proportions_referendum_vote_all_resamples %>%
  map(~ mean(.x))

              #good, median almost equal to mean. not a skewed distribution on bootstrap resamples.
estimate_bootstrap_resamples_median <- estimate_proportions_referendum_vote_all_resamples %>%
  map(~ median(.x))

estimate_bootstrap_resamples_sd <- estimate_proportions_referendum_vote_all_resamples %>%
  map(~ sd(.x))
  
estimate_bootstrap_resamples_q025 <- estimate_proportions_referendum_vote_all_resamples %>%
  map(~ quantile(.x, 0.025))

estimate_bootstrap_resamples_q975 <- estimate_proportions_referendum_vote_all_resamples %>%
  map(~ quantile(.x, 0.975))

  # select only estimates with trimmed 2 weights

estimate_bootstrap_resamples_mean <- estimate_bootstrap_resamples_mean[["resamples_with_trimmed_weights_2"]]
estimate_bootstrap_resamples_median <- estimate_bootstrap_resamples_median[["resamples_with_trimmed_weights_2"]] 
estimate_bootstrap_resamples_sd <- estimate_bootstrap_resamples_sd[["resamples_with_trimmed_weights_2"]]
estimate_bootstrap_resamples_q025 <- estimate_bootstrap_resamples_q025[["resamples_with_trimmed_weights_2"]]
estimate_bootstrap_resamples_q975 <- estimate_bootstrap_resamples_q975[["resamples_with_trimmed_weights_2"]]

## ** export all estimates ----

summary_estimates <- c("response_category" = "voted",
                       "Actual est." = estimate_survey_plain_wt_referendum_participation,
                       "BS Mean" = estimate_bootstrap_resamples_mean,
                       "BS Median" = estimate_bootstrap_resamples_median,
                       "BS se" = estimate_bootstrap_resamples_sd)



# ** compute bootstrap bias ----

estimate_bootstrap_bias <- estimate_bootstrap_resamples_mean



# ** compute Normal.L95 and Normal.U95 from trimmed  ----

# Normal.L95: Actual est. - qnorm(0.975) * SD from bootstrap resamples
# Normal.U95: Actual est. + qnorm(0.975) * SD from bootstrap resamples

estimate_normal_l95 <- estimate_survey_plain_wt_referendum_participation - estimate_bootstrap_resamples_sd * qnorm(0.975)
estimate_normal_u95 <- estimate_survey_plain_wt_referendum_participation + estimate_bootstrap_resamples_sd * qnorm(0.975)

# jackknife estimates ----
# this is needed for computing bootstrap bca

#** Prepare data from jackknife resamples ----

### subset respondent ID (ORDRE_CINE) variable from jackknife resamples 

data_863_jackknife_resamples_analysis <- data_863_jackknife_resamples_list %>% 
  map(~ .x %>% select("ORDRE_CINE"))

### merge respondent ID from jackknife resamples with survey response categories in ref participation

data_863_jackknife_resamples_analysis %<>%
  map(~ .x %>% left_join(data_863_referendum, by  = "ORDRE_CINE"))

### merge bootstrap resamples (ID and ref participation) with calibration weights

data_863_jackknife_resamples_analysis %<>%
  map2(data_863_jackknife_weights_list, ~ .x %>% 
         bind_cols(weights = .y))

# TO DO: SOME CHECKS TO MAKE SURE MERGING OF RESPONSES WITH WEIGHTS IS CORRECT

##** compute jackknife estimates for all resamples -----

data_863_jackknife_resamples_analysis

if(!file.exists(here("interim_outputs", "estimates", "jackknife_estimates_05.rds"))){
  
  estimate_jackknife_resamples <-  data_863_jackknife_resamples_analysis %>%
    map(~ .x %>% compute_participation(weighted = T) %>% bind_rows(.id = "type"))
  
  estimate_jackknife_resamples %>%
    write_rds(here("interim_outputs", "estimates", "jackknife_estimates_05.rds"))
  
}else{
  estimate_jackknife_resamples <- read_rds(here("interim_outputs", "estimates", "jackknife_estimates_05.rds"))
}

### retrieve the proportions of vote to referendum in each resample

estimate_proportions_referendum_vote_all_jackknife <- estimate_jackknife_resamples %>%
  map_dbl(~ .x %>% dplyr::filter(type == "clean_", referendum_participation == "voted") %$% prop ) 






# bootstrap bca ----

# inputs: 
# * point estimates from jacknife resamples (proportion of vote in jacknife resamples) <- I probably need to double-check how to compute this.       
# * point estimates from resamples (proportion of vote in resample)       

# I'll follow the 'notation' used by Sturgis et al. 

nboot <- length(estimate_proportions_referendum_vote_all_resamples[["resamples_with_trimmed_weights_2"]])	
thetahat <- summary_estimates["Actual est."]
thetastar <- estimate_proportions_referendum_vote_all_resamples[["resamples_with_trimmed_weights_2"]] #J: gets the resample estimates
z0 <- qnorm(sum(thetastar < thetahat)/nboot) # qnorm on proportion of resample estimates that are smaller than survey estimate
uu <- mean(estimate_proportions_referendum_vote_all_jackknife) - estimate_proportions_referendum_vote_all_jackknife

# to do: need to go back to 04 and compute estimates from jackknife resamples for vote in referendum


