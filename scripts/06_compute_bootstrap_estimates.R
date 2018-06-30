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

data_863_weights <- read_rds(here("interim_outputs", "calibration", "main_survey_weights_04.csv"))

# ** QUOTA bootstrap resamples -----

## bootstrap resamples

bootstrap_resamples <- read_rds(here("interim_outputs", "resamples_863_bcn", str_c("resamples_", 10000, ".rds")))

### untrimmed weights

resamples_calibration_weights <- read_rds(here("interim_outputs", "calibration", "weights_resamples_untrimmed_04.rds"))

## first trimmed weights

resamples_calibration_weights_trimmed <- read_rds(here("interim_outputs", "calibration", "weights_resamples_trimmed_1_04.rds"))

## final trimmed weights

resamples_calibration_weights_trimmed_2 <- read_rds(here("interim_outputs", "calibration", "weights_resamples_trimmed_2_04.rds"))


# ** SRS bootstrap resamples -----

## bootstrap resamples

bootstrap_resamples_srs <- read_rds(here("interim_outputs", "resamples_863_bcn", str_c("resamples_", 10000, "_SRSdesign_03", ".rds")))

### untrimmed weights (I didn't trim weights for SRS bootstrap resamples)

resamples_calibration_weights_srs <- read_rds(here("interim_outputs", "calibration", "raked_calibration_weights_resamples_srs_04.rds"))


# ** jackknife resamples ----

## jackknife resamples data

data_863_jackknife_resamples_list <- read_rds(here("interim_outputs", "jackknife", "jackknife_resamples_05.rds"))

## jackknife resamples weights

data_863_jackknife_weights_list <- read_rds(here("interim_outputs", "jackknife", "weights_jackknife_resamples_05.rds"))


valid_responses <- c("couldn't participate", "didn't want to participate", 
                     "they prevented resp to participate", "voted")

# compute actual survey estimates ----
# plain estimate from weighted survey

estimate_survey_plain <- data_863_labelled %>% 
  compute_participation(weighted = F)

estimate_survey_plain_wt <- data_863_labelled %>%
  left_join(data_863_weights, by = "ORDRE_CINE") %>% 
  compute_participation(weighted = T)

estimate_survey_plain_wt_referendum_participation <-  estimate_survey_plain_wt$clean_ %>% filter(referendum_participation == "voted") %$% prop

# compute variance under SRS (no bootstrap) ----

## ** without finite population ----

estimate_survey_sd_srs_nofpc <- sqrt((estimate_survey_plain_wt_referendum_participation * (1-estimate_survey_plain_wt_referendum_participation))/
                                             nrow(data_863_labelled))

## ** CI without finite population correction ----

estimate_srs_l95 <- estimate_survey_plain_wt_referendum_participation - estimate_survey_sd_srs_nofpc * qnorm(0.975)

estimate_srs_u95 <- estimate_survey_plain_wt_referendum_participation + estimate_survey_sd_srs_nofpc * qnorm(0.975)

## ** with finite population correction ----

  ### Total population in Barcelona province: 3971666
  ### Taken from:
  ### https://estaticos.elperiodico.com/resources/pdf/4/3/1507302086634.pdf?_ga=2.11952052.1654533961.1528484435-1600052709.1528484435

fpc <- sqrt((3971666-nrow(data_863_labelled)) / (3971666-1) )

estimate_survey_sd_srs_withfpc <- estimate_survey_sd_srs_nofpc * fpc

rm(fpc)

# QUOTA bootstrap percentiles estimates ----
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

  ### looks like the bind cols worked well
bootstrap_resamples_analysis_with_weights_trimmed2[[1]] %>% arrange(ORDRE_CINE)

##** compute bootstrap estimates for all resamples -----

  ### unweighted (used for design effects)

if(!file.exists(here("interim_outputs", "estimates", "bootstrap_estimates_unweighted_06.rds"))){
  
  estimate_bootstrap_resamples_unweighted <-  bootstrap_resamples_analysis %>%
    map(~ .x %>% compute_participation(weighted = F) %>% bind_rows(.id = "type"))
  
  estimate_bootstrap_resamples_unweighted %>%
    write_rds(here("interim_outputs", "estimates", "bootstrap_estimates_unweighted_06.rds"))
  
}else{
  estimate_bootstrap_resamples_unweighted <- read_rds(here("interim_outputs", "estimates", "bootstrap_estimates_unweighted_06.rds"))
}

  ### with untrimmed weights
if(!file.exists(here("interim_outputs", "estimates", "bootstrap_estimates_untrimmed_weights_06.rds"))){

estimate_bootstrap_resamples_untrimmed_weights <-  bootstrap_resamples_analysis_with_weights_untrimmed %>%
  map(~ .x %>% compute_participation(weighted = T) %>% bind_rows(.id = "type"))

estimate_bootstrap_resamples_untrimmed_weights %>%
  write_rds(here("interim_outputs", "estimates", "bootstrap_estimates_untrimmed_weights_06.rds"))

}else{
  estimate_bootstrap_resamples_untrimmed_weights <- read_rds(here("interim_outputs", "estimates", "bootstrap_estimates_untrimmed_weights_06.rds"))
}

  ### with trimmed weights (first trimm method)
if(!file.exists(here("interim_outputs", "estimates", "bootstrap_estimates_trimmed1_weights_06.rds"))){
  
estimate_bootstrap_resamples_trimmed_weights1 <-  bootstrap_resamples_analysis_with_weights_trimmed1 %>%
  map(~ .x %>% compute_participation(weighted = T) %>% bind_rows(.id = "type"))

estimate_bootstrap_resamples_trimmed_weights1 %>%
  write_rds(here("interim_outputs", "estimates", "bootstrap_estimates_trimmed1_weights_06.rds"))

}else{
  estimate_bootstrap_resamples_trimmed_weights1 <- read_rds(here("interim_outputs", "estimates", "bootstrap_estimates_trimmed1_weights_06.rds"))
}


  ### with trimmed weights (second trimm method)
if(!file.exists(here("interim_outputs", "estimates", "bootstrap_estimates_trimmed2_weights_06.rds"))){

estimate_bootstrap_resamples_trimmed_weights2 <- bootstrap_resamples_analysis_with_weights_trimmed2 %>%
  map(~ .x %>% compute_participation(weighted = T) %>% bind_rows(.id = "type"))

estimate_bootstrap_resamples_trimmed_weights2 %>%
  write_rds(here("interim_outputs", "estimates", "bootstrap_estimates_trimmed2_weights_06.rds"))

}else{
  estimate_bootstrap_resamples_trimmed_weights2 <- read_rds(here("interim_outputs", "estimates", "bootstrap_estimates_trimmed2_weights_06.rds"))
}

  ### make a list with all estimates from resamples
estimates_all_bootstrap_resamples <- list(resamples_unweighted = estimate_bootstrap_resamples_unweighted,
                                          resamples_with_untrimmed_weights = estimate_bootstrap_resamples_untrimmed_weights,
                                          resamples_with_trimmed_weights_1 = estimate_bootstrap_resamples_trimmed_weights1,
                                          resamples_with_trimmed_weights_2 = estimate_bootstrap_resamples_trimmed_weights2)

##** mean, median, SD and quantiles of all bootstrap resamples -----

    ### first, retrieve the proportions of vote to referendum in each resample
estimate_proportions_referendum_vote_all_resamples <- estimates_all_bootstrap_resamples %>%
  map(~ .x %>% map_dbl(~ .x %>% filter(type == "clean_", referendum_participation == "voted") %$% prop ) )

    ##**** mean ----

    #good! estimates don't change much using trimmed or untrimmed weights
    #I will keep using only the trimmed2 weights.
estimate_bootstrap_resamples_mean <- estimate_proportions_referendum_vote_all_resamples %>%
  map(~ mean(.x))

    ##**** median ----
              #good, median almost equal to mean. not a skewed distribution on bootstrap resamples.
estimate_bootstrap_resamples_median <- estimate_proportions_referendum_vote_all_resamples %>%
  map(~ median(.x))

    ##**** sd ----
              # bootstrap estimates with untrimmed weights have the same/lower variance than those for trimmed!
              # there really is no error!
estimate_bootstrap_resamples_sd <- estimate_proportions_referendum_vote_all_resamples %>%
  map(~ sd(.x))

# resamples_calibration_weights %>% unlist %>% sd()
# resamples_calibration_weights_trimmed_2%>% unlist %>% sd()

    ##**** quantiles ----
estimate_bootstrap_resamples_q025 <- estimate_proportions_referendum_vote_all_resamples %>%
  map(~ quantile(.x, 0.025))

estimate_bootstrap_resamples_q975 <- estimate_proportions_referendum_vote_all_resamples %>%
  map(~ quantile(.x, 0.975))

# take SD of unweighted as well to calculate the design effect

estimate_bootstrap_resamples_unweighted_sd <- estimate_bootstrap_resamples_sd[["resamples_unweighted"]]

# take SD of untrimmed as well to compare it with SRS design

estimate_bootstrap_resamples_untrimmed_weights_sd <- estimate_bootstrap_resamples_sd[["resamples_with_untrimmed_weights"]]

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
                       "BS se" = estimate_bootstrap_resamples_sd,
                       "BS se unweighted" = estimate_bootstrap_resamples_unweighted_sd,
                       "BS se untrimmed weights"  = estimate_bootstrap_resamples_untrimmed_weights_sd) # use for computation of deff

# ** compute bootstrap bias ----

estimate_bootstrap_bias <- estimate_bootstrap_resamples_mean

# ** compute Normal.L95 and Normal.U95 from trimmed  ----

# Normal.L95: Actual est. - qnorm(0.975) * SD from bootstrap resamples
# Normal.U95: Actual est. + qnorm(0.975) * SD from bootstrap resamples

estimate_normal_l95 <- estimate_survey_plain_wt_referendum_participation - estimate_bootstrap_resamples_sd * qnorm(0.975)
estimate_normal_u95 <- estimate_survey_plain_wt_referendum_participation + estimate_bootstrap_resamples_sd * qnorm(0.975)

# SRS bootstrap percentiles estimates ----

# ** Prepare data from srs bootsrap resamples ----

data_863_referendum

### merge respondent ID from srs bootstrap resamples with survey response categories in referendum participation

bootstrap_resamples_srs_analysis <- bootstrap_resamples_srs %>%
  map(~ data_frame(`ORDRE_CINE` = .x) %>%
        left_join(data_863_referendum, by = "ORDRE_CINE"))


### merge srs bootstrap resamples (ID and ref participation) with calibration weights untrimmed
bootstrap_resamples_srs_analysis_with_weights_untrimmed <- bootstrap_resamples_srs_analysis %>%
  map2(resamples_calibration_weights_srs, ~.x %>%
         bind_cols(weights = .y))


### looks like the bind cols worked well
bootstrap_resamples_srs_analysis_with_weights_untrimmed[[1]] %>% arrange(ORDRE_CINE)

##** compute srs bootstrap estimates for all resamples -----

### unweighted (used for design effects)

if(!file.exists(here("interim_outputs", "estimates", "bootstrap_srs_estimates_unweighted_06.rds"))){
  
  estimate_bootstrap_srs_resamples_unweighted <-  bootstrap_resamples_srs_analysis %>%
    map(~ .x %>% compute_participation(weighted = F) %>% bind_rows(.id = "type"))
  
  estimate_bootstrap_srs_resamples_unweighted %>%
    write_rds(here("interim_outputs", "estimates", "bootstrap_srs_estimates_unweighted_06.rds"))
  
}else{
  estimate_bootstrap_srs_resamples_unweighted <- read_rds(here("interim_outputs", "estimates", "bootstrap_srs_estimates_unweighted_06.rds"))
}


### with untrimmed weights
if(!file.exists(here("interim_outputs", "estimates", "bootstrap_srs_estimates_untrimmed_weights_06.rds"))){
  
  estimate_bootstrap_resamples_srs_untrimmed_weights <-  bootstrap_resamples_srs_analysis_with_weights_untrimmed %>%
    map(~ .x %>% compute_participation(weighted = T) %>% bind_rows(.id = "type"))
  
  estimate_bootstrap_resamples_srs_untrimmed_weights %>%
    write_rds(here("interim_outputs", "estimates", "bootstrap_srs_estimates_untrimmed_weights_06.rds"))
  
}else{
  estimate_bootstrap_resamples_srs_untrimmed_weights <- read_rds(here("interim_outputs", "estimates", "bootstrap_srs_estimates_untrimmed_weights_06.rds"))
}


### make a list with all estimates from resamples
estimates_all_bootstrap_srs_resamples <- list(resamples_unweighted = estimate_bootstrap_srs_resamples_unweighted,
                                          resamples_with_untrimmed_weights = estimate_bootstrap_resamples_srs_untrimmed_weights)

##** SD of all bootstrap resamples -----
## I could also compute mean, median and quantiles, but I don't think I'm gonna use them.

### first, retrieve the proportions of vote to referendum in each resample
estimate_proportions_referendum_vote_all_srs_resamples <- estimates_all_bootstrap_srs_resamples %>%
  map(~ .x %>% map_dbl(~ .x %>% filter(type == "clean_", referendum_participation == "voted") %$% prop ) )

## **** sd ----

estimate_bootstrap_srs_resamples_sd <- estimate_proportions_referendum_vote_all_srs_resamples %>%
  map(~ sd(.x))


## ** export all estimates ----
# add these estimates to the previously computed object 'summary_estimates'
# this object contains the estimates from the bootstrap resamples using quota design

summary_estimates <- c(summary_estimates, 
                       "BS (SRS) se unweighted" = estimate_bootstrap_srs_resamples_sd[["resamples_unweighted"]],
                       "BS (SRS) se untrimmed weights" = estimate_bootstrap_srs_resamples_sd[["resamples_with_untrimmed_weights"]])


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

##** compute jackknife estimates for all resamples -----

if(!file.exists(here("interim_outputs", "estimates", "jackknife_estimates_06.rds"))){
  
  estimate_jackknife_resamples <-  data_863_jackknife_resamples_analysis %>%
    map(~ .x %>% compute_participation(weighted = T) %>% bind_rows(.id = "type"))
  
  estimate_jackknife_resamples %>%
    write_rds(here("interim_outputs", "estimates", "jackknife_estimates_06.rds"))
  
}else{
  estimate_jackknife_resamples <- read_rds(here("interim_outputs", "estimates", "jackknife_estimates_06.rds"))
}

### retrieve the proportions of vote to referendum in each resample

estimate_proportions_referendum_vote_all_jackknife <- estimate_jackknife_resamples %>%
  map_dbl(~ .x %>% dplyr::filter(type == "clean_", referendum_participation == "voted") %$% prop ) 

rm(data_863_jackknife_resamples_list, data_863_referendum, 
   data_863_jackknife_resamples_analysis, estimate_jackknife_resamples)

# bootstrap bca ----

# inputs: 
# * point estimates from jacknife resamples (proportion of vote in jacknife resamples) <- I probably need to double-check how to compute this.       
# * point estimates from resamples (proportion of vote in resample)       

# I'll follow the 'notation' used by Sturgis et al. 

nboot <- length(estimate_proportions_referendum_vote_all_resamples[["resamples_with_trimmed_weights_2"]])	
thetahat <- summary_estimates["Actual est."]
thetastar <- estimate_proportions_referendum_vote_all_resamples[["resamples_with_trimmed_weights_2"]] #J: gets the bootstrap resample estimates
z0 <- qnorm(sum(thetastar < thetahat)/nboot) # qnorm on proportion of resample estimates that are smaller than survey estimate
uu <- mean(estimate_proportions_referendum_vote_all_jackknife) - estimate_proportions_referendum_vote_all_jackknife
acc <- sum(uu * uu * uu)/(6 * (sum(uu * uu))^1.5)
zalpha <- qnorm(c((1-0.95)/2,1-(1-0.95)/2))
tt <- pnorm(z0 + (z0 + zalpha)/(1 - acc * (z0 + zalpha)))

confpoints <- quantile(x = thetastar, probs = tt, type = 1)

rm(nboot, thetahat, thetastar, z0, uu, acc, zalpha, tt)

# confidence intervals -----

confidence_intervals <- data_frame(indicator = "vote", 
                                  # SRS.L95 = estimate_srs_l95,
                                   #SRS.U95 = estimate_srs_u95,
                                   Normal.L95 = estimate_normal_l95,
                                   Normal.U95 = estimate_normal_u95,
                                   Percentile.L95 = estimate_bootstrap_resamples_q025,
                                   Percentile.U95 = estimate_bootstrap_resamples_q975,
                                   BCA.L95 = confpoints[1],
                                   BCA.U95 = confpoints[2])


# compare QUOTA with SRS bootstrap resamples -----

## plots of bootstrap resamples -----

# this is curious: quota design is slightly more efficient when looking at unweighted estimates
# but it seems to lose this efficiency when checking weighted estimates.
# that's curious, because weights were larger for srs design
# might be because of this: http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.692.507&rep=rep1&type=pdf

comparison_se_designs_graph <- data_frame(`Estimates quota design` = estimate_proportions_referendum_vote_all_resamples[["resamples_unweighted"]],
           `Estimates srs design` = estimate_proportions_referendum_vote_all_srs_resamples[["resamples_unweighted"]],
           weights = "unweighted") %>%
  bind_rows(data_frame(`Estimates quota design` = estimate_proportions_referendum_vote_all_resamples[["resamples_with_untrimmed_weights"]],
            `Estimates srs design` = estimate_proportions_referendum_vote_all_srs_resamples[["resamples_with_untrimmed_weights"]],
            weights = "weighted & untrimmed"))

comparison_se_designs_graph %>%
  gather(key = "design", value = "estimate", -weights) %>%
  ggplot(aes(x = estimate, col = design, group = design)) +
  geom_density() +
  facet_wrap(~weights, nrow = 2)


## compute SE, DEFTs and a table -----

sd(estimate_proportions_referendum_vote_all_resamples[["resamples_unweighted"]])

comparison_se_designs_table <- data_frame(design = c("SRS unweighted", "SRS weighted (untrimmed)", 
                                                     "Quota unweighted", "Quota weighted (untrimmed)"),
                                          `estimated SE` = c(estimate_bootstrap_srs_resamples_sd[["resamples_unweighted"]],
                                                             estimate_bootstrap_srs_resamples_sd[["resamples_with_untrimmed_weights"]],
                                                             estimate_bootstrap_resamples_unweighted_sd,
                                                             estimate_bootstrap_resamples_untrimmed_weights_sd))

# Export estimates ----

summary_estimates %>%
  write_rds(here("outputs", "vote_estimates_06.rds"))

confidence_intervals %>%
  write_rds(here("outputs", "vote_confidence_intervals_06.rds"))

## comparison of bootstrap designs

comparison_se_designs_graph %>%
  write_rds(here("outputs", "data_graph_comparison_bootstrap_designs_06.rds"))

comparison_se_designs_table %>%
  write_rds(here("outputs", "data_table_comparison_bootstrap_designs_06.rds"))

