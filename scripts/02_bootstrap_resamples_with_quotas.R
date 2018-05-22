# 02_resample_with_quotas
# I will obtain the resamples in such a way that the distribution of the quota variables in the resample should match those in the observed data.
# I will use only BCN because of small quotas. 

rm(list = ls())

library(foreach)
library(doParallel)
library(sjlabelled)
library(here)
library(magrittr)
library(tidyverse)

# Register parallel backend ----

cl <- makeCluster(5)
registerDoParallel(cl)

getDoParWorkers()
getDoParName()

set.seed(4)

# Import objects ----

 ## datasets

data_863_labelled <- read_rds(here("data", "survey_data_863_recoded_01.rds")) %>%
  filter(province == "Barcelona")

  ## quotas and calibrated proportions for BCN only

prop_province_munsize_863 <- read_rds(here("interim_outputs", "proportions_survey_863", "proportion_province_munsize_863_01.rds")) %>%
  filter(province == "Barcelona")

prop_age_sex_pob_863 <- read_rds(here("interim_outputs", "proportions_survey_863", "proportion_age_sex_863_01.rds")) %>%
  filter(province == "Barcelona")

# Explore and merge small quotas ----

prop_age_sex_pob_863 %>%
  arrange(n)

  ## Merge proportions
    
count_province_munsize_863_recoded <- prop_province_munsize_863 %>%
  select(size_municipality, n) %>%
  spread(key = "size_municipality", value = "n") %>%
  mutate(`<10.000` = `<2.000` + `2.001-10.000`) %>%
  gather(key = "size_municipality", value = "n") %>%
  filter(!size_municipality %in% c("<2.000", "2.001-10.000"))

count_province_munsize_863_recoded %>%
  arrange(n)
  
count_age_sex_pob_863_recoded <- prop_age_sex_pob_863 %>%
  select(quota, n) %>%
  spread(key = "quota", value = "n") %>%
  mutate(`18_24_ESP_Other` = Female_18_24_ESP + Male_18_24_ESP +  Male_18_24_Other + Female_18_24_Other,
         `25_34_ESP_Other` = Female_25_34_ESP + Male_25_34_ESP + Female_25_34_Other + Male_25_34_Other,
         `50_65+_Other` = Male_50_64_Other + Female_50_64_Other + `Male_65+_Other` + `Female_65+_Other`) %>%
  gather(key = "quota", value = "n") %>%
  filter(!quota %in% c("Female_18_24_ESP", "Male_18_24_ESP", "Male_18_24_Other", 
                       "Female_18_24_Other", "Male_65+_Other", "Female_65+_Other",
                       "Female_25_34_ESP", "Male_25_34_ESP", "Female_25_34_Other", 
                       "Male_25_34_Other", "Male_50_64_Other", "Female_50_64_Other"))

count_age_sex_pob_863_recoded %>%
  arrange(n)


  ## Merge quotas in dataset

data_863_labelled %<>%
  mutate(size_municipality = recode(size_municipality,
                                    "<2.000" = "<10.000",
                                    "2.001-10.000" = "<10.000"))

data_863_labelled %<>%
  mutate(quota = recode(quota, 
                        "Female_18_24_ESP" = "18_24_ESP_Other",
                        "Male_18_24_ESP" = "18_24_ESP_Other",
                        "Male_18_24_Other" = "18_24_ESP_Other",
                        "Female_18_24_Other" = "18_24_ESP_Other",
                        "Male_65+_Other" = "50_65+_Other",
                        "Female_65+_Other" = "50_65+_Other",
                        "Female_25_34_ESP" = "25_34_ESP_Other",
                        "Male_25_34_ESP" = "25_34_ESP_Other",
                        "Female_25_34_Other" = "25_34_ESP_Other",
                        "Male_25_34_Other" = "25_34_ESP_Other",
                        "Male_50_64_Other" = "50_65+_Other",
                        "Female_50_64_Other" = "50_65+_Other"
                        ) )

    ## double check that merged quotas are no longer there

test_merged_quotas <- (data_863_labelled$quota %in% c("Female_18_24_ESP", "Male_18_24_ESP", "Male_18_24_Other", 
                               "Female_18_24_Other", "Male_65+_Other", "Female_65+_Other",
                               "Female_25_34_ESP", "Male_25_34_ESP", "Female_25_34_Other", 
                               "Male_25_34_Other", "Male_50_64_Other", "Female_50_64_Other")) %>% 
  sum() != 0

if(test_merged_quotas){
  stop("Failed test: there are still non-merged quotas in the dataset.")
}

rm(test_merged_quotas)


# Create dummy variables for quotas ----

    ## in the technical report they call the crossed province x size_municipality as stratum, 
    ## but it's actually a quota

data_863_labelled %<>%
  mutate(quota_2 = size_municipality,
         quota_1 = quota,
         value = 1)

  data_863_for_bootstrap <- data_863_labelled %>%
    select(ORDRE_CINE, province, quota_1, quota_2, value)

data_quota_1 <- data_863_for_bootstrap %>%
  select(ORDRE_CINE, quota_1, value) %>%
  spread(key = "quota_1", value = "value") 

data_quota_2  <- data_863_for_bootstrap %>%
  select(ORDRE_CINE, quota_2, value) %>%
  spread(key = "quota_2", value = "value") 


## check that spread has been done correctly

counts_quota_1 <- data_863_for_bootstrap %>%
  count(quota_1)

counts_quota_2 <- data_863_for_bootstrap %>%
 count(quota_2)

  ### sum in dummy vars must be equal to counts

data_quota_1_for_test <- data_quota_1 %>%
  select(-ORDRE_CINE) %>% 
  colSums(na.rm = T)

test_spread_quota_1 <- all(counts_quota_1$n == data_quota_1_for_test)

if(!test_spread_quota_1){
  stop("Failed test: counts from original variable in quota_1 should be the same as column sums in dummy variables.")}

data_quota_2_for_test <- data_quota_2 %>%
  select(-ORDRE_CINE) %>% 
  colSums(na.rm = T)

test_spread_quota_2 <- all(counts_quota_2$n == data_quota_2_for_test)

if(!test_spread_quota_2){
  stop("Failed test: counts from original variable in quota_2 should be the same as column sums in dummy variables.")}

rm(counts_quota_1, test_spread_quota_1, counts_quota_2,
   test_spread_quota_2, data_quota_1_for_test,
   data_quota_2_for_test)

### change NAs for 0s

data_quota_1 %<>% 
  map_df(.f = function(x){if_else(is.na(x), true = 0, false = x)})

data_quota_2 %<>% 
  map_df(.f = function(x){if_else(is.na(x), true = 0, false = x)})

  ### merge 

data_863_for_bootstrap_with_quotas <- data_863_for_bootstrap %>%
                                             left_join(data_quota_1, by = "ORDRE_CINE")

data_863_for_bootstrap_with_quotas <- data_863_for_bootstrap_with_quotas %>%
  left_join(data_quota_2, by = "ORDRE_CINE") %>%
  select(-province, -quota_1, -quota_2, -value)

rm(data_quota_1, data_quota_2)


# bootstrap resampling ----
## this uses code from:
# Supplementary materials for 
# Sturgis, P., Kuha, J., Baker, N., Callegaro, M., Fisher, S., Green, J., 
# Jennings, W., Lauderdale, B. E., and Smith, P. (2017)   
# `An assessment of the causes of the errors In the 2015 
# UK General Election opinion polls?

# IMPORTANT: ACTUALLY, I THINK I NEED TO DO THIS BY STRATUM: PROVINCE 

# Start by trying it only on BCN sample


n_bootstrap_resamples <- 10000

if(!file.exists(here("interim_outputs", "resamples_863_bcn", str_c("resamples_", n_bootstrap_resamples, ".rds") ))){
  
resamples <- foreach(icount(n_bootstrap_resamples))%dopar%{

  quota.cols <- names(data_863_for_bootstrap_with_quotas[,c(-1)])
  data <- data_863_for_bootstrap_with_quotas[]
  
  quota.vars <- data[,quota.cols]
  quota.targets <- colSums(quota.vars)
  
  final.sample <- NULL
  continue <- T
  sample.from <- data
  n.sample <- nrow(data)
  quota.totals.sofar <- rep(0,length=ncol(data[,quota.cols]))

 
  while(continue){
    new.sample <- sample.from[sample(seq(n.sample),n.sample,replace=T),,drop=F]
    quota.vars.sample <- new.sample[,quota.cols,drop=F]
    csums <- apply(quota.vars.sample,2,cumsum)
    if(nrow(quota.vars.sample)==1)csums <- matrix(csums,nrow=1)
    csums <- t(csums)+quota.totals.sofar
    not.over.quota <- apply(csums<=quota.targets,2,all)
    final.sample <- rbind(final.sample, new.sample[not.over.quota,,drop=F])
    quota.totals.sofar <- colSums(final.sample[,quota.cols])
    quota.targets.met <- quota.totals.sofar == quota.targets
    if(all(quota.targets.met))continue <- F # Stop if all quota targets met
    else{
      qvars.complete <- (sample.from[,quota.cols,drop=F])[,which(quota.targets.met),drop=F] 
      sample.from <- sample.from[rowSums(qvars.complete)==0,,drop=F]
      n.sample <- nrow(sample.from)
      if(n.sample==0)continue <- F # Stop is no more observations left to sample from
    }	
  }	
  
   final.sample
  
}

if(!dir.exists(here("interim_outputs", "resamples_863_bcn"))){
  
  dir.create(here("interim_outputs", "resamples_863_bcn"))
}

resamples %>% 
  write_rds(here("interim_outputs", "resamples_863_bcn", str_c("resamples_", n_bootstrap_resamples, ".rds")))

}else{
  
resamples <-  read_rds(here("interim_outputs", "resamples_863_bcn", str_c("resamples_", n_bootstrap_resamples, ".rds")))
  
}

rm(quota.vars, final.sample, continue, sample.from, n.sample, quota.totals.sofar,
   n_bootstrap_resamples)

# check quota counts ----

## check quota counts in final sample are identical

map_dbl(resamples, nrow) %>%
  table()

test_quota_counts_resample_warning <- map_dbl(resamples, nrow) %>%
  table() %>%
  length() != 1

if(test_quota_counts_resample_warning){
warning("Failed test warning: not all resamples have the same number of observations.")
}
  
rm(test_quota_counts_resample_warning)

n_test <- length(resamples)

output_tests <- list(quota_1 = rep(NA, n_test),
                     quota_2 = rep(NA, n_test))

for(i in 1:n_test){
 
    data_checks <- resamples[[i]] %>%
    select(ORDRE_CINE) %>%
    left_join(data_863_for_bootstrap %>% select(ORDRE_CINE, quota_1, quota_2), by = "ORDRE_CINE")

output_tests[["quota_1"]][[i]] <- identical(data_checks %>%
    count(quota_1), 
    data_863_for_bootstrap %>%
    count(quota_1))
  
output_tests[["quota_2"]][[i]] <-  identical(data_checks %>%
    count(quota_2), 
    data_863_for_bootstrap %>%
    count(quota_2))
  
}
  
test_identical <- output_tests %>%
  unlist %>%
  all()

if(!test_identical){
  warning("Failed test warning: quota counts are not the same for all resamples.")
}

rm(test_identical, output_tests, n_test, data_checks)


## create a table with quota counts

output_quotas_test <- map(resamples, function(x){
  
  x[,-1] %>% colSums()
  
})

  ###Ensure they all have the same number of quotas

test_number_quotas <- output_quotas_test %>%
  map_dbl(length) %>%
  table() %>%
  length() != 1

if(test_number_quotas){
  stop("Failed test: all resamples should have the same number of quotas/columns.")
}

rm(test_number_quotas)

summary_quotas_resamples <- output_quotas_test %>%
  unlist %>%
  matrix(nrow = length(resamples), ncol = ncol(data_863_for_bootstrap_with_quotas)-1 , byrow = T)  %>%
  as_data_frame()

names(summary_quotas_resamples) <- names(data_863_for_bootstrap_with_quotas)[-1]


## Explore which quotas have different numbers of observations in resamples
  ### At first glance it looks like it's the smaller quotas (e.g. `Male_35_49_ESP`, `Male_35_49_Other` &  `Female_35_49_ESP`)
  ### which are not appearing within certain sizes of municipalities. 


map_dbl(summary_quotas_resamples, ~ .x %>% table %>% length)
    

## Spot unique samples

all_samples_sorted_char <- resamples %>%
  map("ORDRE_CINE") %>%
  map(sort) %>%
  map_chr(~.x %>% str_c(collapse = "_")) 

all_samples_sorted_char %>%
  table %>% length()


## Explore the number of times each individual appeared in resamples
    ### Doesn't look like there are anomalies at first glance.    

resamples %>%
  map("ORDRE_CINE") %>%
  unlist() %>%
  table() %>%
  sort(decreasing = T) %>%
  head(20)

data_frame(resamples_ = resamples %>%
  map("ORDRE_CINE") %>%
  unlist() %>%
  table() %>%
  as.numeric()) %>%
  ggplot(aes(y = resamples_, x = factor(1))) +
  geom_boxplot()


data_frame(resamples_ = resamples %>%
             map("ORDRE_CINE") %>%
             unlist() %>%
             table() %>%
             as.numeric()) %>%
  ggplot(aes(x = resamples_)) +
  geom_density()  

