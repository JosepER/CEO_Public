#03_calibrate_resamples

rm(list = ls())

options(scipen = 999999)

library(rebus)
library(survey)
library(sjlabelled)
library(here)
library(magrittr)
library(tidyverse)

# Import data ----

  ## Resamples

resamples <-  read_rds(here("interim_outputs", "resamples_863_bcn", str_c("resamples_", 10000, ".rds")))

  ## survey dataset

data_863_labelled <- read_rds(here("data", "survey_data_863_recoded_01.rds")) %>%
  filter(province == "Barcelona")

  ##calibrated proportions for BCN only

      
  ### First language

  #### This info is at a new administrative level ('vegueria'). Needs to be weighted by population in county to compute the province level estimates.
  #### methodological info said this is over 15 yo. 
  #### This might not be ideal but it's nonetheless the best thing I can do.

first_language <- read_csv(here("data", "from_IDESCAT", "llengua_inicial_2.csv"), 
                           skip = 8, na = "..") %>%
  mutate(Literal = str_replace_all(Literal, "à", "a") %>% str_replace_all("è", "e"))

county_files <- list.files(here("data", "from_IDESCAT", "counties"))
  
  #### name of each county
  #### reading only row 2 gives encoding problems (probably due to accents)

# Counties included in Barcelona province
# https://ca.wikipedia.org/wiki/Prov%C3%ADncia_de_Barcelona

#Alt Penedès	- Penedès  x
#Anoia	- Comarques centrals x
#Bages	- Comarques centrals x
#Baix Llobregat	-Metropolità x
#Barcelonès	-Metropolità x
#Berguedà	- Comarques centrals x
#Garraf	 - Penedès  x
#Maresme	-Metropolità x
#Moianès	- Comarques centrals x
#Osona	- Comarques centrals x
#Vallès Occidental -Metropolità	 x
#Vallès Oriental -Metropolità x


county_names <- c("Alt Penedes", "Anoia", "Bages", "Baix Llobregat", 
                  "Barcelones", "Bergueda", "Garraf", "Maresme", 
                  "Osona", "Valles Occidental",
                  "Valles Oriental", "Moianes")

counties_in_vegueria <- data_frame(county = county_names,
                                   vegueria = c("Penedes", "Comarques Centrals", "Comarques Centrals",
                                                "Metropolita", "Metropolita", "Comarques Centrals",
                                                "Penedes", "Metropolita", "Comarques Centrals", 
                                                "Metropolita", "Metropolita",
                                                "Comarques Centrals"))

county_population <- county_files %>% map(~ read_csv(str_c(here("data", "from_IDESCAT", "counties"), 
                                     .x, sep = "/"), skip = 5))

names(county_population) <- county_names

county_population %<>%
  map(function(x){attributes(x)$names <- c("Age", "Men", "Women", "Total")
  return(x)})

county_population %<>%
  bind_rows(.id = "county")

county_population %<>%
  mutate(Age = as_numeric(str_extract(Age, pattern = "\\d+"))) %>%
  filter(Age >= 18)

  ### Place of birth
    #### I could actually play much more with this. There's not only place of birth, but also pob by age and gender.
place_of_birth <- read_csv(here("data", "from_IDESCAT", "lloc_naixement_sexe_edat_barcelona.csv"), skip = 5)
      

rm(county_files, county_names)

# Compute proportions for calibration ----

  # First language

summary_population_vegueria <- county_population %>%
  left_join(counties_in_vegueria, by = "county") %>%
  select(vegueria, everything()) %>%
  group_by(vegueria) %>%
  summarise(sum_population_county = sum(Total)) %>%
  mutate(prop_county = sum_population_county/sum(sum_population_county)) 
  
rm(county_population, counties_in_vegueria)

summary_population_vegueria

  ## group all other languages

first_language %>%
  gather(key = "language", "percentage", -Codi, -Literal, -Català, -Castellà)

  ### all languages don't seem to add up to 100%! 
  ### I think it's better to compute 'other' as 100 - (CAT + CAST)

first_language[c(3:8, 10)] %>% as.matrix %>% rowSums(na.rm = T)

first_language %<>%
  select(Literal:4) %>%
  mutate(other = 100-(Català + Castellà))

first_language %<>%
  filter(Literal %in% c("Penedes", "Comarques Centrals", "Metropolita"))

  ## compute province estimate

first_language %<>%
  left_join(summary_population_vegueria, by = c("Literal" = "vegueria")) %>%
  mutate(Catalan = Català * prop_county,
         Spanish = Castellà * prop_county,
         Other = other * prop_county)

first_language_province <- first_language %>%
  summarise(Catalan = sum(Catalan),
            Spanish = sum(Spanish),
            Other = sum(Other))

first_language_province %<>%
  gather(key = "first_language_calibration", value = "prop")

rm(first_language, summary_population_vegueria)

    # Place of birth

fifteen_to_twenty <- which(place_of_birth$X1 == "De 15 a 19 anys")

eighty_five_plus <- which(place_of_birth$X1 == "De 85 anys i més")

place_of_birth %<>%
  slice(fifteen_to_twenty:eighty_five_plus)

rm(fifteen_to_twenty, eighty_five_plus)

place_of_birth %<>%
  select(X1, ends_with("Total"), -`Total. Total`)
    
names(place_of_birth) <- c("Age", "CAT", "ESP", "Other")

place_of_birth %<>%
  mutate(Age = str_replace(Age, pattern = " a ", replacement = "_") %>%
           str_replace_all(pattern = "(?:De | anys)", replacement = "") %>%
           str_replace(pattern = " i més", replacement = "+"))

    ### modify category from 15-19 years proportionally to account only for 18-19 yo
 
place_of_birth[place_of_birth$Age == "15_19",-1] <- place_of_birth[place_of_birth$Age == "15_19",-1]*2/5
  
place_of_birth[place_of_birth$Age == "15_19", "Age"] <- "18-19"

    ### aggregate to the following age categories: 
    #### 18-34, 35-49, 50-64, 65+

place_of_birth %<>%
  mutate(age_cat = c(rep("18_34", 4), rep("35_49", 3), rep("50_64", 3), rep("65+", 5) )) %>%
  group_by(age_cat) %>%
  summarise(CAT = sum(CAT),
            ESP = sum(ESP),
            Other = sum(Other)) %>%
  gather(key = "place_of_birth", value = "Freq", -age_cat) %>%
  unite(col = "age_place_of_birth_calibration", age_cat, place_of_birth) %>%
  mutate(prop = Freq/sum(Freq)) %>%
  select(-Freq)

    ### re-aggregate 65+_Other with 50_64_Other. Else, there is trouble with some recalibration
    ###   resamples which include 0 elements in this category 

place_of_birth %<>%
  spread(key = "age_place_of_birth_calibration", value = "prop") %>%
  mutate(`50+_Other` = `50_64_Other` + `65+_Other`) %>%
  select(-`50_64_Other`, -`65+_Other`) %>%
  gather(key = "age_place_of_birth_calibration", value = "prop")

# Check calibration proportions

if(!dir.exists(here("interim_outputs", "calibration"))){
  dir.create(here("interim_outputs", "calibration"))
}

first_language_province %>%
  write_csv(here("interim_outputs", "calibration", "first_language_calibration_proportions_03.csv"))

place_of_birth %>%
  write_csv(here("interim_outputs", "calibration", "place_of_birth_proportions_03.csv"))

first_language_province

place_of_birth

## compute a different calibration frequencies for each length of resample

resamples_length <- map_dbl(resamples, nrow) 

resamples_length_unique <- resamples_length %>%
  unique %>%
  sort()

first_language_province_lengths_list <- map(resamples_length_unique, 
                                            ~ first_language_province %>%
                                              mutate(Freq = prop/100 * .x) %>%
                                              select(-prop))

place_of_birth_different_lengths_list <- map(resamples_length_unique, 
                                             ~ place_of_birth %>%
                                               mutate(Freq = prop * .x) %>%
                                               select(-prop))

names(first_language_province_lengths_list) <- resamples_length_unique

names(place_of_birth_different_lengths_list) <- resamples_length_unique

# Subset main survey dataset with calibration variables ----

data_863_for_calibration <- data_863_labelled %>%
  select(ORDRE_CINE, first_language_calibration, age_place_of_birth_calibration)

# Check calibrate main survey dataset ----
## This is not really requiered for the script. It's informative and shows which categories needed more adjustment.

## compute proportions in survey

data_863_place_of_birth_prop <- data_863_for_calibration %>%
  count(age_place_of_birth_calibration) %>%
  mutate(prop_survey = n/sum(n))

data_863_first_language <- data_863_for_calibration %>%
  count(first_language_calibration) %>%
  mutate(prop_survey = n/sum(n))

## merge with population proportions

  ### respondents with 'place of birth = Other' are very underrepresented in the survey!!
comparison_proportions_place_of_birth <- data_863_place_of_birth_prop %>%
  left_join(place_of_birth, by = "age_place_of_birth_calibration") %>%
  mutate(ratio = prop/prop_survey)

comparison_proportions_place_of_birth

comparison_proportions_first_language <- data_863_first_language %>%
  left_join(first_language_province, by = "first_language_calibration") %>%
  mutate(ratio = prop/100/prop_survey)

comparison_proportions_first_language

comparison_proportions_place_of_birth %<>%
  mutate(var = "age_place_of_birth_calibration") %>%
  rename(category = age_place_of_birth_calibration )

comparison_proportions_first_language %<>%
  mutate(var = "first_language_calibration") %>%
  rename(category = first_language_calibration )

bind_rows(comparison_proportions_place_of_birth, comparison_proportions_first_language) %>%
  select(var, everything()) %>%
  write_csv(here("interim_outputs", "calibration", "comparison_survey_863_population_calibration_proportions_03.csv"))


## Calibrate main survey ----

first_language_calibration_survey_863 <- first_language_province %>%
  mutate(Freq = prop * nrow(data_863_for_calibration)) %>%
  select(-prop)

place_of_birth_survey_863 <- place_of_birth %>%
  mutate(Freq = prop * nrow(data_863_for_calibration)) %>%
  select(-prop)

survey_design_data_863 <- svydesign(ids = ~ 0, data = data_863_for_calibration)

survey_design_data_863

raked_data_863 <- rake(survey_design_data_863, 
     sample.margins = list(~first_language_calibration, ~age_place_of_birth_calibration),
     population = list(first_language_calibration_survey_863, place_of_birth_survey_863),
     control = list(maxit = 30, epsilon = 1))

weights(raked_data_863)

# Note: categories of 18_34_Other and 35_49_Other have the largest calibration weights.

data_863_for_calibration %>%
  bind_cols(weight = weights(raked_data_863)) %>%
  select(first_language_calibration, age_place_of_birth_calibration, weight) %>%
  arrange(desc(weight)) %>%
  unique()

##** summary of main survey weights----
## notes:
## max = ~4
## q995 = ~4
## ratio = ~5.8

summary_main_survey_weights <- weights(raked_data_863) %>%
  data_frame(val = .) %>%
  summarise(min = min(val),
            median = median(val),
            max = max(val),
            ratio = max/min,
            q95 = quantile(val, 0.95),
            q99 = quantile(val, 0.99),
            q995 = quantile(val, 0.995),
            ratio_min_q995 = q995/min)

if(!file.exists(here("interim_outputs", "calibration", "summary_main_survey_weights_03.csv"))){

summary_main_survey_weights %>%
  gather(key = "indicator") %>%
  write_csv(here("interim_outputs", "calibration", "summary_main_survey_weights_03.csv"))
  
}

data_frame(ORDRE_CINE = raked_data_863$variables$ORDRE_CINE, 
           weights = weights(raked_data_863)) %>%
  write_rds(here("interim_outputs", "calibration", "main_survey_weights_03.csv"))

rm(comparison_proportions_place_of_birth, comparison_proportions_first_language,
   data_863_place_of_birth_prop, data_863_first_language, first_language_calibration_survey_863,
   place_of_birth_survey_863, summary_main_survey_weights, survey_design_data_863,
   raked_data_863)

# calibrate resamples ----

## create resamples for calibration

resamples_for_calibration <- resamples %>%
  map(~ data_863_for_calibration[match(.x[["ORDRE_CINE"]], data_863_for_calibration[["ORDRE_CINE"]]), ])

## **create 'survey' survey designs for each resample----

if(!file.exists(here("interim_outputs", "resample_survey_desings_863_bcn", "survey_designs_03.rds"))){

resamples_survey_design_list <- resamples_for_calibration %>%
  map(~ svydesign(ids = ~ 0, data = .x))

resamples_survey_design_list %>%
  write_rds(here("interim_outputs", "resample_survey_desings_863_bcn", "survey_designs_03.rds"))

}else{
  resamples_survey_design_list <- read_rds(here("interim_outputs", "resample_survey_desings_863_bcn", "survey_designs_03.rds"))
}

## **calibrate resamples ----

raked_resamples_list <- map2(.x = resamples_survey_design_list, .y = resamples_length, 
     ~ try(rake(.x, 
            sample.margins = list(~first_language_calibration, ~age_place_of_birth_calibration),
            population = list(first_language_province_lengths_list[[as.character(.y)]], place_of_birth_different_lengths_list[[as.character(.y)]]),
            control = list(maxit = 30, epsilon = 1))))

error_in_raking <- raked_resamples_list %>%
  map_lgl(~ any(class(.x) == "try-error")) %>%
  which()

if(length(error_in_raking) != 0){
  stop("Failed test: raking of resamples failed. Check object 'raked_resamples_list'. It might be that some calibration categories were not found in resamples.")  
}

rm(error_in_raking)  

raked_resamples_list %>%
  write_rds(here("interim_outputs", "calibration", "raked_sampling_designs_resamples_03.rds"))

resamples_calibration_weights <- raked_resamples_list %>%
  map(~ weights(.x))

resamples_calibration_weights %>%
  write_rds(here("interim_outputs", "calibration", "raked_calibration_weights_resamples_03.rds"))

rm(raked_resamples_list, resamples_survey_design_list)

# Check calibrated resamples ----

first_language_calibrated_resamples <- map2(.x = resamples_for_calibration, 
                                            .y = resamples_calibration_weights,
                                            ~ .x %>%
                                              bind_cols(weights = .y) %>%
                                              count(first_language_calibration, wt = weights))

age_place_of_birth_calibrated_resamples <- map2(.x = resamples_for_calibration, 
                                            .y = resamples_calibration_weights,
                                            ~ .x %>%
                                              bind_cols(weights = .y) %>%
                                              count(age_place_of_birth_calibration, wt = weights))

    ## **first language----
    ## there seems to be some rounding error. I think it's acceptable overall.

map2(.x = first_language_calibrated_resamples,
     .y = resamples_length, function(.x, .y){ as.integer(.x[["n"]]) ==  as.integer(first_language_province_lengths_list[[as.character(.y)]][["Freq"]])}) %>%
  head(15)

first_language_calibrated_resamples_checks <- first_language_calibrated_resamples %>%
  map("n") %>% unlist %>%
  matrix(ncol = 3, nrow = length(first_language_calibrated_resamples), byrow = T)

colnames(first_language_calibrated_resamples_checks) <- c("Catalan", "Spanish", "Other")

first_language_calibrated_resamples_checks %<>%
  as_data_frame

population_margins_first_language_checks <- first_language_province_lengths_list %>%
  map("Freq") %>% unlist %>%
  matrix(ncol = 3, nrow = length(first_language_province_lengths_list), 
         byrow = T, dimnames = list(NULL, c("Catalan_pop", "Spanish_pop", "Other_pop"))) %>%
  as_data_frame() %>%
  mutate(resample_length = names(first_language_province_lengths_list))
  
first_language_calibrated_resamples_checks %<>%
  bind_cols(resample_length = as.character(resamples_length)) %>%
  left_join(population_margins_first_language_checks, by = "resample_length") # need to merge with population estimates

first_language_calibrated_resamples_checks %<>%
  mutate(check = abs(Catalan-Catalan_pop) + abs(Spanish-Spanish_pop) + abs(Other-Other_pop))

first_language_calibrated_resamples_checks %>%
  write_csv(here("interim_outputs", "calibration", "raked_calibration_checks_first_language_03.csv"))

first_language_calibrated_resamples_checks %>%
  arrange(desc(check))

first_language_calibrated_resamples_checks %>%
  ggplot(aes(x = check)) +
  geom_density()
  
first_language_calibrated_resamples_checks %>%
  ggplot(aes(x = check)) +
  geom_density() +
  facet_wrap(~resample_length)

## **place of birth----

map2(.x = age_place_of_birth_calibrated_resamples,
     .y = resamples_length, function(.x, .y){ try(as.integer(.x[["n"]]) ==  as.integer(place_of_birth_different_lengths_list[[as.character(.y)]][["Freq"]]))}) %>%
  head(15)
            ### careful here because I have samples with different lengths
age_place_of_birth_calibrated_resamples_checks <- age_place_of_birth_calibrated_resamples %>%
  bind_rows(.id = "resample")
  
age_place_of_birth_calibrated_resamples_checks %<>%
  mutate(resample = as.numeric(resample)) %>%
  spread(key = "age_place_of_birth_calibration", value = "n") %>%
  arrange(resample)
  
population_margins_age_place_of_birth_checks <- place_of_birth_different_lengths_list %>%
  bind_rows(.id = "resample_length") %>% 
  spread(key = "age_place_of_birth_calibration", value = "Freq")

names(population_margins_age_place_of_birth_checks)[2:ncol(population_margins_age_place_of_birth_checks)] <- str_c(names(population_margins_age_place_of_birth_checks)[2:ncol(population_margins_age_place_of_birth_checks)], "_pop") 

number_categories <- ncol(age_place_of_birth_calibrated_resamples_checks)

age_place_of_birth_calibrated_resamples_checks %<>%
  bind_cols(resample_length = as.character(resamples_length)) %>%
  left_join(population_margins_age_place_of_birth_checks, by = "resample_length")

    ### compute a matrix with differences between resamples and population counts
age_place_of_birth_calibrated_resamples_checks_diffs <- matrix(NA, ncol = (number_categories-1), nrow = nrow(age_place_of_birth_calibrated_resamples_checks))

for(i in 1:(number_categories-1)){

  age_place_of_birth_calibrated_resamples_checks_diffs[,i] <- abs(age_place_of_birth_calibrated_resamples_checks[[i+1]] - age_place_of_birth_calibrated_resamples_checks[[i+number_categories+1]]) 
} #double check this!

colnames(age_place_of_birth_calibrated_resamples_checks_diffs) <- names(age_place_of_birth_calibrated_resamples_checks)[2:(number_categories)]

    ### Calibration seems to have worked very well 

age_place_of_birth_calibrated_resamples_checks_diffs %>% as_data_frame %>% map(max)

age_place_of_birth_calibrated_resamples_checks_diffs %>% as_data_frame %>% map(sd)

age_place_of_birth_calibrated_resamples_checks$check <- age_place_of_birth_calibrated_resamples_checks_diffs %>%
  array_branch(1) %>% map_dbl(sum)

age_place_of_birth_calibrated_resamples_checks %>%
  write_csv(here("interim_outputs", "calibration", "raked_calibration_checks_place_of_birth_03.csv"))

age_place_of_birth_calibrated_resamples_checks %>%
  arrange(desc(check))

age_place_of_birth_calibrated_resamples_checks %>%
  ggplot(aes(x = check)) +
  geom_density()

rm(i, first_language_calibrated_resamples, 
   age_place_of_birth_calibrated_resamples,
   first_language_province_lengths_list,
   place_of_birth_different_lengths_list,
   resamples_length_unique,
   age_place_of_birth_calibrated_resamples_checks_diffs,
   age_place_of_birth_calibrated_resamples_checks,
   number_categories)



# variance before trimming----

## there is a tail with some resamples having rather larger weights
resamples_calibration_weights %>%
  map_dbl(sd) %>%
  data_frame(standard_devs = .) %>%
  ggplot(aes(y = standard_devs, x = factor(1)))+
  geom_boxplot()

resamples_calibration_weights %>%
  map_dbl(sd) %>%
  data_frame(standard_devs = .) %>%
  ggplot(aes(x = standard_devs))+
  geom_density()

###**compute summary of weights----
### sd of weights
  ### max weights
  ### q0.95, q0.90 of weights
  ### q0.005

weights_sd <- resamples_calibration_weights %>%
  map_dbl(sd)

weights_min <- resamples_calibration_weights %>%
  map_dbl(min)

weights_max <- resamples_calibration_weights %>%
  map_dbl(max)

weights_ratio <- weights_max/weights_min

weights_quantiles <- resamples_calibration_weights %>%
  map(~.x %>% quantile(probs = c(0.95,0.99,0.995)) %>% 
        data_frame(quantile = c("q0.95", "q0.99", "q0.995"),
                   value = .) ) %>%
  bind_rows(.id = "resample") %>%
  mutate(resample = as.numeric(resample))
  
weights_quantiles %<>%
  spread(key = "quantile", value = "value")

weigths_summary <- bind_cols(sd = weights_sd, min =  weights_min, max = weights_max, weights_quantiles, weights_ratio = weights_ratio) %>%
    select(resample, everything())

weigths_summary %>%
  write_csv(here("interim_outputs", "calibration", "summary_resamples_weights_03.csv"))

  ### max weight is over 5 for a relatively large proportion of resamples
  ### trimming at 0.995 seems very reasonable
  ### still need to check which resamples and why are these weights relatively larger
weigths_summary %>%
  select(max:q0.995) %>%
  gather(key = "indicator", value = "value") %>%
  ggplot(aes(x = value, group = indicator, col = indicator))+
  geom_density()

weigths_summary %>%
  select(weights_ratio) %>%
  ggplot(aes(x = weights_ratio))+
  geom_density()


### **check categories with high weights----

#### largest 10 weights in each resampling

tenth_threshold_list <- resamples_calibration_weights %>%
  map(~ .x %>% sort(decreasing = T) %>% .[1:10] %>% min )

index_max_weights <- map2(resamples_calibration_weights, tenth_threshold_list, ~ which(.x >= .y) )

top_10_categories <- map2(resamples_for_calibration, index_max_weights, ~.x %>% slice(.y)) 

  #### At first glance it's clear that age_place_of_birth_calibration = 18_34_Other has largest weights
  #### Not really a surprise. it was the same 
top_10_categories_proportions <-  top_10_categories %>%
  map(~ .x %>% 
        unite(col = "crossed_categories", first_language_calibration, age_place_of_birth_calibration) %>%
        count(crossed_categories) %>% 
        mutate(prop = n/sum(n)))

top_10_categories_summary <- top_10_categories_proportions %>%
  bind_rows() %>%
  select(-n) %>%
  group_by(crossed_categories) %>%
  summarise(total = sum(prop)) %>%
  mutate(prop = total/sum(total)) %>%
  arrange(desc(prop))

  #### ~75% all max weights are from place of birth: 18_34_Other
top_10_categories_summary

rm(tenth_threshold_list, index_max_weights, top_10_categories_proportions,
   top_10_categories, weights_quantiles)

weigths_summary$q0.995 %>% max()

### think about trimming top 4 or 5 weights 
### think about trimming by ratio of weights

sum(weigths_summary$weights_ratio > 5)

sum(weigths_summary$weights_ratio > 6)

# trim weights ----
## trim to 99.5 percentile
  
resamples_calibration_weights_trimmed <- map2(.x = resamples_calibration_weights, .y = as.list(weigths_summary$q0.995), function(x = .x, y = .y){ 
  x[which(x > y)] <- y 
  return(x)}) 
  
### Check. Max in each element of the list should be equal to q0.995

 test_trim_0.995 <- all((resamples_calibration_weights_trimmed %>%
  map_dbl(~ .x %>% max()) ) != (weigths_summary$q0.995))

 if(test_trim_0.995){stop("Failed test: weight trim seems to have failed.")}

 rm(test_trim_0.995)
 
### some trimmed weights still have very large ratios
ratio_trimmed_weights <-  resamples_calibration_weights_trimmed %>%
   map_dbl(function(x){max(x)/min(x)})
 
ratio_trimmed_weights %>% quantile(c(0.95,0.99, 0.995) )


### check max weight of weights with ratio larger than 6
### some look like they have large max weights, but not many of them.
resamples_calibration_weights_trimmed[which(ratio_trimmed_weights > 6)] %>%
  map_dbl(max) %>%
  data_frame(val = .) %>%
  ggplot(aes(y = val, x = factor(1))) +
  geom_boxplot()

### trim according to a max ratio of 8
### this is slightly higher than original ratio of ~5.8

resamples_calibration_weights_trimmed_2 <- resamples_calibration_weights_trimmed

max_ratio <- 8

resamples_calibration_weights_trimmed_2[which(ratio_trimmed_weights > max_ratio)] <- map2(.x = resamples_calibration_weights_trimmed[which(ratio_trimmed_weights > max_ratio)],
     .y = ratio_trimmed_weights[which(ratio_trimmed_weights > max_ratio)],
     .f = function(x = .x, y = .y){
    
    x[which(x > min(x) * max_ratio)] <-  min(x) * max_ratio
    
    return(x)
    
  })

# variance after trimming----
# check reduction in variance of weights within resamples

resamples_calibration_weights_variance_comparison <- pmap(.l = list(x = resamples_calibration_weights, 
               y = resamples_calibration_weights_trimmed,
               z = resamples_calibration_weights_trimmed_2),
     .f = function(x, y, z){
       
       data_frame(weights_sd = sd(x),
                  weights_sd_tr = sd(y),
                  weights_sd_tr_2 = sd(z)) 
       
     }) %>%
  bind_rows(.id = "resample")

## Note: Reduction in variance within resamples after first trim. Looks like good reduction in outliers after second.

resamples_calibration_weights_variance_comparison %>%
  gather(key = "weights", value = "sd", -resample) %>%
  ggplot(aes(x = factor(weights), y = sd)) +
  geom_boxplot()

# variance between resamples

resamples_calibration_weights_variance_comparison %>%
  select(-resample) %>%
  map(sd)

rm(resamples_calibration_weights_variance_comparison, max_ratio)

# sum of trimmed weights

resamples_sum_trimmed_weights <- pmap(.l = list(x = resamples_calibration_weights, 
               y = resamples_calibration_weights_trimmed,
               z = resamples_calibration_weights_trimmed_2),
     function(x, y, z){
       
       data_frame(sum_diff_trimmed = sum(x-y),
                  sum_diff_trimmed_2 = sum(x-z))
       
     }) %>%
  bind_rows(.id = "resample")

# Note: some sum of trimmed weights look rather large.
# Inflation in sum of trimmed weights appears in second trimming

resamples_sum_trimmed_weights %>%
  arrange(desc(sum_diff_trimmed_2)) %>%
  head(15)

resamples_sum_trimmed_weights %>%
  arrange(desc(sum_diff_trimmed)) %>%
  head(15)

##**explore what happened to samples with so many trimmed weights----

resamples_to_check_index <- resamples_sum_trimmed_weights %>%
  arrange(desc(sum_diff_trimmed_2)) %>%
  head(20) %$%
  resample %>%
  as.numeric

  ##****first language----

first_language_calibrated_resamples_to_check <- map2(.x = resamples_for_calibration[resamples_to_check_index], 
                                            .y = resamples_calibration_weights_trimmed_2[resamples_to_check_index],
                                            ~ .x %>%
                                              bind_cols(weights = .y) %>%
                                              count(first_language_calibration, wt = weights))

lengths_resamples_to_check <- resamples_length[resamples_to_check_index]

first_language_calibrated_resamples_to_check_tidy <- map2(.x = first_language_calibrated_resamples_to_check,
     .y = lengths_resamples_to_check,
     function(x, y){
       
       x %>%
         spread(key = "first_language_calibration", value = "n") %>%
         bind_cols(resample_length = y, .)
       
      }) %>%
  bind_rows %>%
  bind_cols(resample = resamples_to_check_index) %>%
  select(resample, everything()) %>% 
  gather(key = "category", value = "resample_value", -resample, -resample_length)

first_language_population_to_check <- population_margins_first_language_checks %>%
  gather(key = "category", value = "population_value", -resample_length) %>%
  mutate(category = str_extract(category, pattern = "^[[:alpha:]]+"),
         resample_length = as.numeric(resample_length))

check_language <- first_language_calibrated_resamples_to_check_tidy %>%
  left_join(first_language_population_to_check, by = c("resample_length", "category")) %>%
  mutate(diff = population_value - resample_value) %>%
  select(-resample_length)

# note: differences are mostly in 'Other' category after trimming. 

check_language %>%
  arrange(desc(diff))

check_language_resample_value <- check_language %>%
  select(resample, category, resample_value) %>%
  spread(key = "category", value = "resample_value")

names(check_language_resample_value)[2:4] <- str_c(names(check_language_resample_value)[2:4], "_resample")

check_language_population_value <- check_language %>%
  select(resample, category, population_value) %>%
  spread(key = "category", value = "population_value")

names(check_language_population_value)[2:4] <- str_c(names(check_language_resample_value)[2:4], "_population")

check_language_diff_value <- check_language %>%
  select(resample, category, diff) %>%
  spread(key = "category", value = "diff")

names(check_language_diff_value)[2:4] <- str_c(names(check_language_diff_value)[2:4], "_diff")

check_language_summary <- check_language_resample_value %>%
  left_join(check_language_population_value, by = "resample") %>%
  left_join(check_language_diff_value, by = "resample")

check_language_summary

rm(check_language_resample_value, check_language_population_value, check_language_diff_value)

#****age x place_of_birth----

age_place_of_birth_calibrated_resamples_to_check <- map2(.x = resamples_for_calibration[resamples_to_check_index], 
                                                         .y = resamples_calibration_weights_trimmed_2[resamples_to_check_index],
                                                         ~ .x %>%
                                                           bind_cols(weights = .y) %>%
                                                           count(age_place_of_birth_calibration, wt = weights))

age_place_of_birth_calibrated_resamples_to_check_tidy <- map2(.x = age_place_of_birth_calibrated_resamples_to_check,
                                                              .y = lengths_resamples_to_check,
                                                              .f = function(x, y){
                                                                
                                                                x %>%
                                                                  spread(key = "age_place_of_birth_calibration",
                                                                         value = "n") %>%
                                                                  bind_cols(resample_length = y, .)
                                                                
                                                              }) %>%
  bind_rows() %>%
  bind_cols(resample = resamples_to_check_index) %>%
  select(resample, everything()) %>% 
  gather(key = "category", value = "resample_value", -resample, -resample_length)

age_place_of_birth_population_to_check <- population_margins_age_place_of_birth_checks %>%
  gather(key = "category", value = "population_value", -resample_length) %>%
  mutate(category = str_replace(category, pattern = "_pop$", replacement = ""),
         resample_length = as.numeric(resample_length))

# Note: there are some differences in the 18_34_Other category after trimming weights. This happens for around 1% or resamples. 

age_place_of_birth_calibrated_resamples_to_check_tidy %>%
  left_join(age_place_of_birth_population_to_check, by = c("resample_length", "category")) %>%
  mutate(diff = population_value -resample_value) %>%
  filter(diff > 1) %>%
  arrange(resample, desc(diff)) 

rm(population_margins_first_language_checks, resamples_to_check_index, 
   lengths_resamples_to_check, resamples_sum_trimmed_weights, 
   weigths_summary, resamples_length, first_language_population_to_check, 
   check_language, check_language_summary)


# Export weights for resamples ----

## untrimmed weights

resamples_calibration_weights %>% 
  write_rds(here("interim_outputs", "calibration", "weights_resamples_untrimmed_03.rds"))

## first trimmed weights

resamples_calibration_weights_trimmed %>%
  write_rds(here("interim_outputs", "calibration", "weights_resamples_trimmed_1_03.rds"))

## final trimmed weights

resamples_calibration_weights_trimmed_2 %>%
  write_rds(here("interim_outputs", "calibration", "weights_resamples_trimmed_2_03.rds"))











