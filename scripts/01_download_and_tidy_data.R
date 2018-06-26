# Download and explore data

rm(list = ls())

library(sjlabelled)
library(here)
library(haven)
library(magrittr)
library(tidyverse)

# data obtained from:
#http://ceo.gencat.cat/ca/barometre/detall/index.html?id=6408
#http://ceo.gencat.cat/ca/barometre/matrius-fusionada-BOP/

# create folder structure

folder_directories <- list("data", c("data", "from_CEO"), c("data", "from_IDESCAT"), c("data", "subsets"),
  "interim_outputs", c("interim_outputs", "calibration"), c("interim_outputs", "calibration", "plots"), c("interim_outputs", "proportions_survey_863"),
  c("interim_outputs", "resample_survey_desings_863_bcn"), c("interim_outputs", "resamples_863_bcn"),
  c("interim_outputs", "jackknife"), c("interim_outputs", "estimates"),
  "other",
  "outputs", c("outputs", "all_categories"),
  "outputs", c("outputs", "plots"),
  "outputs", c("outputs", "tables"),
  "r_markdown"
  )

folder_directories %>%
  map(function(.x){
    
   if(length(.x) > 1){
          
         temp_dir  <- str_c(.x, collapse = "/")
        }else{
          temp_dir <- .x
        }
        
        if(!dir.exists(here(temp_dir))){
    
    dir.create(temp_dir)
    
  } 
  })

rm(folder_directories)


# Import data for all surveys----

## Dowlnoad and unzip raw files ---- 

if(!file.exists(here("data", "from_CEO", "survey_data_863.csv"))){

download.file("http://upceo.ceo.gencat.cat/wsceop/6408/Microdades%20anonimitzades%20cine-863%20%20csv.csv", 
              here("data", "from_CEO", "survey_data_863.csv"))

}


if(!file.exists(here("data", "from_CEO", "survey_data_all_since_2014.rar"))){
  
  download.file("http://ceo.gencat.cat/web/.content/20_barometre/Matrius_BOP/2014_Microdades_anonimitzades_fusio_cine_pres.rar", 
                here("data", "from_CEO", "survey_data_all_since_2014.rar"))
  
  #I had to manually decompress the file. 
}


# Read data ----

## Unlabeled version of the data
if(!file.exists(here("data", "survey_data_863_01.rds"))){

data_863 <- read_delim(here("data", "from_CEO", "survey_data_863.csv"), delim = ";")

data_863 %>%
  write_rds(here("data", "survey_data_863_01.rds"))

}else{
  
  data_863 <- read_rds(here("data", "survey_data_863_01.rds"))
  
}

## labeled version of the data
if(!file.exists(here("data", "all_surveys_01.rds"))){

data_all_surveys <- read_sav(here("data", "from_CEO",
                                  "Microdades anonimitzades fusio cine pres.sav"))

data_all_surveys %>%
  write_rds(here("data", "all_surveys_01.rds"))

}else{
  
  data_all_surveys <- read_rds(here("data", "all_surveys_01.rds"))
  
}

## ORDRE_CINE can be used as ID!

# Create a map of variables to labels ----

if(!file.exists(here("other", "variable_label_map_all_surveys.csv"))){

variable_label_map <- data_frame(variable = names(data_all_surveys),
                                 label = sjlabelled::get_label(data_all_surveys) )

variable_label_map %>%
  write_csv(here("other", "variable_label_map_all_surveys.csv"))

}else{
  variable_label_map <- read_csv(here("other", "variable_label_map_all_surveys.csv"))
}

# Recode variables ---- 

data_all_surveys %<>%
  mutate(age = recode(as_factor(GR_EDAT), 
                      `De 18 a 24 anys` = "18_24",
                      `De 25 a 34 anys` = "25_34",
                      `De 35 a 49 anys` = "35_49",
                      `De 50 a 64 anys` = "50_64",
                      `Mès de 64 anys` = "65+"),
         age_sex = if_else(SEXE == 1,
                           true = str_c("Male_", age),
                           false = str_c("Female_", age)),
         place_of_birth = recode(as_factor(C100), 
                                 "Catalunya" = "CAT",
                                 "Altres comunitats autònomes" = "ESP",
                                 .default = "Other"
         ),
         quota = str_c(age_sex, place_of_birth, sep = "_"),
         first_language = recode(as_factor(C705), 
                                 "Català (valencià / balear)" = "Catalan",
                                 "Castellà" = "Spanish",
                                 "Totes dues igual: català (valencià / balear) i castellà" = "Both",
                                 "Aranès" = "Aranese/Occitan",
                                 "Altres llengües o altres combinacions" = "Other",
                                 .default = "No answer"),
         first_language_calibration = recode(as_factor(C705), 
                                 "Català (valencià / balear)" = "Catalan",
                                 "Castellà" = "Spanish",
                                 .default = "Other"),
         referendum_participation = recode(as_factor(P82A),
                                           "No hi vaig participar ni votar perquè no vaig poder (feina, malaltia)" = "couldn't participate",
                                           "No hi vaig participar ni votar perquè no vaig voler" = "didn't want to participate",
                                           "No hi vaig participar ni votar perquè m’ho van impedir" = "they prevented resp to participate",
                                           "Estic segur/a que hi vaig participar i votar" = "voted",
                                           .default = "DK/NA" ),
         age_place_of_birth_calibration = str_c(if_else(age %in% c("18_24", "25_34"),
                                                  true = "18_34",
                                                  false = as.character(age)), place_of_birth, sep = "_") %>%
           if_else(. %in% c("50_64_Other","65+_Other"),
                   true = "50+_Other",
                   false = .),
         province = as_factor(PROVI),
         size_municipality = as_character(HABITAT) %>% 
           str_replace(pattern = " habitants", replacement = "") %>%
           factor(levels = get_labels(data_all_surveys$HABITAT) %>% str_replace(pattern = " habitants", replacement = ""), 
                     ordered = T))



# Explore quotas ----

## How were quotas assigned within stratums? 
## It's not so easy to say, but it looks like the only proper stratum was Province (province),
## Then they had quotas of size (size_municipality) and sex + gender + place of birth (crossed)

## email from CEO: 
#La selecció de la persona a entrevistar es fa en tres etapes:
# -	Etapa 1: Municipi mitjançant una selecció aleatòria dins de cada estat de la mostra. Aquesta selecció la fa l'empresa contractista.
# -	Etapa 2: seció censal. En funció del nombre d'enquestes a realitzar, es decideixen quantes rutes es realitzaran per municipi. Es fa una única ruta per secció censal. El punt de sortida de cada ruta és aleatrori a partir del directori de carrers.
# -	Etapa 3: individu. Un cop decidit el punt de sortida els enquestadors realitzen rutes aleatòries fins a completar la mostra seguint les quotes detallades a la fitxa tècnica. No s'utilitza cap llistat de domicilis. Els enquestadors es mouen per la secció censal amb bastanta llibertat fins a completar la mostra.

# A cada estrat de la mostra s'assignen sempre dos enquestadors diferents. 

# Pràcticament totes les variables de mostreig s'eliminen del fitxer final garantir la dissociació de les dades (LOPD). 

 

  ## Proportions by province and habitat
  ### Quotas don't seem to be allocated within crossed Province and size of municipality!

data_all_surveys %>%
  select(ANY, BOP_NUM, province, size_municipality, quota) %>%
  filter(ANY == 2017) %>%
  count(BOP_NUM, province, size_municipality, quota) %>%
  group_by(BOP_NUM, province, size_municipality) %>%
  mutate(prop_quota = n/sum(n)) %>%
  arrange(province, size_municipality, quota) %>%
  filter(province == 8) 

data_all_surveys %>%
  select(ANY, BOP_NUM, province, size_municipality, SEXE, GR_EDAT) %>%
  filter(ANY == 2017) %>%
  group_by(BOP_NUM, province, size_municipality) %>%
  summarise(n_female = sum(SEXE -1),
                           prop_female = mean(SEXE-1)) %>%
  arrange(province, size_municipality) %>%
  filter(province == 8)  

data_all_surveys %>%
  select(ANY, BOP_NUM, province, size_municipality, SEXE, GR_EDAT) %>%
  filter(ANY == 2017) %>%
  count(BOP_NUM, province, size_municipality, GR_EDAT) %>%
  group_by(BOP_NUM, province, size_municipality) %>%
  mutate(prop_age = n/sum(n)) %>%
  arrange(GR_EDAT) %>%
  filter(province == 8)

  ### Quotas were not allocated within size of municipality?
data_all_surveys %>%
  select(ANY, BOP_NUM, province, size_municipality, quota) %>%
  filter(ANY == 2017) %>%
  count(BOP_NUM, size_municipality, quota) %>%
  group_by(BOP_NUM, size_municipality) %>%
  mutate(prop_quota = n/sum(n)) %>%
  arrange(size_municipality, quota) 

data_all_surveys %>%
  select(ANY, BOP_NUM, province, size_municipality, SEXE, GR_EDAT) %>%
  filter(ANY == 2017) %>%
  group_by(BOP_NUM, size_municipality) %>%
  summarise(prop_female = mean(SEXE-1)) %>%
  arrange(size_municipality)

  ### Quotas were allocated within province only!

data_all_surveys %>%
  select(ANY, BOP_NUM, province, size_municipality, quota) %>%
  filter(ANY == 2017) %>%
  count(BOP_NUM, province, quota) %>%
  group_by(BOP_NUM, province) %>%
  mutate(prop_quota = n/sum(n)) %>%
  arrange(province, quota) 

data_all_surveys %>%
  select(ANY, BOP_NUM, province, size_municipality, SEXE, GR_EDAT) %>%
  filter(ANY == 2017) %>%
  group_by(BOP_NUM, province) %>%
  summarise(prop_female = mean(SEXE-1)) %>%
  arrange(province)

### size of municipality used only as another quota!

data_all_surveys %>%
  select(ANY, BOP_NUM, province, size_municipality, SEXE, GR_EDAT) %>%
  filter(ANY == 2017) %>%
  count(BOP_NUM, province, size_municipality) %>%
  arrange(province,size_municipality) 

# Explore PSU variables ----
# Counties and municipalities (IGNORE FOR NOW)
# I could do something like this:
##INTERVIEWER
##  - MUNICIPALITY
##      -- NUM *
##      -- NA	
##        --- size_municipality *

data_all_surveys %>% select(BOP_NUM, CODI_ENQ, size_municipality ,province, MUN, COMARCA)

data_all_surveys %>%
  count(MUN)

map_province_municipality <- data_all_surveys %>% select(province, MUN) %>% unique

# Explore weights ----
# Surveys 37 to 41 are unweighted.
# Previous ones are weighted by province.
# Before BOP 34, there's an extra weighting step.
# I'm not entirely sure what this extra step is, but it looks like a weight
# for place of birth of individual. 
# Looking at documentation from CEO's webpage I see they didn't use this
# variable in quotas in early surveys.

data_all_surveys %>% 
  select(BOP_NUM, province, PONDERA, POND_NAIX)

data_all_surveys %>% 
  filter(BOP_NUM == 33) %>%
  select(BOP_NUM, province, C100, PONDERA, POND_NAIX) 

data_all_surveys %>%
  filter(BOP_NUM %in% c(37:41)) %>%
  count(BOP_NUM, province, PONDERA) %>%
  arrange(province)


# BOP 42 has the particularity of being weighted by age x place of birth x first language

data_all_surveys %>%
  filter(BOP_NUM == 42) %>%
  select(province, age, place_of_birth, first_language, PONDERA) %>%
  arrange(age, place_of_birth, first_language) %>% 
  unique()


# Obtain labeled version of survey 863 ----

## Subset survey 42 (bar 863)

data_863_labelled <- data_all_surveys %>%
  filter(BOP_NUM == 42)


  ## Subset variables----

vars_not_in_863 <- names(data_863_labelled)[!names(data_863_labelled) %in% names(data_863)]
    

  ### Save large variables in a separate datafile

data_863_labelled %>%
  names() %>%
  str_extract("^[[:upper:]]{1}\\d{1,2}") %>% table() %>% sort

  ### QUESTION P1

data_863_labelled %>%
  select(ORDRE_CINE, starts_with("P1_")) %>%
  write_rds(here("data", "subsets", "data_863_labelled_P1_only.rds"))

data_863_labelled %<>%
  select(-starts_with("P1_"))


    ### Take out variables with only NAs
      # These might be variables which were applicable to other surveys. 
empty_vars_in_863<- data_863_labelled %>%
  map_lgl(~ .x %>% is.na %>% sum == 1338) %>% which()

data_863_labelled %<>%
  select(-empty_vars_in_863)

rm(vars_not_in_863)
  
    ### Variables with no variance  
vars_no_variance <- data_863_labelled %>%
  map_df(as.character) %>%
  map_lgl(~ .x %>% table %>% length() == 1) %>%
  which()
   
vars_no_variance

data_863_labelled %<>%
  select(-vars_no_variance) 

    ### P41 (knowledge and score of main political leaders)

data_863_labelled %>%
  select(ORDRE_CINE, starts_with("coneix_"), starts_with("val_")) %>%
  write_rds(here("data", "subsets", "data_863_labelled_P41_only.rds"))

data_863_labelled %<>%
  select(-starts_with("coneix_"), -starts_with("val_"))

data_863_labelled %>% names()

  ### P21 (confidence in institutions)

data_863_labelled %>%
  select(ORDRE_CINE, starts_with("P21")) %>%
  write_rds(here("data", "subsets", "data_863_labelled_P21_only.rds"))

data_863_labelled %<>%
  select(-starts_with("P21"))

# Check proportions in survey 863 ----

## unweighted
  ### there are some very small quotas

prop_province_munsize_863 <- data_863_labelled %>%
  count(province, size_municipality) %>%
  mutate(prop_863 = n/sum(n) )

prop_age_sex_pob_863 <- data_863_labelled %>%
  count(province, quota) %>%
  mutate(prop_863 = n/sum(n) )

## weigthed

prop_province_munsize_863_wt <- data_863_labelled %>%
  count(province, size_municipality, wt = PONDERA) %>%
  group_by(province) %>%
  mutate(prop_863_wt = n/sum(n) )

prop_age_sex_pob_863_wt <- data_863_labelled %>%
  count(province, quota, wt = PONDERA) %>%
  group_by(province) %>%
    mutate(prop_863_wt = n/sum(n) )


# Compare population proportions with those in survey 863 ----

  ## Compare which quota categories existed in previous surveys and in 863. Not done any more.
  ### from previous scripts:
    ### that's bad. Two quotas less in Girona, one less in Tarragona and one switching in Lleida
    ### I'll probably have to use BCN only.

  ### Make sure there are unique values of weights in each age x pob x language category
  ### i.e. each category in the weighting table has a unique weight

# Export----

## recoded surveys

data_all_surveys %>%
  write_rds(here("data", "all_surveys_recoded_01.rds"))

data_863_labelled %>%
  write_rds(here("data", "survey_data_863_recoded_01.rds"))

## proportions in survey 863 

## RDS

prop_province_munsize_863 %>%
  write_rds(here("interim_outputs", "proportions_survey_863", "proportion_province_munsize_863_01.rds"))

prop_age_sex_pob_863 %>%
  write_rds(here("interim_outputs", "proportions_survey_863", "proportion_age_sex_863_01.rds"))

## csv 

prop_province_munsize_863 %>%
  write_csv(here("interim_outputs", "proportions_survey_863", "proportion_province_munsize_863_01.csv"))

prop_age_sex_pob_863 %>%
  write_csv(here("interim_outputs", "proportions_survey_863", "proportion_age_sex_863_01.csv"))
