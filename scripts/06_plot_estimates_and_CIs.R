# 06_plot_estimates_and_CIs

rm(list = ls())

options(scipen = 999999)

library(plotly)
library(here)
library(magrittr)
library(tidyverse)
library(RColorBrewer)


qual_col_pals <- brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector <- unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

# Import data ----

summary_estimates <- read_rds(here("outputs", "vote_estimates_05.rds"))

confidence_intervals <- read_rds(here("outputs", "vote_confidence_intervals_05.rds"))

# Create graphs ----

# ** tidy data ---- 

lower_ <- confidence_intervals %>% 
  select(matches(".L95")) %>%
  gather(key = "type", value = "ci_lower") %>%
  mutate(type = str_extract(type, pattern = "^[[:alpha:]]+"))

upper_ <- confidence_intervals %>% 
  select(matches(".U95")) %>%
  gather(key = "type", value = "ci_upper") %>%
  mutate(type = str_extract(type, pattern = "^[[:alpha:]]+"))

data_plots_ci <- lower_ %>%
  left_join(upper_, by = "type")

data_plots_ci$point_estimate <- as.numeric(rep(summary_estimates[["Actual est."]], nrow(lower_)))

data_plots_ci %<>%
  mutate(type = as_factor(type))


# ** make plot ----

#"SRS"
#Standard Percentile Intervals 
#"Adjusted percentile intervals"

p_1 <- ggplot(data_plots_ci, aes(x=type, y=round(point_estimate, 3))) +
    geom_errorbar(aes(ymin=ci_lower, ymax=ci_upper), width=.2, 
                col = col_vector[[1]],
                size = 1)+
  geom_point(col = col_vector[[3]], size = 1.9) +
  theme_dark() +
  labs(title = "Proportion of respondents that voted in the 1st-October referendum",
    x = "Episode", y = "% respondents") 
  
p_1

# Export plot ----

p_1 %>%
  write_rds(here("outputs", "plots", "vote_estimates_intervals.rds"))

