# 07_plot_estimates_and_CIs

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

summary_estimates <- read_rds(here("outputs", "vote_estimates_06.rds"))

confidence_intervals <- read_rds(here("outputs", "vote_confidence_intervals_06.rds"))

comparison_se_designs <- read_rds(here("outputs", "comparison_bootstrap_designs_06.rds"))

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
    geom_errorbar(aes(ymin=ci_lower, ymax=ci_upper), width=.5, 
                col = col_vector[[1]],
                size = 3)+
  geom_point(col = col_vector[[3]], size = 3) +
  scale_y_continuous(labels = scales::percent, limits = c(0.5,0.6)) +
  theme_dark() +
  labs(title = "Figure 1: Proportion of respondents that voted in the 1st-October referendum",
       subtitle = "With 95% confidence intervals",
    x = "Method for variance estimation", y = "% respondents") 
  
p_1

## ** data for summary table ----

t_1 <- data_plots_ci %>%
  select(-point_estimate)

names(t_1) <-  c("Method", "Lower bound CI", "Upper bound CI")


# Comparison of bootstrap designs ----

p_2 <- comparison_se_designs %>%
  gather(key = "design", value = "estimate", -weights) %>%
  ggplot(aes(x = estimate, col = design, group = design)) +
  geom_density() +
  facet_wrap(~weights, nrow = 2)



# Export plot and table ----

p_1 %>%
  write_rds(here("outputs", "plots", "plot_vote_estimates_intervals_07.rds"))

p_2 %>%
  write_rds(here("outputs", "plots", "plot_comparison_bootstrap_designs_07.rds"))

t_1 %>%
  write_rds(here("outputs", "tables", "table_vote_estimates_intervals_07.rds"))
