# Load required libraries
library(tidyverse)
library(cowplot)
library(ggplot2)
library(car)  # For Levene's test

# Load the data
LL_flu <- readRDS("results/LL_flu_v3.rds")
LL_rsv <- readRDS("results/LL_rsv_v4.rds")

# Define city order
city_ordered <- c("Beijing", "Lanzhou", "Xian", "Suzhou", "Wuhan", "Wenzhou", "Guangzhou", "Yunfu")

# MODIFIED FUNCTION: Use comparable models instead of top x models
get_comparable <- function(){
  
  # Process flu data
  flu_stl_all <- LL_flu %>% 
    bind_rows() %>% 
    dplyr::filter(model == "stl") %>% 
    group_by(city) %>% 
    mutate(metric_optimal = min(distribution_specific_AIC),
           metric_similar = distribution_specific_AIC <= (metric_optimal + 10),
           step_size_standardised = case_when(step_size_char == "daily" ~ length_cycle1/30,
                                              step_size_char == "weekly" ~ length_cycle1/4,
                                              step_size_char == "monthly" ~ length_cycle1)) %>%
    dplyr::filter(metric_similar == T) %>%
    mutate(city = factor(city, levels = city_ordered),
           cycle1_selected = step_size_standardised)
  
  flu_mstl_all <- LL_flu %>% 
    bind_rows() %>% 
    dplyr::filter(model == "mstl") %>% 
    group_by(city) %>% 
    mutate(metric_optimal = min(distribution_specific_AIC),
           metric_similar = distribution_specific_AIC < (metric_optimal + 10),
           step_size1_standardised = case_when(step_size_char == "daily" ~ length_cycle1/30,
                                              step_size_char == "weekly" ~ length_cycle1/4,
                                              step_size_char == "monthly" ~ length_cycle1),
           step_size2_standardised = case_when(step_size_char == "daily" ~ length_cycle2/30,
                                               step_size_char == "weekly" ~ length_cycle2/4,
                                               step_size_char == "monthly" ~ length_cycle2)) %>%
    dplyr::filter(metric_similar == T)
  
  # Process RSV data
  rsv_stl_all <- LL_rsv %>% 
    bind_rows() %>% 
    dplyr::filter(model == "stl") %>% 
    group_by(city) %>% 
    mutate(metric_optimal = min(distribution_specific_AIC),
           metric_similar = distribution_specific_AIC < (metric_optimal + 10),
           step_size_standardised = case_when(step_size_char == "daily" ~ length_cycle1/30,
                                              step_size_char == "weekly" ~ length_cycle1/4,
                                              step_size_char == "monthly" ~ length_cycle1)) %>%
    dplyr::filter(metric_similar == T) %>% 
    mutate(city = factor(city, levels = city_ordered),
           cycle1_selected = step_size_standardised)
  
  rsv_mstl_all <- LL_rsv %>% 
    bind_rows() %>% 
    dplyr::filter(model == "mstl") %>% 
    group_by(city) %>% 
    mutate(metric_optimal = min(distribution_specific_AIC),
           metric_similar = distribution_specific_AIC < (metric_optimal + 10),
           step_size1_standardised = case_when(step_size_char == "daily" ~ length_cycle1/30,
                                              step_size_char == "weekly" ~ length_cycle1/4,
                                              step_size_char == "monthly" ~ length_cycle1),
           step_size2_standardised = case_when(step_size_char == "daily" ~ length_cycle2/30,
                                               step_size_char == "weekly" ~ length_cycle2/4,
                                               step_size_char == "monthly" ~ length_cycle2)) %>%
    dplyr::filter(metric_similar == T)
  
  # Combine flu data
  flu_stl_all %>% bind_rows(flu_mstl_all) %>% 
    group_by(city) %>% group_split() %>% 
    map(mutate,
        rank_aic = rank(distribution_specific_AIC),
        rank_bic = rank(distribution_specific_BIC)) %>% 
    map(dplyr::select,
        length_cycle1, length_cycle2, model,
        distribution_specific_AIC,
        distribution_specific_BIC,
        rank_aic, rank_bic,
        step_size1_standardised,
        step_size2_standardised,
        city, metric_similar) %>% 
    map(dplyr::filter, metric_similar == TRUE) %>%  # Filter for comparable models only
    bind_rows() %>% 
    mutate(city = factor(city, levels = city_ordered),
           direction = if_else(city %in% c("Beijing", "Xian", "Lanzhou"),
                               "North",
                               "South"),
           disease = "Influenza") -> p_flu 
    
  # Combine RSV data
  rsv_stl_all %>% bind_rows(rsv_mstl_all) %>% 
    group_by(city) %>% group_split() %>% 
    map(mutate,
        rank_aic = rank(distribution_specific_AIC),
        rank_bic = rank(distribution_specific_BIC)) %>% 
    map(dplyr::select,
        length_cycle1, length_cycle2, model,
        distribution_specific_AIC,
        distribution_specific_BIC,
        rank_aic, rank_bic,
        step_size1_standardised,
        step_size2_standardised,
        city, metric_similar) %>% 
    map(dplyr::filter, metric_similar == TRUE) %>%  # Filter for comparable models only
    bind_rows() %>% 
    mutate(city = factor(city, levels = city_ordered),
           direction = if_else(city %in% c("Beijing", "Xian", "Lanzhou"),
                               "North",
                               "South"),
           disease = "RSV") -> p_rsv
  
  bind_rows(p_flu, p_rsv) -> tmp
  
  return(tmp)
}

# Run the analysis
cat("=== COMPARABLE MODELS ANALYSIS ===\n")
p5_tab_comparable <- get_comparable() %>% mutate(analysis = "Comparable Models")

# Calculate mean and standard deviation of between-year cycles for all comparable models
cat("\n=== SUMMARY BY DISEASE AND DIRECTION ===\n")
p5_tab_comparable %>% 
  group_by(disease, direction) %>% 
  summarise(
    mean_cycle2 = mean(step_size2_standardised, na.rm = TRUE),
    sd_cycle2 = sd(step_size2_standardised, na.rm = TRUE),
    n_models = n(),
    .groups = "drop"
  ) %>% 
  print()

# Two-way summary table (disease x direction) with additional quantiles
cat("\n=== EXTENDED SUMMARY (DISEASE x DIRECTION) ===\n")
p5_tab_comparable %>%
  group_by(disease, direction) %>%
  summarise(
    n_models = n(),
    mean_cycle = mean(step_size2_standardised, na.rm = TRUE),
    median_cycle = median(step_size2_standardised, na.rm = TRUE),
    sd_cycle = sd(step_size2_standardised, na.rm = TRUE),
    q25 = quantile(step_size2_standardised, 0.25, na.rm = TRUE),
    q75 = quantile(step_size2_standardised, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(disease, direction) %>%
  print()

# CITY-LEVEL AGGREGATION (to avoid overweighting cities with more models)
cat("\n=== CITY-LEVEL SUMMARY (DISEASE x DIRECTION) ===\n")
city_level_summary <- p5_tab_comparable %>%
  group_by(disease, direction, city) %>%
  summarise(
    city_mean_cycle = mean(step_size2_standardised, na.rm = TRUE),
    city_n_models = n(),
    .groups = "drop"
  ) %>%
  group_by(disease, direction) %>%
  summarise(
    n_cities = n(),
    mean_city_cycle = mean(city_mean_cycle, na.rm = TRUE),
    sd_city_cycle = sd(city_mean_cycle, na.rm = TRUE),
    median_city_cycle = median(city_mean_cycle, na.rm = TRUE),
    q25_city = quantile(city_mean_cycle, 0.25, na.rm = TRUE),
    q75_city = quantile(city_mean_cycle, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(disease, direction)

print(city_level_summary)

# CITY-LEVEL STATISTICAL TESTS
cat("\n=== CITY-LEVEL STATISTICAL TESTS ===\n")

# Prepare city-level data for testing
city_test_data <- p5_tab_comparable %>%
  group_by(disease, direction, city) %>%
  summarise(city_mean_cycle = mean(step_size2_standardised, na.rm = TRUE), .groups = "drop")

# Overall North vs South (city-level)
overall_city_test <- t.test(city_mean_cycle ~ direction, data = city_test_data)
cat("Overall city-level comparison (North vs South):\n")
print(overall_city_test)

# By pathogen (city-level)
for(this_disease in c("Influenza", "RSV")){
  disease_city_data <- city_test_data %>% filter(disease == this_disease)
  if(nrow(disease_city_data) > 0 && length(unique(disease_city_data$direction)) == 2){
    cat("\n", this_disease, " city-level comparison (North vs South):\n", sep = "")
    print(t.test(city_mean_cycle ~ direction, data = disease_city_data))
  }
}

# Within-region comparisons (city-level)
for(this_dir in c("North", "South")){
  dir_city_data <- city_test_data %>% filter(direction == this_dir)
  if(nrow(dir_city_data) > 0 && length(unique(dir_city_data$disease)) == 2){
    cat("\n", this_dir, " city-level comparison (Influenza vs RSV):\n", sep = "")
    print(t.test(city_mean_cycle ~ disease, data = dir_city_data))
  }
}

# Overall statistics (across all diseases and directions)
cat("\n=== OVERALL STATISTICS ===\n")
p5_tab_comparable %>% 
  summarise(
    overall_mean = mean(step_size2_standardised, na.rm = TRUE),
    overall_sd = sd(step_size2_standardised, na.rm = TRUE),
    total_models = n()
  ) %>% 
  print()

# Statistical comparison between North and South
cat("\n=== STATISTICAL COMPARISON: NORTH vs SOUTH ===\n")

# T-test for difference in means between North and South
north_south_test <- t.test(step_size2_standardised ~ direction, data = p5_tab_comparable)
print("T-test results (North vs South):")
print(north_south_test)

# Separate hypothesis testing for each pathogen
cat("\n=== HYPOTHESIS TESTING BY PATHOGEN ===\n")

# Influenza: North vs South
flu_data <- p5_tab_comparable %>% filter(disease == "Influenza")
if(nrow(flu_data) > 0) {
  flu_test <- t.test(step_size2_standardised ~ direction, data = flu_data)
  cat("\nInfluenza - North vs South:\n")
  print(flu_test)
  
  # Effect size for Influenza
  flu_north <- flu_data %>% filter(direction == "North") %>% pull(step_size2_standardised)
  flu_south <- flu_data %>% filter(direction == "South") %>% pull(step_size2_standardised)
  
  if(length(flu_north) > 0 && length(flu_south) > 0) {
    flu_cohens_d <- (mean(flu_north, na.rm = TRUE) - mean(flu_south, na.rm = TRUE)) / 
      sqrt(((length(flu_north) - 1) * var(flu_north, na.rm = TRUE) + 
            (length(flu_south) - 1) * var(flu_south, na.rm = TRUE)) / 
           (length(flu_north) + length(flu_south) - 2))
    
    cat("Influenza Cohen's d:", round(flu_cohens_d, 3), "\n")
    cat("Influenza effect size:", 
        ifelse(abs(flu_cohens_d) < 0.2, "negligible", 
               ifelse(abs(flu_cohens_d) < 0.5, "small", 
                      ifelse(abs(flu_cohens_d) < 0.8, "medium", "large"))), "\n")
  }
}

# RSV: North vs South
rsv_data <- p5_tab_comparable %>% filter(disease == "RSV")
if(nrow(rsv_data) > 0) {
  rsv_test <- t.test(step_size2_standardised ~ direction, data = rsv_data)
  cat("\nRSV - North vs South:\n")
  print(rsv_test)
  
  # Effect size for RSV
  rsv_north <- rsv_data %>% filter(direction == "North") %>% pull(step_size2_standardised)
  rsv_south <- rsv_data %>% filter(direction == "South") %>% pull(step_size2_standardised)
  
  if(length(rsv_north) > 0 && length(rsv_south) > 0) {
    rsv_cohens_d <- (mean(rsv_north, na.rm = TRUE) - mean(rsv_south, na.rm = TRUE)) / 
      sqrt(((length(rsv_north) - 1) * var(rsv_north, na.rm = TRUE) + 
            (length(rsv_south) - 1) * var(rsv_south, na.rm = TRUE)) / 
           (length(rsv_north) + length(rsv_south) - 2))
    
    cat("RSV Cohen's d:", round(rsv_cohens_d, 3), "\n")
    cat("RSV effect size:", 
        ifelse(abs(rsv_cohens_d) < 0.2, "negligible", 
               ifelse(abs(rsv_cohens_d) < 0.5, "small", 
                      ifelse(abs(rsv_cohens_d) < 0.8, "medium", "large"))), "\n")
  }
}

# Within-region comparisons: Influenza vs RSV for each direction
cat("\n=== WITHIN-REGION COMPARISONS (PATHOGENS) ===\n")

for(this_dir in c("North", "South")){
  dir_data <- p5_tab_comparable %>% filter(direction == this_dir)
  if(nrow(dir_data) > 0){
    cat("\n", this_dir, "- Influenza vs RSV:\n", sep = "")
    # Ensure disease is a factor with both levels present
    if(length(unique(dir_data$disease)) == 2){
      print(t.test(step_size2_standardised ~ disease, data = dir_data))
    } else {
      cat("Insufficient disease levels in", this_dir, "for comparison.\n")
    }
  }
}

# Two-way ANOVA (disease x direction) with interaction
cat("\n=== TWO-WAY ANOVA (DISEASE x DIRECTION) ===\n")
anova_data <- p5_tab_comparable %>%
  mutate(
    disease = factor(disease, levels = c("Influenza", "RSV")),
    direction = factor(direction, levels = c("North", "South"))
  ) %>%
  drop_na(step_size2_standardised)

if(nrow(anova_data) > 0){
  two_way <- aov(step_size2_standardised ~ disease * direction, data = anova_data)
  print(summary(two_way))
}

# Variance comparison tests
cat("\n=== VARIANCE COMPARISON TESTS ===\n")

# Influenza: Variance comparison North vs South
if(nrow(flu_data) > 0) {
  flu_north <- flu_data %>% filter(direction == "North") %>% pull(step_size2_standardised)
  flu_south <- flu_data %>% filter(direction == "South") %>% pull(step_size2_standardised)
  
  if(length(flu_north) > 1 && length(flu_south) > 1) {
    flu_var_test <- var.test(step_size2_standardised ~ direction, data = flu_data)
    cat("\nInfluenza - Variance comparison (F-test):\n")
    print(flu_var_test)
    
    # Levene's test for Influenza (more robust)
    flu_levene <- car::leveneTest(step_size2_standardised ~ direction, data = flu_data)
    cat("Influenza - Levene's test:\n")
    print(flu_levene)
  }
}

# RSV: Variance comparison North vs South
if(nrow(rsv_data) > 0) {
  rsv_north <- rsv_data %>% filter(direction == "North") %>% pull(step_size2_standardised)
  rsv_south <- rsv_data %>% filter(direction == "South") %>% pull(step_size2_standardised)
  
  if(length(rsv_north) > 1 && length(rsv_south) > 1) {
    rsv_var_test <- var.test(step_size2_standardised ~ direction, data = rsv_data)
    cat("\nRSV - Variance comparison (F-test):\n")
    print(rsv_var_test)
    
    # Levene's test for RSV (more robust)
    rsv_levene <- car::leveneTest(step_size2_standardised ~ direction, data = rsv_data)
    cat("RSV - Levene's test:\n")
    print(rsv_levene)
  }
}

# Overall variance comparison
overall_var_test <- var.test(step_size2_standardised ~ direction, data = p5_tab_comparable)
cat("\nOverall - Variance comparison (F-test):\n")
print(overall_var_test)

# Overall Levene's test
overall_levene <- car::leveneTest(step_size2_standardised ~ direction, data = p5_tab_comparable)
cat("Overall - Levene's test:\n")
print(overall_levene)

# Effect size (Cohen's d)
north_data <- p5_tab_comparable %>% filter(direction == "North") %>% pull(step_size2_standardised)
south_data <- p5_tab_comparable %>% filter(direction == "South") %>% pull(step_size2_standardised)

cohens_d <- (mean(north_data, na.rm = TRUE) - mean(south_data, na.rm = TRUE)) / 
  sqrt(((length(north_data) - 1) * var(north_data, na.rm = TRUE) + 
        (length(south_data) - 1) * var(south_data, na.rm = TRUE)) / 
       (length(north_data) + length(south_data) - 2))

cat("\nCohen's d (effect size):", round(cohens_d, 3), "\n")
cat("Interpretation: ", 
    ifelse(abs(cohens_d) < 0.2, "negligible", 
           ifelse(abs(cohens_d) < 0.5, "small", 
                  ifelse(abs(cohens_d) < 0.8, "medium", "large"))), "effect\n")

# Mann-Whitney U test (non-parametric alternative)
wilcox_test <- wilcox.test(step_size2_standardised ~ direction, data = p5_tab_comparable)
print("\nMann-Whitney U test (non-parametric):")
print(wilcox_test)

# Summary by direction
direction_summary <- p5_tab_comparable %>% 
  group_by(direction) %>% 
  summarise(
    n_models = n(),
    mean_cycle = mean(step_size2_standardised, na.rm = TRUE),
    median_cycle = median(step_size2_standardised, na.rm = TRUE),
    sd_cycle = sd(step_size2_standardised, na.rm = TRUE),
    q25 = quantile(step_size2_standardised, 0.25, na.rm = TRUE),
    q75 = quantile(step_size2_standardised, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

print("\nSummary by geographic direction:")
print(direction_summary)

cat("\n=== ANALYSIS COMPLETE ===\n")
