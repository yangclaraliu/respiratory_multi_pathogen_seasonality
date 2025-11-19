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

# Overall comparison across all cities (pooling North and South)
cat("\nOverall city-level comparison (Influenza vs RSV, all 8 cities):\n")
overall_disease_test <- t.test(city_mean_cycle ~ disease, data = city_test_data)
print(overall_disease_test)

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

# Test all pathogen x direction combinations (CITY-LEVEL)
cat("\n=== PAIRWISE COMPARISONS: ALL PATHOGEN x DIRECTION COMBINATIONS (CITY-LEVEL) ===\n")

# First, aggregate to city-level means (one value per city per disease)
city_level_data <- p5_tab_comparable %>%
  group_by(disease, direction, city) %>%
  summarise(
    city_mean_cycle = mean(step_size2_standardised, na.rm = TRUE),
    city_n_models = n(),
    .groups = "drop"
  )

# Get means for each combination (city-level)
combination_means_city <- city_level_data %>%
  group_by(disease, direction) %>%
  summarise(
    mean_cycle = mean(city_mean_cycle, na.rm = TRUE),
    median_cycle = median(city_mean_cycle, na.rm = TRUE),
    sd_cycle = sd(city_mean_cycle, na.rm = TRUE),
    n_cities = n(),
    .groups = "drop"
  ) %>%
  arrange(disease, direction)

cat("\nCity-level means by combination:\n")
print(combination_means_city)

# Test 1: Influenza North vs Influenza South (city-level)
cat("\n--- Test 1: Influenza North vs Influenza South (city-level) ---\n")
flu_north_cities <- city_level_data %>% 
  filter(disease == "Influenza", direction == "North") %>% 
  pull(city_mean_cycle)
flu_south_cities <- city_level_data %>% 
  filter(disease == "Influenza", direction == "South") %>% 
  pull(city_mean_cycle)

if(length(flu_north_cities) > 0 && length(flu_south_cities) > 0) {
  test1 <- t.test(flu_north_cities, flu_south_cities)
  print(test1)
  cat("Significant:", ifelse(test1$p.value < 0.05, "YES", "NO"), 
      "(p =", round(test1$p.value, 4), ")\n")
  cat("North cities:", length(flu_north_cities), ", South cities:", length(flu_south_cities), "\n")
} else {
  cat("Insufficient data\n")
}

# Test 2: RSV North vs RSV South (city-level)
cat("\n--- Test 2: RSV North vs RSV South (city-level) ---\n")
rsv_north_cities <- city_level_data %>% 
  filter(disease == "RSV", direction == "North") %>% 
  pull(city_mean_cycle)
rsv_south_cities <- city_level_data %>% 
  filter(disease == "RSV", direction == "South") %>% 
  pull(city_mean_cycle)

if(length(rsv_north_cities) > 0 && length(rsv_south_cities) > 0) {
  test2 <- t.test(rsv_north_cities, rsv_south_cities)
  print(test2)
  cat("Significant:", ifelse(test2$p.value < 0.05, "YES", "NO"), 
      "(p =", round(test2$p.value, 4), ")\n")
  cat("North cities:", length(rsv_north_cities), ", South cities:", length(rsv_south_cities), "\n")
} else {
  cat("Insufficient data\n")
}

# Test 3: North Influenza vs North RSV (city-level)
cat("\n--- Test 3: North Influenza vs North RSV (city-level) ---\n")
north_flu_cities <- city_level_data %>% 
  filter(direction == "North", disease == "Influenza") %>% 
  pull(city_mean_cycle)
north_rsv_cities <- city_level_data %>% 
  filter(direction == "North", disease == "RSV") %>% 
  pull(city_mean_cycle)

if(length(north_flu_cities) > 0 && length(north_rsv_cities) > 0) {
  test3 <- t.test(north_flu_cities, north_rsv_cities)
  print(test3)
  cat("Significant:", ifelse(test3$p.value < 0.05, "YES", "NO"), 
      "(p =", round(test3$p.value, 4), ")\n")
  cat("Influenza cities:", length(north_flu_cities), ", RSV cities:", length(north_rsv_cities), "\n")
} else {
  cat("Insufficient data\n")
}

# Test 4: South Influenza vs South RSV (city-level)
cat("\n--- Test 4: South Influenza vs South RSV (city-level) ---\n")
south_flu_cities <- city_level_data %>% 
  filter(direction == "South", disease == "Influenza") %>% 
  pull(city_mean_cycle)
south_rsv_cities <- city_level_data %>% 
  filter(direction == "South", disease == "RSV") %>% 
  pull(city_mean_cycle)

if(length(south_flu_cities) > 0 && length(south_rsv_cities) > 0) {
  test4 <- t.test(south_flu_cities, south_rsv_cities)
  print(test4)
  cat("Significant:", ifelse(test4$p.value < 0.05, "YES", "NO"), 
      "(p =", round(test4$p.value, 4), ")\n")
  cat("Influenza cities:", length(south_flu_cities), ", RSV cities:", length(south_rsv_cities), "\n")
} else {
  cat("Insufficient data\n")
}

# Summary table of all pairwise comparisons (city-level)
cat("\n=== SUMMARY OF ALL PAIRWISE COMPARISONS (CITY-LEVEL) ===\n")
comparison_summary <- data.frame(
  Comparison = c(
    "Influenza: North vs South",
    "RSV: North vs South",
    "North: Influenza vs RSV",
    "South: Influenza vs RSV"
  ),
  Mean_Diff = c(
    mean(flu_north_cities, na.rm = TRUE) - mean(flu_south_cities, na.rm = TRUE),
    mean(rsv_north_cities, na.rm = TRUE) - mean(rsv_south_cities, na.rm = TRUE),
    mean(north_flu_cities, na.rm = TRUE) - mean(north_rsv_cities, na.rm = TRUE),
    mean(south_flu_cities, na.rm = TRUE) - mean(south_rsv_cities, na.rm = TRUE)
  ),
  P_value = c(
    ifelse(length(flu_north_cities) > 0 && length(flu_south_cities) > 0, 
           t.test(flu_north_cities, flu_south_cities)$p.value, NA),
    ifelse(length(rsv_north_cities) > 0 && length(rsv_south_cities) > 0, 
           t.test(rsv_north_cities, rsv_south_cities)$p.value, NA),
    ifelse(length(north_flu_cities) > 0 && length(north_rsv_cities) > 0, 
           t.test(north_flu_cities, north_rsv_cities)$p.value, NA),
    ifelse(length(south_flu_cities) > 0 && length(south_rsv_cities) > 0, 
           t.test(south_flu_cities, south_rsv_cities)$p.value, NA)
  )
) %>%
  mutate(
    Significant = ifelse(P_value < 0.05, "YES", "NO"),
    P_value = round(P_value, 4)
  )

print(comparison_summary)

# Pairwise city comparisons (comparing models between cities)
cat("\n=== PAIRWISE CITY COMPARISONS (MODEL-LEVEL) ===\n")

# Get summary of models per city
city_summary <- p5_tab_comparable %>%
  group_by(disease, city, direction) %>%
  summarise(
    city_mean_cycle = mean(step_size2_standardised, na.rm = TRUE),
    city_n_models = n(),
    .groups = "drop"
  ) %>%
  arrange(disease, city)

cat("\nCity summary (mean cycle length and number of models):\n")
print(city_summary)

# Function to compare two cities for a given disease (using all models)
compare_cities <- function(disease_name, city1, city2, data) {
  city1_data <- data %>% 
    filter(disease == disease_name, city == city1) %>% 
    pull(step_size2_standardised) %>%
    na.omit()
  city2_data <- data %>% 
    filter(disease == disease_name, city == city2) %>% 
    pull(step_size2_standardised) %>%
    na.omit()
  
  if(length(city1_data) > 1 && length(city2_data) > 1) {
    tryCatch({
      test_result <- t.test(city1_data, city2_data)
      return(list(
        city1 = city1,
        city2 = city2,
        city1_mean = mean(city1_data, na.rm = TRUE),
        city2_mean = mean(city2_data, na.rm = TRUE),
        city1_n = length(city1_data),
        city2_n = length(city2_data),
        mean_diff = mean(city1_data, na.rm = TRUE) - mean(city2_data, na.rm = TRUE),
        p_value = test_result$p.value,
        significant = test_result$p.value < 0.05
      ))
    }, error = function(e) {
      return(NULL)
    })
  } else {
    return(NULL)
  }
}

# Specific comparisons: Beijing vs Wuhan
cat("\n--- Beijing vs Wuhan (Influenza) ---\n")
beijing_wuhan_flu <- compare_cities("Influenza", "Beijing", "Wuhan", p5_tab_comparable)
if(!is.null(beijing_wuhan_flu)) {
  cat("Beijing: mean =", round(beijing_wuhan_flu$city1_mean, 2), "months, n =", beijing_wuhan_flu$city1_n, "models\n")
  cat("Wuhan: mean =", round(beijing_wuhan_flu$city2_mean, 2), "months, n =", beijing_wuhan_flu$city2_n, "models\n")
  cat("Mean difference:", round(beijing_wuhan_flu$mean_diff, 2), "months\n")
  beijing_flu <- p5_tab_comparable %>% 
    filter(disease == "Influenza", city == "Beijing") %>% 
    pull(step_size2_standardised)
  wuhan_flu <- p5_tab_comparable %>% 
    filter(disease == "Influenza", city == "Wuhan") %>% 
    pull(step_size2_standardised)
  if(length(beijing_flu) > 0 && length(wuhan_flu) > 0) {
    test_result <- t.test(beijing_flu, wuhan_flu)
    print(test_result)
    cat("P-value:", round(test_result$p.value, 4), "\n")
    cat("Significant:", ifelse(test_result$p.value < 0.05, "YES", "NO"), "\n")
  }
} else {
  cat("Insufficient data\n")
}

cat("\n--- Beijing vs Wuhan (RSV) ---\n")
beijing_wuhan_rsv <- compare_cities("RSV", "Beijing", "Wuhan", p5_tab_comparable)
if(!is.null(beijing_wuhan_rsv)) {
  cat("Beijing: mean =", round(beijing_wuhan_rsv$city1_mean, 2), "months, n =", beijing_wuhan_rsv$city1_n, "models\n")
  cat("Wuhan: mean =", round(beijing_wuhan_rsv$city2_mean, 2), "months, n =", beijing_wuhan_rsv$city2_n, "models\n")
  cat("Mean difference:", round(beijing_wuhan_rsv$mean_diff, 2), "months\n")
  beijing_rsv <- p5_tab_comparable %>% 
    filter(disease == "RSV", city == "Beijing") %>% 
    pull(step_size2_standardised)
  wuhan_rsv <- p5_tab_comparable %>% 
    filter(disease == "RSV", city == "Wuhan") %>% 
    pull(step_size2_standardised)
  if(length(beijing_rsv) > 0 && length(wuhan_rsv) > 0) {
    test_result <- t.test(beijing_rsv, wuhan_rsv)
    print(test_result)
    cat("P-value:", round(test_result$p.value, 4), "\n")
    cat("Significant:", ifelse(test_result$p.value < 0.05, "YES", "NO"), "\n")
  }
} else {
  cat("Insufficient data\n")
}

# All pairwise city comparisons for each disease
cat("\n=== ALL PAIRWISE CITY COMPARISONS BY DISEASE ===\n")

for(this_disease in c("Influenza", "RSV")) {
  cat("\n---", this_disease, "---\n")
  disease_cities <- p5_tab_comparable %>% 
    filter(disease == this_disease) %>% 
    pull(city) %>% 
    unique()
  
  if(length(disease_cities) > 1) {
    # Create all pairwise combinations
    city_pairs <- combn(disease_cities, 2, simplify = FALSE)
    
    comparison_results <- map_dfr(city_pairs, function(pair) {
      city1 <- pair[1]
      city2 <- pair[2]
      result <- compare_cities(this_disease, city1, city2, p5_tab_comparable)
      if(!is.null(result)) {
        return(data.frame(
          Disease = this_disease,
          City1 = city1,
          City2 = city2,
          City1_Mean = result$city1_mean,
          City2_Mean = result$city2_mean,
          City1_N = result$city1_n,
          City2_N = result$city2_n,
          Mean_Diff = result$mean_diff,
          P_value = result$p_value,
          Significant = result$significant,
          stringsAsFactors = FALSE
        ))
      } else {
        return(NULL)
      }
    })
    
    if(nrow(comparison_results) > 0) {
      comparison_results <- comparison_results %>%
        mutate(
          P_value = round(P_value, 4),
          Mean_Diff = round(Mean_Diff, 2),
          City1_Mean = round(City1_Mean, 2),
          City2_Mean = round(City2_Mean, 2)
        ) %>%
        arrange(P_value)
      print(comparison_results)
    }
  }
}

cat("\n=== ANALYSIS COMPLETE ===\n")

# Figure 4: Median probability of joint outbreaks by threshold and by city
cat("\n=== FIGURE 4: MEDIAN PROBABILITY OF JOINT OUTBREAKS BY THRESHOLD AND CITY ===\n")

# Load required libraries for joint outbreak analysis
library(qs)
library(lubridate)

# Load the simulation results
if(file.exists("results/res_flu.qs") && file.exists("results/res_rsv.qs")) {
  res_flu <- qread("results/res_flu.qs")
  res_rsv <- qread("results/res_rsv.qs")
  
  city_names <- sapply(1:8, function(x) unique(LL_flu[[x]]$city))
  
  # Define risk thresholds (percentiles)
  risk_def_flu <- lapply(1:8, function(x) quantile(exp(res_flu[[x]][[1]]$original$value), seq(0,1,0.1))) 
  risk_def_rsv <- lapply(1:8, function(x) quantile(exp(res_rsv[[x]][[1]]$original$value), seq(0,1,0.1))) 
  
  for(i in 1:8){
    risk_def_flu[[i]][1] <- risk_def_rsv[[i]][1] <- -Inf
    risk_def_flu[[i]][length(risk_def_flu[[i]])] <- risk_def_rsv[[i]][length(risk_def_rsv[[i]])] <- Inf
  }
  
  # Merge time series between flu and rsv
  p_tab <- list()
  
  for(i in 1:length(city_names)){
    risk_def_flu[[i]] <- risk_def_flu[[i]] + cumsum(c(0, diff(risk_def_flu[[i]]) == 0) * 1e-8)
    risk_def_rsv[[i]] <- risk_def_rsv[[i]] + cumsum(c(0, diff(risk_def_rsv[[i]]) == 0) * 1e-8)
    
    res_flu[[i]][[1]]$stochastic_forecast %>% 
      mutate(risk_cat_flu = cut(exp(value), breaks = risk_def_flu[[i]], labels = 1:10)) %>% 
      dplyr::select(-date_start, -time) %>% 
      rename(flu = value) %>%
      left_join(res_rsv[[i]][[1]]$stochastic_forecast %>% 
                  mutate(risk_cat_rsv = cut(exp(value), breaks = risk_def_rsv[[i]], labels = 1:10)) %>% 
                  rename(rsv = value) %>% 
                  dplyr::select(-date_start, -time),
                by = c("path",  "date")) %>%
      .[complete.cases(.),] %>% 
      mutate(risk_cat_rsv_num = as.numeric(risk_cat_rsv),
             risk_cat_flu_num = as.numeric(risk_cat_flu),
             year = year(date),
             outbreak_8 = risk_cat_rsv_num >= 8 & risk_cat_flu_num >= 8,
             outbreak_9 = risk_cat_rsv_num >= 9 & risk_cat_flu_num >= 9,
             outbreak_10 = risk_cat_rsv_num >= 10 & risk_cat_flu_num >= 10) %>% 
      group_by(year, path) %>% 
      summarise(outbreak_8 = sum(outbreak_8),
                outbreak_9 = sum(outbreak_9),
                outbreak_10 = sum(outbreak_10),
                .groups = "drop") %>% 
      mutate(grid = parse_number(path),
             city = city_names[[i]]) -> p_tab[[i]]
  }
  
  # Calculate proportion of joint outbreaks per path
  p_tab %>% 
    bind_rows() %>% 
    group_by(path, grid, city) %>% 
    mutate_at(vars(starts_with("outbreak_")),
              ~replace(.>0,1,0)) -> p_tab_1
    
  p_tab_1 %>% 
    group_by(path, city) %>% 
    summarise(outbreak_8 = sum(outbreak_8), 
              outbreak_9 = sum(outbreak_9),
              outbreak_10 = sum(outbreak_10),
              year_tot = n(),
              .groups = "keep") %>% 
    mutate_at(vars(starts_with("outbreak_")),
              ~./year_tot) %>% 
    pivot_longer(cols = starts_with("outbreak")) %>% 
    mutate(city = factor(city, levels = city_names),
           threshold = factor(name, 
                              levels = c("outbreak_8", "outbreak_9", "outbreak_10"),
                              labels = c("Joint top 30 percentiles",
                                         "Joint top 20 percentiles",
                                         "Joint top 10 percentiles"))) -> p_tab_2
  
  # Calculate medians by threshold and city
  median_joint_outbreaks <- p_tab_2 %>%
    group_by(threshold, city) %>%
    summarise(
      median_proportion = median(value, na.rm = TRUE),
      mean_proportion = mean(value, na.rm = TRUE),
      q25 = quantile(value, 0.25, na.rm = TRUE),
      q75 = quantile(value, 0.75, na.rm = TRUE),
      n_paths = n(),
      .groups = "drop"
    ) %>%
    arrange(threshold, city)
  
  cat("\nMedian probability of joint outbreaks by threshold and city:\n")
  print(median_joint_outbreaks)
  
  # Summary by threshold (across all cities)
  cat("\n=== SUMMARY BY THRESHOLD (ACROSS ALL CITIES) ===\n")
  summary_by_threshold <- median_joint_outbreaks %>%
    group_by(threshold) %>%
    summarise(
      median_proportion = median(median_proportion, na.rm = TRUE),
      mean_proportion = mean(mean_proportion, na.rm = TRUE),
      q25 = quantile(median_proportion, 0.25, na.rm = TRUE),
      q75 = quantile(median_proportion, 0.75, na.rm = TRUE),
      n_cities = n(),
      .groups = "drop"
    ) %>%
    arrange(threshold)
  
  print(summary_by_threshold)
  
  cat("\n=== FIGURE 4 ANALYSIS COMPLETE ===\n")
  
} else {
  cat("Warning: Joint outbreak simulation results not found.\n")
  cat("Files 'results/res_flu.qs' and 'results/res_rsv.qs' are required for Figure 4 analysis.\n")
  cat("Skipping Figure 4 analysis.\n")
}
