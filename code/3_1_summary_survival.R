# Load required libraries
library(tidyverse)
library(survival)
library(broom)
library(qs)
library(lubridate)

# Load the data
LL_flu <- readRDS("results/LL_flu_v3.rds")
LL_rsv <- readRDS("results/LL_rsv_v4.rds")
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

cat("=== SURVIVAL ANALYSIS SUMMARY STATISTICS FOR FIGURE 5 (figure6.png) ===\n\n")

# Prepare data for survival analysis
# Extract individual outbreak intervals
individual_intervals <- p_tab_1 %>% 
  pivot_longer(starts_with("outbreak_"), names_to = "threshold") %>% 
  filter(value == 1) %>% 
  group_by(threshold, path, grid, city) %>% 
  arrange(year) %>% 
  mutate(
    intervals = c(NA, diff(year))
  ) %>% 
  filter(!is.na(intervals)) %>% 
  ungroup() %>% 
  mutate(
    gap_years = intervals,
    event = 1,
    city = city,
    name = threshold
  ) %>% 
  filter(gap_years > 0)

# Add censored cases (no outbreaks)
censored_cases <- p_tab_1 %>% 
  pivot_longer(starts_with("outbreak_"), names_to = "threshold") %>% 
  group_by(threshold, path, grid, city) %>% 
  summarise(
    year_range = max(year) - min(year),
    has_outbreak = any(value == 1),
    .groups = "drop"
  ) %>% 
  filter(!has_outbreak) %>% 
  mutate(
    gap_years = year_range,
    event = 0,  # Censored
    city = city,
    name = threshold
  )

single_outbreak_cases <- p_tab_1 %>% 
  pivot_longer(starts_with("outbreak_"), names_to = "threshold") %>% 
  filter(threshold != "outbreak_7") %>%  # Exclude outbreak_7
  group_by(threshold, path, grid, city) %>% 
  summarise(
    n_outbreaks = sum(value == 1),
    year_range = max(year) - min(year),
    .groups = "drop"
  ) %>% 
  filter(n_outbreaks == 1) %>%  # Exactly 1 outbreak
  mutate(
    gap_years = year_range,
    event = 0,  # Treat as censored (no interval to calculate)
    city = city,
    name = threshold
  )

# Combine for survival analysis
df_s <- bind_rows(individual_intervals, censored_cases, single_outbreak_cases) %>% 
  dplyr::filter(threshold != "outbreak_7")

# KM fits by city × threshold
fit <- survfit(Surv(gap_years, event) ~ city + name, data = df_s, conf.int = 0.95)

# Get median with CI using quantile() - this is the proper way
median_with_ci <- map_dfr(names(fit$strata), function(stratum_name) {
  stratum_idx <- which(names(fit$strata) == stratum_name)
  stratum_fit <- fit[stratum_idx]
  
  # Get median with CI
  median_quantile <- tryCatch({
    quantile(stratum_fit, probs = 0.5, conf.int = TRUE)
  }, error = function(e) {
    # If median doesn't exist (never reaches 50%), return NA
    list(quantile = NA, lower = NA, upper = NA)
  })
  
  # Parse stratum name
  parts <- strsplit(stratum_name, ", ")[[1]]
  city_part <- sub("^city=", "", parts[1])
  name_part <- sub("^name=", "", parts[2])
  
  data.frame(
    city = city_part,
    name = trimws(name_part),
    median_time = as.numeric(median_quantile$quantile),
    ci_lower = as.numeric(median_quantile$lower),
    ci_upper = as.numeric(median_quantile$upper),
    stringsAsFactors = FALSE
  )
}) %>%
  mutate(
    city = factor(city, levels = rev(city_names)),
    name = factor(name, 
                  levels = c("outbreak_8", "outbreak_9", "outbreak_10"),
                  labels = c("Joint top 30 percentiles",
                             "Joint top 20 percentiles",
                             "Joint top 10 percentiles")),
    reaches_50 = !is.na(median_time)
  )

# Tidy for ggplot (still needed for other purposes)
km <- broom::tidy(fit) %>%
  separate(strata, into = c("city", "name"), sep = ", ?") %>%
  mutate(
    city = sub("^city=", "", city),
    name = sub("^name=", "", name),
    name = trimws(name)
  )  %>% 
  mutate(city = factor(city, levels = rev(city_names)),
         name = factor(name, 
                       levels = c("outbreak_8", "outbreak_9", "outbreak_10"),
                       labels = c("Joint top 30 percentiles",
                                  "Joint top 20 percentiles",
                                  "Joint top 10 percentiles")))

# Use the median_with_ci calculated from quantile() - this is the proper method
median_times_with_ci <- median_with_ci %>%
  select(city, name, median_time, ci_lower, ci_upper, reaches_50) %>%
  rename(ci_lower_cross = ci_lower, ci_upper_cross = ci_upper)

# All median times are already in median_times_with_ci (includes those that don't reach 50%)
all_median_times <- median_times_with_ci %>%
  select(city, name, median_time, ci_lower_cross, ci_upper_cross, reaches_50)

cat("=== X-INTERCEPTS OF DASHED VERTICAL LINES (MEDIAN TIME TO NEXT JOINT OUTBREAK) ===\n\n")
cat("These represent the time (in years) at which 50% of simulations have experienced their next joint outbreak.\n")
cat("Confidence intervals (95% CI) are shown in parentheses.\n\n")

cat("Median times by city and risk threshold (with 95% CI):\n")
print(all_median_times %>% 
        arrange(city, name) %>%
        mutate(
          time_display = ifelse(is.na(median_time), 
                                "Not reached", 
                                ifelse(is.na(ci_lower_cross) | is.na(ci_upper_cross),
                                       sprintf("%.1f years", median_time),
                                       sprintf("%.1f (%.1f--%.1f) years", 
                                               median_time, ci_lower_cross, ci_upper_cross)))
        ) %>%
        select(city, name, time_display, reaches_50))

cat("\n=== SUMMARY STATISTICS BY RISK THRESHOLD ===\n\n")

# Summary by threshold (excluding cities that never reach 50%)
for(threshold_name in c("Joint top 30 percentiles", "Joint top 20 percentiles", "Joint top 10 percentiles")) {
  threshold_data <- median_times_with_ci %>% 
    filter(name == threshold_name, reaches_50 == TRUE)
  
  cat(sprintf("--- %s ---\n", threshold_name))
  if(nrow(threshold_data) > 0) {
    cat(sprintf("Number of cities reaching 50%%: %d out of %d\n", 
                nrow(threshold_data), length(city_names)))
    cat(sprintf("Median time (years):\n"))
    cat(sprintf("  Mean: %.2f\n", mean(threshold_data$median_time, na.rm = TRUE)))
    cat(sprintf("  Median: %.2f\n", median(threshold_data$median_time, na.rm = TRUE)))
    cat(sprintf("  SD: %.2f\n", sd(threshold_data$median_time, na.rm = TRUE)))
    cat(sprintf("  Min: %.2f\n", min(threshold_data$median_time, na.rm = TRUE)))
    cat(sprintf("  Max: %.2f\n", max(threshold_data$median_time, na.rm = TRUE)))
    cat(sprintf("  Q25: %.2f\n", quantile(threshold_data$median_time, 0.25, na.rm = TRUE)))
    cat(sprintf("  Q75: %.2f\n", quantile(threshold_data$median_time, 0.75, na.rm = TRUE)))
    
    cat("\nCities that never reach 50%:\n")
    never_reach <- all_median_times %>% filter(name == threshold_name, !reaches_50)
    if(nrow(never_reach) > 0) {
      cat(paste(unique(never_reach$city), collapse = ", "), "\n")
    } else {
      cat("None\n")
    }
  } else {
    cat("No cities reached 50% for this threshold.\n")
  }
  cat("\n")
}

cat("=== SUMMARY STATISTICS BY CITY ===\n\n")

# Summary by city
for(city_name in city_names) {
  city_data <- median_times_with_ci %>% 
    filter(city == city_name, reaches_50 == TRUE)
  
  cat(sprintf("--- %s ---\n", city_name))
  if(nrow(city_data) > 0) {
    cat("Median times by threshold (with 95% CI):\n")
    print(city_data %>% 
            select(name, median_time, ci_lower_cross, ci_upper_cross) %>%
            arrange(name) %>%
            mutate(
              time_display = ifelse(is.na(ci_lower_cross) | is.na(ci_upper_cross),
                                    sprintf("%.1f years", median_time),
                                    sprintf("%.1f (%.1f--%.1f) years", 
                                            median_time, ci_lower_cross, ci_upper_cross))
            ) %>%
            select(name, time_display))
  } else {
    cat("No thresholds reached 50% for this city.\n")
  }
  
  never_reach_city <- all_median_times %>% filter(city == city_name, !reaches_50)
  if(nrow(never_reach_city) > 0) {
    cat("Thresholds that never reach 50%:\n")
    cat(paste(unique(never_reach_city$name), collapse = ", "), "\n")
  }
  cat("\n")
}

cat("=== COMPLETE TABLE OF X-INTERCEPTS (WITH 95% CI) ===\n\n")
complete_table <- all_median_times %>%
  arrange(city, name) %>%
  mutate(
    x_intercept_years = ifelse(is.na(median_time), NA, round(median_time, 1)),
    ci_lower = ifelse(is.na(ci_lower_cross), NA, round(ci_lower_cross, 1)),
    ci_upper = ifelse(is.na(ci_upper_cross), NA, round(ci_upper_cross, 1)),
    status = ifelse(reaches_50, "Reached 50%", "Never reached 50%"),
    value_with_ci = ifelse(is.na(median_time), 
                           "---",
                           ifelse(is.na(ci_lower) | is.na(ci_upper),
                                  sprintf("%.1f", median_time),
                                  sprintf("%.1f (%.1f--%.1f)", median_time, ci_lower, ci_upper)))
  ) %>%
  select(city, name, x_intercept_years, ci_lower, ci_upper, value_with_ci, status)

print(complete_table)

cat("\n=== ANALYSIS COMPLETE ===\n")

