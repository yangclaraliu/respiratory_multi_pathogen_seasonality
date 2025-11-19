# Script to create table of optimal cycle lengths for supplemental material
library(tidyverse)

# Load the data
LL_flu <- readRDS("results/LL_flu_v3.rds")
LL_rsv <- readRDS("results/LL_rsv_v4.rds")

# Define city order
city_ordered <- c("Beijing", "Lanzhou", "Xian", "Suzhou", "Wuhan", "Wenzhou", "Guangzhou", "Yunfu")

# Function to extract optimal cycles
extract_optimal_cycles <- function(LL_data, disease_name) {
  # Get overall optimal AIC for each city (across all models)
  overall_optimal <- LL_data %>% 
    bind_rows() %>% 
    group_by(city) %>% 
    summarise(overall_optimal_aic = min(distribution_specific_AIC), .groups = "drop")
  
  # Get optimal within-year cycles (STL models)
  stl_optimal <- LL_data %>% 
    bind_rows() %>% 
    dplyr::filter(model == "stl") %>% 
    group_by(city) %>% 
    mutate(metric_optimal = min(distribution_specific_AIC),
           step_size_standardised = case_when(step_size_char == "daily" ~ length_cycle1/30,
                                              step_size_char == "weekly" ~ length_cycle1/4,
                                              step_size_char == "monthly" ~ length_cycle1)) %>%
    dplyr::filter(metric_optimal == distribution_specific_AIC) %>%
    dplyr::select(city, cycle1_optimal = step_size_standardised, stl_aic = distribution_specific_AIC) %>%
    distinct()
  
  # Get optimal between-year cycles (MSTL models)
  mstl_optimal <- LL_data %>% 
    bind_rows() %>% 
    dplyr::filter(model == "mstl") %>% 
    group_by(city) %>% 
    mutate(metric_optimal = min(distribution_specific_AIC),
           step_size1_standardised = case_when(step_size_char == "daily" ~ length_cycle1/30,
                                              step_size_char == "weekly" ~ length_cycle1/4,
                                              step_size_char == "monthly" ~ length_cycle1),
           step_size2_standardised = case_when(step_size_char == "daily" ~ length_cycle2/30,
                                               step_size_char == "weekly" ~ length_cycle2/4,
                                               step_size_char == "monthly" ~ length_cycle2)) %>%
    dplyr::filter(metric_optimal == distribution_specific_AIC) %>%
    dplyr::select(city, cycle1_optimal_mstl = step_size1_standardised, 
                 cycle2_optimal = step_size2_standardised,
                 mstl_aic = distribution_specific_AIC) %>%
    distinct()
  
  # Combine and determine if overall optimal is STL or MSTL
  result <- stl_optimal %>%
    full_join(mstl_optimal, by = "city") %>%
    left_join(overall_optimal, by = "city") %>%
    mutate(
      # If overall optimal is STL, then no between-year cycle
      cycle2_optimal = ifelse(!is.na(stl_aic) & !is.na(overall_optimal_aic) & 
                               abs(stl_aic - overall_optimal_aic) < 0.01, 
                               NA, cycle2_optimal),
      # Use MSTL cycle1 if MSTL is optimal, otherwise use STL
      cycle1_final = ifelse(!is.na(mstl_aic) & !is.na(overall_optimal_aic) & 
                            abs(mstl_aic - overall_optimal_aic) < 0.01,
                            cycle1_optimal_mstl, cycle1_optimal),
      disease = disease_name,
      city = factor(city, levels = city_ordered)
    ) %>%
    dplyr::select(city, disease, cycle1_final, cycle2_optimal)
  
  return(result)
}

# Extract for both diseases
flu_optimal <- extract_optimal_cycles(LL_flu, "Influenza")
rsv_optimal <- extract_optimal_cycles(LL_rsv, "RSV")

# Combine and format for table
optimal_table <- bind_rows(flu_optimal, rsv_optimal) %>%
  arrange(city, disease) %>%
  mutate(
    # Round to 1 decimal place
    cycle1_final = round(cycle1_final, 1),
    # Handle missing between-year cycles (STL optimal models)
    between_year = ifelse(is.na(cycle2_optimal), "---", 
                         as.character(round(cycle2_optimal, 1)))
  ) %>%
  dplyr::select(city, disease, within_year = cycle1_final, between_year) %>%
  pivot_wider(names_from = disease, values_from = c(within_year, between_year)) %>%
  arrange(city)

# Add geographic region
optimal_table <- optimal_table %>%
  mutate(
    region = ifelse(city %in% c("Beijing", "Lanzhou", "Xian"), "North", "South"),
    city = as.character(city)
  ) %>%
  dplyr::select(region, city, everything())

# Print for manual LaTeX table creation
cat("\n=== OPTIMAL CYCLE LENGTHS TABLE ===\n\n")
print(optimal_table)

# Create LaTeX table
cat("\n=== LaTeX TABLE CODE ===\n\n")
cat("\\begin{table}[htbp]\n")
cat("\\centering\n")
cat("\\caption{Table S4: Optimal within-year and between-year cycle lengths (months) for each city and pathogen combination. Values represent the cycle lengths of the best-performing model (lowest AIC) for each city-pathogen pair. Within-year cycles are shown for both pathogens; between-year cycles are only shown for models that included multi-seasonal decomposition (MSTL). For RSV in Wuhan, the optimal model was a single-seasonal model (STL) without a between-year component, indicated by ``---''.}\n")
cat("\\label{tab:optimal_cycles}\n")
cat("\\begin{tabular}{lccccc}\n")
cat("\\toprule\n")
cat("\\textbf{Region} & \\textbf{City} & \\multicolumn{2}{c}{\\textbf{Within-year cycle (months)}} & \\multicolumn{2}{c}{\\textbf{Between-year cycle (months)}} \\\\\n")
cat("\\cmidrule(lr){3-4} \\cmidrule(lr){5-6}\n")
cat(" & & \\textbf{Influenza} & \\textbf{RSV} & \\textbf{Influenza} & \\textbf{RSV} \\\\\n")
cat("\\midrule\n")

# Group by region
for(reg in c("North", "South")) {
  reg_data <- optimal_table %>% filter(region == reg)
  
  for(i in 1:nrow(reg_data)) {
    row <- reg_data[i, ]
    # Handle "---" for between-year cycles
    flu_between <- ifelse(row$between_year_Influenza == "---", "---", 
                         sprintf("%.1f", as.numeric(row$between_year_Influenza)))
    rsv_between <- ifelse(row$between_year_RSV == "---", "---", 
                         sprintf("%.1f", as.numeric(row$between_year_RSV)))
    
    cat(sprintf("%s & %s & %.1f & %.1f & %s & %s \\\\\n",
                ifelse(i == 1, reg, ""),
                row$city,
                as.numeric(row$within_year_Influenza),
                as.numeric(row$within_year_RSV),
                flu_between,
                rsv_between))
  }
  if(reg == "North") cat("\\midrule\n")
}

cat("\\bottomrule\n")
cat("\\end{tabular}\n")
cat("\\end{table}\n")

