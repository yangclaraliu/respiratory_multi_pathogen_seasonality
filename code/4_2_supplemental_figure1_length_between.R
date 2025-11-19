# Load required libraries
library(tidyverse)
library(ggplot2)
library(cowplot)

# Load the data
LL_flu <- readRDS("results/LL_flu_v3.rds")
LL_rsv <- readRDS("results/LL_rsv_v4.rds")

# Define city order
city_ordered <- c("Beijing", "Lanzhou", "Xian", "Suzhou", "Wuhan", "Wenzhou", "Guangzhou", "Yunfu")

# Function to get comparable models with configurable threshold
get_comparable <- function(aic_threshold = 10){
  
  # Process flu data
  flu_stl_all <- LL_flu %>% 
    bind_rows() %>% 
    dplyr::filter(model == "stl") %>% 
    group_by(city) %>% 
    mutate(metric_optimal = min(distribution_specific_AIC),
           metric_similar = distribution_specific_AIC <= (metric_optimal + aic_threshold),
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
           metric_similar = distribution_specific_AIC < (metric_optimal + aic_threshold),
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
           metric_similar = distribution_specific_AIC < (metric_optimal + aic_threshold),
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
           metric_similar = distribution_specific_AIC < (metric_optimal + aic_threshold),
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
    map(dplyr::filter, metric_similar == TRUE) %>% 
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
    map(dplyr::filter, metric_similar == TRUE) %>% 
    bind_rows() %>% 
    mutate(city = factor(city, levels = city_ordered),
           direction = if_else(city %in% c("Beijing", "Xian", "Lanzhou"),
                               "North",
                               "South"),
           disease = "RSV") -> p_rsv
  
  bind_rows(p_flu, p_rsv) -> tmp
  return(tmp)
}

# Function to generate figure for a given threshold
generate_figure <- function(aic_threshold) {
  # Get comparable models data
  p5_tab_comparable <- get_comparable(aic_threshold = aic_threshold)
  
  cat(sprintf("\n=== Generating figure for ΔAIC < %d ===\n", aic_threshold))
  cat("Total comparable models: ", nrow(p5_tab_comparable), "\n")
  cat("Influenza models: ", sum(p5_tab_comparable$disease == "Influenza"), "\n")
  cat("RSV models: ", sum(p5_tab_comparable$disease == "RSV"), "\n")
  
  # Prepare data for plotting
  plot_data <- p5_tab_comparable %>% 
    # Filter to only include MSTL models (which have between-year cycles)
    filter(!is.na(step_size2_standardised)) %>%
    # Create combined label for facet
    mutate(panel_label = paste(direction, disease, sep = ", ")) %>%
    # Drop unused factor levels for city to remove empty x-axis elements
    mutate(city = droplevels(city))
  
  # Find best performing model (lowest AIC) for each city-disease combination
  best_models <- plot_data %>%
    group_by(city, disease) %>%
    slice_min(distribution_specific_AIC, n = 1) %>%
    ungroup()
  
  supplemental_fig <- plot_data %>%
    ggplot(aes(x = city, y = step_size2_standardised, fill = direction)) +
    geom_boxplot(alpha = 0.7, outlier.alpha = 0.5) +
    # Add star for best performing model
    geom_point(data = best_models,
               aes(x = city, y = step_size2_standardised),
               shape = 8, size = 2, color = "black", stroke = 1) +
    theme_bw() +
    labs(x = "City",
         y = sprintf("Length of between-year cycles (months)\namong comparable models (ΔAIC < %d)", aic_threshold),
         fill = "Geographic category") +
    facet_wrap(~ panel_label, scales = "free_x", drop = TRUE, nrow = 2, ncol = 2) +
    scale_fill_manual(values = c("#ffc38b", "#b63679")) +
    scale_x_discrete(drop = TRUE) +
    theme(legend.position = "bottom",
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 12),        
          strip.text = element_text(size = 12),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.spacing = unit(1, "lines"))
  
  # Save the figure
  filename <- sprintf("figures/supplemental_Figure1_threshold_%d.png", aic_threshold)
  ggsave(filename, 
         plot = supplemental_fig, 
         width = 12, 
         height = 10,
         dpi = 300)
  
  cat(sprintf("Figure saved to %s\n", filename))
  
  return(supplemental_fig)
}

# Generate figures for different thresholds
cat("Data loaded successfully.\n")

# Generate for threshold 4
fig_4 <- generate_figure(4)

# Generate for threshold 7
fig_7 <- generate_figure(7)

# Generate for threshold 10 (original)
fig_10 <- generate_figure(10)

cat("\nAll figures generated successfully!\n")

