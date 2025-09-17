LL_flu <- read_rds(paste0("results/LL_flu_v3.rds"))
LL_rsv <- read_rds(paste0("results/LL_rsv_v4.rds"))
source("code/0_LoadData.R")
source("code/0_1_run_model.R")
source("code/0_3_simulate_outbreaks.R")
source("code/0_4_plot_outbreaks.R")

# we are going to generate results for all of the top 10 models

LL_flu %>% 
  map(arrange, distribution_specific_AIC) %>% 
  map(dplyr::filter,  distribution_specific_AIC <= min(distribution_specific_AIC) + 10) -> flu_top

LL_rsv %>% 
  map(arrange, distribution_specific_AIC) %>% 
  map(dplyr::filter,  distribution_specific_AIC <= min(distribution_specific_AIC) + 10) -> rsv_top

models_fit_rsv <- models_fit_flu <- p_list_flu <- p_list_rsv <- res_flu <- res_rsv <- list()

# generate the "chosen" model results
for(j in 1:length(city_ordered)){
  
  pmap(
    list(
      disease = "flu",
      city = flu_top[[j]]$city,
      model_type = flu_top[[j]]$model,        # or use model_type if you saved it separately
      cycle1_len = flu_top[[j]]$length_cycle1,
      cycle2_len = flu_top[[j]]$length_cycle2
    ),
    ~ run_model(
      disease    = ..1,
      city       = ..2,
      model_type = ..3,
      cycle1_len = ..4,
      cycle2_len = ..5
    )
  ) -> models_fit_flu[[j]]
  
  pmap(
    list(
      disease = "rsv",
      city = rsv_top[[j]]$city,
      model_type = rsv_top[[j]]$model,        # or use model_type if you saved it separately
      cycle1_len = rsv_top[[j]]$length_cycle1,
      cycle2_len = rsv_top[[j]]$length_cycle2
    ),
    ~ run_model(
      disease    = ..1,
      city       = ..2,
      model_type = ..3,
      cycle1_len = ..4,
      cycle2_len = ..5
    )
  ) -> models_fit_rsv[[j]]
  
  res_flu[[j]] <- res_rsv[[j]] <- p_list_flu[[j]] <- p_list_rsv[[j]] <- list()
  
  forecast_unit_flu <- case_when(unique(flu_top[[j]]$step_size_char) == "monthly" ~ 12,
                                 unique(flu_top[[j]]$step_size_char) == "daily" ~ 365,
                                 unique(flu_top[[j]]$step_size_char) == "weekly" ~ 52)
  
  forecast_unit_rsv <- case_when(unique(rsv_top[[j]]$step_size_char) == "monthly" ~ 12,
                                 unique(rsv_top[[j]]$step_size_char) == "daily" ~ 365,
                                 unique(rsv_top[[j]]$step_size_char) == "weekly" ~ 52)
  
  tmp_date_start_flu <- yaml_data[[unique(flu_top[[j]]$city)]][["flu"]][["start_date"]]
  tmp_date_start_rsv <- yaml_data[[unique(rsv_top[[j]]$city)]][["rsv"]][["start_date"]]
  
  for(i in 1:nrow(flu_top[[j]])){
    
    simulate_outbreaks(model = models_fit_flu[[j]][[i]],
                       horizon = 20*forecast_unit_flu,
                       n = 1000,
                       city = unique(flu_top[[j]]$city),
                       disease = "flu",
                       yaml_data = yaml_data) -> res_flu[[j]][[i]]
    
    p_list_flu[[j]][[i]] <- plot_outbreaks(original = res_flu[[j]][[i]]$original,
                                      stochastic_forecast = res_flu[[j]][[i]]$stochastic_forecast,
                                      deterministic_forecast = res_flu[[j]][[i]]$deterministic_forecast)
    
    print(paste0(i, ", flu"))   
  }
  
  for(i in 1:nrow(rsv_top[[j]])){
    simulate_outbreaks(model = models_fit_rsv[[j]][[i]],
                       horizon = 20*forecast_unit_rsv,
                       n = 1000,
                       city = unique(rsv_top[[j]]$city),
                       disease = "rsv",
                       yaml_data = yaml_data)  -> res_rsv[[j]][[i]]
    
    p_list_rsv[[j]][[i]] <- plot_outbreaks(original = res_rsv[[j]][[i]]$original,
                                      stochastic_forecast = res_rsv[[j]][[i]]$stochastic_forecast,
                                      deterministic_forecast = res_rsv[[j]][[i]]$deterministic_forecast)
    
    print(paste0(i, ", rsv"))    
  }
  
  ggsave(cowplot::ggdraw() +
           cowplot::draw_label(paste("Top 10 models for", unique(flu_top[[j]]$city), "&", "flu"), x = 0.5, y = 0.95, hjust = 0.5, vjust = 0.5, size = 14) +
           cowplot::draw_plot(cowplot::plot_grid(plotlist = p_list_flu[[j]]), 
                              x = 0, y = 0, width = 1, height = 0.9),
         filename = paste0("figures/diagnostics/prediction/", unique(flu_top[[j]]$city), "_flu.pdf"),
         width = 18, height = 12)

  qsave(res_flu, "results/res_flu.qs")
  
  
  ggsave(cowplot::ggdraw() +
           cowplot::draw_label(paste("Top 10 models for", unique(rsv_top[[j]]$city), "&", "rsv"), x = 0.5, y = 0.95, hjust = 0.5, vjust = 0.5, size = 14) +
           cowplot::draw_plot(cowplot::plot_grid(plotlist = p_list_rsv[[j]]), 
                              x = 0, y = 0, width = 1, height = 0.9),
         filename = paste0("figures/diagnostics/prediction/", unique(rsv_top[[j]]$city), "_rsv.pdf"),
         width = 18, height = 12)
 
  qsave(res_rsv, "results/res_rsv.qs")
  
   
}



ggplot(p_tab, aes(x = year, y = grid, fill = outbreak_9)) +
  geom_tile()

p_tab %>% group_by(path) %>% group_split() %>% map(arrange, year) %>% 
  map(dplyr::filter, outbreak_9 == 1) %>% 
  map(mutate, 
      diff = c(0, diff(year)),
      count_consecutive = sum(diff == 1),
      prop_consecutive = count_consecutive/n()) %>% 
  bind_rows() %>% 
  dplyr::select(path, count_consecutive, prop_consecutive, grid) %>% 
  distinct() %>% 
  ggplot(., aes(x = prop_consecutive)) +
  geom_density()


p_tab %>%
  group_by(path) %>%
  arrange(year) %>%
  filter(outbreak_9 == 1) %>%  
  mutate(diff = c(0, diff(year))) %>% 
  filter(diff != 0) %>%
  summarise(diff = mean(diff), .groups = "drop") %>% 
  ggplot(., aes(x = diff)) +
  geom_density()
  
  pull(diff) -> observed_intervals

observed_rate <- 1 / mean(observed_intervals)
T_max <- 15  # Maximum observable interval
correction_factor <- 1 / (1 - exp(-observed_rate * T_max))
corrected_rate <- observed_rate * correction_factor

p_tab %>% 
  mutate_at(vars(starts_with("outbreak_")),
            ~if_else(. >=1, 1, 0)) %>% 
  pivot_longer(starts_with("outbreak_")) %>% 
  group_by(path, name) %>% group_split() %>% 
  map(mutate, 
      prop_outbreaks = sum(value)/n(),
      year_tot = n()) %>% 
  map(dplyr::filter, value > 0) %>% 
  map(arrange, year) %>% 
  map(mutate, diff = c(0, diff(year))) -> tmp

tmp %>% 
  bind_rows() %>% 
  dplyr::filter(diff != 0) %>% 
  ggplot(., aes(x = diff, group = path)) +
  geom_density() +
  facet_wrap(~name)

tmp %>% 
  bind_rows() %>% 
  dplyr::select(path, name, prop_outbreaks) %>% 
  unique() %>% 
  ggplot(., aes(x = prop_outbreaks, group = name, color = name, fill = name)) +
  geom_density(aes(alpha = 0.1)) 












models_fit_flu %>% 
  map(data.frame) %>% 
  map(mutate, date = ymd(yaml_data[[unique(flu_top[[m_tmp]]$city)]][["flu"]]$start_date)) %>% 
  map(rownames_to_column) %>% 
  map(mutate, date = date %m+% months(as.numeric(rowname))) -> tmp_observed_flu

models_fit_rsv %>% 
  map(data.frame) %>% 
  map(mutate, date = ymd(yaml_data[[unique(rsv_top[[m_tmp]]$city)]][["rsv"]]$start_date)) %>% 
  map(rownames_to_column) %>% 
  map(mutate, date = date %m+% months(as.numeric(rowname))) -> tmp_observed_rsv


models_fit_flu %>% 
  map(~ forecast::forecast(.x[[1]], h = 12 * 20)) %>% 
  map(data.frame) %>% 
  map(dplyr::select, `Point.Forecast`) %>% 
  map(mutate, date = max(tmp_observed_flu[[1]]$date) %m+% months(1:n())) %>% 
  map(rename, Data = `Point.Forecast`) -> tmp_future_flu

models_fit_rsv %>% 
  map(~ forecast::forecast(.x[[1]], h = 12 * 20)) %>% 
  map(data.frame) %>% 
  map(dplyr::select, `Point.Forecast`) %>% 
  map(mutate, date = max(tmp_observed_rsv[[1]]$date) %m+% months(1:n())) %>% 
  map(rename, Data = `Point.Forecast`) -> tmp_future_rsv


risk_def_flu <- quantile(exp(tmp_observed_flu[[1]]$Data), seq(0,1,0.1))
risk_def_rsv <- quantile(exp(tmp_observed_rsv[[1]]$Data), seq(0,1,0.1))

risk_def_flu[1] <- risk_def_rsv[1] <- -Inf
risk_def_flu[length(risk_def_flu)] <- risk_def_rsv[length(risk_def_rsv)] <- Inf
risk_def_flu
risk_def_rsv

map2(tmp_observed_flu, tmp_future_flu, ~ bind_rows(.x, .y)) %>% 
  map(dplyr::select, date, Data, Trend) %>% 
  map(mutate, 
      status = if_else(is.na(Trend), "predicted", "observed"),
      disease = "flu") %>% 
  bind_rows(.id = "idx_model") %>% 
  mutate(risk = exp(Data),
         risk_cat = cut(risk,
                        breaks = risk_def_flu, labels = 1:10),
         risk_cat_num = as.numeric(risk_cat)) -> p_flu

map2(tmp_observed_rsv, tmp_future_rsv, ~ bind_rows(.x, .y)) %>% 
  map(dplyr::select, date, Data, Trend) %>% 
  map(mutate, 
      status = if_else(is.na(Trend), "predicted", "observed"),
      disease = "rsv") %>% 
  bind_rows(.id = "idx_model") %>% 
  mutate(risk = exp(Data),
         risk_cat = cut(risk,
                        breaks = risk_def_rsv, labels = 1:10),
         risk_cat_num = as.numeric(risk_cat)) -> p_rsv

p_flu %>% 
  dplyr::select(idx_model, risk_cat_num, date, status) %>% 
  group_by(date, status) %>% 
  summarise(risk_flu = mean(risk_cat_num)) %>% 
  left_join(p_rsv %>% 
              dplyr::select(idx_model, risk_cat_num, date, status) %>% 
              group_by(date, status) %>% 
              summarise(risk_rsv = mean(risk_cat_num)),
            by = c("date", "status")) %>% 
  dplyr::filter(status == "predicted") %>% 
  .[complete.cases(.),] %>% 
  mutate(joint_peak_6 = risk_flu >= 6 & risk_rsv >= 6,
         joint_peak_7 = risk_flu >= 7 & risk_rsv >= 7,
         joint_peak_8 = risk_flu >= 8 & risk_rsv >= 8,
         joint_peak_9 = risk_flu >= 9 & risk_rsv >= 9) %>% 
  ggplot(., aes(x = date, y = 1, color = joint_peak_7)) +
  geom_point() +
  geom_vline(xintercept = seq(ymd("2020-01-01"),
                              ymd("2035-01-01"),
                              by = "year")) 



# 
#   pivot_longer(cols = starts_with("risk_")) %>% 
#   ggplot(., aes(x = date, y = value, fill = name)) +
#   geom_bar(stat = "identity", position = "stack") +
#   geom_vline(xintercept = seq(ymd("2020-01-01"),
#                               ymd("2035-01-01"),
#                               by = "year")) +
#   geom_vline(xintercept = seq(ymd("2020-06-01"),
#                               ymd("2035-06-01"),
#                               by = "year"),
#              color = "green")
# 
#   ggplot(., aes(x = date)) +
#   geom_line(aes(y = risk_flu), color = "red") +
#   geom_line(aes(y = risk_rsv), color = "purple")
  


bind_rows(p_flu, p_rsv) %>% 
  dplyr::filter(date > "2020-01-01", date < "2030-01-01") %>%
  
  
  
  ggplot(., aes(x = date, y = factor(idx_model, levels = 1:10), fill = risk_cat_num, color = status)) +
  geom_tile(height = 1, color = "black") +
  scale_y_discrete(breaks = 1:10, expand = c(0, 0,0,0))  +
  facet_wrap(~disease + status, ncol = 1)



p_rsv %>% 
  ggplot(., aes(x = date, y = exp(Data), color = status)) +
  geom_point() +
  geom_line() +
  facet_wrap(~idx_model)

