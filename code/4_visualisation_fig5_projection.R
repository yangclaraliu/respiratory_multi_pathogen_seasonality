LL_flu <- read_rds(paste0("results/LL_flu_v3.rds"))
LL_rsv <- read_rds(paste0("results/LL_rsv_v4.rds"))

m = 5

LL_flu %>% 
  map(arrange, distribution_specific_BIC) %>% 
  map(~.[1:m,]) -> flu_top

LL_rsv %>% d
  map(arrange, distribution_specific_BIC) %>% 
  map(~.[1:m,]) -> rsv_top

pmap(
  list(
    disease = "flu",
    city = flu_top[[1]]$city,
    model_type = flu_top[[1]]$model,        # or use model_type if you saved it separately
    cycle1_len = flu_top[[1]]$length_cycle1,
    cycle2_len = flu_top[[1]]$length_cycle2
  ),
  ~ run_model(
    disease    = ..1,
    city       = ..2,
    model_type = ..3,
    cycle1_len = ..4,
    cycle2_len = ..5
  )
) -> models_fit_flu

pmap(
  list(
    disease = "rsv",
    city = rsv_top[[1]]$city,
    model_type = rsv_top[[1]]$model,        # or use model_type if you saved it separately
    cycle1_len = rsv_top[[1]]$length_cycle1,
    cycle2_len = rsv_top[[1]]$length_cycle2
  ),
  ~ run_model(
    disease    = ..1,
    city       = ..2,
    model_type = ..3,
    cycle1_len = ..4,
    cycle2_len = ..5
  )
) -> models_fit_rsv

models_fit_flu %>% 
  map(data.frame) %>% 
  map(mutate, date = ymd(yaml_data[[unique(flu_top[[1]]$city)]][["flu"]]$start_date)) %>% 
  map(rownames_to_column) %>% 
  map(mutate, date = date %m+% months(as.numeric(rowname))) -> tmp_observed_flu

models_fit_rsv %>% 
  map(data.frame) %>% 
  map(mutate, date = ymd(yaml_data[[unique(rsv_top[[1]]$city)]][["rsv"]]$start_date)) %>% 
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
                        breaks = risk_def, labels = 1:10),
         risk_cat_num = as.numeric(risk_cat)) -> p_flu

map2(tmp_observed_rsv, tmp_future_rsv, ~ bind_rows(.x, .y)) %>% 
  map(dplyr::select, date, Data, Trend) %>% 
  map(mutate, 
      status = if_else(is.na(Trend), "predicted", "observed"),
      disease = "rsv") %>% 
  bind_rows(.id = "idx_model") %>% 
  mutate(risk = exp(Data),
         risk_cat = cut(risk,
                        breaks = risk_def, labels = 1:10),
         risk_cat_num = as.numeric(risk_cat)) -> p_rsv


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

