# @study name: 
# (1) disease type
# (2) data type
# (3) cycle 1 and cycle 2 assumption will included in this

get_LL <- function(disease = NULL,
                   city = NULL){
  
  study_name = yaml_data[[city]][[disease]]$name
  data_type = yaml_data[[city]][[disease]]$data_type
  cat("Study currently in use is:", study_name)
  cat("Data type currently in use is:", data_type)
  
  if(disease == "flu") {tmp <- data_flu[[study_name]]}
  if(disease == "rsv") {tmp <- data_rsv[[study_name]]}
  
  # tmp %<>% mutate(X2 = zoo::rollmean(X2, k = 2, fill = NA)) %>% .[complete.cases(.),]
  
  n_steps <- nrow(tmp)
  step_size_char <- yaml_data[[city]][[disease]]$raw_data_freq
  step_size_num <- case_when(step_size_char == "daily" ~ 30,
                             step_size_char == "weekly" ~ 4,
                             step_size_char == "monthly"~ 1)
  cycle_max <- round(n_steps/2)
  
  # if(step_size_char == "weekly") tmp <- tmp %>% mutate(X2 = zoo::rollmean(X2, k = 4, fill = NA)) %>% .[complete.cases(.),]
  # if(step_size_char == "daily") tmp <- tmp %>% mutate(X2 = zoo::rollmean(X2, k = 30, fill = NA)) %>% .[complete.cases(.),]
  
  cycle1 <- seq(10*step_size_num, 14*step_size_num, step_size_num)
  
  if(cycle_max > 36*step_size_num){
    cycle2 <- seq(18*step_size_num, 
                  36*step_size_num, 
                  step_size_num)
  } else{
    cycle2 <- seq(18*step_size_num, 
                  floor(cycle_max/step_size_num)*step_size_num - 1, 
                  step_size_num)  
  }
  
  stopifnot(length(cycle1) > 0, length(cycle2) > 0)
  
  cols_required <- c("length_cycle1", "length_cycle2","model", 
                        "ll_sum", "mae_mean", "acf1", 
                        "size_cycle1", "size_cycle1_v2", 
                        "size_cycle2", "size_cycle2_v2",
                        "size_trend_v2", "size_remainder_v2",
                        "size_base_v2",  "ic_logLik","ic_k","ic_AIC",
                     "ic_AICc","ic_BIC",
                     "distribution_specific_AIC", "distribution_specific_BIC"
                     )
  
  if(data_type == "cases" & disease == "flu"){
    
    cycle1 %>% 
      map(~ts(log(tmp$X2), frequency = .)) %>% 
      map(stl, s.window = "periodic") %>% 
      map(~.$time.series) %>% 
      map(data.table) %>% 
      map(\(.dt){
        y_sa_log <- .dt$trend + .dt$remainder
        fit      <- auto.arima(y_sa_log, stepwise = FALSE, approximation = FALSE)
        ll       <- logLik(fit)
        .dt[, `:=`(
          ic_logLik = as.numeric(ll),
          ic_k      = attr(ll, "df"),
          ic_AIC    = fit$aic,
          ic_AICc   = fit$aicc,
          ic_BIC    = fit$bic
        )]
        .dt
      }) %>%
      map(mutate,
          predicted = exp(seasonal + trend),
          observed = exp(seasonal + trend + remainder),
          acf1 = acf(remainder, plot = F)$acf[2]) %>% 
      setNames(cycle1) %>% 
      bind_rows(.id = "length_cycle1") %>% 
      mutate(model = "stl",
             ll = dpois(round(observed, 0),
                        predicted,
                        log = T),
             mae = abs(predicted - observed)) %>% 
      group_by(length_cycle1) %>% 
      summarise(ll_sum = sum(ll),
                mae_mean = mean(mae),
                acf1 = mean(acf1),
                length_cycle2 = as.numeric(NA),
                size_cycle1 = max(abs(exp(seasonal))),
                size_cycle1_v2 = var(seasonal),
                size_base_v2 = var(seasonal + trend + remainder),
                size_trend_v2 = var(trend),
                size_remainder_v2 = var(remainder),
                size_cycle2_v2 = as.numeric(NA),
                size_cycle2 = as.numeric(NA),
                ic_logLik = unique(ic_logLik),
                ic_k      = unique(ic_k),
                ic_AIC    = unique(ic_AIC),
                ic_AICc   = unique(ic_AICc),
                ic_BIC    = unique(ic_BIC),
                distribution_specific_AIC  = -2 * ll_sum + 2 * pmax(ic_k - 1, 0),
                distribution_specific_BIC  = -2 * ll_sum + pmax(ic_k - 1, 0) * log(n()),
                .groups = "drop") %>% 
      mutate(model = "stl") -> output_stl
    
    map2(CJ(cycle1, cycle2) %>% pull(cycle1), 
         CJ(cycle1, cycle2) %>% pull(cycle2),
         ~msts(data = log(tmp$X2), 
               seasonal.periods = c(.x, .y))) %>% 
      map(mstl, iterate = 3) %>% 
      map(data.table) %>% 
      map(setNames,
          c("Data", "Trend", "component_cycle1", "component_cycle2", "remainder")) %>% 
      map(\(dt){
        # ICs from ARIMA on seasonally adjusted log-series
        y_sa_log <- dt$Trend + dt$remainder
        fit      <- auto.arima(y_sa_log, stepwise = FALSE, approximation = FALSE)
        ll       <- logLik(fit)
        dt[, `:=`(
          ic_logLik = as.numeric(ll),
          ic_k      = attr(ll, "df"),
          ic_AIC    = fit$aic,
          ic_AICc   = fit$aicc,
          ic_BIC    = fit$bic
        )]
        dt
      }) %>%
      map(mutate, 
          acf1 = acf(remainder, plot = F)$acf[2],
          size_cycle1 = max(abs(exp(component_cycle1))),
          size_cycle1_v2 = var(component_cycle1),
          size_cycle2_v2 = var(component_cycle2),
          size_base_v2 = var(Trend + component_cycle1 + component_cycle2 + remainder),
          size_trend_v2 = var(Trend),
          size_remainder_v2 = var(remainder),
          size_cycle2 = max(abs(exp(component_cycle2)))) %>% 
      bind_rows(.id = "combo_index") -> output_mstl
    
    CJ(length_cycle1 = cycle1, length_cycle2 = cycle2) %>% 
      rownames_to_column(var = "combo_index") %>% 
      left_join(output_mstl, by = "combo_index") %>% 
      mutate(predicted = exp(Trend + component_cycle1 + component_cycle2),
             observed = exp(Data)) %>% 
      mutate(ll = dpois(round(observed, 0),
                        predicted,
                        log = T),
             mae = abs(observed - predicted)) %>% 
      group_by(combo_index, length_cycle1, length_cycle2) %>% 
      summarise(ll_sum = sum(ll),
                mae_mean = mean(mae),
                acf1 = mean(acf1),
                size_cycle1 = unique(size_cycle1),
                size_cycle1_v2 = unique(size_cycle1_v2),
                size_cycle2_v2 = unique(size_cycle2_v2),
                size_trend_v2 = unique(size_trend_v2),
                size_remainder_v2 = unique(size_remainder_v2),
                size_base_v2 = unique(size_base_v2),
                size_cycle2 = unique(size_cycle2),
                model = "mstl",
                # carry ICs from the ARIMA fit (one per combo)
                ic_logLik = unique(ic_logLik),
                ic_k      = unique(ic_k),
                ic_AIC    = unique(ic_AIC),
                ic_AICc   = unique(ic_AICc),
                ic_BIC    = unique(ic_BIC),
                # Poisson AIC from your own Poisson ll
                distribution_specific_AIC  = -2 * ll_sum + 2 * pmax(ic_k - 1, 0),
                distribution_specific_BIC  = -2 * ll_sum + pmax(ic_k - 1, 0) * log(n()),
                .groups = "drop") -> output2_mstl
    
    output_stl[,cols_required] %>% 
      mutate(length_cycle1 = as.numeric(length_cycle1)) %>% 
      ungroup %>% 
      bind_rows(output2_mstl %>% ungroup %>% 
                  dplyr::select(all_of(cols_required))) %>% 
      distinct() -> res
  }
  
  if(data_type == "cases" & disease == "rsv"){
    tmp$X2[tmp$X2==0] <- 1
    
    cycle1 %>% 
      map(~ts(log(tmp$X2), frequency = .)) %>% 
      map(stl, s.window = "periodic") %>% 
      map(~.$time.series) %>% 
      map(data.table) %>% 
      map(\(.dt){
        y_sa <- .dt$trend + .dt$remainder
        fit  <- auto.arima(y_sa, stepwise = FALSE, approximation = FALSE)
        ll   <- logLik(fit)
        .dt[, `:=`(
          ic_logLik = as.numeric(ll),
          ic_k      = attr(ll, "df"),
          ic_AIC    = fit$aic,
          ic_AICc   = fit$aicc,
          ic_BIC    = fit$bic
        )]
        .dt
      }) %>%
      map(mutate,
          predicted = exp(seasonal + trend),
          observed = exp(seasonal + trend + remainder),
          predicted = if_else(predicted < 0, 0.01, predicted),
          acf1 = acf(remainder, plot = F)$acf[2],
          length_cycle2 = as.numeric(NA),
          size_cycle1 = max(abs(seasonal)),
          size_cycle1_v2 = var(seasonal),
          size_base_v2 = var(observed),
          size_trend_v2 = var(trend),
          size_remainder_v2 = var(remainder),
          size_cycle2 = as.numeric(NA),
          size_cycle2_v2 = as.numeric(NA)
          ) %>% 
      setNames(cycle1) %>% 
      bind_rows(.id = "length_cycle1") %>% 
      mutate(model = "stl",
             ll = dpois(round(observed, 0),
                        predicted, 
                        log = T),
             mae = abs(predicted - observed)) %>% 
      group_by(length_cycle1) %>% 
      summarise(ll_sum = sum(ll),
                mae_mean = mean(mae),
                acf1 = mean(acf1),
                size_cycle1 = unique(size_cycle1),
                size_cycle1_v2 = unique(size_cycle1_v2),
                size_cycle2 = unique(size_cycle2),
                size_cycle2_v2 = unique(size_cycle2_v2),
                size_base_v2 = unique(size_base_v2),
                size_trend_v2 = unique(size_trend_v2),
                size_remainder_v2 = unique(size_remainder_v2),
                ic_logLik = unique(ic_logLik),
                ic_k      = unique(ic_k),
                ic_AIC    = unique(ic_AIC),
                ic_AICc   = unique(ic_AICc),
                ic_BIC    = unique(ic_BIC),
                # NEW: Poisson ICs (drop Gaussian variance → k_pois = ic_k - 1)
                distribution_specific_AIC = -2 * ll_sum + 2 * pmax(ic_k - 1, 0),
                distribution_specific_BIC = -2 * ll_sum + pmax(ic_k - 1, 0) * log(n()),
                .groups = "drop"
      ) %>% 
      mutate(model = "stl",
             length_cycle2 = as.numeric(NA),
             length_cycle1 = as.numeric(length_cycle1)) -> output_stl
    
    map2(CJ(cycle1, cycle2) %>% pull(cycle1), 
         CJ(cycle1, cycle2) %>% pull(cycle2),
         ~msts(data = log(tmp$X2), 
               seasonal.periods = c(.x, .y))) %>% 
      map(mstl, iterate = 3) %>% 
      map(data.table) %>% 
      map(setNames,
          c("Data", "Trend", "component_cycle1", 
            "component_cycle2", "remainder")) %>% 
      map(\(dt){
        y_sa <- dt$Trend + dt$remainder
        fit  <- auto.arima(y_sa, stepwise = FALSE, approximation = FALSE)
        ll   <- logLik(fit)
        dt[, `:=`(
          ic_logLik = as.numeric(ll),
          ic_k      = attr(ll, "df"),
          ic_AIC    = fit$aic,
          ic_AICc   = fit$aicc,
          ic_BIC    = fit$bic
        )]
        dt
      }) %>%
      map(mutate, 
          acf1 = acf(remainder, plot = F)$acf[2],
          size_cycle1 = max(abs(component_cycle1)),
          size_cycle1_v2 = var(component_cycle1),
          size_cycle2 = max(abs(component_cycle2)),
          size_cycle2_v2 = var(component_cycle2),
          size_base_v2 = var(Trend + component_cycle1 + component_cycle2 + remainder),
          size_trend_v2 = var(Trend),
          size_remainder_v2 = var(remainder)
          ) %>% 
      bind_rows(.id = "combo_index") -> output_mstl
    
    CJ(length_cycle1 = cycle1, length_cycle2 = cycle2) %>% 
      rownames_to_column(var = "combo_index") %>% 
      left_join(output_mstl, by = "combo_index") %>% 
      mutate(predicted = exp(Trend + component_cycle1 + component_cycle2),
             observed = exp(Data),
             predicted = if_else(predicted < 0, 0.01, predicted)) %>% 
      mutate(ll = dpois(round(observed, 0),
                        predicted,
                        log = T),
             mae = abs(predicted - observed)) %>% 
      group_by(combo_index, length_cycle1, length_cycle2) %>% 
      summarise(ll_sum = sum(ll),
                mae_mean = mean(mae),
                acf1 = mean(acf1),
                size_cycle1 = unique(size_cycle1),
                size_cycle1_v2 = unique(size_cycle1_v2),
                size_cycle2 = unique(size_cycle2),
                size_cycle2_v2 = unique(size_cycle2_v2),
                size_base_v2 = unique(size_base_v2),
                size_trend_v2 = unique(size_trend_v2),
                size_remainder_v2 = unique(size_remainder_v2),
                ic_logLik = unique(ic_logLik),
                ic_k      = unique(ic_k),
                ic_AIC    = unique(ic_AIC),
                ic_AICc   = unique(ic_AICc),
                ic_BIC    = unique(ic_BIC),
                distribution_specific_AIC = -2 * ll_sum + 2 * pmax(ic_k - 1, 0),
                distribution_specific_BIC = -2 * ll_sum + pmax(ic_k - 1, 0) * log(n()),
                .groups = "drop"
      ) %>% 
      mutate(model = "mstl") -> output2_mstl
    
    output2_mstl %>% 
      ungroup %>% 
      dplyr::select(all_of(cols_required)) %>% 
      bind_rows(output_stl %>% ungroup %>% 
                  dplyr::select(all_of(cols_required))) %>% 
      distinct() -> res
  }
  
  if(data_type == "rates"){
    
    cycle1 %>% 
      map(~ts(log(tmp$X2/100), frequency = .)) %>% 
      map(stl, s.window = "periodic") %>% 
      map(~.$time.series) %>% 
      map(data.table) %>% 
      map(\(.dt){
        y_sa_log <- .dt$trend + .dt$remainder
        fit      <- auto.arima(y_sa_log, stepwise = FALSE, approximation = FALSE)
        ll       <- logLik(fit)
        .dt[, `:=`(
          ic_logLik = as.numeric(ll),
          ic_k      = attr(ll, "df"),
          ic_AIC    = fit$aic,
          ic_AICc   = fit$aicc,
          ic_BIC    = fit$bic
        )]
        .dt
      }) %>%
      map(mutate,
          predicted = exp(seasonal + trend),
          predicted_logged = seasonal + trend,
          observed = exp(seasonal + trend + remainder),
          observed_logged = (seasonal + trend + remainder),
          acf1 = acf(remainder, plot = F)$acf[2],
          size_cycle1 = max(abs(exp(seasonal))),
          size_cycle1_v2 = var(seasonal),
          size_base_v2 = var(observed_logged),
          size_trend_v2 = var(trend),
          size_remainder_v2 = var(remainder),
          size_cycle2 = as.numeric(NA)) %>% 
      setNames(cycle1) -> output_stl
  
    map(.x = output_stl,
         .f = function(.x) {
           .x %>% 
             mutate(ll = dnorm(mean = predicted_logged, 
                               x = observed_logged, 
                               sd   = sd(log(tmp$X2/100)),
                               log  = T),
                    mae = abs(observed_logged - predicted_logged))
         } 
         ) %>% 
      map(rownames_to_column,
          var = "t") %>% 
      map(mutate, t = as.numeric(t)) %>% 
      bind_rows(.id = "length_cycle1") %>% 
      group_by(length_cycle1) %>% 
      summarise(ll_sum = sum(ll),
                mae_mean = mean(mae),
                acf1 = mean(acf1),
                size_cycle1 = unique(size_cycle1),
                size_cycle1_v2 = unique(size_cycle1_v2),
                size_trend_v2 = unique(size_trend_v2),
                size_remainder_v2 = unique(size_remainder_v2),
                size_base_v2 = unique(size_base_v2),
                # carry ICs from ARIMA fit
                ic_logLik = unique(ic_logLik),
                ic_k      = unique(ic_k),
                ic_AIC    = unique(ic_AIC),
                ic_AICc   = unique(ic_AICc),
                ic_BIC    = unique(ic_BIC),
                # distribution-specific ICs (Gaussian with fixed sd → drop σ²)
                distribution_specific_AIC = -2 * ll_sum + 2 * pmax(ic_k - 1, 0),
                distribution_specific_BIC = -2 * ll_sum + pmax(ic_k - 1, 0) * log(n()),
                .groups = "drop") %>% 
      mutate(length_cycle2 = as.numeric(NA),
             length_cycle1 = as.numeric(length_cycle1),
             size_cycle2 = as.numeric(NA),
             size_cycle2_v2 = as.numeric(NA),
             model = "stl") -> res_stl
    
    map2(CJ(cycle1, cycle2) %>% pull(cycle1), 
         CJ(cycle1, cycle2) %>% pull(cycle2),
         ~msts(data = log(tmp$X2/100), 
               seasonal.periods = c(.x, .y))) %>% 
      map(mstl, iterate = 3) %>% 
      map(data.table) %>% 
      map(setNames,
          c("Data", 
            "Trend", 
            "component_cycle1", 
            "component_cycle2", 
            "remainder")) %>% 
      # ICs from ARIMA on seasonally adjusted LOG series
      map(\(dt){
        y_sa_log <- dt$Trend + dt$remainder
        fit      <- auto.arima(y_sa_log, stepwise = FALSE, approximation = FALSE)
        ll       <- logLik(fit)
        dt[, `:=`(
          ic_logLik = as.numeric(ll),
          ic_k      = attr(ll, "df"),
          ic_AIC    = fit$aic,
          ic_AICc   = fit$aicc,
          ic_BIC    = fit$bic
        )]
        dt
      }) %>%
      map(mutate,
          observed = exp(Data),
          predicted = exp(Trend + component_cycle1 + component_cycle2),
          predicted_logged = Trend + component_cycle1 + component_cycle2,
          observed_logged = Data,
          acf1 = acf(remainder, plot = F)$acf[2],
          size_cycle1 = max(abs(exp(component_cycle1))),
          size_cycle1_v2 = var(component_cycle1),
          size_base_v2 = var(observed_logged),
          size_trend_v2 = var(Trend),
          size_remainder_v2 = var(remainder),
          size_cycle2 = max(abs(exp(component_cycle2))),
          size_cycle2_v2 = var(component_cycle2)) -> output_mstl
  
    map(.x = output_mstl,
         .f = function(.x) {
           .x %>% 
             mutate(ll = dnorm(mean = predicted_logged, 
                               x = observed_logged, 
                               sd = sd(log(tmp$X2/100)),
                               log = T),
                    mae = abs(observed_logged - predicted_logged))
         } 
    ) %>% 
      bind_rows(.id = "combo_index") %>% 
      left_join(CJ(length_cycle1 = cycle1,
                   length_cycle2 = cycle2) %>% 
                  rownames_to_column(var = "combo_index"),
                by = "combo_index") %>% 
      mutate(model = "mstl") %>% 
      group_by(length_cycle1, length_cycle2, model) %>% 
      summarise(ll_sum = sum(ll),
                mae_mean = mean(mae),
                acf1 = mean(acf1),
                size_cycle1 = unique(size_cycle1),
                size_cycle1_v2 = unique(size_cycle1_v2),
                size_base_v2 = unique(size_base_v2),
                size_cycle2_v2 = unique(size_cycle2_v2),
                size_trend_v2 = unique(size_trend_v2),
                size_remainder_v2 = unique(size_remainder_v2),
                size_cycle2 = unique(size_cycle2),# carry ICs from ARIMA fit
                ic_logLik = unique(ic_logLik),
                ic_k      = unique(ic_k),
                ic_AIC    = unique(ic_AIC),
                ic_AICc   = unique(ic_AICc),
                ic_BIC    = unique(ic_BIC),
                # distribution-specific ICs (Gaussian with fixed sd → drop σ²)
                distribution_specific_AIC = -2 * ll_sum + 2 * pmax(ic_k - 1, 0),
                distribution_specific_BIC = -2 * ll_sum + pmax(ic_k - 1, 0) * log(n()),
                .groups = "drop") -> res_mstl
    
    res_stl %>% 
      dplyr::select(all_of(cols_required)) %>% 
      bind_rows(res_mstl %>% 
                  dplyr::select(all_of(cols_required))) -> res
  
  }
  
  res <- res %>% 
    mutate(model = factor(model, levels = c("stl", "mstl")),
           disease = disease,
           city = city,
           step_size_char = step_size_char,
           data_type = data_type,
           contribution_cycle1 = size_cycle1_v2/size_base_v2,
           contribution_cycle2 = size_cycle2_v2/size_base_v2,
           contribution_trend = size_trend_v2/size_base_v2,
           contribution_remainder = size_remainder_v2/size_base_v2) %>% 
    arrange(model)
  
  return(res)
}


draw_LL_comparison <- function(city = NULL,
                               disease = NULL){
  
  LL_table <-  get_LL(disease = disease, city = city) 
  
  LL_table %>% 
    dplyr::filter(model == "mstl") %>% 
    ggplot(., aes(x = ll_sum)) + 
    geom_density() +
    facet_wrap(~length_cycle1) +
    geom_vline(data = LL_table %>% 
                 dplyr::filter(model == "stl"),
               aes(xintercept = ll_sum),
               color = "red") +
    labs(x = "LogLik", y = "density",
         title = paste0("city = ", city, "; disease = ", disease)) -> p
  
  return(p)

  }

# get_LL(disease = "flu", city = "Xian") # works
# get_LL(disease = "flu", city = "Beijing") # works
# get_LL(disease = "flu", city = "Guangzhou") # works
# get_LL(disease = "flu", city = "Lanzhou") # works
# get_LL(disease = "flu", city = "Wenzhou") # works
# get_LL(disease = "flu", city = "Wuhan") # works
# get_LL(disease = "flu", city = "Yunfu") # works
# get_LL(disease = "flu", city = "Suzhou")
#  
# get_LL(disease = "rsv", city = "Xian") # works
# get_LL(disease = "rsv", city = "Beijing") # works
# get_LL(disease = "rsv", city = "Guangzhou") # works
# get_LL(disease = "rsv", city = "Lanzhou")
# get_LL(disease = "rsv", city = "Wenzhou") # works
# get_LL(disease = "rsv", city = "Wuhan") # works
# get_LL(disease = "rsv", city = "Yunfu") # works
# get_LL(disease = "rsv", city = "Suzhou") # works


# get_LL(disease = "flu", city = "Xian") -> x
# x %>% ggplot(., aes(x = length_cycle1, y = acf1)) + geom_point()
# 
# run_model(disease = "flu",
#           city = "Xian",
#           model_type = "stl",
#           cycle1_len = 12,
#           cycle2_len = NULL) 
 
# for(i in 1:7){
#   
#   city <- c("Xian", "Beijing", "Guangzhou", "Wenzhou",
#             "Wuhan", "Yunfu", "Suzhou")
#   
#   draw_LL_comparison(city = city[i],
#                      disease = "rsv") -> p_save
#   
#   ggsave(paste0(paste0("figs/diagnostics/","rsv","-",city[i],".png")),
#          p_save)
#   
# }
# 
# 
# for(i in 1:7){
#   
#   city <- c("Xian", "Beijing", "Guangzhou", "Lanzhou", "Wenzhou",
#             "Wuhan", "Yunfu")
#   
#   draw_LL_comparison(city = city[i],
#                           disease = "flu") -> p_save
#   
#   ggsave(paste0(paste0("figs/diagnostics/","flu","-",city[i],".png")),
#          p_save)
# 
#   }
