# simulate_outbreaks(model = models_fit_flu[[2]][[7]],
#                    horizon = 20*52,
#                    n = 10,
#                    city = "Guangzhou",
#                    disease = "rsv",
#                    yaml_data = yaml_data)


simulate_outbreaks <- function(
    model,
    horizon = NULL,
    n = 100,
    city = NULL,
    disease = NULL,
    yaml_data = NULL
){
  
  m  <- model
  h  <- horizon
  N  <- n
  model_type <- class(m[[1]])[1]
  
  # 1) seasonally adjusted series
  y_sa <- forecast::seasadj(m[[1]])
  
  # 2) fit ARIMA (stationary, no differencing)
  fit <- forecast::auto.arima(
    y_sa, d = 0, D = 0, 
    stepwise = TRUE, approximation = TRUE
  )
  
  # 3) future seasonal effects
  if(model_type == "mstl"){
    seasonal_cols <- grep("^Seasonal", colnames(m[[1]]), value = TRUE)
    periods <- as.numeric(sub("Seasonal", "", seasonal_cols))
  } else {
    seasonal_cols <- "seasonal"
    periods <- 12
  }
  
  make_future_season <- function(season_col, period, h) {
    last_cycle <- utils::tail(season_col, period)
    rep(last_cycle, length.out = h)
  }
  
  S_future <- sapply(seq_along(seasonal_cols), function(i) {
    if(model_type == "mstl"){
      make_future_season(m[[1]][, seasonal_cols[i]], periods[i], h)
    } else {
      make_future_season(m[[1]]$time.series[, seasonal_cols], periods, h)
    }
  })
  
  S_future <- if (is.null(dim(S_future))) matrix(S_future, ncol = 1) else S_future
  S_future_sum <- rowSums(S_future)
  
  # 4) simulate N stochastic sample paths
  sim_sa <- replicate(N, simulate(fit, nsim = h, future = TRUE))
  sim_y  <- sweep(sim_sa, 1, S_future_sum, "+")
  
  # observed series & indices
  if(model_type == "mstl") {
    y <- m[[1]][, "Data"]
  } else {
      y <- rowSums(m[[1]]$time.series)
    }
  time_index   <- seq_along(y)
  future_index <- max(time_index) + seq_len(h)
  orig_df <- tibble::tibble(time = time_index, value = as.numeric(y))
  
  # deterministic forecast mean only
  det_sa <- forecast::forecast(fit, h = h, level = 0)$mean
  det_y <- det_sa + S_future_sum
  det_df <- tibble::tibble(
    time = future_index,
    mean = as.numeric(det_y)
  )
  
  # tidy stochastic simulations
  sim_df <- as.data.frame(sim_y)
  names(sim_df) <- paste0("path", seq_len(N))
  sim_df <- tibble::as_tibble(sim_df)
  sim_df <- dplyr::mutate(sim_df, time = future_index)
  sim_df <- tidyr::pivot_longer(sim_df, -time, names_to = "path", values_to = "value")


  # Convert time indices to real dates if yaml_data provided

  start_date <- yaml_data[[city]][[disease]]$start_date
  step_size <- yaml_data[[city]][[disease]]$raw_data_freq
  
  list(
    original               = orig_df,
    deterministic_forecast = det_df,
    stochastic_forecast    = sim_df
  ) %>% 
    map(mutate, 
        date_start = ymd(start_date),
        date = case_when(step_size == "weekly" ~ date_start %m+% weeks(time),
                         step_size == "monthly" ~ date_start %m+% months(time),
                         step_size == "daily" ~ date_start %m+% days(time)))
}
