plot_outbreaks <- function(
    original,
    deterministic_forecast,
    stochastic_forecast,
    back_transform = identity,   # e.g. exp for log-scale data; default = no transform
    n_paths = NULL,              # optionally plot only a subset of sample paths
    path_alpha = 0.18,           # transparency for simulated paths
    path_linewidth = 0.4,        # line width for simulated paths
    mean_linewidth = 0.9,        # line width for deterministic mean
    hist_linewidth = 0.7# line width for historical series
){
  # deps
  requireNamespace("ggplot2", quietly = TRUE)
  requireNamespace("dplyr", quietly = TRUE)
  
  bt <- back_transform
  
  # Prepare data (apply back-transform once here, not inside aes)
  orig_df <- dplyr::mutate(original, value_bt = bt(value))
  det_df  <- dplyr::mutate(deterministic_forecast, mean_bt = bt(mean))
  sim_df  <- dplyr::mutate(stochastic_forecast, value_bt = bt(value))
  
  # Optionally down-sample number of paths to draw
  if (!is.null(n_paths)) {
    set.seed(1L)
    keep <- sample(unique(sim_df$path), size = min(n_paths, length(unique(sim_df$path))))
    sim_df <- dplyr::filter(sim_df, path %in% keep)
  }
  
  ggplot2::ggplot() +
    # historical series
    ggplot2::geom_line(
      data = orig_df,
      ggplot2::aes(x = time, y = value_bt),
      linewidth = hist_linewidth, colour = "black"
    ) +
    # stochastic trajectories
    ggplot2::geom_line(
      data = sim_df,
      ggplot2::aes(x = time, y = value_bt, group = path),
      alpha = path_alpha, linewidth = path_linewidth,
      color = "#f7EACA"
    ) +
    # deterministic mean
    ggplot2::geom_line(
      data = det_df,
      ggplot2::aes(x = time, y = mean_bt),
      linewidth = mean_linewidth,
      color = "#6A9E85"
    ) +
    ggplot2::labs(
      x = "Time", y = "Value",
      title = "Deterministic mean vs. stochastic sample paths"
    ) +
    ggplot2::theme_minimal()
}
