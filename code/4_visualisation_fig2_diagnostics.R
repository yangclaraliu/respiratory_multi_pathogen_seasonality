LL_flu %>% 
  bind_rows() %>% 
  mutate(step_size1_standardised = case_when(step_size_char == "daily" ~ length_cycle1/30,
                                             step_size_char == "weekly" ~ length_cycle1/4,
                                             step_size_char == "monthly" ~ length_cycle1),
         step_size1_standardised = factor(step_size1_standardised),
         step_size2_standardised = case_when(step_size_char == "daily" ~ length_cycle2/30,
                                             step_size_char == "weekly" ~ length_cycle2/4,
                                             step_size_char == "monthly" ~ length_cycle2),
         city = factor(city, levels = rev(city_ordered)),
         direction = if_else(city %in% c("Beijing", "Xian", "Lanzhou"),
                             "North",
                             "South")) -> p_tab


ggplot(p_tab %>% dplyr::filter(model == "mstl"), aes(x = step_size1_standardised, y = distribution_specific_AIC)) +
  geom_boxplot(outlier.shape = 1) +
  geom_point(data = p_tab %>% dplyr::filter(model == "stl"), aes(x = step_size1_standardised, y = distribution_specific_AIC), 
             color = "#800080", shape = 18, size = 2, stroke = 2) +
  facet_wrap(~city, scales = "free", nrow = 2) +
  theme_bw() +
  labs(y = "LogLik",
       x = "Length of ~Annual cycle",
       title = "Seasonal Influenza") -> p1

LL_rsv %>% 
  bind_rows() %>% 
  mutate(step_size1_standardised = case_when(step_size_char == "daily" ~ length_cycle1/30,
                                             step_size_char == "weekly" ~ length_cycle1/4,
                                             step_size_char == "monthly" ~ length_cycle1),
         step_size1_standardised = factor(step_size1_standardised),
         step_size2_standardised = case_when(step_size_char == "daily" ~ length_cycle2/30,
                                             step_size_char == "weekly" ~ length_cycle2/4,
                                             step_size_char == "monthly" ~ length_cycle2),
         city = factor(city, levels = rev(city_ordered)),
         direction = if_else(city %in% c("Beijing", "Xian", "Lanzhou"),
                             "North",
                             "South")) -> p_tab


ggplot(p_tab %>% dplyr::filter(model == "mstl"), 
       aes(x = step_size1_standardised, y = distribution_specific_AIC)) +
  geom_boxplot(outlier.shape = 1) +
  geom_point(data = p_tab %>% dplyr::filter(model == "stl"), 
             aes(x = step_size1_standardised, y = distribution_specific_AIC), 
             color = "#800080", shape = 18, size = 2, stroke = 2) +
  facet_wrap(~city, scales = "free", nrow = 2) +
  theme_bw() +
  labs(y = "LogLik",
       x = "Length of ~Annual cycle",
       title = "RSV") -> p2

p_save <- plot_grid(p1, p2, ncol = 1)
ggsave("figures/fig2_v3.png", plot = p_save, width = 10, height = 10)

