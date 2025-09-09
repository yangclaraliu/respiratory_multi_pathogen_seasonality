# Figure 2: are they chosen? are they doing better? is there a disease 
# combination where more complex models are not necessarily doing better?

# Figure 3: annual and interannual component selection: how long are they? 
# how large are these components?

##### cycle1: selected####
LL_flu %>% 
  bind_rows() %>% 
  dplyr::filter(model == "stl") %>% 
  group_by(city) %>% 
  mutate(metric_optimal = min(distribution_specific_AIC),
         metric_similar = distribution_specific_AIC <= (metric_optimal + 10),
         step_size_standardised = case_when(step_size_char == "daily" ~ length_cycle1/30,
                                            step_size_char == "weekly" ~ length_cycle1/4,
                                            step_size_char == "monthly" ~ length_cycle1),
         acf1_max = max(acf1),
         acf1_relative = acf1/acf1_max,
         acf1_similar = sum(acf1_relative >= 0.9) - 1,
         rank_ll = rank(desc(ll_sum)),
         rank_aic = rank(desc(distribution_specific_AIC)),
         rank_mae = rank(mae_mean),
         rank_acf = rank(acf1),
         rank_size = rank(desc(size_cycle1)),
         rank_mean = (rank_acf + rank_ll + rank_size)/3,
         rank_mean_min = min(rank_mean)) %>%
  # dplyr::filter(rank_aic == max(rank_aic)) %>% 
  dplyr::filter(metric_similar == T) %>%
  # dplyr::filter(metric_optimal == distribution_specific_AIC) %>% 
  mutate(city = factor(city, levels = city_ordered),
         cycle1_selected = step_size_standardised) -> flu_stl_all
  
flu_stl_all %>% 
  dplyr::filter(metric_optimal == distribution_specific_AIC) -> flu_stl

LL_rsv %>% 
  bind_rows() %>% 
  dplyr::filter(model == "stl") %>% 
  group_by(city) %>% 
  mutate(metric_optimal = min(distribution_specific_AIC),
         metric_similar = distribution_specific_AIC < (metric_optimal + 10),
         # ll_relative = ll_sum_max/ll_sum,
         step_size_standardised = case_when(step_size_char == "daily" ~ length_cycle1/30,
                                            step_size_char == "weekly" ~ length_cycle1/4,
                                            step_size_char == "monthly" ~ length_cycle1),
         acf1_max = max(acf1),
         acf1_relative = acf1/acf1_max,
         acf1_similar = sum(acf1_relative >= 0.9) - 1,
         rank_ll = rank(desc(ll_sum)),
         rank_aic = rank(desc(distribution_specific_AIC)),
         rank_mae = rank(mae_mean),
         rank_acf = rank(acf1),
         rank_size = rank(desc(size_cycle1)),
         rank_mean = (rank_acf + rank_ll + rank_size)/3,
         rank_mean_min = min(rank_mean)) %>%
  dplyr::filter(rank_aic == max(rank_aic)) %>% 
  dplyr::filter(metric_similar == T) %>% 
  mutate(city = factor(city, levels = city_ordered),
         cycle1_selected = step_size_standardised) -> rsv_stl
  

rsv_stl_all <- rsv_stl

bind_rows(flu_stl, 
          rsv_stl) %>% 
  mutate(disease = factor(disease,
                          levels = c("flu", "rsv"),
                          labels = c("Influenza",
                                     "RSV")))-> p1_tab

p1_tab %>% 
  ggplot(., aes(x = disease, y = city, fill = cycle1_selected)) +
  geom_tile(color = "black", size = 1) +
  labs(x = "", y = "") +
  scale_fill_viridis_c(limits = c(10, 14), option = "magma") + 
  coord_fixed() -> p1

#### cycle2: selected ####
LL_flu %>% 
  bind_rows() %>% 
  dplyr::filter(model == "mstl") %>% 
  group_by(city) %>% 
  mutate(metric_optimal = min(distribution_specific_AIC),
         metric_similar = distribution_specific_AIC < (metric_optimal + 10),
         # ll_sum_max = max(ll_sum),
         # ll_relative = if_else(ll_sum < 0, ll_sum_max/ll_sum, ll_sum/ll_sum_max),
         # ll_similar = sum(ll_relative >= 0.9) - 1,
         # ll_relative = ll_sum_max/ll_sum,
         step_size1_standardised = case_when(step_size_char == "daily" ~ length_cycle1/30,
                                            step_size_char == "weekly" ~ length_cycle1/4,
                                            step_size_char == "monthly" ~ length_cycle1),
         step_size2_standardised = case_when(step_size_char == "daily" ~ length_cycle2/30,
                                             step_size_char == "weekly" ~ length_cycle2/4,
                                             step_size_char == "monthly" ~ length_cycle2),
         acf1_max = max(acf1),
         acf1_relative = acf1/acf1_max,
         acf1_similar = sum(acf1_relative >= 0.9) - 1,
         rank_ll = rank(desc(ll_sum)),
         rank_mae = rank(mae_mean),
         rank_acf = rank(acf1),
         rank_size = rank(desc(size_cycle1)),
         rank_mean = (rank_acf + rank_ll + rank_size)/3,
         rank_mean_min = min(rank_mean)) -> flu_mstl_all

flu_mstl_all %>% 
  dplyr::filter(metric_optimal == distribution_specific_AIC) -> flu_mstl
  # dplyr::filter(rank_mean == rank_mean_min) %>% 

LL_rsv %>% 
  bind_rows() %>% 
  dplyr::filter(model == "mstl") %>% 
  group_by(city) %>% 
  mutate(metric_optimal = min(distribution_specific_AIC),
         metric_similar = distribution_specific_AIC < (metric_optimal + 10),
         # ll_sum_max = max(ll_sum),
         # ll_relative = if_else(ll_sum < 0, ll_sum_max/ll_sum, ll_sum/ll_sum_max),
         # ll_similar = sum(ll_relative >= 0.9) - 1,
         # ll_relative = ll_sum_max/ll_sum,
         step_size1_standardised = case_when(step_size_char == "daily" ~ length_cycle1/30,
                                             step_size_char == "weekly" ~ length_cycle1/4,
                                             step_size_char == "monthly" ~ length_cycle1),
         step_size2_standardised = case_when(step_size_char == "daily" ~ length_cycle2/30,
                                             step_size_char == "weekly" ~ length_cycle2/4,
                                             step_size_char == "monthly" ~ length_cycle2),
         acf1_max = max(acf1),
         acf1_relative = acf1/acf1_max,
         acf1_similar = sum(acf1_relative >= 0.9) - 1,
         rank_ll = rank(desc(ll_sum)),
         rank_mae = rank(mae_mean),
         rank_acf = rank(acf1),
         rank_size = rank(desc(size_cycle1)),
         rank_mean = (rank_acf + rank_ll + rank_size)/3,
         rank_mean_min = min(rank_mean)) -> rsv_mstl_all

rsv_mstl_all %>% 
  dplyr::filter(metric_optimal == distribution_specific_AIC) -> rsv_mstl
  # dplyr::filter(rank_mean == rank_mean_min))


bind_rows(flu_mstl,
          rsv_mstl) %>% 
  mutate(city = factor(city, levels = city_ordered),
         disease = factor(disease,
                          levels = c("flu", "rsv"),
                          labels = c("Influenza",
                                     "RSV"))) -> p2_tab

p2_tab %>% 
  ggplot(., aes(x = disease, y = city, fill = step_size2_standardised)) +
  geom_tile(color = "black", size = 1) +
  scale_fill_viridis_c(option = "magma", limits = c(18, 36)) +
  labs(x = "", y = "") +
  coord_fixed() -> p2

#### panel3: data range analysed ####
data_flu %>% 
  bind_rows(.id = "study") %>% 
  separate(study, into = c("first_author", "year", "city")) %>% 
  group_by(year, city, first_author) %>% 
  summarise(date_min = min(date),
            date_max = max(date)) %>% 
  mutate(date_min = if_else(first_author == "Yan", lubridate::ymd("2010-10-04"), date_min),
         date_max = if_else(first_author == "Yan", lubridate::ymd("2020-01-05"), date_max),
         duration = date_max - date_min,
         disease = "Influenza") %>% 
  bind_rows(data_rsv %>% 
              bind_rows(.id = "study") %>% 
              separate(study, into = c("first_author", "year", "city")) %>% 
              group_by(year, city, first_author) %>% 
              summarise(date_min = min(date),
                        date_max = max(date)) %>% 
              mutate(date_min = if_else(first_author == "Yan", lubridate::ymd("2010-10-04"), date_min),
                     date_max = if_else(first_author == "Yan", lubridate::ymd("2020-01-05"), date_max),
                     duration = date_max - date_min,
                     disease = "RSV")) %>% 
  mutate(city = factor(city, levels = city_ordered),
         duration = as.numeric(duration)) %>% 
  ggplot(., aes(x = disease, y = city, fill = duration)) +
  geom_tile(color = "black", size = 1) +
  scale_fill_viridis_c(option = "magma") +
  coord_fixed() +
  labs(x = "", y = "") -> p3
  
#### panel4: unit ####
LL_flu %>% 
  bind_rows() %>% 
  dplyr::select(city, step_size_char, data_type) %>% 
  distinct() %>% 
  mutate(disease = "Influenza") %>% 
  bind_rows(LL_rsv %>% 
              bind_rows() %>% 
              dplyr::select(city, step_size_char, data_type) %>% 
              distinct() %>% 
              mutate(disease = "RSV")) %>% 
  mutate(city = factor(city, levels = city_ordered),
         step_size_char = factor(step_size_char,
                                 levels = c("daily", "weekly", "monthly"),
                                 labels = c("Daily", "Weekly", "Monthly"))) %>% 
  ggplot(., aes(x = disease, y = city, fill = step_size_char)) +
  geom_tile() +
  geom_tile(color = "black", size = 1) +
  scale_fill_viridis_d(option = "magma") +
  coord_fixed() +
  labs(x = "", y = "") -> p4

 theme_here <- theme_bw() +
  theme(legend.position = "top",
        axis.text = element_text(size = 14),
        title = element_text(size = 14),
        legend.text = element_text(size = 12))

plot_grid(p1 + 
            labs(fill = "",
                 title = "Length of\nwithin-year cycles\n(weeks)") +
            theme_here +
            guides(fill = guide_colourbar(barwidth = 10)), 
          p2 + 
            labs(title = "Length of\nbetween-year cycles\n(weeks)", fill = "") +
            theme_here +
            guides(fill = guide_colourbar(barwidth = 10)) ,
          p3 + 
            labs(title = "Time series\ncoverage\n(days)", fill = "") +
            theme_here +
            guides(fill = guide_colourbar(barwidth = 10)), 
          p4 + 
            labs(fill = "", title = "Unit of\ntime series") +
            theme_here +
            guides(fill = guide_legend(nrow = 2)), 
          nrow = 1,
          labels = c("(A)", 
                     "(B)", 
                     "(C)", 
                     "(D)"),
          label_x = 0.2, label_y = 0.89,
          align = "hv",
          axis = "tblr") -> p_top

 ggsave("figures/fig3_v2.jpg", width = 14, height = 8)

get_top <- function(m = 6){
  
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
        city) %>% 
    map(arrange, distribution_specific_AIC) %>% 
    map(mutate, rank_sum = rank_aic + rank_bic) %>% 
    map(head, m) %>% 
    bind_rows() %>% 
    mutate(city = factor(city, levels = city_ordered),
           direction = if_else(city %in% c("Beijing", "Xian", "Lanzhou"),
                               "North",
                               "South"),
           disease = "Influenza") -> p_flu 
    
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
        city) %>% 
    map(arrange, distribution_specific_AIC) %>% 
    map(mutate, rank_sum = rank_aic + rank_bic) %>% 
    map(head, m) %>% 
    bind_rows() %>% 
    mutate(city = factor(city, levels = city_ordered),
           direction = if_else(city %in% c("Beijing", "Xian", "Lanzhou"),
                               "North",
                               "South"),
           disease = "RSV") -> p_rsv
  
  bind_rows(p_flu,
            p_rsv)-> tmp
  
  return(tmp)
}

p5_tab_5 <- get_top(m = 5) %>% mutate(top = "Top 5")
p5_tab_10 <- get_top(m = 10) %>% mutate(top = "Top 10")

bind_rows(p5_tab_5, 
          p5_tab_10) %>% 
  mutate(top = factor(top, levels = c("Top 5", "Top 10"),
                      labels = c("Top 5",
                                 "Top 10"))) -> p5_tab

p5_tab %>% 
  dplyr::filter(top == "Top 5") %>% 
  ggplot(., aes(x = step_size2_standardised, fill = direction)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  labs(x = "Length of between-year cycles\namong top performing models",
       y = "Density",
       fill = "Geographic category",
       color = "Geographic category") +
  facet_grid(top~disease) +
  scale_fill_manual(values = c("#ffc38b", "#b63679")) +
  geom_vline(data = p5_tab %>% 
               group_by(top, disease, direction) %>% 
               dplyr::filter(top == "Top 5") %>% 
               summarise(md = median(step_size2_standardised, na.rm = T)),
             aes(xintercept = md,
                 color = direction),
             linetype = 2,
             size = 2) +
  scale_colour_manual(values = c("#ffc38b", "#b63679")) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),        
        strip.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14)) -> p5

ggsave("figs/fig_s1.jpg")

plot_grid(p_top, p5, nrow = 2)
ggsave("figures/fig3_v3_update.jpg", width = 14, height = 16)
       