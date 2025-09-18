library(qs)
library(tidyverse)
library(yaml)
library(ggridges)

yaml_data <- yaml.load_file("data/metadata.yaml")

LL_flu <- read_rds(paste0("results/LL_flu_v3.rds"))
LL_rsv <- read_rds(paste0("results/LL_rsv_v4.rds"))

res_flu <- qread("results/res_flu.qs")
res_rsv <- qread("results/res_rsv.qs")

city_names <- sapply(1:8, function(x) unique(LL_flu[[x]]$city))


# 
risk_def_flu <- lapply(1:8, function(x) quantile(exp(res_flu[[x]][[1]]$original$value), seq(0,1,0.1))) 
risk_def_rsv <- lapply(1:8, function(x) quantile(exp(res_rsv[[x]][[1]]$original$value), seq(0,1,0.1))) 

for(i in 1:8){
  risk_def_flu[[i]][1] <- risk_def_rsv[[i]][1] <- -Inf
  risk_def_flu[[i]][length(risk_def_flu[[i]])] <- risk_def_rsv[[i]][length(risk_def_rsv[[i]])] <- Inf
}

# merge time series between flu and rsv
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
           outbreak_7 = risk_cat_rsv_num >= 7 & risk_cat_flu_num >= 7,
           outbreak_8 = risk_cat_rsv_num >= 8 & risk_cat_flu_num >= 8,
           outbreak_9 = risk_cat_rsv_num >= 9 & risk_cat_flu_num >= 9,
           outbreak_10 = risk_cat_rsv_num >= 10 & risk_cat_flu_num >= 10) %>% 
    group_by(year, path) %>% 
    summarise(outbreak_7 = sum(outbreak_7),
              outbreak_8 = sum(outbreak_8),
              outbreak_9 = sum(outbreak_9),
              outbreak_10 = sum(outbreak_10),
              .groups = "drop") %>% 
    mutate(grid = parse_number(path),
           city = city_names[[i]]) -> p_tab[[i]]
}

# proportion of joint outbreaks
p_tab %>% 
  bind_rows() %>% 
  group_by(path, grid, city) %>% 
  mutate_at(vars(starts_with("outbreak_")),
            ~replace(.>0,1,0)) -> p_tab_1
  
p_tab_1 %>% 
  group_by(path, city) %>% 
  summarise(outbreak_8 = sum(outbreak_8), 
            outbreak_9 = sum(outbreak_9),
            outbreak_10 = sum(outbreak_10),
            year_tot = n(),
            .groups = "keep") %>% 
  mutate_at(vars(starts_with("outbreak_")),
            ~./year_tot) %>% 
  pivot_longer(cols = starts_with("outbreak")) %>% 
  mutate(city = factor(city, levels = city_names),
         name = factor(name, 
                       levels = c("outbreak_8", "outbreak_9", "outbreak_10"),
                       labels = c("Joint top 30 percentiles",
                                  "Joint top 20 percentiles",
                                  "Joint top 10 percentiles"))) -> p_tab_2

ggplot(p_tab_2, aes(x = value, y = city, fill = name, color = name)) +
  geom_density_ridges(
    alpha = 0.6, 
    scale = 0.9,
    bandwidth = 0.1  # Increase this (default is usually 0.05
  ) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_fill_viridis_d(option = "plasma", alpha = 0.6, direction = -1) +
  scale_color_viridis_d(option = "plasma", alpha = 0.8, direction = -1) +
  theme_ridges() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 12, face = "bold"),
    panel.spacing = unit(0.5, "lines"),
    axis.text.y = element_text(hjust = 0.5),  # Center the y-axis labels
    plot.title = element_text(size = 16, face = "bold")
  ) +
  labs(
    # title = "Ridge Plot: Joint Outbreak Proportions by City",
    # subtitle = "Distribution of concurrent high-risk periods across different thresholds",
    x = "Proportion of Years with Joint Outbreaks",
    y = "",
    fill = "Risk Threshold",
    color = "Risk Threshold"
  ) -> p_save

ggsave("figures/figure5.png", p_save,
       height = 10, width = 12)



  # ggplot(., aes(y = city, x = value, color = name)) +
  # geom_boxplot() + facet_wrap(~name)
  # ggplot(., aes(x = value, group = name, color = name, fill = name)) +
  # geom_density(adjust = 3, alpha = 0.2) +
  # facet_wrap(~city, ncol = 4, scales = "free") +
  # theme_bw() +
  # lims(x = c(0,1)) +
  # theme(legend.position = "top",
  #       legend.text = element_text(size = 14),
  #       legend.title = element_text(size = 14),
  #       strip.text = element_text(size = 16),
  #       axis.text = element_text(size = 14),
  #       axis.title = element_text(size = 14)) 



p_tab_1 %>%
  pivot_longer(starts_with("outbreak_")) %>%
  group_by(name, path, grid, city) %>%
  summarise(
    year_range = max(year) - min(year),
    outbreak_years = list(year[value == 1]),
    n_outbreaks = sum(value == 1),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    diff_mean = case_when(
      n_outbreaks == 0 ~ year_range,
      n_outbreaks == 1 ~ year_range,
      n_outbreaks > 1 ~ {
        outbreak_years_sorted <- sort(unlist(outbreak_years))
        intervals <- diff(outbreak_years_sorted)
        mean(intervals)
      }
    )
  ) -> p_tab_3


p_tab_3 %>%
  dplyr::filter( name != "outbreak_7") %>%
  mutate(outbreak_cat = if_else(n_outbreaks %in% c(0,1), "No interval calculated", "Outbreak intervals"),
         mean_log = log(diff_mean)) %>%
  ggplot(., aes(x = diff_mean, color = outbreak_cat, fill = outbreak_cat)) +
  geom_histogram() +
  facet_wrap(~ city + name, scales = "free",ncol=3) +
  scale_y_log10()
