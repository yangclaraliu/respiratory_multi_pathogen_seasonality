library(dplyr)
library(survival)
library(broom)
library(ggplot2)
library(stringr)

# Prepare your data for survival analysis
# Extract individual outbreak intervals
individual_intervals <- p_tab_1 %>% 
  pivot_longer(starts_with("outbreak_"), names_to = "threshold") %>% 
  filter(value == 1) %>% 
  group_by(threshold, path, grid, city) %>% 
  arrange(year) %>% 
  mutate(
    intervals = c(NA, diff(year))
  ) %>% 
  filter(!is.na(intervals)) %>% 
  ungroup() %>% 
  mutate(
    gap_years = intervals,
    event = 1,
    city = city,
    name = threshold
  ) %>% 
  filter(gap_years > 0)

# Add censored cases (no outbreaks)
censored_cases <- p_tab_1 %>% 
  pivot_longer(starts_with("outbreak_"), names_to = "threshold") %>% 
  group_by(threshold, path, grid, city) %>% 
  summarise(
    year_range = max(year) - min(year),
    has_outbreak = any(value == 1),
    .groups = "drop"
  ) %>% 
  filter(!has_outbreak) %>% 
  mutate(
    gap_years = year_range,
    event = 0,  # Censored
    city = city,
    name = threshold
  )

single_outbreak_cases <- p_tab_1 %>% 
  pivot_longer(starts_with("outbreak_"), names_to = "threshold") %>% 
  filter(threshold != "outbreak_7") %>%  # Exclude outbreak_7
  group_by(threshold, path, grid, city) %>% 
  summarise(
    n_outbreaks = sum(value == 1),
    year_range = max(year) - min(year),
    .groups = "drop"
  ) %>% 
  filter(n_outbreaks == 1) %>%  # Exactly 1 outbreak
  mutate(
    gap_years = year_range,
    event = 0,  # Treat as censored (no interval to calculate)
    city = city,
    name = threshold
  )

# individual_intervals %>% 
#   dplyr::filter(grid == 1, city == "Beijing", threshold == "outbreak_9")

# Combine for survival analysis
df_s <- bind_rows(individual_intervals, censored_cases, single_outbreak_cases) %>% 
  dplyr::filter(threshold != "outbreak_7")

# KM fits by city × threshold
fit <- survfit(Surv(gap_years, event) ~ city + name, data = df_s, conf.int = 0.95)

# Tidy for ggplot
km <- broom::tidy(fit) %>%
  separate(strata, into = c("city", "name"), sep = ", ?") %>%
  mutate(
    city = sub("^city=", "", city),
    name = sub("^name=", "", name),
    name = trimws(name)
  )  %>% 
  mutate(city = factor(city, levels = rev(city_names)),
         name = factor(name, 
                       levels = c("outbreak_8", "outbreak_9", "outbreak_10"),
                       labels = c("Joint top 30 percentiles",
                                  "Joint top 20 percentiles",
                                  "Joint top 10 percentiles")))

median_times <- km %>% 
  group_by(city, name) %>% 
  filter(1 - estimate >= 0.5) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(city, name, time) %>% 
  # Add a flag for groups that reach 50%
  mutate(reaches_50 = TRUE)

# Add groups that never reach 50% (optional - for annotation)
never_reach_50 <- km %>% 
  group_by(city, name) %>% 
  summarise(max_prop = max(1 - estimate), .groups = "drop") %>% 
  filter(max_prop < 0.5) %>% 
  mutate(time = NA, reaches_50 = FALSE)

# Combine for annotation
all_groups <- bind_rows(median_times, never_reach_50)  # %>% 
  # dplyr::filter(name != "Joint top 30 percentiles")

# Plot as cumulative incidence (1 - S(t))
p <- ggplot(km, aes(time, 1 - estimate, colour = name, group = interaction(name, city))) +
  # geom_ribbon(aes(ymin = 1 - conf.high, ymax = 1 - conf.low, fill = name), 
  #             alpha = 0.2, color = NA) +  # Add this line for CI ribbon
  geom_step(linewidth = 0.7) +
  geom_hline(yintercept = 0.5, linetype = "dashed", alpha = 0.5) +
  geom_vline(data = all_groups, 
             aes(xintercept = time, color = name), 
             linetype = "dashed", alpha = 0.7) +
  facet_wrap(~ city, ncol = 2) +
  scale_y_continuous(
    labels = scales::percent_format(), 
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2)
  ) +
  scale_x_continuous(
    limits = c(0, 10),
    breaks = seq(0, max(km$time, na.rm = TRUE), by = 5)
  ) +
  # scale_x_continuous(limits = c(0,
  #                               10
  #                               # max(km$time, na.rm = TRUE)
  #                               )) +
  labs(
    # title = "Cumulative Incidence of Next Joint Outbreak",
    # subtitle = "By t years, Y% simulations have had the next joint outbreak",
    x = "Gap to next joint outbreak (years)",
    y = "Cumulative proportion with next outbreak",
    colour = "Threshold",
    fill = "Threshold"  # Add this for the legend
  ) +
  theme_bw(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom",
    plot.title = element_text(size = 16, face = "bold")
  )  +
  scale_color_viridis_d(option = "plasma", direction = -1) 

print(p)

ggsave("figures/figure6.png", p,
       height = 12, width = 8)
