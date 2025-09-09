LL_flu %>% 
  map(arrange, desc(ll_sum)) %>% 
  map(mutate,
      rank_ll = 1:n(),
      n = max(rank_ll),
      top_np = round(n*0.1)) %>% 
  map(dplyr::filter,
      rank_ll <= top_np) %>% 
  bind_rows() %>% 
  dplyr::select(-contribution_remainder, -contribution_trend) %>% 
  mutate(disease = "Influenza") %>% 
  bind_rows(LL_rsv %>% 
              map(arrange, desc(ll_sum)) %>% 
              map(mutate,
                  rank_ll = 1:n(),
                  n = max(rank_ll),
                  top_np = round(n*0.1)) %>% 
              map(dplyr::filter,
                  rank_ll <= top_np) %>% 
              bind_rows() %>% 
              dplyr::select(-contribution_remainder, -contribution_trend) %>% 
              mutate(disease = "RSV")) %>% 
  mutate(direction = if_else(city %in% c("Beijing", "Xian", "Lanzhou"),
                             "North",
                             "South"),
         city = factor(city,
                       levels = rev(city_ordered))) %>% 
  pivot_longer(cols = starts_with("contribution")) %>% 
  mutate(name = factor(name,
                       levels = c("contribution_cycle1",
                                  "contribution_cycle2"),
                       labels = c("Within-year",
                                  "Between-year"))) -> p_tab

ggplot(p_tab, aes(x = 1, y =  value, color = name, fill = name)) +
  geom_boxplot() +
  scale_x_discrete(drop = T) +
  ggh4x::facet_nested(direction + city  ~ disease) +
  theme_bw() +
  theme(legend.position = "top", 
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),        
        strip.text = element_text(size = 14),
        strip.background = element_rect(fill = NA),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14)) +
  scale_colour_manual(values = c("#800080", "#536D33")) +
  scale_fill_manual(values = alpha(c("#800080", "#536D33"), 0.2)) + 
  guides(color = guide_legend(nrow=2,byrow=TRUE)) +
  labs(color = "Component", fill = "Component", 
       x = "City",
       y = expression(frac("variance(component)", "variance(observed)"))) -> p_save

ggsave("figs/fig4.png", plot = p_save, 
       width = 4, height = 10)

ggplot(p_tab, aes(x = 1, y =  value, color = name, fill = name)) +
  geom_boxplot() +
  scale_x_discrete(drop = T) +
  ggh4x::facet_nested(direction  ~ disease) +
  theme_bw() +
  theme(legend.position = "bottom", 
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),        
        strip.text = element_text(size = 14),
        strip.background = element_rect(fill = NA),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14)) +
  scale_colour_manual(values = c("#800080", "#536D33")) +
  scale_fill_manual(values = alpha(c("#800080", "#536D33"), 0.2)) + 
  guides(color = guide_legend(nrow = 1,
                              byrow = TRUE)) +
  labs(color = "Component", fill = "Component", 
       x = "",
       y = expression(frac("variance(component)", "variance(observed)"))) -> p_save

ggsave("figs/fig4_combined.png", plot = p_save, 
       width = 4, height = 10)
 