library(pacman)
path_root <- "code/"
source(paste0(path_root, "0_LoadData.R"))
source(paste0(path_root, "0_1_run_model.R"))

city_ordered <- c("Yunfu", "Guangzhou", "Wenzhou","Wuhan", 
                  "Suzhou", "Xian", "Lanzhou", "Beijing")
 
# city_ordered %>%
#   map(~get_LL(disease = "flu", city = .)) %>%
#   map(arrange, desc(contribution_cycle2)) -> LL_flu
# 
# city_ordered %>%
#   map(~get_LL(disease = "rsv", city = .)) %>%
#   map(arrange, desc(contribution_cycle2)) -> LL_rsv
# 
# write_rds(LL_flu,
#           paste0("results/LL_flu_v3.rds"))
# write_rds(LL_rsv,
#           paste0("results/LL_rsv_v3.rds"))

LL_flu <- read_rds(paste0("results/LL_flu_v3.rds"))
LL_rsv <- read_rds(paste0("results/LL_rsv_v3.rds"))

LL_flu %>% 
  setNames(city_ordered) %>% 
  bind_rows(.id = "city") %>% 
  dplyr::filter(model == "stl") %>% 
  group_by(city) %>% 
  mutate(ll_max = max(ll_sum)) %>% 
  dplyr::filter(ll_sum == ll_max) -> stl_model_selected_flu

LL_flu %>% 
  setNames(city_ordered) %>% 
  bind_rows(.id = "city") %>% 
  dplyr::filter(model == "mstl") %>% 
  dplyr::select(-model) %>%
  rename(ll_sum_mstl = ll_sum) %>% 
  right_join(stl_model_selected_flu %>% 
              dplyr::select(-length_cycle2, -model, -ll_max) %>% 
              rename(ll_sum_stl = ll_sum),
            by = c("city",
                   "disease",
                   "length_cycle1")) #%>% 
  group_by(city_ordered) %>% 
  mutate(ll_sum_mstl_max = max(ll_sum_mstl)) %>% 
  dplyr::filter(ll_sum_mstl == ll_sum_mstl_max) %>% 
  mutate(ll_diff_relative = ll_sum_stl/ll_sum_mstl,
         ll_diff_absolute = ll_sum_mstl - ll_sum_stl)

LL_rsv %>% 
  setNames(city_ordered) %>% 
  bind_rows(.id = "city") %>% 
  dplyr::filter(model == "stl") %>% 
  group_by(city) %>% 
  mutate(ll_max = max(ll_sum)) %>% 
  dplyr::filter(ll_sum == ll_max) -> stl_model_selected_rsv

LL_rsv %>% 
  setNames(city_ordered) %>% 
  bind_rows(.id = "city") %>% 
  dplyr::filter(model == "mstl") %>% 
  dplyr::select(-model) %>%
  rename(ll_sum_mstl = ll_sum) %>% 
  right_join(stl_model_selected_rsv %>% 
               dplyr::select(-length_cycle2, -model, -ll_max) %>% 
               rename(ll_sum_stl = ll_sum),
             by = c("city",
                    "length_cycle1")) %>% 
  group_by(city) %>% 
  mutate(ll_sum_mstl_max = max(ll_sum_mstl)) %>% 
  dplyr::filter(ll_sum_mstl == ll_sum_mstl_max) %>% 
  mutate(ll = ll_sum_stl/ll_sum_mstl)


run_model(disease = "flu", city = "Lanzhou", model_type = "mstl",
          cycle1_len = 12, cycle2_len = 22) %>% 
  .[[1]] %>% data.frame() %>% pull(Remainder) %>% auto.arima %>% AIC


run_model(disease = "flu", city = "Lanzhou", model_type = "stl",
          cycle1_len = 12, cycle2_len = NA) %>% 
  .[[1]] %>% .$time.series %>% .[,3] %>% auto.arima() %>% AIC
  

run_model(disease = "flu", city = "Lanzhou", model_type = "mstl",
          cycle1_len = 12, cycle2_len = 22) %>% .[[1]] %>% plot()
  map(data.table) %>% 
  .[[1]] %>% 
  mutate(predicted = Trend + Seasonal48 + Seasonal88) -> x

run_model(disease = "flu", city = "Lanzhou", model_type = "stl",
          cycle1_len = 12, cycle2_len = NULL) %>% 
  map(~.$time.series) %>% 
  .[[1]] %>% data.table %>% 
  mutate(predicted = seasonal + trend,
         Data = seasonal + trend + remainder) -> x

x[,c("Data", "predicted")] %>% mutate(model = "mstl") %>% 
  bind_rows(y[,c("Data", "predicted")] %>% mutate(model = "stl")) %>% 
  ggplot(., aes(x = (predicted), y = (Data), color = model)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) 

x[,c("Data", "predicted")] %>% mutate(model = "mstl") %>% rownames_to_column() %>% 
  bind_rows(y[,c("Data", "predicted")] %>% mutate(model = "stl") %>% rownames_to_column()) %>% 
  mutate(rowname = as.numeric(rowname)) %>% 
  ggplot(., aes(x = rowname)) +
  geom_line(aes(y = Data)) +
  geom_line(aes(y = predicted, color = model)) +
  facet_wrap(~model)
  
LL_rsv %>% 
  setNames(city_ordered) %>% 
  bind_rows(.id = "city") %>% 
  dplyr::filter(model == "mstl") %>% 
  dplyr::select(-model) %>%
  rename(ll_sum_mstl = ll_sum) %>% 
  right_join(stl_model_selected_rsv %>% 
               dplyr::select(-length_cycle2, -model, -ll_max) %>% 
               rename(ll_sum_stl = ll_sum),
             by = c("city",
                    "length_cycle1")) %>% 
  mutate(ll = ll_sum_stl/ll_sum_mstl) %>% 
  ggplot(., aes(y= city, x = length_cycle2, fill = ll)) +
  geom_tile()
