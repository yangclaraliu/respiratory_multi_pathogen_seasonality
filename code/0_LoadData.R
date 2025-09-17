library(tidyverse)
library(forecast)
library(data.table)
library(yaml)
library(magrittr)
library(cowplot)
library(qs)

yaml_data <- yaml.load_file("data/metadata.yaml")

dir_flu <- "data/raw/flu/"
dir_rsv <- "data/raw/rsv/"

fn_flu <- list("Chen-2023-Xian.csv",
               "Liu-2022-Lanzhou.csv",
               "Liu-2023-Wuhan.csv",
               "Peng-2016-Yunfu.csv",
               "Wu-2018-Beijing.csv",
               "Yan-2024-Guangzhou.csv",
               "Yu-2019-Suzhou.csv",        
               "Zhong-2016-Wenzhou.csv")


city_ordered <- c("Yunfu", "Guangzhou", "Wenzhou","Wuhan", 
                  "Suzhou", "Xian", "Lanzhou", "Beijing")

fn_rsv <- paste0(dir_rsv, list.files(dir_rsv)) %>% 
  map(list.files, pattern = ".csv")

paste0(paste0(dir_flu, list.files(dir_flu), "/"), fn_flu) %>% 
  map(read_csv, col_names = F) %>% 
  setNames(gsub(".csv", "",fn_flu)) -> data_flu

yaml_data$Guangzhou$flu$raw_data_freq <- "weekly"
data_flu$`Yan-2024-Guangzhou` %<>% 
  mutate(X2 = if_else(X2 < 0, 0.01, X2))

fn_rsv %>% 
  setNames(list.files(dir_rsv)) %>% 
  map(data.frame) %>% 
  bind_rows(., .id = "folder_name") %>% 
  mutate(root = dir_rsv) %>% 
  rename(fn = `.x..i..`) %>% 
  mutate(dir_all = paste0(root, 
                          "/",
                          folder_name,
                          "/",
                          fn)) %>% 
  pull(dir_all) -> dir_rsv_file

dir_rsv_file %>% 
  map(read_csv, col_names = F) %>% 
  setNames(dir_rsv_file) -> data_rsv_raw

data_rsv <- list()
data_rsv[[yaml_data$Beijing$rsv$name]] <- data_rsv_raw$`data/raw/rsv//Cui-2013-Beijing/Cui-2013-Beijing-Curve.csv`
yaml_data$Beijing$rsv$data_type <- "rates"

data_rsv[[yaml_data$Wuhan$rsv$name]] <- data_rsv_raw$`data/raw/rsv//Hu-2023-Wuhan/Hu-2023-Wuhan-Curve.csv`
yaml_data$Wuhan$rsv$data_type <- "rates"

data_rsv[[yaml_data$Wenzhou$rsv$name]] <- data_rsv_raw$`data/raw/rsv//Jie-2011-Wenzhou/Jie-2011-Wenzhou.csv`
yaml_data$Wenzhou$rsv$data_type 

data_rsv[[yaml_data$Xian$rsv$name]] <- data_rsv_raw$`data/raw/rsv//Lei-2016-Xian/Lei-2016-Xian.csv`
yaml_data$Xian$rsv$data_type 
data_rsv$`Lei-2016-Xian` %<>% 
  mutate(X2 = if_else(X2 == 0, 0.01, X2))

data_rsv[[yaml_data$Suzhou$rsv$name]] <- data_rsv_raw$`data/raw/rsv//Lu-2015-Suzhou/Lu-2015-Suzhou.csv`
yaml_data$Suzhou$rsv$data_type 

data_rsv[[yaml_data$Yunfu$rsv$name]] <- data_rsv_raw$`data/raw/rsv//Qin-2022-Yunfu/Qin-2022-Yunfu.csv`
yaml_data$Yunfu$rsv$data_type 

data_rsv[[yaml_data$Guangzhou$rsv$name]] <- data_rsv_raw$`data/raw/rsv//Zou-2016-Guangzhou/Zou-2016-Guangzhou-Curve.csv`
yaml_data$Guangzhou$rsv$data_type <- "rates"

# Lanzhou
data_rsv[[yaml_data$Lanzhou$rsv$name]] <- read_csv("data/processed/rsv/Liang-2015-Lanzhou-Cases.csv") %>% 
  rename(X2 = value)

# paste dates
paste0("data/processed/flu/", list.files("data/processed/flu", pattern = ".csv")) %>% 
  map(read_csv) %>% 
  setNames(gsub(".csv", "",list.files("data/processed/flu", pattern = ".csv")) %>% 
             gsub("-Cases","",.) %>% 
             gsub("-Rates","",.)) -> dates_flu

data_flu %>% map(dim) %>% map(data.table) %>% map(rownames_to_column) %>% bind_rows(.id = "study_name") %>% mutate(source = "data") %>% 
  bind_rows(dates_flu %>% map(dim)  %>% map(data.table) %>% map(rownames_to_column) %>% bind_rows(.id = "study_name") %>% mutate(source = "dates")) %>% 
  pivot_wider(names_from = source,
              values_from = V1) %>% 
  dplyr::filter(data != dates)

paste0("data/processed/flu/", list.files("data/processed/flu", pattern = ".csv")) %>% 
  map(read_csv) %>% 
  setNames(gsub(".csv", "",list.files("data/processed/flu", pattern = ".csv")) %>% 
             gsub("-Cases","",.) %>% 
             gsub("-Rates","",.)) -> dates_flu

paste0("data/processed/rsv/", list.files("data/processed/rsv", pattern = ".csv")) %>% 
  map(read_csv) %>% 
  setNames(gsub(".csv", "",list.files("data/processed/rsv", pattern = ".csv")) %>% 
             gsub("-Cases","",.) %>% 
             gsub("-Rates","",.)) %>%  
  map(~.[complete.cases(.),]) -> dates_rsv

data_flu %>% map(dim) %>% map(data.table) %>% map(rownames_to_column) %>% bind_rows(.id = "study_name") %>% mutate(source = "data") %>%
  bind_rows(dates_flu %>% map(dim)  %>% map(data.table) %>% map(rownames_to_column) %>% bind_rows(.id = "study_name") %>% mutate(source = "dates")) %>%
  pivot_wider(names_from = source,
              values_from = V1) %>%
  dplyr::filter(data != dates)

data_rsv %>% map(dim) %>% map(data.table) %>% map(rownames_to_column) %>% bind_rows(.id = "study_name") %>% mutate(source = "data") %>%
  bind_rows(dates_rsv %>% map(dim)  %>% map(data.table) %>% map(rownames_to_column) %>% bind_rows(.id = "study_name") %>% mutate(source = "dates")) %>%
  pivot_wider(names_from = source,
              values_from = V1) %>%
  dplyr::filter(data != dates)

# rsv dates can be moved in directly
for(i in 1:8){
  name_tmp <- names(data_rsv)[i]
  data_rsv[[i]][, "date"] <- lubridate::ymd(dates_rsv[[name_tmp]]$date)
}

# flu dates - five of them can be moved in
for(i in 1:8){
  name_tmp <- names(data_flu)[i]
  if(!name_tmp %in% c("Chen-2023-Xian",
                     "Wu-2018-Beijing",
                     "Yan-2024-Guangzhou")){
    data_flu[[i]][, "date"] <- lubridate::ymd(dates_flu[[name_tmp]]$date)
  }
}

data_flu$`Chen-2023-Xian` %<>% 
  mutate(date = seq(lubridate::ymd(yaml_data$Xian$flu$start_date),
                    lubridate::ymd(yaml_data$Xian$flu$start_date) + n() -1,
                    by = "day"))

data_flu$`Wu-2018-Beijing` %<>% 
  mutate(date = seq(lubridate::ymd(yaml_data$Beijing$flu$start_date),
                    lubridate::ymd(yaml_data$Beijing$flu$start_date) + (n() -1)*7,
                    by = "week"))

data_flu$`Yan-2024-Guangzhou` %>% 
  mutate(date = seq(lubridate::ymd("2011-01-01"),
                    lubridate::ymd("2011-01-01") + (n() -1)*7,
                    by = "week")) %>% 
  ggplot(., aes(x = date, y = X2)) +
  geom_line() +
  geom_vline(xintercept = seq(lubridate::ymd("2011-01-01"),
                              lubridate::ymd("2019-01-01"),
                              "year"),
             linetype = 2)

data_rsv %>% 
  map(summarise,
      date_min = min(date),
      date_max = max(date)) %>% 
  bind_rows(.id = "loc") %>% 
  mutate(date_diff = date_max - date_min) %>% 
  separate(loc, into = c("seg1", "seg2", "seg3")) %>% 
  mutate(direction = if_else(seg3 %in% c("Beijing", "Xian", "Lanzhou"),
                             "North",
                             "South")) %>% 
  group_by(direction) %>% group_split()

