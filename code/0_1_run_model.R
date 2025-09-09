run_model <- function(disease = NULL,
                      city = NULL,
                      model_type = NULL,
                      cycle1_len = NULL,
                      cycle2_len = NULL){
  
  study_name = yaml_data[[city]][[disease]]$name
  data_type = yaml_data[[city]][[disease]]$data_type
  cat("Study currently in use is:", study_name)
  cat("Data type currently in use is:", data_type)
  
  if(disease == "flu") {tmp <- data_flu[[study_name]]}
  if(disease == "rsv") {tmp <- data_rsv[[study_name]]}
  
  n_steps <- nrow(tmp)
  step_size_char <- yaml_data[[city]][[disease]]$raw_data_freq 
  step_size_num <- case_when(step_size_char == "daily" ~ 30,
                             step_size_char == "weekly" ~ 4,
                             step_size_char == "monthly"~ 1)
  cycle_max <- round(n_steps/2)
  
  cycle1 <- cycle1_len*step_size_num #seq(10*step_size_num, 14*step_size_num, step_size_num)
  cycle2 <- cycle2_len*step_size_num
  
  if(data_type == "cases" & model_type == "stl"){
    cycle1 %>% 
      map(~ts(log(tmp$X2), frequency = .)) %>% 
      map(stl, s.window = "periodic") -> output
  }
  
  if(data_type == "cases" & model_type == "mstl"){
    map2(CJ(cycle1, cycle2) %>% pull(cycle1), 
         CJ(cycle1, cycle2) %>% pull(cycle2),
         ~msts(data = log(tmp$X2), 
               seasonal.periods = c(.x, .y))) %>% 
      map(mstl, iterate = 3) -> output
  }
  
  if(data_type == "rates" & model_type == "stl"){
    cycle1 %>% 
      map(~ts(log(tmp$X2/100), frequency = .)) %>% 
      map(stl, s.window = "periodic") -> output
  }
  
  if(data_type == "rates" & model_type == "mstl"){
    map2(CJ(cycle1, cycle2) %>% pull(cycle1), 
         CJ(cycle1, cycle2) %>% pull(cycle2),
         ~msts(data = log(tmp$X2/100), 
               seasonal.periods = c(.x, .y))) %>% 
      map(mstl, iterate = 3) -> output
  }
  
  return(output)
}
