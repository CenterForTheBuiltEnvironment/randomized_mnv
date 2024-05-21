library(tidyverse)

resample <- function(data){
  
  #' Resampling function for TOWT model fitting
  #' @description
    #' This is a function defined to balance the sample size of each day of the week,
    #' since the reference is selected as the highest number, the method is mostly oversampling minority
  #' @param data The unbalanced dataset
  #'
  #' @return a dataset has equal amount of day of week samples
  #' @note
    #' 1. Before using the function, carefully check the distribution of sample sizes of each day of week
    #' 2. Using the highest number as oversampling reference may produce error in the towt model,
      #' too much oversampling of one sample may lead to incorrect datetime interval value (less than 15-minute in prediction)
  #' 

  # summarize the sample size of each day of week
  df_summary <- data %>%
    mutate(wday = wday(time, week_start = 1, label = TRUE)) %>%
    group_by(wday,
             strategy) %>%
    summarise(n = n()) %>% 
    mutate(n = n, 
           perc = n / sum(n)) %>%
    ungroup() %>%
    group_by(wday) %>%
    mutate(weekday_total = sum(n)) %>%
    ungroup()
  
  # get sampling reference
  l <- max(df_summary$n)
  
  data <- data %>%
    mutate(wday = wday(time, week_start = 1, label = FALSE)) 
  
  post_samp <- tibble()
  
  # start over-sampling
  for (s in 1:3){
    for (i in 1:7){
      pre_samp <- data %>%
        filter(strategy == s, 
               wday == i)
      len <- nrow(pre_samp)
  
      
      if (len <= l){
        sampled_rows <- pre_samp[sample(nrow(pre_samp), l - len, replace = TRUE), ]
      } else {
        sampled_rows <- pre_samp[sample(nrow(pre_samp), l, replace = TRUE), ]
      }

      if (len <= l){
        post_samp <- rbind(pre_samp, sampled_rows, post_samp)
      } else {
        post_samp <- rbind(sampled_rows, post_samp)
      }
      
      
    }
  } 
  post_samp <- post_samp %>%
    dplyr::select(-wday)
  
return(post_samp)
}
