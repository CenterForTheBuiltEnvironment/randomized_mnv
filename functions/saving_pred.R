library(tidyverse)
library(lubridate)
library(dplyr)
source("../functions/model_pred.R")
source("../functions/resample.R")
source("../functions/find_occ_unocc.R")

saving_pred <- function(dataframe, carryover_check = FALSE, baseline_results = NULL){
  
  #' Energy saving calculation defined for TOWT prediction results (contains model_pred function)
  #' @description 
  #' by calculating the difference between baseline estimation and intervention estimation,
  #' and normalized on baseline for fractional savings.
  #'
  #' @param dataframe Dataframe that contains towt prediction info
  #' @param carryover_check Specify whether estimate for carryover purpose
  #' @param baseline_results If carryover_check == TRUE, baseline_results dataframe is needed
  #'
  #' @return A numerical percentage as annual fraction saving

  # load weather file  
  weather <- read_csv("../readfiles/USA_IL_Chicago.Midway.Intl.AP.725340_TMY3.epw",
                                skip = 8, col_types = "ddddd-d---------------------------------",
                                col_names = c("year", "month", "day", "hour", "min", "tmy")) %>%
    mutate(year = 2022,
           time = ymd_h(paste(paste(year, month, day, sep = "-"), hour, sep = " ")),
           eload = NA,
           temp = tmy) %>%
    mutate(temp = temp*(9/5) + 32) %>%
    dplyr::select(time, temp, eload)

  # load holidays
  list_holidays <- read_csv("../readfiles/us_holidays_2022.csv",
                            col_names = c("date", "weekday", "holiday")) %>%
    mutate(date = mdy(date))
  
  # normal energy saving calculation
  if (carryover_check == FALSE){
    
    df_towt <- dataframe %>%
      mutate(date = date(datetime),
             time = format(datetime,format = "%Y%m%d %H:%M"),
             day = wday(datetime, week_start = 1),
             hour = hour(datetime),
             tow = ((day-1) * 24) + hour,
             # time_of_week of every hour: range(0, 24*7)
             occ = case_when(datetime %in% list_holidays$date ~ 0,
                             day > 5 ~ 0,
                             day <= 5 & hour < 8 | hour >= 18 ~ 0,
                             TRUE ~ 1),
             # Specify that holidays, weekends, off-hours: 0; normal operational hours: 1
             occ = factor(occ),
             strategy = factor(strategy),
             t_out = replace(t_out, is.nan(t_out), NA)) %>%
      # Convert NaN to NA
      dplyr::select(time, 
                    strategy,
                    # strategy = 2, 
                    eload = power,
                    temp = t_out) %>%
      mutate(across(c(eload, temp), ~ zoo::na.approx(., na.rm = FALSE)),
             time = ymd_hm(time)) %>%
      # filter(time < ymd('2022-12-31')) %>%
      mutate(temp = temp*(9/5) + 32)
    
    
    baseline_train <- df_towt %>%
      filter(strategy != 0) %>% 
      resample(.) %>% 
      filter(strategy == 1) %>%
      dplyr::select(-strategy)
    
    s2_train <- df_towt %>%
      filter(strategy != 0) %>% 
      resample(.) %>%
      filter(strategy == 2) %>%
      dplyr::select(-strategy)
    
    
    model_input_options <- nmecr::assign_model_inputs(regression_type = "TOWT", 
                                                      occupancy_threshold = 0.65)
    
    # do baseline model to estimate occupancy
    base <- nmecr::model_with_TOWT(training_data = baseline_train,
                                   model_input_options = model_input_options)
    
    base_model_input_options <- map(model_input_options, ~.) %>% 
      append(list(find_occ_unocc(training_data = baseline_train, model_input_options = base$model_input_options))) %>% 
      set_names(., c(names(model_input_options), "occupancy_info"))
    
    # re-estimate baseline with occupancy
    base <- nmecr::model_with_TOWT(training_data = baseline_train,
                                   model_input_options = base_model_input_options)
    
    # do strategy 2 model
    s2 <- nmecr::model_with_TOWT(training_data = s2_train,
                                 model_input_options = model_input_options)
    
    s2_model_input_options <- map(model_input_options, ~.) %>% 
      append(list(find_occ_unocc(training_data = s2_train, model_input_options = s2$model_input_options))) %>% 
      set_names(., c(names(model_input_options), "occupancy_info"))
    
    # re-estimate baseline with occupancy
    s2 <- nmecr::model_with_TOWT(training_data = s2_train,
                                 model_input_options = s2_model_input_options)
    

    
    baseline_results <- model_pred(amod = base$model_occupied, 
                                   bmod = base$model_unoccupied,
                                   time = weather$time, 
                                   temp = weather$temp,
                                   temp_knots = base$model_input_options$calculated_temp_knots, 
                                   dataframe = weather,
                                   occ_info = base$model_input_options$occupancy_info,
                                   interval_minutes = base$model_input_options$interval_minutes)
    
    s2_results <- model_pred(amod = s2$model_occupied, 
                             bmod = s2$model_unoccupied,
                             time = weather$time, 
                             temp = weather$temp,
                             temp_knots = s2$model_input_options$calculated_temp_knots, 
                             dataframe = weather,
                             occ_info = s2$model_input_options$occupancy_info,
                             interval_minutes = s2$model_input_options$interval_minutes)
    
    saving_df <- data.frame(time = weather$time,
                            temp = weather$temp,
                            S2 = baseline_results$towt - s2_results$towt)
    
    saving_df_week <- saving_df %>%
      mutate(week = week(time)) %>%
      group_by(week) %>%
      summarise(S2_avg = mean(S2))
    
    FS <- 100*sum(saving_df$S2, na.rm = TRUE)/sum(baseline_results$towt)
    
    # energy saving calculation for carryover detection purpose (needs baseline)
  } else{
    
    s2_train <- dataframe %>% 
      mutate(date = date(datetime),
             time = format(datetime,format = "%Y%m%d %H:%M"),
             day = wday(datetime, week_start = 1),
             hour = hour(datetime),
             tow = ((day-1) * 24) + hour,
             occ = case_when(datetime %in% list_holidays$date ~ 0,
                             day > 5 ~ 0,
                             day <= 5 & hour < 8 | hour >= 18 ~ 0,
                             TRUE ~ 1),
             occ = factor(occ),
             strategy = factor(strategy),
             t_out = replace(t_out, is.nan(t_out), NA)) %>%
      dplyr::select(time, 
                    eload = power, 
                    strategy, 
                    temp = t_out) %>% 
      mutate(across(c(eload, temp), ~ zoo::na.approx(., na.rm = FALSE)),
             time = ymd_hm(time)) %>%
      mutate(temp = temp*(9/5) + 32) %>%
      # resample
      filter(strategy == 2) %>% 
      resample(.) %>% 
      arrange(time) %>% 
      dplyr::select(-strategy)
    
    model_input_options <- nmecr::assign_model_inputs(regression_type = "TOWT", 
                                                      occupancy_threshold = 0.65)
    
    s2 <- nmecr::model_with_TOWT(training_data = s2_train,
                                 model_input_options = model_input_options)
    
    s2_model_input_options <- map(model_input_options, ~.) %>% 
      append(list(find_occ_unocc(training_data = s2_train, 
                                 model_input_options = s2$model_input_options))) %>% 
      set_names(., c(names(model_input_options), "occupancy_info"))
    
    s2 <- nmecr::model_with_TOWT(training_data = s2_train,
                                           model_input_options = s2_model_input_options)
    
    s2_results <- model_pred(amod = s2$model_occupied, 
                             bmod = s2$model_unoccupied,
                             time = weather$time, 
                             temp = weather$temp,
                             temp_knots = s2$model_input_options$calculated_temp_knots, 
                             dataframe = weather,
                             occ_info = s2$model_input_options$occupancy_info,
                             interval_minutes = s2$model_input_options$interval_minutes)
    
    # Make saving dataframe
    saving_df <- data.frame(time = weather$time,
                            temp = weather$temp,
                            S2 = baseline_results$towt - s2_results$towt)
    
    # Make saving dataframe averaged weekly
    saving_df_week <- saving_df %>%
      mutate(week = week(time)) %>%
      group_by(week) %>%
      summarise(S2_avg = mean(S2))
    
    # Calculate typical year annual energy saving for consecutive days
    FS <- 100*sum(saving_df$S2, na.rm = TRUE)/sum(baseline_results$towt)
  }

  
  
  return(FS)
}