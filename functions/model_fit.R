library(tidyverse)
library(lubridate)
library(nmecr)
library(dplyr)
source("../functions/resample.R")
source("../functions/find_occ_unocc.R")

model_fit <- function(train_data, strategy = 1, occ = 0.65){
  
  #' Blocking schedule generation function
  #' @description This is a function defined to fit TOWT model and predict energy consumption given datetime and outdoor temeprature. 
  #' Part of the code is extracted from blockdesign package: https://cran.r-project.org/web/packages/blocksdesign
  #' @usage model_fit(dataframe, strategy, occ = 0.65)
  #' @param dataframe Training dataset consisting of datetime, measured energy consumption and outdoor temperature
  #' @param strategy Predicted control strategy
  #' @param occ occupancy schedule: 
  #'            0 indicates no occupancy schedule but will derive from training dataset;
  #'            0.65 indicates using default occupancy schedule;
  #'            other input indicates customized occupancy schedule
  #' @return The function will return a fitted energy prediction model for the specified strategy
  
  if (occ == 0){
    
    # specify TOWT model input options with default occupancy threshold 0.65
    model_input_options <- nmecr::assign_model_inputs(regression_type = "TOWT", 
                                                      occupancy_threshold = 0.65)
    
    # prepare data Time Of Week Temperature Model
    df_towt <- train_data %>%
      mutate(strategy = factor(strategy),
             t_out = replace(t_out, is.nan(t_out), NA)) %>%
      dplyr::select(time = datetime, strategy, eload = power, temp = t_out) %>% 
      mutate(across(c(eload, temp), ~ zoo::na.approx(., na.rm = FALSE))) %>%
      mutate(temp = temp*(9/5) + 32)
    
    # arrange training dataset
    data_train <- df_towt %>%
      filter(strategy != 0) %>% 
      resample(.) %>% 
      filter(strategy == 1) %>%
      dplyr::select(-strategy)
    
    # do baseline model to estimate occupancy
    towt <- nmecr::model_with_TOWT(training_data = data_train,
                                        model_input_options = model_input_options)
    
    # Add occupancy info to model specification
    base_model_input_options <- map(model_input_options, ~.) %>% 
      append(list(find_occ_unocc(training_data = data_train, model_input_options = towt$model_input_options))) %>% 
      set_names(., c(names(model_input_options), "occupancy_info"))
    
    # re-estimate baseline with occupancy
    towt <- nmecr::model_with_TOWT(training_data = data_train,
                                        model_input_options = base_model_input_options)
    
  }
  else if (occ == 0.65){
    # specify TOWT model input options with default occupancy threshold 0.65
    model_input_options <- nmecr::assign_model_inputs(regression_type = "TOWT", 
                                                      occupancy_threshold = 0.65)
    
    # prepare data Time Of Week Temperature Model
    df_towt <- train_data %>%
      mutate(strategy = factor(strategy),
             t_out = replace(t_out, is.nan(t_out), NA)) %>%
      dplyr::select(time = datetime, strategy, eload = power, temp = t_out) %>% 
      mutate(across(c(eload, temp), ~ zoo::na.approx(., na.rm = FALSE))) %>%
      mutate(temp = temp*(9/5) + 32)
    
    # arrange training dataset
    data_train <- df_towt %>%
      filter(strategy != 0) %>% 
      resample(.) %>% 
      filter(strategy == 1) %>%
      dplyr::select(-strategy)
    
    # do baseline model to estimate occupancy
    towt <- nmecr::model_with_TOWT(training_data = data_train,
                                   model_input_options = model_input_options)
    
  }
  else {
    # specify TOWT model input options with default occupancy threshold 0.65
    model_input_options <- nmecr::assign_model_inputs(regression_type = "TOWT", 
                                                      occupancy_threshold = occ)
    
    # prepare data Time Of Week Temperature Model
    df_towt <- train_data %>%
      mutate(strategy = factor(strategy),
             t_out = replace(t_out, is.nan(t_out), NA)) %>%
      dplyr::select(time = datetime, strategy, eload = power, temp = t_out) %>% 
      mutate(across(c(eload, temp), ~ zoo::na.approx(., na.rm = FALSE))) %>%
      mutate(temp = temp*(9/5) + 32)
    
    # arrange training dataset
    data_train <- df_towt %>%
      filter(strategy != 0) %>% 
      resample(.) %>% 
      filter(strategy == 1) %>%
      dplyr::select(-strategy)
    
    # do baseline model to estimate occupancy
    towt <- nmecr::model_with_TOWT(training_data = data_train,
                                   model_input_options = model_input_options)
    
  }
  
  return(towt)
  
}