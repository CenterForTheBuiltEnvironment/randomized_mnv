library(tidyverse)
library(lubridate)
library(stats)
library(dplyr)
source("../functions/create_temp_matrix.R")

model_pred <- function(amod, bmod, time, temp, temp_knots, dataframe, occ_info, interval_minutes){
  
  #' TOWT model prediction
  #' @description This is a function defined to use fitted TOWT model to make predictions on the given dataset. 
    #' Part of the code is extracted from nmecr package: https://github.com/kW-Labs/nmecr
  #' @param amod The fitted model for occupied hours
  #' @param bmod The fitted model for unoccupied hours
  #' @param time Predictor variable: prediction timeline (datetime object)
  #' @param temp Predictor variable: temperature file
  #' @param temp_knots Temperature levels derived from fitted model_input_options
  #' @param dataframe dataframe that stores the prediction info and results
  #' @param occ_info occupancy information derived from fitted model_input_options
  #' @usage model_pred(amod, bmod, time, temp, temp_knots, dataframe, occ_info)
  #' @return The function returns a dataframe containing the prediction datetime, towt(predicted energy) and eload (if contained in the input dataset)

  # prepare interval of week for training data
  
  minute_of_week <- (lubridate::wday(time) - 1) * 24 * 60 + 
    lubridate::hour(time) * 60 + lubridate::minute(time)
  
  interval_of_week <- 1 + floor(minute_of_week / interval_minutes)
  
  # which time intervals are 'occupied'?
  occ_intervals <- occ_info[occ_info[, 2] == 1, 1] 
  
  # create an occupancy vector for training dataset
  occ_vec <- rep(0, nrow(dataframe))
  for (i in 1 : length(occ_intervals)) {
    occ_vec[interval_of_week == occ_intervals[i]] <- 1
  }
  
  ok_occ <- occ_vec == 1
  ok_occ[is.na(ok_occ)] <- TRUE
  
  ftow <- factor(interval_of_week)
  
  temp_mat <- create_temp_matrix(temp, temp_knots)
  
  dframe <- data.frame(dataframe, ftow, temp_mat)
  
  # make predictions
  amod_towt <- stats::predict(amod, dplyr::select(dframe[ok_occ, ], -eload))
  
  amod_results <- data.frame(dframe[ok_occ, ]$time, dframe[ok_occ, ]$eload, amod_towt)
  names(amod_results) <- c('time','eload','towt')
  
  # make predictions
  bmod_towt <- stats::predict(bmod, dplyr::select(dframe[!ok_occ, ], -eload))
  
  bmod_results <- data.frame(dframe[!ok_occ, ]$time, dframe[!ok_occ, ]$eload, bmod_towt)
  names(bmod_results) <- c('time','eload','towt')
  
  # return results
  results <- rbind(amod_results, bmod_results) %>% 
    arrange(time)
  
  return(results)
}