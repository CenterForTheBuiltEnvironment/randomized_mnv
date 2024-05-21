library(tidyverse)
library(lubridate)
library(blocksdesign)
library(dplyr)

blocking <- function(start_date, n_weeks, n_seasons, seed, searches, jumps, treatments, consec, max_try = NULL, max_margin = NULL){
  
  #' Blocking schedule generation function
  #' @description This is a function defined to design randomized blocking experiment for control switchback schedule 
  #' Part of the code is extracted from blockdesign package: https://cran.r-project.org/web/packages/blocksdesign
  #' @usage block(start_date, n_weeks, n_seasons, searches, jumps, treatments, consec, max_try, max_margin)
  #' @param start_date The starting date of the random schedule generation
  #' @param n_weeks Number of weeks 
  #' @param n_seaons Number of seasons
  #' @param seed Random seed to stary blocking schedule search
  #' @param searches 	the maximum number of local optima searched for a design optimization.
  #' @param jumps the number of pairwise random treatment swaps used to escape a local maxima.
  #' @param treatments the number of treatments
  #' @param consec the number of consecutive days considered when switching between strategies (sampling interval)
  #' @param max_try the maximum number of tries to serach for balanced schedule
  #' @param max_margin the maximum allowable difference in sampled days among all days of the week
  #' @return The function will return the schedule of the blocking design if successfully generated and return weekdays, consecutive days summary.
  
  # do block design
  if (consec == 1){
    df_blocks <- blocksdesign::blocks(treatments = treatments, # 2 intervention strategies, 1 control strategy
                                      replicates = floor((7*n_weeks)/treatments), # 7 days a week for n weeks divided by 3 treatments
                                      blocks = list(n_seasons, 7), # 7 weekdays
                                      searches = searches, 
                                      jumps = jumps,
                                      seed = seed)
    
    # build timeseries and assign strategies
    df_blocks <- df_blocks[["Design"]] %>%
      # tibble() function creates a dataframe
      tibble() %>%
      select(Level_2, strategy = treatments) %>%
      separate(col = Level_2, into = c("season", "weekday"), sep = "\\.", remove = TRUE) %>%
      # arrange function: sort the data frame by week and weekday
      arrange(weekday, season) %>%
      mutate(season = as.numeric(str_remove_all(season, "B")),
             weekday = as.numeric(str_remove_all(weekday, "B")),
             week = rep(1:n_weeks, length.out = nrow(df_blocks$Design))) %>%
      arrange(week, weekday) %>%
      mutate(date = ymd(start_date) + days(row_number()-1),
             weekday = wday(date, label = TRUE)) %>%
      select(season, date, weekday, strategy)
    
    
    #### Check balance ####
    # get count by weekday
    weekday_summary <- df_blocks %>%
      group_by(season, 
               weekday = wday(date),
               strategy) %>%
      count()
    
    # summarise consecutive strategies
    consec_summary <- df_blocks %>%
      # lag calculate the difference between current value and previous value
      # returns 1 when two consecutive same strategy, 0 when strategy changes
      mutate(consec = ifelse(lag(strategy, 1) == strategy, 1, 0)) %>%
      group_by(strategy) %>%
      summarise(consecutive = sum(consec, na.rm = TRUE)) %>% 
      ungroup()
    
    
    return_list <- list("schedule" = df_blocks, 
                        "weekday_summary" = weekday_summary,
                        "consec_summary" = consec_summary)
    
    return(return_list)
    
  }
  else{
    params_block <- list(current_seed = seed, # set first seed
                         nos_try = 0, # number of tries
                         max_try = 1000, # maximum number of tries
                         n_weeks = n_weeks,
                         n_seasons = n_seasons,
                         chunk = consec, 
                         max_consecutive_criteria = 3 * consec, # consecutive if more than X days. stops. 9 for chunk == 3, 12 for chunk == 4
                         max_margin_criteria = 3, # number of weekdays difference between max and min, 4 for chunk == 3, 3 for chunk == 4
                         success = TRUE)
    
    log <- {}
    
    while (params_block$success){
      # do block design
      df_blocks <- blocksdesign::blocks(treatments = 2, # 2 intervention strategies, 1 control strategy
                                        replicates = floor(params_block$n_weeks * 7 / params_block$chunk / 2), # 2 strategies, 3 days as a randomization unit
                                        blocks = params_block$n_seasons, # 7 weekdays
                                        searches = 20, 
                                        jumps = 1,
                                        seed = params_block$current_seed)
      
      # build timeseries and assign strategies
      df_blocks <- df_blocks[["Design"]] %>%
        # tibble() function creates a dataframe
        tibble() %>%
        select(season = Level_1, strategy = treatments) %>%
        arrange(season) %>%
        mutate(season = as.numeric(str_remove_all(season, "B"))) %>%
        slice(rep(row_number(), each = params_block$chunk)) %>%
        mutate(date = ymd(start_date) + days(row_number()-1),
               weekday = wday(date, label = TRUE)) %>%
        select(season, date, weekday, strategy)
      
      # count consecutive days
      df_cons <- df_blocks %>%
        select(strategy) %>%
        mutate(strategy = as.numeric(as.character(strategy)),
               consec = ifelse(lag(strategy) == strategy, FALSE, TRUE),
               consec = replace_na(consec, FALSE),
               cumul = cumsum(consec)) %>%
        group_by(cumul) %>%
        mutate(row_number = row_number()) %>%
        ungroup()
      
      # get count by weekday
      df_wdays <- df_blocks %>%
        group_by(weekday = wday(date),
                 strategy) %>%
        count()
      
      # determine the max value
      #consecutive_max <- max(df_cons$row_number, na.rm = TRUE)
      
      # find the maximum consecutive repeat if more than max_consecutive_criteria days, stop if not
      if(max(df_cons$row_number, na.rm = TRUE) > params_block$max_consecutive_criteria) {
        message <- paste0("Highest consecutive days is ", max(df_cons$row_number, na.rm = TRUE),
                          ", trying again. Current seed is ", params_block$current_seed)
        log <- append(log, message)
        
        # generate new seed
        params_block$current_seed <- sample(1:2^15, 1)
        
        # set seed
        set.seed(params_block$current_seed)
        
        # update counter
        params_block$nos_try <- params_block$nos_try + 1
        
        # check the number of tries
        if (params_block$nos_try >= params_block$max_try){
          params_block$success <- FALSE
          message <- 'Reached the max number of tries. Please try changing the parameters.'
          log <- append(log, message)
          
        }
        
      } else if (max(df_wdays$n) - min(df_wdays$n) > params_block$max_margin_criteria) {
        message <- paste0("Highest margin between weekdays is ", max(df_wdays$n) - min(df_wdays$n),
                          ", trying again. Current seed is ", params_block$current_seed)
        log <- append(log, message)
        
        # generate new seed
        params_block$current_seed <- sample(1:2^15, 1)
        
        # set seed
        set.seed(params_block$current_seed)
        
        # update counter
        params_block$nos_try <- params_block$nos_try + 1
        
        # check the number of tries
        if (params_block$nos_try >= params_block$max_try){
          params_block$success <- FALSE
          message <- 'Reached the max number of tries. Please try changing the parameters.'
          log <- append(log, message)
          
        }
      } else {
        
        print(paste0("Success! Highest consecutive days is ", max(df_cons$row_number, na.rm = TRUE), "."))
        print(paste0("Success! Highest margin weekdays is ", max(df_wdays$n) - min(df_wdays$n), "."))
        
        params_block$success <- FALSE
        
        
      }
    }
    
    
    
    
    
    #### Check balance ####
    # get count by strategy
    strategy_summary <- df_blocks %>% 
      group_by(season, strategy) %>% 
      count() %>% 
      ungroup()
    
    # get count by weekday
    weekday_summary <- df_blocks %>%
      group_by(weekday = wday(date),
               strategy) %>%
      count()
    
    # summarise consecutive strategies
    consec_summary <- df_blocks %>%
      # lag calculate the difference between current value and previous value
      # returns 1 when two consecutive same strategy, 0 when strategy changes
      mutate(consec = ifelse(lag(strategy, 1) == strategy, 1, 0)) %>%
      group_by(strategy) %>%
      summarise(consecutive = sum(consec, na.rm = TRUE)) %>% 
      ungroup()
    
    
    return_list <- list("schedule" = df_blocks, 
                        "weekday_summary" = weekday_summary,
                        "consec_summary" = consec_summary, 
                        "log" = log)
    
    return(return_list)
  }
  
}