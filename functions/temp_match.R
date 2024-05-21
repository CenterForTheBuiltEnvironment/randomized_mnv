library(hms)

temp_match <- function(overlap = FALSE, max = FALSE, s, time = "hourly", target_date, df){
  if (max == TRUE){
    
    target_maxT <- df %>% 
      filter(dt %in% as.Date(target_date)) %>% 
      dplyr::select(t_out) %>% 
      max(na.omit = TRUE)
    
    sorted_date <- df %>% 
      filter(dt >= as.Date(target_date) - days(90)) %>% 
      filter(dt < as.Date(target_date)) %>% 
      filter(strategy == s) %>% 
      group_by(dt) %>% 
      summarise(t_max = max(t_out, na.omit = TRUE)) %>% 
      mutate(diff = abs(t_max - target_maxT)) %>% 
      arrange(diff) %>% 
      filter(dt < as.Date(target_date)) %>% 
      ungroup() %>% 
      slice(1:4) %>% 
      dplyr::select(dt)

    date <- df %>% 
      filter(dt %in% target_date) %>% 
      dplyr::select(datetime) %>% 
      distinct()
    
    
    if (time == "hourly"){
      
      df_ave <- df %>% 
        filter(dt %in% sorted_date$dt) %>% 
        group_by(hour = hour(datetime)) %>% 
        summarise(power_ave = mean(power, na.rm = TRUE)) %>% 
        ungroup()%>% 
        mutate(dt = target_date)
      
    } else if (time == "15-min"){
      df_ave <- df %>% 
        filter(dt %in% sorted_date$dt) %>% 
        mutate(tod = hour(datetime)*4 + minute(datetime)/15 + 1) %>% 
        group_by(tod) %>% 
        summarise(avg = mean(power, na.rm = TRUE)) %>% 
        ungroup()
      
      df_ave <- df %>% 
        filter(dt %in% as.Date(target_date)) %>% 
        mutate(tod = hour(datetime)*4 + minute(datetime)/15 + 1) %>% 
        left_join(., df_ave, by = "tod") %>% 
        select(-tod)
    }
    

    
  } else if (overlap == TRUE){
    
    target_temp <- df %>% 
      filter(dt %in% as.Date(target_date)) %>% 
      dplyr::select(t_out)
    
    sorted_date <- df %>% 
      filter(dt >= as.Date(target_date) - days(90)) %>% 
      filter(dt < as.Date(target_date)) %>% 
      filter(strategy == s) %>% 
      dplyr::select(dt) %>% 
      unique()
    
    overlap_temp <- list()
    
    for (i in 1:nrow(sorted_date)) {
      
      temp <- df %>% 
        filter(dt %in% as.Date(sorted_date[i, ]$dt)) %>% 
        dplyr::select(datetime, t_out)
      
      print(paste0("Found ", i))
      
      X <- list(X1 = na.omit(target_temp$t_out), X2 = na.omit(temp$t_out))
      ol <- boot.overlap(X, B = 100)
      overlap_temp[[i]] <- tibble("dt" = sorted_date[i, ]$dt,
                                  "ol" = ol$OVboot_stats$estOV)
    }
    
    sorted_date <- overlap_temp %>% 
      bind_rows() %>% 
      arrange(desc(ol)) %>% 
      slice(1:4)
    
    if (time == "hourly"){
      
      df_ave <- df %>% 
        filter(dt %in% sorted_date$dt) %>% 
        group_by(hour = hour(datetime)) %>% 
        summarise(power_ave = mean(power, na.rm = TRUE)) %>% 
        ungroup() %>% 
        mutate(dt = target_date)
      
    } else if (time == "15-min"){
      df_ave <- df %>% 
        filter(dt %in% sorted_date$dt) %>% 
        mutate(tod = hour(datetime)*4 + minute(datetime)/15 + 1) %>% 
        group_by(tod) %>% 
        summarise(avg = mean(power, na.rm = TRUE)) %>% 
        ungroup() %>% 
        bind_cols(date)
    }
  }
  
  return(df_ave)
}