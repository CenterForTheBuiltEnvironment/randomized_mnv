ol_est <- function(data, reference){
  
  #' Overlapping percentage calculation of independent variables
  #' @description
    #' A function that calculates the percentage overlapping in quantile
    #' 
  #' @param data Dataframe that contains the independent variable values
  #' @param reference The reference quantile that needs to be compared in order to calculate overlapping 
  #'
  #' @return Numeric overlapping percentage

  # Calculate overlapping range
  quantile_sprt <- data %>% 
    dplyr::select(t_out) %>%
    quantile(., na.rm = TRUE, probs = c(0, 1))
  
  # Calculate overlapping percentage
  if (quantile_sprt[[2]] >= reference[[2]]){
    quantile_sprt[[2]] = reference[[2]]
  }
  if (quantile_sprt[[1]] <= reference[[1]]){
    quantile_sprt[[1]] = reference[[1]]
  }

  ol <- (quantile_sprt[[2]] - quantile_sprt[[1]])/(reference[[2]] - reference[[1]])
  
  return(ol)
}
