
library(dplyr)

compute_participation <- function(x, weighted = F){

  results <- list(raw_ = NA, clean_ = NA)
  
  if(weighted == F){
    
    results[["raw"]] <- x %>%
      count(referendum_participation) %>%
      mutate(prop = n/sum(n))
    
    results[["clean"]] <- x %>%
      count(referendum_participation) %>%
      filter(referendum_participation %in% valid_responses) %>%
      mutate(prop = n/sum(n))
    
  }else if(weighted == T) {
    
      # test weight variable 
    if(is.null(x[["weights"]])){
      stop("Failed test: If argument 'weighted' == T, then there should be a variable called 'weights'.")
    }
    
    results[["raw"]] <- x %>%
      count(referendum_participation, wt = weights) %>%
      mutate(prop = n/sum(n))
    
    results[["clean"]] <- x %>%
      count(referendum_participation, wt = weights) %>%
      filter(referendum_participation %in% valid_responses) %>%
      mutate(prop = n/sum(n))
    
  }
  
  return(results)
  
}
