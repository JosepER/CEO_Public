


compute_participation <- function(x, weighted = F){

  require(dplyr)
    
  results <- list(raw = NA, clean = NA)
  
  if(weighted = F){
    
    results[["raw"]] <- x %>%
      count(referendum_participation) %>%
      mutate(prop = n/sum(n))
    
    results[["clean"]] <- x %>%
      count(referendum_participation) %>%
      filter(referendum_participation %in% valid_responses) %>%
      mutate(prop = n/sum(n))
    
  }else if(weighted = T) {
    
    results[["raw"]] <- x %>%
      count(referendum_participation, wt = weight) %>%
      mutate(prop = n/sum(n))
    
    results[["clean"]] <- x %>%
      count(referendum_participation, wt = weight) %>%
      filter(referendum_participation %in% valid_responses) %>%
      mutate(prop = n/sum(n))
    
  }
  
  return(results)
  
}
