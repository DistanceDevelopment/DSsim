add.g0.values <- function(pop.data, g0){
# Adds the g0 values to the data
# These values can be strata specific
  # Find number of strata
  strata.ids <- unique(pop.data$strata)
  # Check that the length of g0 is ok
  if(!(length(g0) == 1 || length(g0) == length(strata.ids))){
    warning("The length of the g0 values is not equal to the number of strata only the first value will be used", call. = FALSE, immediate. = TRUE)
    g0 <- rep(g0[1], length(strata.ids))
  }else if(length(g0) == 1){
    g0 <- rep(g0, length(strata.ids))
  }
  # Add g0 values
  pop.data$g0 <- rep(NA, nrow(pop.data))
  for(strat in seq(along = strata.ids)){
    pop.data$g0[pop.data$strata == strata.ids[strat]] <- g0[strat]
  }
  return(pop.data)
}