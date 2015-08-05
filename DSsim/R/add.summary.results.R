#' @importFrom stats na.omit sd
add.summary.results <- function(results){
  strata.names <- dimnames(results$individuals$summary)[[1]]
  reps <- length(dimnames(results$individuals$summary)[[3]])-2
  #create function for calculating mean with nas
  na.omit.mean <- function(x){return(mean(na.omit(x)))}
  #Summarise results for individuals
  if(reps > 1){
    for(strat in seq(along = strata.names)){
      results$individuals$summary[strat,,"mean"] <- apply(results$individuals$summary[strat,,1:reps], 1, FUN = mean, na.rm = TRUE)
      results$individuals$summary[strat,,"sd"] <- apply(results$individuals$summary[strat,,1:reps], 1, FUN = sd, na.rm = TRUE)
      results$individuals$N[strat,,"mean"] <- apply(results$individuals$N[strat,,1:reps], 1, FUN = mean, na.rm = TRUE)
      results$individuals$N[strat,,"sd"] <- apply(results$individuals$N[strat,,1:reps], 1, FUN = sd, na.rm = TRUE)
      results$individuals$D[strat,,"mean"] <- apply(results$individuals$D[strat,,1:reps], 1, FUN = mean, na.rm = TRUE)
      results$individuals$D[strat,,"sd"] <- apply(results$individuals$D[strat,,1:reps], 1, FUN = sd, na.rm = TRUE) 
    }
    results$Detection[1,,"mean"] <- apply(results$Detection[1,,1:reps], 1, FUN = mean, na.rm = TRUE)
    results$Detection[1,,"sd"] <- apply(results$Detection[1,,1:reps], 1, FUN = sd, na.rm = TRUE)
    #Summarise results for clusters if they exists
    if(!is.null(results$clusters)){
      for(strat in seq(along = strata.names)){
        results$clusters$summary[strat,,"mean"] <- apply(results$clusters$summary[strat,,1:reps], 1, FUN = mean, na.rm = TRUE)
        results$clusters$summary[strat,,"sd"] <- apply(results$clusters$summary[strat,,1:reps], 1, FUN = sd, na.rm = TRUE)
        results$clusters$N[strat,,"mean"] <- apply(results$clusters$N[strat,,1:reps], 1, FUN = mean, na.rm = TRUE)
        results$clusters$N[strat,,"sd"] <- apply(results$clusters$N[strat,,1:reps], 1, FUN = sd, na.rm = TRUE)
        results$clusters$D[strat,,"mean"] <- apply(results$clusters$D[strat,,1:reps], 1, FUN = mean, na.rm = TRUE)
        results$clusters$D[strat,,"sd"] <- apply(results$clusters$D[strat,,1:reps], 1, FUN = sd, na.rm = TRUE) 
        results$expected.size[strat,,"mean"] <- apply(results$expected.size[strat,,1:reps], 1, FUN = mean, na.rm = TRUE)
        results$expected.size[strat,,"sd"] <- apply(results$expected.size[strat,,1:reps], 1, FUN = sd, na.rm = TRUE)
      }
    }
  #if the sinulation has only been run once
  }else if (reps == 1){
    for(strat in seq(along = strata.names)){
      results$individuals$summary[strat,,"mean"] <- results$individuals$summary[strat,,1]
      results$individuals$summary[strat,,"sd"] <- rep(NA,length(results$individuals$summary[strat,,1]))
      results$individuals$N[strat,,"mean"] <- results$individuals$N[strat,,1]
      results$individuals$N[strat,,"sd"] <- rep(NA,length(results$individuals$N[strat,,1]))
      results$individuals$D[strat,,"mean"] <- results$individuals$D[strat,,1]
      results$individuals$D[strat,,"sd"] <- rep(NA, length(results$individuals$D[strat,,1])) 
    }
    results$Detection[1,,"mean"] <- results$Detection[1,,1]
    results$Detection[1,,"sd"] <- rep(NA,length(results$Detection[1,,1]))
    #Summarise results for clusters if they exists
    if(!is.null(results$clusters)){
      for(strat in seq(along = strata.names)){
        results$clusters$summary[strat,,"mean"] <- results$clusters$summary[strat,,1]
        results$clusters$summary[strat,,"sd"] <- rep(NA, length(results$clusters$summary[strat,,1]))
        results$clusters$N[strat,,"mean"] <- results$clusters$N[strat,,1]
        results$clusters$N[strat,,"sd"] <- rep(NA, length(results$clusters$N[strat,,1]))
        results$clusters$D[strat,,"mean"] <- results$clusters$D[strat,,1]
        results$clusters$D[strat,,"sd"] <- rep(NA, length(results$clusters$D[strat,,1])) 
        results$expected.size[strat,,"mean"] <- results$expected.size[strat,,1]
        results$expected.size[strat,,"sd"] <- rep(NA, length(results$expected.size[strat,,1]))
      }
    }  
  }
  return(results)
}
