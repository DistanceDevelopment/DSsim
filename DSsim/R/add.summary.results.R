add.summary.results <- function(results){
  strata.names <- dimnames(results$individuals$summary)[[1]]
  reps <- length(dimnames(results$individuals$summary)[[3]])-2
  #Summarise results for individuals
  for(strat in seq(along = strata.names)){
    results$individuals$summary[strat,,"mean"] <- apply(results$individuals$summary[strat,,1:reps], 1, FUN = mean)
    results$individuals$summary[strat,,"sd"] <- apply(results$individuals$summary[strat,,1:reps], 1, FUN = sd)
    results$individuals$N[strat,,"mean"] <- apply(results$individuals$N[strat,,1:reps], 1, FUN = mean)
    results$individuals$N[strat,,"sd"] <- apply(results$individuals$N[strat,,1:reps], 1, FUN = sd)
    results$individuals$D[strat,,"mean"] <- apply(results$individuals$D[strat,,1:reps], 1, FUN = mean)
    results$individuals$D[strat,,"sd"] <- apply(results$individuals$D[strat,,1:reps], 1, FUN = sd) 
  }
  results$Detection[1,,"mean"] <- apply(results$Detection[1,,1:reps], 1, FUN = mean)
  results$Detection[1,,"sd"] <- apply(results$Detection[1,,1:reps], 1, FUN = sd)
  #Summarise results for clusters if they exists
  if(!is.null(results$clusters)){
    for(strat in seq(along = strata.names)){
      results$clusters$summary[strat,,"mean"] <- apply(results$clusters$summary[strat,,1:reps], 1, FUN = mean)
      results$clusters$summary[strat,,"sd"] <- apply(results$clusters$summary[strat,,1:reps], 1, FUN = sd)
      results$clusters$N[strat,,"mean"] <- apply(results$clusters$N[strat,,1:reps], 1, FUN = mean)
      results$clusters$N[strat,,"sd"] <- apply(results$clusters$N[strat,,1:reps], 1, FUN = sd)
      results$clusters$D[strat,,"mean"] <- apply(results$clusters$D[strat,,1:reps], 1, FUN = mean)
      results$clusters$D[strat,,"sd"] <- apply(results$clusters$D[strat,,1:reps], 1, FUN = sd) 
      results$expected.size[strat,,"mean"] <- apply(results$expected.size[strat,,1:reps], 1, FUN = mean)
      results$expected.size[strat,,"sd"] <- apply(results$expected.size[strat,,1:reps], 1, FUN = sd)
    }
  }
  return(results)
}
