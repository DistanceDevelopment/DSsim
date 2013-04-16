add.summary.results <- function(results){
  strata.names <- dimnames(results$summary)[[1]]
  reps <- length(dimnames(results$summary)[[3]])-2
  for(strat in seq(along = strata.names)){
    results$summary[strat,,"mean"] <- apply(results$summary[strat,,1:reps], 1, FUN = mean)
    results$summary[strat,,"sd"] <- apply(results$summary[strat,,1:reps], 1, FUN = sd)
    results$N[strat,,"mean"] <- apply(results$N[strat,,1:reps], 1, FUN = mean)
    results$N[strat,,"sd"] <- apply(results$N[strat,,1:reps], 1, FUN = sd)
    results$D[strat,,"mean"] <- apply(results$D[strat,,1:reps], 1, FUN = mean)
    results$D[strat,,"sd"] <- apply(results$D[strat,,1:reps], 1, FUN = sd) 
  }
  results$Detection[1,,"mean"] <- apply(results$Detection[1,,1:reps], 1, FUN = mean)
  results$Detection[1,,"sd"] <- apply(results$Detection[1,,1:reps], 1, FUN = sd)
  return(results)
}
