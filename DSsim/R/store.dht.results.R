store.dht.results <- function(results, dht.results, i){
  strata.names <- dimnames(results$summary)[[1]]
  temp <- as.character(dht.results$individual$N$Label)
  for(strat in seq(along = strata.names)){
    results$summary[strat,,i] <- as.matrix(dht.results$individual$summary[dht.results$individual$summary$Region == strata.names[strat],c("Area", "CoveredArea", "Effort", "n", "k", "ER", "se.ER", "cv.ER")])
    results$N[strat,,i] <- as.matrix(dht.results$individual$N[dht.results$individual$N$Label == strata.names[strat],c("Estimate", "se", "cv", "lcl", "ucl", "df")])
    results$D[strat,,i] <- as.matrix(dht.results$individual$D[dht.results$individual$D$Label == strata.names[strat],c("Estimate", "se", "cv", "lcl", "ucl", "df")])
  }
  return(results)
}