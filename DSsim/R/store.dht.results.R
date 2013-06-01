store.dht.results <- function(results, dht.results, i, clusters){
  strata.names <- dimnames(results$individuals$summary)[[1]]
  strata.names.ND <- as.character(dht.results$individual$N$Label)
  for(strat in seq(along = strata.names)){
    results$individuals$summary[strat,,i] <- as.matrix(dht.results$individuals$summary[dht.results$individual$summary$Region == strata.names[strat],c("Area", "CoveredArea", "Effort", "n", "ER", "se.ER", "cv.ER")])
    results$individuals$N[strat,,i] <- as.matrix(dht.results$individuals$N[dht.results$individual$N$Label == strata.names.ND[strat],c("Estimate", "se", "cv", "lcl", "ucl", "df")])
    results$individuals$D[strat,,i] <- as.matrix(dht.results$individuals$D[dht.results$individual$D$Label == strata.names.ND[strat],c("Estimate", "se", "cv", "lcl", "ucl", "df")])
  }
  if(clusters){
    for(strat in seq(along = strata.names)){
      results$clusters$summary[strat,,i] <- as.matrix(dht.results$clusters$summary[dht.results$clusters$summary$Region == strata.names[strat],c("Area", "CoveredArea", "Effort", "n", "k", "ER", "se.ER", "cv.ER")])
      results$clusters$N[strat,,i] <- as.matrix(dht.results$clusters$N[dht.results$clusters$N$Label == strata.names.ND[strat],c("Estimate", "se", "cv", "lcl", "ucl", "df")])
      results$clusters$D[strat,,i] <- as.matrix(dht.results$clusters$D[dht.results$clusters$D$Label == strata.names.ND[strat],c("Estimate", "se", "cv", "lcl", "ucl", "df")])
      results$expected.size[strat, c("Expected.S","se.Expected.S"), i] <- as.matrix(dht.results$Expected.S[dht.results$Expected.S$Region == strata.names[strat], c("Expected.S","se.Expected.S")])
    }       
  }
  return(results)
}