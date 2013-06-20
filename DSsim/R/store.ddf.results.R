store.ddf.results <- function(detection.results, ddf.results, i, N.in.covered){
  Nhat <- ddf.results$Nhat
  n <- length(ddf.results$fitted)
  True.Pa <- n/N.in.covered
  Ave.Pa <- n/Nhat
  ESW <- Ave.Pa * ddf.results$meta.data$width
  f0 <- 1/ESW
  detection.results[1, "True.Pa", i] <- True.Pa
  detection.results[1, "Pa", i] <- Ave.Pa
  detection.results[1, "ESW", i] <- ESW
  detection.results[1, "f(0)", i] <- f0
  return(detection.results)
}

