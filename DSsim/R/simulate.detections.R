simulate.detections <- function(poss.distances, detectability){
  #calculate.probability of detection
  probs <- switch(detectability@key.function,
    "hn" = hn.detect(poss.distances$distance, detectability)
  )
  detected <- numeric(0)
  for(i in seq(along = probs)){
    detected[i] <- rbinom(1, 1, probs[i])
  }
  dist.data <- poss.distances[detected == 1, c("object", "transect.ID", "distance", "x", "y")] 
  return(dist.data)
}