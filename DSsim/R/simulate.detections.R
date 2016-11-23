#' @importFrom stats rbinom
simulate.detections <- function(poss.distances, detectability){
  #calculate.probability of detection
  probs <- switch(detectability@key.function,
    "hn" = hn.detect(poss.distances$distance, poss.distances$scale.param),
    "hr" = hr.detect(poss.distances$distance, poss.distances$scale.param, poss.distances$shape.param),
    "uf" = poss.distances$scale.param              
  )
  detected <- numeric(0)
  for(i in seq(along = probs)){
    detected[i] <- rbinom(1, 1, probs[i])
  }
  #remove the logical availability variable columns
  var.names <- names(poss.distances)[!names(poss.distances)%in%c("available.from.pdist", "available.from.rdist.to.start", "available.from.rdist.to.end")] 
  dist.data <- poss.distances[detected == 1, var.names] 
  return(dist.data)
}

