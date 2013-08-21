hr.detect <- function(perp.dists, detectability){
  #for now just implement a straightforward hn detection function
  x <- perp.dists
  scale <- detectability@scale.param
  shape <- detectability@shape.param
  return(1-exp(-(x/scale)^-shape))  
}