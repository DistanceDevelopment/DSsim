hn.detect <- function(perp.dists, detectability){
  #for now just implement a straightforward hn detection function
  x <- perp.dists
  sig <- detectability@scale.param
  return(exp(-x^2/(2*sig^2)))
}