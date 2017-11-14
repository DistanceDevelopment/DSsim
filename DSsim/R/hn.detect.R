hn.detect <- function(x, scale, g0 = 1){
#returns the probability of detection based on a half-normal detection function
  return(g0*(exp(-x^2/(2*scale^2))))
}