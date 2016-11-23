hn.detect <- function(x, scale){
#returns the probability of detection based on a half-normal detection function
  return(exp(-x^2/(2*scale^2)))
}