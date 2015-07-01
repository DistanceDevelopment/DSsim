hn.detect <- function(perp.dists, detectability){
#returns the probability of detection based on a half-normal detection function
  x <- perp.dists
  if(length(detectability@scale.param) > 1){
    sig <- sample(detectability@scale.param, length(x), replace = TRUE)
  }else{
    sig <- detectability@scale.param
  }
  return(exp(-x^2/(2*sig^2)))
}