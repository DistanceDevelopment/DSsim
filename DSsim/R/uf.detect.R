uf.detect <- function(dists, detectability){
  #for now just implement a straightforward hn detection function
  x <- dists
  if(length(detectability@scale.param) > 1){
    probs <- sample(detectability@scale.param, length(x), replace = TRUE)
  }else{
    probs <- rep(detectability@scale.param, length(x))
  }
  return(probs)
}