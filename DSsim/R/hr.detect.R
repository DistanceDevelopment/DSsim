hr.detect <- function(perp.dists, detectability){
  #if multiple scale and shape parameters are supplied then resample for
  #detection function.
  x <- perp.dists
  no.params <- max(length(detectability@scale.param), length(detectability@shape.param))
  if(no.params > 1){
    index <- sample(1:no.params, length(x), replace = TRUE)
  }
  if(length(detectability@scale.param) > 1){
    scale <- detectability@scale.param[index]  
  }else{
    scale <- detectability@scale.param
  }
  if(length(detectability@shape.param) > 1){
    shape <- detectability@shape.param[index]
  }else{
    shape <- detectability@shape.param
  }
  return(1-exp(-(x/scale)^-shape))  
}