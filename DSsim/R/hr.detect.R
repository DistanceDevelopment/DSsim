hr.detect <- function(x, scale, shape, g0 = 1){
#returns the probability of detection based on a hazard-rate detection function
  return(g0*(1-exp(-(x/scale)^-shape)))  
}