hr.detect <- function(x, scale, shape){
#returns the probability of detection based on a hazard-rate detection function
  return(1-exp(-(x/scale)^-shape))  
}