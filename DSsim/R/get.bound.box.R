get.bound.box <- function(coords){
#Find the minimum bounding box around the whole study area
  xmin <- coords[[1]][[1]]$x[1]
  ymin <- coords[[1]][[1]]$y[1]
  xmax <- coords[[1]][[1]]$x[1]
  ymax <- coords[[1]][[1]]$y[1]
  
  for(strata in seq(along = coords)){
    for(poly in seq(along = coords[[strata]])){
      x.coords <- coords[[strata]][[poly]]$x
      y.coords <- coords[[strata]][[poly]]$y
      xmin <- min(xmin, x.coords)
      ymin <- min(ymin, y.coords)
      xmax <- max(xmax, x.coords)
      ymax <- max(ymax, y.coords)
    }
  }
  total.bound.box <- c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)
  return(total.bound.box) 
}
