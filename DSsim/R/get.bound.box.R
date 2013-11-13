# get.bound.box <- function(shapefile){
#   bound.box <- shapefile$shp$shp[[1]]$box
#   if(length(shapefile$shp$shp) == 1){
#     return(bound.box)
#   }
#   for(strat in seq(along = shapefile$shp$shp)[-1]){
#     bound.box <- rbind(bound.box, shapefile$shp$shp[[strat]]$box)  
#   }     
#   bound.box <- as.array(bound.box)
#   dimnames(bound.box)[[1]] <- seq(along = shapefile$shp$shp)
#   bound.box <- as.data.frame(bound.box)
#   total.bound.box <- c(xmin = min(bound.box[,"xmin"]), ymin = min(bound.box[,"ymin"]), xmax = max(bound.box[,"xmax"]), ymax = max(bound.box[,"ymax"]))
#   return(total.bound.box) 
# }


get.bound.box <- function(coords){
  xmin <- coords[[1]][[1]]$x[1]
  ymin <- coords[[1]][[1]]$y[1]
  xmax <- coords[[1]][[1]]$x[1]
  ymax <- coords[[1]][[1]]$y[1]
  
  for(strata in seq(along = coords)){
    for(poly in seq(along = coords[strata])){
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
