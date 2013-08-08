get.bound.box <- function(shapefile){
  bound.box <- shapefile$shp$shp[[1]]$box
  if(length(shapefile$shp$shp) == 1){
    return(bound.box)
  }
  for(strat in seq(along = shapefile$shp$shp)[-1]){
    bound.box <- rbind(bound.box, shapefile$shp$shp[[strat]]$box)  
  }     
  bound.box <- as.array(bound.box)
  dimnames(bound.box)[[1]] <- seq(along = shapefile$shp$shp)
  bound.box <- as.data.frame(bound.box)
  total.bound.box <- c(xmin = min(bound.box[,"xmin"]), ymin = min(bound.box[,"ymin"]), xmax = max(bound.box[,"xmax"]), ymax = max(bound.box[,"ymax"]))
  return(total.bound.box) 
}
