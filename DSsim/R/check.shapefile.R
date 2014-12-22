check.shapefile <- function(shapefile){
  keep <- rep(NA, length(shapefile$shp$shp))
  for(i in seq(along=shapefile$shp$shp)){
    keep[i] <- shapefile$shp$shp[[i]]$shape.type == 5
  }
  index <- which(keep)
  new.shp.list <- list()
  for(i in seq(along = index)){
    new.shp.list[[i]] <- shapefile$shp$shp[[index[i]]]
  }
  shapefile$shp$shp <- new.shp.list
  return(shapefile)
}