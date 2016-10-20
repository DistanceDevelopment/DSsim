extract.spat.poly.coords <- function(spatial.polygon){
  # Extracts the coordinated from a spatial polygon object and returns as a list of data.frames.
  polygon.list <- list()
  polygons <- spatial.polygon@polygons[[1]]@Polygons
  for(i in seq(along = polygons)){
    polygon.list[[i]] <-polygons[[i]]@coords
  }
  return(polygon.list)
}