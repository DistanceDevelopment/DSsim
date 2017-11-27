#' @importFrom sp Polygon Polygons SpatialPolygons
get.surface.gam <- function(region, x.space, y.space, gam.model, buffer){
  
  # Check the value of the buffer
  if(length(buffer) == 0){
    buffer <- min(x.space, y.space)
  }
  
  #Creates a density surface grid based on predictions from a gam object  
  #Create a rectangular grid over the entire region
  no.x.ints <- ceiling((region@box[["xmax"]]-region@box[["xmin"]])/x.space)
  no.y.ints <- ceiling((region@box[["ymax"]]-region@box[["ymin"]])/y.space)
  x.adj <- (x.space*no.x.ints - (region@box[["xmax"]]-region@box[["xmin"]]))/2
  y.adj <- (y.space*no.y.ints - (region@box[["ymax"]]-region@box[["ymin"]]))/2
  x.vals <- seq(region@box[["xmin"]]-x.adj, region@box[["xmax"]]+x.adj, by = x.space)
  y.vals <- seq(region@box[["ymin"]]-y.adj, region@box[["ymax"]]+y.adj, by = y.space)
  temp.coords <- expand.grid(x.vals, y.vals)
  names(temp.coords) <- names(region@coords[[1]][[1]])
  
  # Create buffered regions rather than jittering the grid points 
  buffered.coords <- list()
  buffered.gaps <- list()
  for(strat in seq(along = region@coords)){
    #Extract polygons and gaps for current strata
    strat.poly <- region@coords[[strat]]
    strat.gap <- region@gaps[[strat]]
    temp.list.coord <- list()
    temp.list.gap <- list()
    # Deal with the outer polygons
    for(poly in seq(along = strat.poly)){
      current.poly <- strat.poly[[poly]]
      temp <- Polygon(current.poly, hole = FALSE)
      temp.list.coord[[poly]] <- temp
    }
    polys.rgeos <- Polygons(temp.list.coord, ID = 1)
    region.coords <- SpatialPolygons(list(polys.rgeos))
    # Add positive buffer region
    buffered.coords[[strat]] <- rgeos::gBuffer(region.coords, width = x.space)
    #Just extract the coordinates
    buffered.coords[[strat]] <- extract.spat.poly.coords(buffered.coords[[strat]])
    # Now for the gaps
    for(poly in seq(along = strat.gap)){
      current.poly <- strat.gap[[poly]]
      temp <- Polygon(current.poly, hole = FALSE)
      temp.list.gap[[poly]] <- temp
    }
    if(length(temp.list.gap) > 0){
      polys.rgeos <- Polygons(temp.list.gap, ID = 1)
      region.gaps <- SpatialPolygons(list(polys.rgeos))
      # Negative buffer region for gaps
      temp <- rgeos::gBuffer(region.gaps, width = -1*x.space)
      # Just extract the coordinates if they haven't disappeard with the negative buffering
      if(!is.null(temp)){
        buffered.gaps[[strat]] <- temp
        buffered.gaps[[strat]] <- extract.spat.poly.coords(buffered.gaps[[strat]])
      }else{
        buffered.gaps[[strat]] <- list()
      }
    }else{
      buffered.gaps[[strat]] <- list()
    }
  }
  
  # Now find the grid points which lie within the buffered study region
  density.surfaces <- list()
  for(strat in seq(along = buffered.coords)){
    to.keep <- in.polygons(buffered.coords[[strat]], pts = temp.coords, boundary = TRUE) 
    gridpoints <- temp.coords[to.keep,]
    to.discard <- in.polygons(buffered.gaps[[strat]], pts = gridpoints, boundary = TRUE) 
    gridpoints <- gridpoints[!to.discard,]
    predicted.values <- mgcv::predict.gam(gam.model, newdata = gridpoints, type = "response")
    gridpoints$density <- predicted.values
    density.surfaces[[strat]] <- gridpoints
  }  
  return(density.surfaces)
}