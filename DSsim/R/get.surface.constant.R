#' @importFrom sp Polygon Polygons SpatialPolygons
get.surface.constant <- function(region, x.space, y.space, constant, buffer){ 
  
  # Check the value of the buffer
  if(length(buffer) == 0){
    buffer <- max(x.space, y.space)
  }
  
  #Creates a density surface with a constant value across the whole survey region
  #Create a rectangular grid over the entire region
  region.width <- region@box[["xmax"]]-region@box[["xmin"]]
  region.height <- region@box[["ymax"]]-region@box[["ymin"]]
  no.x.ints <- ceiling((region.width+2*buffer)/x.space)
  no.y.ints <- ceiling((region.height+2*buffer)/y.space)
  x.adj <- (x.space*no.x.ints - region.width)/2
  y.adj <- (y.space*no.y.ints - region.height)/2
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
      buffered.gaps[[strat]] <- rgeos::gBuffer(region.gaps, width = -1*x.space)
      # Just extract the coordinates
      buffered.gaps[[strat]] <- extract.spat.poly.coords(buffered.gaps[[strat]])
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
    gridpoints$density <- rep(constant[strat], nrow(gridpoints))
    density.surfaces[[strat]] <- gridpoints
  }  
  return(density.surfaces)
}
