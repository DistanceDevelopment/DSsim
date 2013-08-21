get.surface.constant <- function(region, x.space, y.space, constant, jit){  
  #Create a rectangular grid over the entire region
  no.x.ints <- ceiling((region@box[["xmax"]]-region@box[["xmin"]])/x.space)
  no.y.ints <- ceiling((region@box[["ymax"]]-region@box[["ymin"]])/y.space)
  x.adj <- (x.space*no.x.ints - (region@box[["xmax"]]-region@box[["xmin"]]))/2
  y.adj <- (y.space*no.y.ints - (region@box[["ymax"]]-region@box[["ymin"]]))/2
  x.vals <- seq(region@box[["xmin"]]-x.adj, region@box[["xmax"]]+x.adj, by = x.space)
  y.vals <- seq(region@box[["ymin"]]-y.adj, region@box[["ymax"]]+y.adj, by = y.space)
  temp.coords <- expand.grid(x.vals, y.vals)
  names(temp.coords) <- names(region@coords[[1]][[1]])
  
  density.surfaces <- list()
  for(strat in seq(along = region@coords)){
    to.keep <- in.polygons(region@coords[[strat]], pts = temp.coords, boundary = TRUE) 
    gridpoints <- temp.coords[to.keep,]
    to.discard <- in.polygons(region@gaps[[strat]], pts = gridpoints, boundary = TRUE) 
    gridpoints <- gridpoints[!to.discard,]
    grid.up <- grid.down <- grid.right <- grid.left <- gridpoints
    grid.up$y <- grid.up$y + y.space
    grid.down$y <-grid.down$y - y.space
    grid.right$x <- grid.right$x + x.space
    grid.left$x <- grid.left$x - x.space
    gridpoints <- rbind(gridpoints, grid.up, grid.down, grid.left, grid.right)
    gridpoints <- unique(gridpoints)
    gridpoints$density <- rep(constant[strat], nrow(gridpoints))
    density.surfaces[[strat]] <- gridpoints
  }  
  return(density.surfaces)
}
