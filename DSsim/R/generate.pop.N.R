generate.pop.N <- function(population.description, region.obj){
  N <- population.description@N
  density.obj <- population.description@density
  for(strat in seq(along = density.obj@density.surface)){
    n.cells <- nrow(density.obj@density.surface[[strat]])
    probs <- density.obj@density.surface[[strat]][["density"]]/sum(density.obj@density.surface[[strat]][["density"]])
    #sample more animals than required as some will fall outside the survey region
    samp <- suppressWarnings(sample(x = 1:n.cells, size = 2*N[strat], replace = TRUE, prob = probs))
    grid.locations <- density.obj@density.surface[[strat]][samp,]
    #generate random locations within grid cell
    rx <- runif(nrow(grid.locations), -density.obj@x.space/2, density.obj@x.space/2)  
    ry <- runif(nrow(grid.locations), -density.obj@y.space/2, density.obj@y.space/2)
    #find x,y coords of animals
    grid.locations$x.coord <- grid.locations$x+rx
    grid.locations$y.coord <- grid.locations$y+ry
    #find which x,y coords are within the region
    pts <- as.points(grid.locations$x.coord, grid.locations$y.coord) 
    grid.locations$in.region <- in.polygons(poly.list = region.obj@coords[[strat]], pts = pts, boundary = TRUE)
    grid.locations$in.gaps   <- in.polygons(poly.list = region.obj@gaps[[strat]], pts = pts, boundary = TRUE)
    #Find the first N animals inside the region
    grid.locations <- grid.locations[grid.locations$in.region,]
    grid.locations <- grid.locations[!grid.locations$in.gaps,]
    grid.locations <- grid.locations[1:N[strat],]
    if(strat == 1){
      all.grid.locations <- grid.locations
    }else{
      all.grid.locations <- rbind(all.grid.locations, grid.locations)
    }
  }
  return(all.grid.locations)  
}
