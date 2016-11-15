#' @importFrom stats runif
#' @importFrom splancs as.points
generate.pop.N <- function(population.description, region.obj){
#This function generates a Population based on a fixed population size
  N <- population.description@N
  density.obj <- population.description@density
  first = TRUE
  for(strat in seq(along = density.obj@density.surface)){
    if(N[strat] > 0){
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
      if(nrow(grid.locations) < N[strat]){
        warning(paste("DSsim is unable to generate the requested population size for strata ", strat, ". We recommend you check the spacing of the density grid is appropriate, it may need reducing. Population size requested = ", N[strat], ", Population size generated = ", nrow(grid.locations),".", sep = ""), call. = FALSE)
      }else{
        grid.locations <- grid.locations[1:N[strat],]
      }
      # Add strata ID
      grid.locations$strata <- rep(strat, nrow(grid.locations))
      # Accumulate all location
      if(first){
        all.grid.locations <- grid.locations
        first <- FALSE
      }else{
        all.grid.locations <- rbind(all.grid.locations, grid.locations)
      }
    }
  }
  return(all.grid.locations)  
}
