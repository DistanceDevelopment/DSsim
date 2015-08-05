#' @importFrom stats rpois runif
generate.pop.D <- function(population.description, region.obj){
#this function generates a population based on the values in the 
#Density object grid (not from a fixed population size)
  density.obj <- population.description@density
  first <- TRUE
  for(strat in seq(along = density.obj@density.surface)){
    n.cells <- nrow(density.obj@density.surface[[strat]])
    densities <- density.obj@density.surface[[strat]][["density"]]
    cell.area <- density.obj@x.space*density.obj@y.space  
    lambdas <- densities*cell.area
    #generate the number of animals to fall in each cell from a Poisson distribution
    no.in.cells <- rpois(n.cells,  lambda = lambdas)
    #check there are some animals
    if(sum(no.in.cells) > 0){
      #repeat each row count the number of times in no.in.cell
      row.ids <- rep(1:n.cells, no.in.cells)
      #extract grid cells
      grid.locations <- density.obj@density.surface[[strat]][row.ids,]
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
      #Find which animals are in the region
      grid.locations <- grid.locations[grid.locations$in.region,]
      grid.locations <- grid.locations[!grid.locations$in.gaps,]
      if(strat == 1){
        all.grid.locations <- grid.locations
      }else{
        all.grid.locations <- rbind(all.grid.locations, grid.locations)
      }
    }
  }
  return(all.grid.locations)  
}


