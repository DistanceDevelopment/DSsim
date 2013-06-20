#' Class "Population.Description" 
#' 
#' Class \code{"Population.Description"} is an S4 class containing a 
#' description of the population. It provides methods to generate an
#' example population.
#'
#' @name Population.Description-class
#' @aliases Population.Description-class generate.population, Population.Description-method
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{make.population.description(N, density.obj, region)} 
#' @keywords classes
#' @export
setClass("Population.Description", representation(N           = "numeric", 
                                                  density     = "Density", 
                                                  region.name = "character",
                                                  size        = "logical",
                                                  size.table  = "data.frame",
                                                  size.dist   = "character",
                                                  gen.by.N    = "logical"))
setMethod(
  f="initialize",
  signature="Population.Description",
  definition=function(.Object, N, density, region.obj, size.table, size, gen.by.N = TRUE){
    #Input pre-processing
    if(!gen.by.N){
      ave.density <- NULL
      message("Calculating average density for each strata.")
      for(strat in seq(along = density@density.surface)){        
        ave.density[strat] <- get.ave.density(density.surface = density@density.surface[[strat]], coords = region.obj@coords[[strat]], gaps = region.obj@gaps[[strat]], x.space = density@x.space, y.space = density@y.space)
        #ave.density[strat] <- mean(object@density@density.surface[[strat]]$density)
      }
      N <- region.obj@area*ave.density
    }
    #Set slots
    .Object@N           <- N
    .Object@density     <- density
    .Object@region.name <- region.obj@region.name
    .Object@size.table  <- size.table
    .Object@size        <- size
    .Object@gen.by.N    <- gen.by.N
    #Check object is valid
    validObject(.Object)
    # return object
    return(.Object) 
  }
)
setValidity("Population.Description",
  function(object){
    if(object@size){
      if(sum(object@size.table$prob) != 1){
        return("Probabilities in cluster size table must sum to 1")
      }
      if(min(object@size.table$size) < 1){
        return("Cannot have cluster sizes less than 1")
      }
    }
    return(TRUE)
  }
)
################################################################################
# GENERIC METHODS
################################################################################
setGeneric("get.N",function(object){standardGeneric ("get.N")})
setMethod("get.N","Population.Description",
  function(object){
    return(object@N)
  }
)

#' @rdname generate.population-methods
#' @aliases generate.population, Population.Description, ANY-method
setMethod(
  f="generate.population",
  signature="Population.Description",
  definition=function(object, detectability, region.obj = NULL){
    if(class(region.obj) != "Region"){
      region.obj <- get(object@region.name)
    }
    N <- object@N
    density.obj <- object@density
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
    #create population object
    population.dataframe <- data.frame(object = 1:nrow(all.grid.locations), x = all.grid.locations$x.coord, y = all.grid.locations$y.coord)
    if(object@size){
      cluster.size <- sample(object@size.table$size, nrow(population.dataframe), replace = TRUE, prob = object@size.table$prob)
      population.dataframe$size <- cluster.size
    }
    population <- new(Class = "Population", region = object@region.name, strata.names = region.obj@region.name, N = N, D = N/region.obj@area, population = population.dataframe, detectability = detectability) 
    return(population)  
  }    
) 


################################################################################
# ASSOCIATED METHODS
################################################################################
#generate.population <- function(population.description, detectability){
##need to expand this to the case where there are strata and clustered data
#  if(!class(population.description) == "Population.Description"){
#    return(NULL)
#  }
#  density.obj <- population.description@density
#  region.obj <- get(population.description@region.name)
#  n.cells <- nrow(density.obj@density.surface)
#  probs <- density.obj@density.surface[["density"]]/sum(density.obj@density.surface[["density"]])
#  #sample more animals than required as some will fall outside the survey region
#  samp <- suppressWarnings(sample(x = 1:n.cells, size = 2*population.description@N, replace = TRUE, prob = probs))
#  grid.locations <- density.obj@density.surface[samp,]
#  #generate random locations within grid cell
#  rx <- runif(nrow(grid.locations), -density.obj@x.space/2, density.obj@x.space/2)  
#  ry <- runif(nrow(grid.locations), -density.obj@y.space/2, density.obj@y.space/2)
#  #find x,y coords of animals
#  grid.locations$x.coord <- grid.locations$x+rx
#  grid.locations$y.coord <- grid.locations$y+ry
#  #find which x,y coords are within the region
#  pts <- as.points(grid.locations$x.coord,grid.locations$y.coord) 
#  #grid.locations$in.region <- inout(pts = pts, poly = as.matrix(region.obj@coords), bound = TRUE)
#  grid.locations$in.region <- in.polygons(pts, region.obj@coords, boundary = TRUE)
#  grid.locations$in.gaps   <- in.polygons(pts, region.obj@gaps, boundary = TRUE)
#  #points(grid.locations$x.coord[grid.locations$in.region], grid.locations$y.coord[grid.locations$in.region], col=3, pch=20) 
#  #points(grid.locations$x.coord[!grid.locations$in.region], grid.locations$y.coord[!grid.locations$in.region], col=2, pch=20) 
#  #Find the first N animals inside the region
#  grid.locations <- grid.locations[grid.locations$in.region,]
#  grid.locations <- grid.locations[!grid.locations$in.gaps,]
#  grid.locations <- grid.locations[1:population.description@N,]
#  #create population object
#  population <- new(Class = "Population", region = population.description@region.name, strata.names = region.obj@region.name, N = population.description@N, D = population.description@N/region.obj@area, population = data.frame(object = 1:nrow(grid.locations), x = grid.locations$x.coord, y = grid.locations$y.coord), detectability = detectability) 
#  return(population) 
#}
#

