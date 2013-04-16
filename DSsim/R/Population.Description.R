#' Class "Population.Description" 
#' 
#' Class \code{"Population.Description"} is an S4 class containing a 
#' description of the population. It provides methods to generate an
#' example population.
#'
#' @name Population.Description-class
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{make.population.description(N, density.obj, region)} 
#' @keywords classes
#' @export
setClass("Population.Description", representation(N           = "numeric", 
                                                  density     = "Density", 
                                                  region.name = "character",
                                                  size        = "logical",
                                                  size.min    = "numeric",
                                                  size.max    = "numeric",
                                                  size.lambda = "numeric",
                                                  size.dist   = "character",
                                                  gen.by.N    = "logical"))
setMethod(
  f="initialize",
  signature="Population.Description",
  definition=function(.Object, N, density, region.name, size, size.min, size.max, size.lambda, gen.by.N = TRUE){
    #Input pre-processing
    #Set slots
    .Object@N           <- N
    .Object@density     <- density
    .Object@region.name <- region.name
    .Object@size        <- size
    .Object@size.min    <- size.min
    .Object@size.max    <- size.max
    .Object@size.lambda <- size.lambda
    #Check object is valid
    validObject(.Object)
    # return object
    return(.Object) 
  }
)
setValidity("Population.Description",
  function(object){
    if(length(object@size.min) > 0){
      if(object@size.min < 1){
        return("Cannot have group sizes less than 1")
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

setMethod(
  f="generate.population",
  signature="Population.Description",
  definition=function(object, detectability, region.obj = NULL){
    density.obj <- object@density
    if(class(region.obj) != "Region"){
      region.obj <- get(object@region.name)
    }
    n.cells <- nrow(density.obj@density.surface)       
    probs <- density.obj@density.surface[["density"]]/sum(density.obj@density.surface[["density"]])
    #sample more animals than required as some will fall outside the survey region
    samp <- suppressWarnings(sample(x = 1:n.cells, size = 2*object@N, replace = TRUE, prob = probs))
    grid.locations <- density.obj@density.surface[samp,]
    #generate random locations within grid cell
    rx <- runif(nrow(grid.locations), -density.obj@x.space/2, density.obj@x.space/2)  
    ry <- runif(nrow(grid.locations), -density.obj@y.space/2, density.obj@y.space/2)
    #find x,y coords of animals
    grid.locations$x.coord <- grid.locations$x+rx
    grid.locations$y.coord <- grid.locations$y+ry
    #find which x,y coords are within the region
    pts <- as.points(grid.locations$x.coord,grid.locations$y.coord) 
    in.region <- lapply(region.obj@coords, FUN = in.polygons, pts = pts, boundary = TRUE)
    in.gaps <- lapply(region.obj@gaps, FUN = in.polygons, pts = pts, boundary = TRUE)
    for(i in seq(along = in.region)){
      if(i == 1){
        in.region.temp <- in.region[[1]] 
        in.gaps.temp <- in.gaps[[1]]
      }else{ 
        in.region.temp <- cbind(in.region.temp, in.region[[i]])
        in.gaps.temp <- cbind(in.gaps.temp, in.gaps[[i]])
      }
    }
    in.region <- apply(in.region.temp, 1, FUN = sum)
    in.gaps <- apply(in.gaps.temp, 1, FUN = sum)
    grid.locations$in.region <- ifelse(in.region == 1, TRUE, FALSE)
    grid.locations$in.gaps <- ifelse(in.gaps == 1, TRUE, FALSE)    
    #grid.locations$in.region <- in.polygons(pts, region.obj@coords, boundary = TRUE)
    #grid.locations$in.gaps   <- in.polygons(pts, region.obj@gaps, boundary = TRUE)
    #Find the first N animals inside the region
    grid.locations <- grid.locations[grid.locations$in.region,]
    grid.locations <- grid.locations[!grid.locations$in.gaps,]
    grid.locations <- grid.locations[1:object@N,]
    #create population object
    population <- new(Class = "Population", region = object@region.name, strata.names = region.obj@region.name, N = object@N, D = object@N/region.obj@area, population = data.frame(object = 1:nrow(grid.locations), x = grid.locations$x.coord, y = grid.locations$y.coord), detectability = detectability) 
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

