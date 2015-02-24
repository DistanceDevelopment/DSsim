#' @include generic.functions.R
#' @include Density.R

#' Class "Population.Description" 
#' 
#' Class \code{"Population.Description"} is an S4 class containing a 
#' description of the population. It provides methods to generate an
#' example population.
#'                                  
#' @name Population.Description-class
#' @title S4 Class "Population.Description"
#' @docType class
#' @section Slots: 
#' \describe{
#'  \item{\code{N}}{Object of class \code{"numeric"}; number of individuals
#'  in the population (optional).}
#'  \item{\code{density}}{Object of class \code{"Density"}; describes the
#'  population density}
#'  \item{\code{region.name}}{Object of class \code{"character"}; name of
#'  the region in which the population exists.}
#'  \item{\code{size}}{Object of class \code{"logical"}; Indicating whether
#'  detections will be made on cluster (TRUE) or individuals (FALSE).}
#'  \item{\code{size.table}}{Object of class \code{"data.frame"}; This must
#'  have 2 columns- \code{size} and \code{prob}. The first column gives the
#'  possible cluster sizes and the second describes the probabilities
#'  of each cluster size.}
#'  \item{\code{gen.by.N}}{Object of class \code{"logical"}; If \code{TRUE} 
#'  N is fixed otherwise it is generated from a Poisson distribution.}
#'  \item{\code{D.dist}}{Object of class \code{character}; Describes the
#'  density distribution (currently not implemented).}
#' }
#' @section Methods:
#' \describe{
#'  \item{\code{get.N}}{\code{signature=(object = "Population.Description")}:
#'  returns the value of \code{N}}
#'  \item{\code{generate.population}}{\code{signature=(object = "Population.Description")}: generates a single realisation of the population.}
#' }
#' @keywords classes
#' @export
#' @seealso \code{\link{make.population.description}}
setClass("Population.Description", representation(N           = "numeric", 
                                                  density     = "Density", 
                                                  region.name = "character",
                                                  size        = "logical",
                                                  size.table  = "data.frame",
                                                  gen.by.N    = "logical",
                                                  D.dist      = "character"))
setMethod(
  f="initialize",
  signature="Population.Description",
  definition=function(.Object, N, density, region.obj, size.table, size, gen.by.N = TRUE, D.dist = character(0)){
    #Input pre-processing
    if(!gen.by.N){
      ave.density <- NULL
      #message("Calculating average density for each strata.")
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
    .Object@D.dist      <- D.dist
    #Check object is valid
    validObject(.Object)
    # return object
    return(.Object) 
  }
)
setValidity("Population.Description",
  function(object){
    if(length(object@N) > 0 & sum(object@N) <= 0){
      return("You must provide a positive, non-zero abundance")
    }
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


#' S4 generic method to return N
#' 
#' Returns the population size
#'
#' @param object an object of class Population.Description
#' @return numeric value of the population size
#' @export
#' @docType methods
#' @rdname get.N-methods
setGeneric("get.N",function(object){standardGeneric ("get.N")})

#' @rdname get.N-methods
#' @aliases get.N,Population.Description-method
setMethod("get.N","Population.Description",
  function(object){
    return(object@N)
  }
)


#' @rdname generate.population-methods
#' @aliases generate.population,Population.Description-method
setMethod(
  f="generate.population",
  signature="Population.Description",
  definition=function(object, detectability, region.obj = NULL){
    #If the user has not passed in the region object
    if(class(region.obj) != "Region"){
      warning("Obtaining region object from the global workspace", call. = TRUE, immediate. = TRUE) 
      region.obj <- get(object@region.name)
    }  
    #If the population has fixed N
    if(object@gen.by.N){
      all.grid.locations <- generate.pop.N(object, region.obj)  
    }else{
      all.grid.locations <- generate.pop.D(object, region.obj)
      
    } 
    N <- nrow(all.grid.locations)   
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


