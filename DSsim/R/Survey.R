#' @include Population.R
#' @include generic.functions.R

#' Virtual Class "Survey"
#'
#' Class \code{"Survey"} is an S4 class containing an instance of a population.
#'
#' @name Survey-class
#' @title S4 Class "Survey"
#' @slot population Object of class \code{"Population"}; an instance of
#' a population.
#' @section Methods:
#' \describe{
#'  \item{\code{create.region.table}}{\code{signature=(object = "Survey", ...)}:
#'  creates a region table for \code{dht}.}
#' }
#' @keywords classes
setClass("Survey", representation(population = "Population", "VIRTUAL"))

# GENERIC METHODS DEFINITIONS --------------------------------------------

#' @rdname create.region.table-methods
setMethod(
  f="create.region.table",
  signature="Survey",
  definition=function(object, region){
    population <- object@population
    if(length(region@strata.name) > 0){
      region.table <- data.frame(Region.Label = region@strata.name, Area = get.area(region))
    }else{
      region.table <- data.frame(Region.Label = region@region.name, Area = get.area(region))
    }
    region.table.obj <- new(Class = "Region.Table", data = region.table)
    return(region.table.obj)
  }
)

#' @rdname create.sample.table-methods
setMethod(
  f="create.sample.table",
  signature="Survey",
  definition=function(object){
    transect.obj <- object@line.transect
    transect.info <- transect.obj@sampler.info
    sample.table <- data.frame(Sample.Label = transect.info$ID, Region.Label = transect.info$region, Effort = transect.info$length)
    sample.table <- unique(sample.table)
    sample.table.obj <- new(Class = "Sample.Table", data = sample.table)
    return(sample.table.obj)
  }
)



