#' @include Population.R
#' @include generic.functions.R

#' @title Virtual Class "Survey"
#'
#' @description Class \code{"Survey"} is an S4 class containing an instance of a population.
#'
#' @name Survey-class
#' @slot population Object of class \code{"Population"}; an instance of
#' a population.
#' @slot transect Object of class \code{"Transect"}; the
#'  transects.
#' @section Methods:
#' \describe{
#'  \item{\code{create.region.table}}{\code{signature=(object = "Survey", ...)}:
#'  creates a region table for \code{dht}.}
#'  \item{\code{create.sample.table}}{\code{signature=(object = "Survey", ...)}:
#'  creates a sample table for \code{dht}.}
#' }
#' @keywords classes
setClass("Survey", representation(population = "Population", transect = "Transect", "VIRTUAL"))

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
    transect.obj <- try(object@transect, silent = TRUE)
    #Check for backwards compatability
    if(class(transect.obj) == "try-error"){
      transect.obj <- object@line.transect
    }
    transect.info <- transect.obj@sampler.info
    if(length(transect.info$length) > 0){
      #Recorded differently in point and line transects
      #To be depricated at some point
      sample.table <- data.frame(Sample.Label = transect.info$ID, Region.Label = transect.info$region, Effort = transect.info$length)
    }else{
      sample.table <- data.frame(Sample.Label = transect.info$ID, Region.Label = transect.info$region, Effort = transect.info$effort)
    }
    sample.table <- unique(sample.table)
    sample.table.obj <- new(Class = "Sample.Table", data = sample.table)
    return(sample.table.obj)
  }
)



