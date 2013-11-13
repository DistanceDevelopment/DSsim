
################################################################################
# CONSTRUCT CLASS AND DEFINE INITIALIZE AND VALIDITY
################################################################################

#' Class "Population" 
#' 
#' Contains an instance of a population including a description of their detectability
#' in the form of an object of calss Detectability.
#'
#' @name Population-class
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' @keywords classes
#' @export
setClass("Population", representation(region.obj   = "character",
                                      strata.names = "character", 
                                      N            = "numeric", 
                                      D            = "numeric",
                                      population   = "data.frame",
                                      detectability = "Detectability")) 
setMethod(
  f="initialize",
  signature="Population",
  definition=function(.Object, region, strata.names, N, D, population, detectability){
    #Input pre-processing
    #Set slots
    .Object@region.obj   <- region
    .Object@strata.names <- strata.names
    .Object@D            <- D
    .Object@N            <- N
    .Object@population   <- population
    .Object@detectability <- detectability
    #Check object is valid
    validObject(.Object)
    # return object
    return(.Object) 
  }
)
setValidity("Population",
  function(object){
    return(TRUE)
  }
)
################################################################################
# GENERIC METHODS
################################################################################
#' @rdname Population-class
#' @aliases plot,Population-method
setMethod(
  f="plot",
  signature="Population",
  definition=function(x, y, type = "p"){
    points(x@population$x, x@population$y, col = 2, pch = 20) 
    invisible(x)
  }    
) 





