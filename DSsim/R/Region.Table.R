#' Class "Region.Table" 
#' 
#' Class \code{"Region.Table"} is an S4 class containing a 
#' region table which is required for Hortvitz-Thompson estimation of 
#' density and abundance.
#'
#' @name Region.Table-class
#' @title S4 Class "Region.Table"
#' @docType class
#' @section Objects from the Class: Objects can be created by calls to the 
#' function \code{create.survey.results(simulation, dht.table = TRUE)} 
#' @keywords classes
#' @export
#' @seealso \code{\link{create.survey.results}}
setClass(Class = "Region.Table", representation(region.table = "data.frame"))

setMethod(
  f="initialize",
  signature="Region.Table",
  definition=function(.Object, data = data.frame(NULL)){
    .Object@region.table <- data
    #Check object is valid
    validObject(.Object)
    # return object
    return(.Object) 
  }
)
setValidity("Region.Table",
  function(object){
    return(TRUE)
  }
)
