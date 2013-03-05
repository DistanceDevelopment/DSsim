#' Class "Region.Table" 
#' 
#' Class \code{"Region.Table"} is an S4 class containing a 
#' region table which is required for Hortvitz-Thompson estimation of 
#' density and abundance.
#'
#' @name Region.Table-class
#' @docType class
#' @section Objects from the Class: Objects can be created by calls to the 
#' function \code{simulate.survey(simulation, dht.table = TRUE)} 
#' @keywords classes
#' @export
setClass(Class = "Region.Table", representation(region.table = "data.frame"))

setMethod(
  f="initialize",
  signature="Region.Table",
  definition=function(.Object, data){
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
