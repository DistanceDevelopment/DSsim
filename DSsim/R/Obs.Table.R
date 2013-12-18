#' Class "Obs.Table" 
#' 
#' Class \code{"Obs.Table"} is an S4 class containing an 
#' observation table which is required for Hortvitz-Thompson estimation of 
#' density and abundance.
#'
#' @name Obs.Table-class
#' @docType class
#' @section Objects from the Class: Objects can be created by calls to the 
#' function \code{create.survey.results(simulation, dht.table = TRUE)} 
#' @keywords classes
#' @export
#' @seealso \code{\link{create.survey.results}}
setClass(Class = "Obs.Table", representation(obs.table = "data.frame"))

setMethod(
  f="initialize",
  signature="Obs.Table",
  definition=function(.Object, data = data.frame(NULL)){
    .Object@obs.table <- data
    #Check object is valid
    validObject(.Object)
    # return object
    return(.Object) 
  }
)
setValidity("Obs.Table",
  function(object){
    return(TRUE)
  }
)
