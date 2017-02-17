#' @title Class "Sample.Table" 
#' 
#' @description Class \code{"Sample.Table"} is an S4 class containing a 
#' region table which is required for Hortvitz-Thompson estimation of 
#' density and abundance.
#'
#' @name Sample.Table-class
#' @slot sample.table data.frame which is the sample table for dht
#' @section Objects from the Class: Objects can be created by calls to the 
#' function \code{create.survey.results(simulation, dht.table = TRUE)} 
#' @keywords classes
#' @seealso \code{\link{create.survey.results}}
setClass(Class = "Sample.Table", representation(sample.table = "data.frame"))

setMethod(
  f="initialize",
  signature="Sample.Table",
  definition=function(.Object, data = data.frame(NULL)){
    .Object@sample.table <- data
    #Check object is valid
    validObject(.Object)
    # return object
    return(.Object) 
  }
)
setValidity("Sample.Table",
  function(object){
    return(TRUE)
  }
)
