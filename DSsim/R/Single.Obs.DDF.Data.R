#' @include DDF.Data.R

#' S4 Class "Single.Obs.DDF.Data" 
#'
#' DDF data resulting from a single observer survey.
#' 
#' @name Single.Obs.DDF.Data-class
#' @title S4 Class "Single.Obs.DDF.Data"
#' @keywords classes
setClass(Class = "Single.Obs.DDF.Data", contains = "DDF.Data")

setMethod(
  f="initialize",
  signature="Single.Obs.DDF.Data",
  definition=function(.Object, data){
    .Object@ddf.dat <- data
    #Check object is valid
    validObject(.Object)
    # return object
    return(.Object) 
  }
)
setValidity("Single.Obs.DDF.Data",
  function(object){
    return(TRUE)
  }
)

# GENERIC METHODS DEFINITIONS --------------------------------------------


