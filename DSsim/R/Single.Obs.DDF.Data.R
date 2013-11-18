################################################################################
# CONSTRUCT CLASS AND DEFINE INITIALIZE AND VALIDITY
################################################################################

#' S4 Class "Single.Obs.DDF.Data" 
#'
#' DDF data resulting from a single observer survey.
#' 
#' @name Single.Obs.DDF.Data-class
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' @keywords classes
#' @export
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

################################################################################
# GNERIC METHOD DEFINITIONS
################################################################################

#' @rdname Single.Obs.DDF.Data-class
#' @aliases plot,Single.Obs.DDF.Data-method
setMethod(
  f="plot",
  signature="Single.Obs.DDF.Data",
  definition=function(x, y, ...){
    points(x@ddf.dat$x, x@ddf.dat$y, col = 5, pch = 20, cex = 1.5) 
    invisible(x)
  }    
)
