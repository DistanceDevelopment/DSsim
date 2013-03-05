################################################################################
# CONSTRUCT CLASS AND DEFINE INITIALIZE AND VALIDITY
################################################################################

##' Class "Single.Obs.DDF.Data"

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

setMethod(
  f="plot",
  signature="Single.Obs.DDF.Data",
  definition=function(x, y, ...){
    points(x@ddf.dat$x, x@ddf.dat$y, col = 3, pch = 20) 
    invisible(x)
  }    
)
