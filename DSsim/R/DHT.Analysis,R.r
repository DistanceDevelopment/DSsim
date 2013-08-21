################################################################################
# CONSTRUCT CLASS AND DEFINE INITIALIZE AND VALIDITY
################################################################################  

setClass(Class = "DHT.Analysis", representation(dsmodel = "formula",
                                                criteria = "character",
                                                dht.result = "list", "VIRTUAL"))
                                                
setMethod(
  f="initialize",
  signature="DDF.Analysis",
  definition=function(.Object, dsmodel, criteria){
    .Object@dsmodel <- dsmodel
    .Object@criteria <- criteria
    #Check object is valid
    validObject(.Object)
    # return object
    return(.Object) 
  }
)
setValidity("DDF.Analysis",
  function(object){
    return(TRUE)
  }
)

################################################################################
# GENERIC METHODS
################################################################################
setGeneric(name = "run.ddf.analysis", def = function(object, ddf.dat, ...){standardGeneric ("run.ddf.analysis")})  

setMethod(
  f="run.ddf.analysis",
  signature="DDF.Analysis",
  definition=function(object, ddf.dat){
    dist.data <- ddf.dat@ddf.dat
    ddf.result <- ddf(dsmodel = object@dsmodel, data = dist.data, method = "ds")
    ddf.result.list <- list(ddf.result = ddf.result)
    object@ddf.result <- ddf.result.list
    return(object)
  }    
) 

