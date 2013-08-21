
################################################################################
# CONSTRUCT CLASS AND DEFINE INITIALIZE AND VALIDITY
################################################################################
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
setMethod(
  f="plot",
  signature="Population",
  definition=function(x, y, type = "p"){
    points(x@population$x, x@population$y, col = 2, pch = 20) 
    invisible(x)
  }    
) 

################################################################################
# ASSOCIATED METHODS
################################################################################



