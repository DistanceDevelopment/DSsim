#' Class "Detectability" 
#' 
#' Class \code{"Detectability"} is an S4 class describing the probablity
#' of detecting individuals / clusters in a population.
#'
#' @name Detectability-class
#' @docType class                             
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{make.detectability(key.function, scale.param, perp.truncation, rad.truncation)} 
#' @keywords classes
#' @export
setClass("Detectability", representation(key.function    = "character",                                        
                                         scale.param     = "numeric",
                                         shape.param     = "numeric",
                                         covariates      = "character",
                                         cov.param       = "numeric",
                                         truncation      = "numeric")) 
setMethod(
  f="initialize",
  signature="Detectability",
  definition=function(.Object, key.function, scale.param, shape.param = numeric(0), covariates = character(0), cov.param = numeric(0), truncation = numeric(0)){
    #Input pre-processing
    #Set slots
    .Object@key.function <- key.function
    .Object@scale.param  <- scale.param
    .Object@shape.param  <- shape.param
    .Object@covariates   <- covariates
    .Object@cov.param    <- cov.param
    .Object@truncation   <- truncation      
    #Check object is valid
    validObject(.Object)
    # return object
    return(.Object) 
  }
)

setValidity("Detectability",
  function(object){
    if(!object@key.function%in%c("hr","hn")){
      return("Unsupported key function")
    }
    if(object@scale.param <= 0){
      return("Invalid scale parameter. Must be greater than zero.")
    }
    return(TRUE)
  }
)
################################################################################
# GENERIC METHODS
################################################################################




