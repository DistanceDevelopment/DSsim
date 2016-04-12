#' Class "Detectability" 
#' 
#' Class \code{"Detectability"} is an S4 class describing the probablity
#' of detecting individuals / clusters in a population.
#'
#' @name Detectability-class
#' @title S4 Class "Detectability"
#' @docType class  
#' @slot key.function Object of class \code{"character"}; a code
#' specifying the detection function form ("hn" = half normal, "hr" = 
#' hazard rate.)
#' @slot scale.param Object of class \code{"numeric"}; The scale
#' parameter for the detection function.
#' @slot shape.param Object of class \code{"numeric"}; The shape
#' parameter for the detection function.
#' @slot covariates Object of class \code{"character"}; The names
#' of the covariates which affect detectability. Not yet implemented
#' @slot cov.param Object of class \code{"numeric"}; The parameter
#' values associated with the covariates. Not yet implemented
#' @slot truncation Object of class \code{"numeric"}; The maximum 
#' distance at which objects may be detected.
#' @keywords classes
#' @export
#' @seealso \code{\link{make.detectability}}
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
    if(!object@key.function%in%c("hr","hn","uf")){
      return("Unsupported key function")
    }
    if(object@key.function == "hr" & length(object@shape.param) == 0){
      return("You have selected the hazard rate model but not supplied a shape parameter.")
    }
    for(i in seq(along = object@scale.param)){
      if(object@scale.param[i] <= 0){
        return("Invalid scale parameter. Must be greater than zero.")
      }else if(object@key.function == "uf" & object@scale.param[i] > 1){
        return("Invalid scale parameter. Must be greater than zero and less than 1 for the uniform distribution.")
      } 
    }
    for(i in seq(along = object@shape.param)){
      if(object@shape.param[i] < 0){
        return("Invalid shape parameter. Must be greater than or equal to zero.")
      }  
    }
    if(length(object@scale.param) > 1 & length(object@shape.param) > 1 & length(object@scale.param) != length(object@shape.param)){
      return("The same number of values must be provided for both the shape and scale parameters or only one value supplied for either the shape or scale parameter.")
    }
    return(TRUE)
  }
)

# GENERIC METHODS DEFINITIONS --------------------------------------------




