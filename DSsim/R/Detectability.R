#' Class "Detectability" 
#' 
#' Class \code{"Detectability"} is an S4 class describing the probablity
#' of detecting individuals / clusters in a population.
#'
#' @name Detectability-class
#' @docType class                             
#' @section Slots: 
#' \describe{
#'  \item{\code{key.function}}{Object of class \code{"character"}; a code
#'  specifying the detection function form ("hn" = half normal, "hr" = 
#'  hazard rate.)}
#'  \item{\code{scale.param}}{Object of class \code{"numeric"}; The scale
#'  parameter for the detection function.}
#'  \item{\code{shape.param}}{Object of class \code{"numeric"}; The shape
#'  parameter for the detection function.}
#'  \item{\code{covariates}}{Object of class \code{"character"}; The names
#'  of the covariates which affect detectability. Not yet implemented}
#'  \item{\code{cov.param}}{Object of class \code{"numeric"}; The parameter
#'  values associated with the covariates. Not yet implemented}
#'  \item{\code{truncation}}{Object of class \code{"numeric"}; The maximum 
#'  distance at which objects may be detected.}
#' }
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




