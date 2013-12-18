#' Class "DDF.Analysis" 
#' 
#' Class \code{"DDF.Analysis"} is an S4 class describing a detection function
#' which is to be fitted to the data.
#'
#' @name DDF.Analysis-class
#' @docType class
#' @section Slots: 
#' \describe{
#'  \item{\code{dsmodel}}{Object of class \code{"formula"}; describing the
#'  detection function model.}
#'  \item{\code{criteria}}{Object of class \code{"character"}; describes 
#'  which model delection croteria to use ("AIC","AICc","BIC").}
#'  \item{\code{ddf.result}}{Object of class \code{"list"}; object of S3 class
#'  ddf.}
#' }
#' @section Methods:
#' \describe{
#'  \item{\code{run.analysis}}{\code{signature=c(object = "DDF.Analysis", 
#'  data = "DDF.Data")}: runs the analysis described in the object on the
#'  data provided.}
#' }
#' @keywords classes
#' @export
#' @seealso \code{\link{make.ddf.analysis.list}}
setClass(Class = "DDF.Analysis", representation(dsmodel = "formula",
                                                criteria = "character",
                                                ddf.result = "list", "VIRTUAL"))
                                                
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
    if(object@criteria %in% c("aic", "AIC")){
      return(TRUE)
    }else{
      message("This selection criteria is not supported")
      return(FALSE)
    }
  }
)

################################################################################
# GENERIC METHODS
################################################################################  


#' @rdname run.analysis-methods
#' @aliases run.analysis,DDF.Analysis,DDF.Data-method
setMethod(
  f="run.analysis",
  signature=c("DDF.Analysis","DDF.Data"),
  definition=function(object, data, dht = FALSE){
    dist.data <- data@ddf.dat
    ddf.result <- ddf(dsmodel = object@dsmodel, data = dist.data, method = "ds")
    #ddf.result.list <- list(ddf.result = ddf.result)
    #object@ddf.result <- ddf.result.list
    return(ddf.result)
  }    
) 



