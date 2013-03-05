#' Class "DDF.Analysis" 
#' 
#' Class \code{"DDF.Analysis"} is an S4 class describing a detection function
#' which is to be fitted to the data.
#'
#' @name DDF.Analysis-class
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{make.ddf.analysis(dsmodel = ~cds(key = "hn", formula = ~1), criteria = "AIC")} 
#' @keywords classes
#' @export
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

setMethod(
  f="get.criteria",
  signature="DDF.Analysis",
  definition=function(object){
    aic <- object@ddf.result[[1]]$criterion
    return(aic)
  }    
)   

setMethod(
  f="run.analysis",
  signature="DDF.Analysis",
  definition=function(object, ddf.dat){
    dist.data <- ddf.dat@ddf.dat
    ddf.result <- ddf(dsmodel = object@dsmodel, data = dist.data, method = "ds")
    #ddf.result.list <- list(ddf.result = ddf.result)
    #object@ddf.result <- ddf.result.list
    return(ddf.result)
  }    
) 

