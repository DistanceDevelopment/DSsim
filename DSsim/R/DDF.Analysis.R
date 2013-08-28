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
                                                truncation = "numeric",
                                                binned.data = "logical",
                                                cutpoints = "numeric",
                                                ddf.result = "list", "VIRTUAL"))
                                                
setMethod(
  f="initialize",
  signature="DDF.Analysis",
  definition=function(.Object, dsmodel, criteria, truncation, binned.data, cutpoints){
    .Object@dsmodel <- dsmodel
    .Object@criteria <- criteria
    .Object@truncation <- truncation
    .Object@binned.data <- binned.data
    .Object@cutpoints <- cutpoints
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
  f="run.analysis",
  signature="DDF.Analysis",
  definition=function(object, ddf.dat){
    dist.data <- ddf.dat@ddf.dat
    if(object@binned.data){
      #binned data
      dist.data <- dist.data[dist.data$distance <= max(object@cutpoints),]
      dist.data <- create.bins(dist.data, cutpoints = object@cutpoints)
      ddf.result <- ddf(dsmodel = object@dsmodel, data = dist.data, method = "ds", meta.data = list(binned = TRUE, breaks = object@cutpoints))
    }else{
      #exact distances
      if(length(object@truncation) == 0){
        ddf.result <- ddf(dsmodel = object@dsmodel, data = dist.data, method = "ds")   
      }else{
        ddf.result <- ddf(dsmodel = object@dsmodel, data = dist.data, method = "ds", meta.data = list(width = object@truncation))   
      }
    }
    return(ddf.result)
  }    
) 

