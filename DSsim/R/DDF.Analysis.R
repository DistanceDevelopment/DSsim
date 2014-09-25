#' @include generic.functions.R
#' @include DDF.Data.R


#' Class "DDF.Analysis" 
#' 
#' Class \code{"DDF.Analysis"} is an S4 class describing a detection function
#' which is to be fitted to the data.
#'
#' @name DDF.Analysis-class
#' @title S4 Class "DDF.Analysis"
#' @docType class
#' @section Slots: 
#' \describe{
#'  \item{\code{dsmodel}}{Object of class \code{"formula"}; describing the
#'  detection function model.}
#'  \item{\code{criteria}}{Object of class \code{"character"}; describes 
#'  which model delection criteria to use ("AIC","AICc","BIC").}
#'  \item{\code{truncation}}{Object of class \code{"character"}; Specifies 
#'  the truncation distance for the analyses.}
#'  \item{\code{binned.data}}{Object of class \code{"character"}; logical 
#'  value specifying if the data should be binned for analysis.}
#'  \item{\code{cutpoints}}{Object of class \code{"character"}; gives the 
#'  cutpoint of the bins for binned data analysis.}
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
#<<<<<<< HEAD
  signature=c("DDF.Analysis","DDF.Data"),
  definition=function(object, data, dht = FALSE){
    #dist.data <- data@ddf.dat
    #ddf.result <- ddf(dsmodel = object@dsmodel, data = dist.data, method = "ds")
    #ddf.result.list <- list(ddf.result = ddf.result)
    #object@ddf.result <- ddf.result.list
#=======
  #signature="DDF.Analysis",
  #definition=function(object, ddf.dat){
    dist.data <- data@ddf.dat
    if(object@binned.data){
      #binned data
      dist.data <- dist.data[dist.data$distance <= max(object@cutpoints),]
      dist.data <- create.bins(dist.data, cutpoints = object@cutpoints)
      ddf.result <- try(ddf(dsmodel = object@dsmodel, data = dist.data, method = "ds", meta.data = list(binned = TRUE, breaks = object@cutpoints, width = max(object@cutpoints))))
      if(class(ddf.result) == "try-error"){
        cat(ddf.result[1])
        call <- paste(object@dsmodel)[2]
        cutpoints <- paste(object@cutpoints, collapse = ',')
        cat(paste("ddf(dsmodel = ~",call,", data = dist.data, method = \"ds\", meta.data = list(binned = TRUE, breaks = ",cutpoints,"))))"), sep = "")
        ddf.result <- NA
      }
    }else{
      #NEED TO ADD TRY STATEMENTS HERE!
      #exact distances
      if(length(object@truncation) == 0){
        ddf.result <- ddf(dsmodel = object@dsmodel, data = dist.data, method = "ds")   
      }else{
        ddf.result <- ddf(dsmodel = object@dsmodel, data = dist.data, method = "ds", meta.data = list(width = object@truncation))   
      }
    }
#>>>>>>> Binned-Data
    return(ddf.result)
  }    
) 



