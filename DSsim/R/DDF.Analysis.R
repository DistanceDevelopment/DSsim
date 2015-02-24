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
#'  which model selection criteria to use ("AIC","AICc","BIC").}
#'  \item{\code{truncation}}{Object of class \code{"character"}; Specifies 
#'  the truncation distance for the analyses.}
#'  \item{\code{binned.data}}{Object of class \code{"character"}; logical 
#'  value specifying if the data should be binned for analysis.}
#'  \item{\code{cutpoints}}{Object of class \code{"character"}; gives the 
#'  cutpoints of the bins for binned data analysis.}
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
  definition=function(.Object, dsmodel = call(), criteria, truncation, binned.data, cutpoints){
    if(criteria %in% c("aic", "AIC")){
    }else{
      warning("This selection criteria is not currently supported, it will be changed to AIC", call. = FALSE, immediate. = TRUE)
      criteria = "AIC"
    }
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
    return(TRUE)
  }
)

################################################################################
# GENERIC METHODS
################################################################################  



setMethod(
  f="run.analysis",
  signature=c("DDF.Analysis","DDF.Data"),
  definition=function(object, data, dht = FALSE){
    dist.data <- data@ddf.dat
    if(object@binned.data){
      #binned data
      dist.data <- dist.data[dist.data$distance <= max(object@cutpoints),]
      dist.data <- create.bins(dist.data, cutpoints = object@cutpoints)
      #ddf.result <- try(ddf(dsmodel = object@dsmodel, data = dist.data, method = "ds", meta.data = list(binned = TRUE, breaks = object@cutpoints, width = max(object@cutpoints))))
      options(show.error.messages = FALSE)
      ddf.result <- try(eval(parse(text = paste("ddf(dsmodel = ~", as.character(object@dsmodel)[2] ,", data = dist.data, method = 'ds', meta.data = list(width = ", max(object@cutpoints), ", binned = TRUE, breaks = ", object@cutpoints ,"))", sep = ""))), silent = TRUE)
      options(show.error.messages = TRUE)
      if(any(class(ddf.result) == "try-error")){
        #cat(ddf.result[1])
        call <- paste(object@dsmodel)[2]
        cutpoints <- paste(object@cutpoints, collapse = ',')
        #cat(paste("ddf(dsmodel = ~",call,", data = dist.data, method = \"ds\", meta.data = list(binned = TRUE, breaks = ",cutpoints,"))))"), sep = "")
        ddf.result <- NA
      }else if(ddf.result$ds$converge != 0){
        ddf.result <- NA
      }
    }else{
      #NEED TO ADD TRY STATEMENTS HERE!
      #exact distances
      if(length(object@truncation) == 0){
        #ddf.result <- try(ddf(dsmodel = object@dsmodel, data = dist.data, method = "ds"), silent = TRUE)  
        options(show.error.messages = FALSE)
        ddf.result <- try(eval(parse(text = paste("ddf(dsmodel = ~", as.character(object@dsmodel)[2] ,", data = dist.data, method = 'ds')", sep = ""))), silent = TRUE)
        options(show.error.messages = TRUE)
      }else{
        #ddf.result <- try(ddf(dsmodel = object@dsmodel, data = dist.data, method = "ds", meta.data = list(width = object@truncation)), silent = TRUE)  
        options(show.error.messages = FALSE)
        ddf.result <- try(eval(parse(text = paste("ddf(dsmodel = ~", as.character(object@dsmodel)[2] ,", data = dist.data, method = 'ds', meta.data = list(width = ", object@truncation,"))", sep = ""))), silent = TRUE)
        options(show.error.messages = TRUE)
      }
      if(class(ddf.result)[1] == "try-error"){
        ddf.result <- NA
      }else if(ddf.result$ds$converge != 0){
        ddf.result <- NA
      }
    }
    return(ddf.result)
  }    
) 



