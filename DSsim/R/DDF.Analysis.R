#' @include generic.functions.R
#' @include DDF.Data.R


#' @title Class "DDF.Analysis" 
#' 
#' @description Class \code{"DDF.Analysis"} is an S4 class describing a detection function
#' which is to be fitted to the data.
#'
#' @name DDF.Analysis-class
#' @title S4 Class "DDF.Analysis"
#' @slot dsmodel Object of class \code{"formula"}; describing the
#'  detection function model.
#' @slot criteria Object of class \code{"character"}; describes 
#'  which model selection criteria to use ("AIC","AICc","BIC").
#' @slot truncation Object of class \code{"character"}; Specifies 
#'  the truncation distance for the analyses.
#' @slot binned.data Object of class \code{"character"}; logical 
#'  value specifying if the data should be binned for analysis.
#' @slot cutpoints Object of class \code{"character"}; gives the 
#'  cutpoints of the bins for binned data analysis.
#' @slot analysis.strata Dataframe with two columns ("design.id" and 
#' "analysis.id"). The former gives the strata names as defined in the
#' design (i.e. the region object) the second specifies how they should 
#' be grouped (into less strata) for the analyses
#' @slot ddf.result Object of class \code{"list"}; object of S3 class
#'  ddf.
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
                                                analysis.strata = "data.frame",
                                                ddf.result = "list", "VIRTUAL"))
                                                
setMethod(
  f="initialize",
  signature="DDF.Analysis",
  definition=function(.Object, dsmodel = call(), criteria, analysis.strata, truncation, binned.data, cutpoints){
    if(criteria %in% c("aic", "AIC", "bic", "BIC", "AICc")){
    }else{
      warning("This selection criteria is not currently supported (please select from 'AIC', 'BIC' or 'AICc'), the simulation is automatically changing it to AIC for this call", call. = FALSE, immediate. = TRUE)
      criteria = "AIC"
    }
    .Object@dsmodel <- dsmodel
    .Object@criteria <- criteria
    .Object@truncation <- truncation
    .Object@binned.data <- binned.data
    .Object@cutpoints <- cutpoints
    .Object@analysis.strata <- analysis.strata
    #make sure they are characters not factors
    .Object@analysis.strata$design.id <- as.character(.Object@analysis.strata$design.id)
    .Object@analysis.strata$analysis.id <- as.character(.Object@analysis.strata$analysis.id)
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

# GENERIC METHODS DEFINITIONS --------------------------------------------
 
#' @rdname run.analysis-methods
#' @param point logical indicating whether it is a point transect survey
#' @export
setMethod(
  f="run.analysis",
  signature=c("DDF.Analysis","DDF.Data"),
  definition=function(object, data, dht = FALSE, point = FALSE){
    dist.data <- data@ddf.dat
    if(object@binned.data){
      #binned data
      dist.data <- dist.data[dist.data$distance <= max(object@cutpoints),]
      dist.data <- create.bins(dist.data, cutpoints = object@cutpoints)
      #Make sure error messages are surpressed
      options(show.error.messages = FALSE)
      #Try to fit ddf model
      if(point){
        ddf.result <- try(eval(parse(text = paste("ddf(dsmodel = ~", as.character(object@dsmodel)[2] ,", data = dist.data, method = 'ds', meta.data = list(width = ", max(object@cutpoints), ", point = TRUE, binned = TRUE, breaks = ", object@cutpoints ,"))", sep = ""))), silent = TRUE)
      }else{
        ddf.result <- try(eval(parse(text = paste("ddf(dsmodel = ~", as.character(object@dsmodel)[2] ,", data = dist.data, method = 'ds', meta.data = list(width = ", max(object@cutpoints), ", binned = TRUE, breaks = ", object@cutpoints ,"))", sep = ""))), silent = TRUE)
      }
      options(show.error.messages = TRUE)
      #Check if it was successful
      if(any(class(ddf.result) == "try-error")){
        #If not
        call <- paste(object@dsmodel)[2]
        cutpoints <- paste(object@cutpoints, collapse = ',')
        ddf.result <- NA
      }else if(ddf.result$ds$converge != 0){
        #If it didn't converge
        ddf.result <- NA
      }
    }else{
      #exact distances
      if(length(object@truncation) == 0){
        #If there is no truncation distance specified
        #Make sure error messages are surpressed
        options(show.error.messages = FALSE)
        #Try to fit ddf model
        if(point){
          ddf.result <- try(eval(parse(text = paste("ddf(dsmodel = ~", as.character(object@dsmodel)[2] ,", data = dist.data, method = 'ds', meta.data = list(point = TRUE))", sep = ""))), silent = TRUE)
        }else{
          ddf.result <- try(eval(parse(text = paste("ddf(dsmodel = ~", as.character(object@dsmodel)[2] ,", data = dist.data, method = 'ds')", sep = ""))), silent = TRUE)
        }
        
        options(show.error.messages = TRUE)
      }else{
        #If there is a truncation distance 
        options(show.error.messages = FALSE)
        if(point){
          ddf.result <- try(eval(parse(text = paste("ddf(dsmodel = ~", as.character(object@dsmodel)[2] ,", data = dist.data, method = 'ds', meta.data = list(point = TRUE, width = ", object@truncation,"))", sep = ""))), silent = TRUE)
        }else{
          ddf.result <- try(eval(parse(text = paste("ddf(dsmodel = ~", as.character(object@dsmodel)[2] ,", data = dist.data, method = 'ds', meta.data = list(width = ", object@truncation,"))", sep = ""))), silent = TRUE)
        }
        options(show.error.messages = TRUE)
      }
      #check if there was an error
      if(class(ddf.result)[1] == "try-error"){
        ddf.result <- NA
      #if it did not converge
      }else if(ddf.result$ds$converge != 0){
        ddf.result <- NA
      }
    }
    return(ddf.result)
  }    
) 



