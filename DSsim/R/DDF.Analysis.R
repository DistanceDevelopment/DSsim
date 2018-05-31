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
#' @param warnings a list of warnings and how many times they arose
#' @export
setMethod(
  f="run.analysis",
  signature=c("DDF.Analysis","DDF.Data"),
  definition=function(object, data, dht = FALSE, point = FALSE, warnings = list()){
    #Get distance data
    dist.data <- data@ddf.dat
    # Strip out missing distances
    dist.data <- dist.data[!is.na(dist.data),]
    # Make sure there is a detected column
    if(is.null(dist.data$detected)){
      dist.data$detected <- rep(1, nrow(dist.data))
    }
    if(object@binned.data){
      #binned data
      dist.data <- dist.data[dist.data$distance <= max(object@cutpoints),]
      dist.data <- create.bins(dist.data, cutpoints = object@cutpoints)
      #Try to fit ddf model
      if(point){
        fit.model <- paste("ddf(dsmodel = ~", as.character(object@dsmodel)[2] ,", data = dist.data, method = 'ds', meta.data = list(width = ", max(object@cutpoints), ", point = TRUE, binned = TRUE, breaks = ", object@cutpoints ,"), control = list(silent = TRUE))", sep = "")
      }else{
        fit.model <- paste("ddf(dsmodel = ~", as.character(object@dsmodel)[2] ,", data = dist.data, method = 'ds', meta.data = list(width = ", max(object@cutpoints), ", binned = TRUE, breaks = ", object@cutpoints ,"), control = list(silent = TRUE))", sep = "")
      }
    }else{
      #exact distances
      if(length(object@truncation) == 0){
        #If there is no truncation distance specified
        if(point){
          model.fit <- paste("ddf(dsmodel = ~", as.character(object@dsmodel)[2] ,", data = dist.data, method = 'ds', meta.data = list(point = TRUE), control = list(silent = TRUE))", sep = "") 
        }else{
          model.fit <- paste("ddf(dsmodel = ~", as.character(object@dsmodel)[2] ,", data = dist.data, method = 'ds', control = list(silent = TRUE))", sep = "")
        }
      }else{
        #If there is a truncation distance 
        if(point){
          model.fit <- paste("ddf(dsmodel = ~", as.character(object@dsmodel)[2] ,", data = dist.data, method = 'ds', meta.data = list(point = TRUE, width = ", object@truncation,"), control = list(silent = TRUE))", sep = "")
        }else{
          model.fit <- paste("ddf(dsmodel = ~", as.character(object@dsmodel)[2] ,", data = dist.data, method = 'ds', meta.data = list(width = ", object@truncation,"), control = list(silent = TRUE))", sep = "")
        }
      }
    }
    
    #Set warning to NULL
    W <- NULL
    # Fit model
    ddf.result <- withCallingHandlers(tryCatch(eval(parse(text = model.fit)), 
                                               error=function(e)e), 
                                      warning=function(w){W <<- w; invokeRestart("muffleWarning")})
    #check if there was an error, warning or non-convergence
    if(any(class(ddf.result) == "error")){
      warnings <- message.handler(warnings, paste("Error: ", ddf.result$message, sep = ""))
      ddf.result <- NA
    }else if(ddf.result$ds$converge != 0){
      ddf.result <- NA
    }else if(any(predict(ddf.result)$fitted < 0)){
      ddf.result <- NA
      warnings <- message.handler(warnings, "Negative predictions, excluding these results")
    } 
    if(!is.null(W)){
      warnings <- message.handler(warnings, W)
    } 
    return(list(ddf.result = ddf.result, warnings = warnings))
  }    
) 



