################################################################################
# CONSTRUCT CLASS AND DEFINE INITIALIZE AND VALIDITY
################################################################################

#' Class "Line.Transect" contains an instance of a Line Transect Survey
#'
#' @name Line.Transect-class
#' @docType class
#' @section Slots: 
#' \describe{
#'  \item{\code{design.obj}}{Object of class \code{"character"}; the object name
#'  of the design object which generated the transects.}
#'  \item{\code{sampler.info}}{Object of class \code{"data.frame"}; the sampler 
#'  end point coordinates.}
#' }
#' @section Methods:
#' \describe{
#'  \item{\code{plot}}{\code{signature=(object = "Line.Transect")}: plots the 
#'  transects.}
#' }
#' @keywords classes            
#' @export
setClass("Line.Transect", representation(design.obj = "character", sampler.info = "data.frame"))

setMethod(
  f="initialize",
  signature="Line.Transect",
  definition=function(.Object, region, sampler.info = NULL, shapefile = NULL){
    #Input pre-processing
    if(is.null(sampler.info) & !is.null(shapefile)){      
      sampler.info <- get.sampler.info(shapefile, region)
    }else{      
    }
    #Set slots
    #.Object@design.obj <- design.obj
    .Object@sampler.info <- sampler.info
    #Check object is valid
    validObject(.Object)
    # return object
    return(.Object) 
  }
)

setValidity("Line.Transect",
  function(object){
    sampler.info.names <- names(object@sampler.info)
    required.var.names <- c("ID", "start.X", "start.Y", "end.X", "end.Y", "length", "region")
    if(length(sampler.info.names) != length(required.var.names)){
      return("The sampler.info data.frame contains an incorect number of variables.")
    }
    for(i in seq(along = sampler.info.names)){
      if(!sampler.info.names[i]%in%required.var.names){
        return("The variable names provided for the sampler.info data.frame do not match those which are required.")
      }
    }
    return(TRUE)
  }
)


################################################################################
# GENERIC METHODS
################################################################################

#' @rdname Line.Transect-class
#' @aliases plot,Line.Transect-method
setMethod(
  f="plot",
  signature="Line.Transect",
  definition=function(x, y, transect.ID = numeric(0), col = 1, ...){
    plot.transect <- function(sampler.info, col, ...){
      lines(x = c(sampler.info[["start.X"]],sampler.info[["end.X"]]), y = c(sampler.info[["start.Y"]],sampler.info[["end.Y"]]), col = col, ...)
      invisible(sampler.info)  
    }
    sampler.info <- x@sampler.info
    sampler.info$ID <- as.numeric(sampler.info$ID)
    if(length(transect.ID) == 0){
      transect.ID <- unique(sampler.info$ID)
    }
    apply(as.matrix(sampler.info[sampler.info$ID%in%transect.ID,]), 1, FUN = plot.transect, col = col, ...)
    invisible()
  }
)







