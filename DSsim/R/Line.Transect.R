#' @include Transect.R

#' @description  Class "Line.Transect" contains an instance of a Line Transect Survey
#'
#' @name Line.Transect-class
#' @title S4 Class "Line.Transect"
#' @section Methods:
#' \describe{
#'  \item{\code{plot}}{\code{signature=(object = "Line.Transect")}: plots the 
#'  transects.}
#' }
#' @keywords classes            
#' @export
setClass("Line.Transect", contains = "Transect")

setMethod(
  f="initialize",
  signature="Line.Transect",
  definition=function(.Object, region = NULL, sampler.info = NULL, shapefile = NULL, meta = NULL){
    #Input pre-processing
    if(is.null(sampler.info) & !is.null(shapefile)){      
      sampler.info <- get.line.sampler.info(shapefile, region, meta)
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
    required.var.names <- c("ID", "start.X", "start.Y", "end.X", "end.Y", "length", "region", "d7.length")
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

# GENERIC METHODS DEFINITIONS --------------------------------------------

#' Plot
#' 
#' Plots an S4 object of class 'Line.Transect'. Requires that the
#' associated region has already been plotted. This function adds 
#' the transect lines.
#' 
#' @param x object of class Line.Transect
#' @param y not used
#' @param transect.ID allows individual or groups of transects 
#' to be added
#' @param col colour of the lines
#' @param ... other general plot parameters e.g. lwd
#' @rdname plot.Line.Transect-methods
#' @importFrom graphics lines
#' @exportMethod plot
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







