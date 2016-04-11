#' @include Transect.R

#' Class "Point.Transect" contains an instance of a Line Transect Survey
#'
#' @name Point.Transect-class
#' @title S4 Class "Point.Transect"
#' @slot design.obj Object of class \code{"character"}; the object name
#' of the design object which generated the transects.
#' @slot sampler.info Object of class \code{"data.frame"}; the sampler 
#' end point coordinates.
#' @section Methods:
#' \describe{
#'  \item{\code{plot}}{\code{signature=(object = "Point.Transect")}: plots the 
#'  transects.}
#' }
#' @keywords classes            
#' @export
setClass("Point.Transect", contains = "Transect")

setMethod(
  f="initialize",
  signature="Point.Transect",
  definition=function(.Object, region, sampler.info = NULL, shapefile = NULL, meta = NULL){
    #Input pre-processing
    if(is.null(sampler.info) & !is.null(shapefile)){      
      sampler.info <- get.point.sampler.info(shapefile, region, meta)
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
              required.var.names <- c("ID", "X", "Y", "region")
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
#' Plots an S4 object of class 'Point.Transect'. Requires that the
#' associated region has already been plotted. This function adds 
#' the transect lines.
#' 
#' @param x object of class Point.Transect
#' @param y not used
#' @param transect.ID allows individual or groups of transects 
#' to be added
#' @param col colour of the lines
#' @param ... other general plot parameters e.g. lwd
#' @rdname plot.Point.Transect-methods
#' @importFrom graphics lines
#' @exportMethod 
setMethod(
  f="plot",
  signature="Point.Transect",
  definition=function(x, y, transect.ID = numeric(0), col = 1, ...){
    plot.transect <- function(sampler.info, col, ...){
      points(x = sampler.info[["X"]], y = sampler.info[["Y"]], col = col, ...)
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







