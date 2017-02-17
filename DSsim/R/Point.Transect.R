#' @include Transect.R

#' @description Class "Point.Transect" contains an instance of a Line Transect Survey
#'
#' @name Point.Transect-class
#' @title S4 Class "Point.Transect"
#' @slot design.obj Object of class \code{"character"}; the object name
#' of the design object which generated the transects.
#' @slot sampler.info Object of class \code{"data.frame"}; the sampler point coordinates.
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

setValidity("Point.Transect",
            function(object){
              sampler.info.names <- names(object@sampler.info)
              required.var.names <- c("ID", "X", "Y", "region", "effort")
              for(i in seq(along = required.var.names)){
                if(!(required.var.names[i] %in% sampler.info.names)){
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
#' @exportMethod plot
setMethod(
  f="plot",
  signature="Point.Transect",
  definition=function(x, y, transect.ID = numeric(0), col = 1, ...){
    sampler.info <- x@sampler.info
    if(is.null(sampler.info$ac.simple)){
      points(sampler.info$X, sampler.info$Y, pch = 20, col = 4, cex = 1.75, ...)
    }else{
      simple <- sampler.info[sampler.info$ac.simple,]
      adv.detectors <- sampler.info[!sampler.info$ac.simple,]
      points(adv.detectors$X, adv.detectors$Y, pch = 17, col = "blueviolet", cex = 1.75)
      #points(adv.detectors$X, adv.detectors$Y, pch = 20, col = 6, cex = 1.5)
      points(simple$X, simple$Y, pch = 20, col = 4, cex = 1.75)
    }
    invisible(x)
  }
)







