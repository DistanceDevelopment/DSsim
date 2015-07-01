#' Virtual Class "Survey.Design" 
#'
#' Virtual Class \code{"Survey.Design"} is an S4 class detailing the survey 
#' design. Currently only line transect designs are implemented and transects 
#' from these designs must be generated using the Distance software in 
#' advance.
#' 
#' @name Survey.Design-class
#' @title S4 Class "Survey.Design"
#' @slot region.obj Object of class \code{"character"}; The name of
#'  the region which the survey design has been made for.
#' @slot plus.sampling Object of class \code{"logical"}; Whether 
#'  a plus sampling protocol is to be used.
#' @slot path Object of class \code{"character"}; Describing the
#'  folder where the shapefiles containing the transects are located.
#' @slot filenames Object of class \code{"character"}; stores the 
#'  filenames of the transect shapefiles. These are automatically added
#'  when the object is created using all the files in the specified path.
#' @slot file.index Object of class \code{"numeric"}; Keeps track
#'  of which shapefile is to be loaded.
#' @keywords classes
#' @export
#' @seealso \code{\link{make.design}}
setClass(Class = "Survey.Design", 
         representation = representation(region.obj = "character",
                                         plus.sampling = "logical",  
                                         path = "character",
                                         filenames = "character",
                                         file.index = "numeric", "VIRTUAL")
)

# GENERIC METHODS DEFINITIONS --------------------------------------------













