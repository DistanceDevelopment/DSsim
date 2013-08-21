################################################################################
# CONSTRUCT CLASS AND DEFINE INITIALIZE AND VALIDITY
################################################################################

#' Virtual Class "Survey.Design" 
#'
#' Virtual Class \code{"Survey.Design"} is an S4 class detailing the type of line transect 
#' design and the co-ordinates of the end points of the transects.
#' @name Survey.Design-class
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{make.design(transect.type, design.details, region, design.axis, spacing, plus.sampling, path)}
#' @keywords classes
#' @export

setClass(Class = "Survey.Design", 
         representation = representation(region.obj = "character",
                                         plus.sampling = "logical", 
                                         realised = "logical",
                                         transect.obj.names = "character", 
                                         path = "character",
                                         filenames = "character",
                                         file.index = "numeric", "VIRTUAL")
)

################################################################################
# GENERIC METHODS
################################################################################













