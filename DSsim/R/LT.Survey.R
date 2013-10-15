################################################################################
# CONSTRUCT CLASS AND DEFINE INITIALIZE AND VALIDITY
################################################################################

#' Virtual Class "LT.Survey" 
#'
#' Virtual Class \code{"LT.Survey"} is an S4 class containing a population
#' and a set of transects.
#' @name LT.Survey-class
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' @keywords classes
#' @export

setClass(Class = "LT.Survey", 
         representation = representation(line.transect = "Line.Transect",
                                         perpendicular.truncation = "numeric", "VIRTUAL"),
         contains = "Survey"
) 

################################################################################
# GENERIC METHODS
################################################################################

#' @rdname create.sample.table-methods
#' @aliases create.sample.table, Survey-method
setMethod(
  f="create.sample.table",
  signature="Survey",
  definition=function(object){
    transect.obj <- object@line.transect 
    transect.info <- transect.obj@sampler.info
    sample.table <- data.frame(Sample.Label = transect.info$ID, Region.Label = transect.info$region, Effort = transect.info$length)
    sample.table <- unique(sample.table)
    sample.table.obj <- new(Class = "Sample.Table", data = sample.table)
    return(sample.table.obj)
  }    
) 

