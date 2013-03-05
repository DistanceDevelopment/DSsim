################################################################################
# CONSTRUCT CLASS AND DEFINE INITIALIZE AND VALIDITY
################################################################################

##' Virtual Class "LT.Survey" 
##'
##' Virtual Class \code{"Survey.Design"} is an S4 class detailing the type of line transect 
##' design and the co-ordinates of the end points of the transects.
##' class \code{\link{lm}} with a common model.
##' @name lmList-class
##' @aliases lmList-class show,lmList-method
##' @docType class
##' @section Objects from the Class: Objects can be created by calls of the form
##' \code{new("lmList", ...)} or, more commonly, by a call to
##' \code{\link{lmList}}.
##' @keywords classes
##' @export

setClass(Class = "LT.Survey", 
         representation = representation(line.transect = "Line.Transect",
                                         perpendicular.truncation = "numeric", "VIRTUAL"),
         contains = "Survey"
) 

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

