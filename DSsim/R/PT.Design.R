################################################################################
# CONSTRUCT CLASS AND DEFINE INITIALIZE AND VALIDITY
################################################################################

##' Virtual Class "Survey.Design" 
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

setClass(Class = "PT.Design", 
         representation = representation(plus.sampling = "character"),
         contains = "Survey.Design"
)






