#' @include Survey.R
#' @include generic.function.R

################################################################################
# CONSTRUCT CLASS AND DEFINE INITIALIZE AND VALIDITY
################################################################################

#' Virtual Class "LT.Survey" extends class "Survey"
#'
#' Virtual Class \code{"LT.Survey"} is an S4 class containing a population
#' and a set of transects.
#' @name LT.Survey-class
#' @title S4 Class "LT.Survey"
#' @section Slots: 
#' \describe{
#'  \item{\code{line.transect}}{Object of class \code{"Line.Transect"}; the 
#'  transects.}
#'  \item{\code{perpendicular.truncation}}{Object of class \code{"numeric"}; the 
#'  maximum distance from the transect at which animals may be detected.} 
#'  }
#' @section Methods:
#' \describe{
#'  \item{\code{create.sample.table}}{\code{signature=(object = "LT.Survey", ...)}: 
#'  creates a sample table for \code{dht}.}
#' }
#' @keywords classes
#' @seealso \code{\link{make.design}}
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

