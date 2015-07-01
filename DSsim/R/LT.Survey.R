#' @include Survey.R

#' Virtual Class "LT.Survey" extends class "Survey"
#'
#' Virtual Class \code{"LT.Survey"} is an S4 class containing a population
#' and a set of transects.
#' @name LT.Survey-class
#' @title S4 Class "LT.Survey"
#' @slot line.transect Object of class \code{"Line.Transect"}; the
#'  transects.
#' @slot perpendicular.truncation Object of class \code{"numeric"}; the
#'  maximum distance from the transect at which animals may be detected.
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

# GENERIC METHODS DEFINITIONS --------------------------------------------



