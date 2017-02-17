#' @include Survey.R

#' @title Virtual Class "LT.Survey" extends class "Survey"
#'
#' @description Virtual Class \code{"LT.Survey"} is an S4 class containing a population
#' and a set of transects.
#' @name LT.Survey-class
#' @title S4 Class "LT.Survey"
#' @slot perpendicular.truncation Object of class \code{"numeric"}; the
#'  maximum distance from the transect at which animals may be detected.
#' @keywords classes
#' @seealso \code{\link{make.design}}
#' @export
setClass(Class = "LT.Survey",
         representation = representation(perpendicular.truncation = "numeric", "VIRTUAL"),
         contains = "Survey"
)

# GENERIC METHODS DEFINITIONS --------------------------------------------



