#' @include Survey.R
#' @include Point.Transect.R

#' @title Virtual Class "PT.Survey" extends class "Survey"
#'
#' @description Virtual Class \code{"PT.Survey"} is an S4 class containing a population
#' and a set of transects.
#' @name PT.Survey-class
#' @title S4 Class "PT.Survey"
#' @slot transect Object of class \code{"Transect"}; the
#'  transects.
#' @slot radial.truncation Object of class \code{"numeric"}; the
#'  maximum distance from the transect at which animals may be detected.
#' @section Methods:
#' \describe{
#'  \item{\code{create.sample.table}}{\code{signature=(object = "PT.Survey", ...)}:
#'  creates a sample table for \code{dht}.}
#' }
#' @keywords classes
#' @seealso \code{\link{make.design}}
#' @export
setClass(Class = "PT.Survey",
         representation = representation(radial.truncation = "numeric", "VIRTUAL"),
         contains = "Survey"
)

# GENERIC METHODS DEFINITIONS --------------------------------------------



