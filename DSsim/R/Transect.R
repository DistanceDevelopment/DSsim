#' Class "Transect" contains an instance of a Line Transect Survey
#'
#' @name Transect-class
#' @title S4 Virtual Class "Transect"
#' @slot design.obj Object of class \code{"character"}; the object name
#' of the design object which generated the transects.
#' @slot sampler.info Object of class \code{"data.frame"}; the sampler 
#' coordinates.
#' @section Methods:
#' \describe{
#'  \item{\code{plot}}{\code{signature=(object = "Transect")}: plots the 
#'  transects.}
#' }
#' @keywords classes            
#' @export
setClass("Transect", representation(design.obj = "character", sampler.info = "data.frame", "VIRTUAL"))







