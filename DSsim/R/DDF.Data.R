#' Class "DDF.Data" 
#' 
#' A virtual class containing a data.frame with distance sampling data in the 
#' correct format for mrds.
#'
#' @name DDF.Data-class
#' @title S4 Class "DDF.Data"
#' @docType class
#' @section Slots: 
#' \describe{
#'  \item{\code{ddf.dat}}{Object of class \code{"data.frame"}; dataframe with
#'  all the necessary column to fit a detection function using mrds.}
#' }
#' @keywords classes
#' @export
setClass(Class = "DDF.Data", representation(ddf.dat = "data.frame", "VIRTUAL"))


