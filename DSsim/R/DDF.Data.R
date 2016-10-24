#' @include generic.functions.R

#' @title Class "DDF.Data" 
#' 
#' @description A virtual class containing a data.frame with distance sampling data in the 
#' correct format for mrds.
#'
#' @name DDF.Data-class
#' @title S4 Class "DDF.Data"
#' @slot ddf.dat Object of class \code{"data.frame"}; dataframe with
#'  all the necessary column to fit a detection function using mrds.
#' @keywords classes
#' @export
setClass(Class = "DDF.Data", representation(ddf.dat = "data.frame", "VIRTUAL"))


#' Plot
#' 
#' Plots an S4 object of class 'DDF.Data'. Requires that the
#' associated region has already been plotted. This function adds 
#' the locations of the individuals/clusters in the population who
#' were detected.
#' 
#' @param x object of class DDF.Data
#' @param y not used
#' @param ... other general plot parameters 
#' @rdname plot.DDF.Data-methods
#' @importFrom graphics points
#' @exportMethod plot 
setMethod(
  f="plot",
  signature="DDF.Data",
  definition=function(x, y, ...){
    points(x@ddf.dat$x, x@ddf.dat$y, col = 5, pch = 20, cex = 1.5, ...) 
    invisible(x)
  }    
)