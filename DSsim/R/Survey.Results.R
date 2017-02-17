#' @include generic.functions.R
#' @include Region.R
#' @include Population.R
#' @include Line.Transect.R
#' @include Single.Obs.DDF.Data.R
#' @include Obs.Table.R
#' @include Sample.Table.R
#' @include Region.Table.R
#' @include LT.Survey.Results.R

#' @title S4 Class "Survey.Results"
#'
#' @description Class containing all the components relating to a single realisation of a
#' survey.
#'
#' @name Survey.Results-class
#' @title S4 Class "Survey.Results"
#' @slot region Object of class \code{"Region"}; the region
#' representation.
#' @slot population Object of class \code{"Population"}; the
#' population.
#' @slot transects Object of class \code{"Transect"}; the
#' transects.
#' @slot ddf.data Object of class \code{"Single.Obs.DDF.Data"}; The
#' ddf data for \code{ddf}.
#'  @slot obs.table Object of class \code{"Obs.Table"}; One of the
#'  tables for \code{dht}.
#'  @slot sample.table Object of class \code{"Sample.Table"}; One of
#'  the tables for \code{dht}.
#'  @slot region.table Object of class \code{"Region.Table"}; One of
#'  the tables for \code{dht}.
#' @section Methods:
#' \describe{
#'  \item{\code{plot}}{\code{signature=(object = "Survey.Results")}: plots
#'  the region, the location of individuals in the population, the transects
#'  and the successful sightings.}
#'  \item{\code{get.distance.data}}{\code{signature=(object = "Survey.Results")}: returns the ddf data as a dataframe..}
#' }
#' @keywords classes
#'
setClass("Survey.Results", representation(region = "Region",
                                             population = "Population",
                                             transects = "Transect",
                                             ddf.data = "Single.Obs.DDF.Data",
                                             obs.table = "Obs.Table",
                                             sample.table = "Sample.Table",
                                             region.table = "Region.Table"))

setMethod(
  f="initialize",
  signature="Survey.Results",
  definition=function(.Object, region, population, transects, ddf.data, obs.table = data.frame(NULL), sample.table = data.frame(NULL), region.table = data.frame(NULL)){
    #Set slots
    .Object@region        <- region
    .Object@population    <- population
    .Object@transects     <- transects
    .Object@ddf.data      <- ddf.data
    .Object@obs.table     <- obs.table
    .Object@sample.table  <- sample.table
    .Object@region.table  <- region.table
    #Check object is valid
    #validObject(.Object)
    # return object
    return(.Object)
  }
)

# GENERIC METHODS DEFINITIONS --------------------------------------------

#' Plot
#' 
#' Plots an S4 object of class 'Survey.Results'. Plots the
#' region, the transects, the population and colour codes the 
#' detections
#' 
#' @param x object of class LT.Survey.Results
#' @param y not used
#' @param ... other general plot parameters 
#' @rdname plot.Survey.Results-methods
#' @exportMethod plot
setMethod(
  signature="Survey.Results",
  f="plot",
  definition=function(x, y, ...){
    plot(x@region, main = "Example Survey", ...)
    plot(x@population, ...)
    plot(x@transects, ...)
    plot(x@ddf.data, ...)
    invisible(x)
  }
)

#' @rdname get.distance.data-methods
#' @export
setMethod(
  f="get.distance.data",
  signature="Survey.Results",
  definition=function(object){
    #extracts and returns data.frame of distance data
    dist.data <- object@ddf.data@ddf.dat
    return(dist.data)
  }
)





