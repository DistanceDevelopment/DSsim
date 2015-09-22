#' @include Region.R
#' @include Population.R
#' @include Line.Transect.R
#' @include Single.Obs.DDF.Data.R
#' @include Obs.Table.R
#' @include Sample.Table.R
#' @include Region.Table.R
#' @include generic.functions.R

#' S4 Class "LT.Survey.Results"
#'
#' Class containing all the components relating to a single realisation of a
#' survey.
#'
#' @name LT.Survey.Results-class
#' @title S4 Class "LT.Survey.Results"
#' @slot region Object of class \code{"Region"}; the region
#' representation.
#' @slot population Object of class \code{"Population"}; the
#' population.
#' @slot transects Object of class \code{"Line.Transect"}; the
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
#'  \item{\code{plot}}{\code{signature=(object = "LT.Survey.Results")}: plots
#'  the region, the location of individuals in the population, the transects
#'  and the successful sightings.}
#'  \item{\code{get.distance.data}}{\code{signature=(object = "LT.Survey.Results")}: returns the ddf data as a dataframe..}
#' }
#' @keywords classes
#'
setClass("LT.Survey.Results", representation(region = "Region",
                                             population = "Population",
                                             transects = "Line.Transect",
                                             ddf.data = "Single.Obs.DDF.Data",
                                             obs.table = "Obs.Table",
                                             sample.table = "Sample.Table",
                                             region.table = "Region.Table"))

setMethod(
  f="initialize",
  signature="LT.Survey.Results",
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
#' Plots an S4 object of class 'LT.Survey.Results'. Plots the
#' region, the transects, the population and colour codes the 
#' detections
#' 
#' @param x object of class LT.Survey.Results
#' @param y not used
#' @param ... other general plot parameters 
#' @rdname plot.LT.Survey.Results-methods
#' @exportMethod
setMethod(
  signature="LT.Survey.Results",
  f="plot",
  definition=function(x, y, ...){
    #plot(x@region, ...)
    #plot(x@transects, ...)
    #plot(x@population, ...)
    #plot(x@ddf.data, ...)
    #Temporary fix while until the issue is investigated more
    lt.results <- x
    temp.func(lt.results)
    invisible(lt.results)
  }
)

# temporary function
# 
# A work around for a generic function issue
#
# @param lt.results
# @export
temp.func <- function(lt.results){
  #PLOT REGION
  x <- lt.results@region
  add = FALSE
  plot.units = character(0)
  region.col = NULL
  gap.col = NULL
  plot.list <- function(list.coords, border = 1, fill.col = NULL){
    #lapply(list.coords, FUN = lines, type = type, col = col)
    lapply(list.coords, FUN = polygon, border = border, col = fill.col)
    invisible(list.coords)                          
  }
  #Set up plot
  if(length(plot.units) == 0){
    plot.units <- x@units
  }
  if(!add){      
    xlabel <- paste("X-coords (",plot.units[1],")", sep = "")
    ylabel <- paste("Y-coords (",plot.units[1],")", sep = "")
    plot(c(x@box[["xmin"]], x@box[["xmax"]]), c(x@box[["ymin"]], x@box[["ymax"]]), col = "white", xlab = xlabel, ylab = ylabel, main = x@region.name, yaxt = "n", xaxt = "n")
    xticks <- axTicks(1)
    yticks <- axTicks(2)
    #Set up axes
    if(plot.units != x@units){
      #convert units
      if(x@units == "m" & plot.units == "km"){ 
        axis(1, at = xticks, labels = xticks/1000)
        axis(2, at = yticks, labels = yticks/1000)
      }else if(x@units == "km" & plot.units == "m"){
        axis(1, at = xticks, labels = xticks*1000)
        axis(2, at = yticks, labels = yticks*1000)
      }else{
        warning("The requested conversion of units is not currently supported, this option will be ignored.", call. = FALSE, immediate. = TRUE)
      }
    }else{
      #no unit conversion needed
      axis(1, at = xticks, labels = xticks)
      axis(2, at = yticks, labels = yticks)
    }
  }
  lapply(x@coords, FUN = plot.list, fill.col = region.col)
  lapply(x@gaps, FUN = plot.list, fill.col = gap.col)
  #PLOT TRANSECTS
  x <- lt.results@transects
  transect.ID = numeric(0) 
  col = 1
  plot.transect <- function(sampler.info, col){
    lines(x = c(sampler.info[["start.X"]],sampler.info[["end.X"]]), y = c(sampler.info[["start.Y"]],sampler.info[["end.Y"]]), col = col)
    invisible(sampler.info)  
  }
  sampler.info <- x@sampler.info
  sampler.info$ID <- as.numeric(sampler.info$ID)
  if(length(transect.ID) == 0){
    transect.ID <- unique(sampler.info$ID)
  }
  apply(as.matrix(sampler.info[sampler.info$ID%in%transect.ID,]), 1, FUN = plot.transect, col = col)
  #PLOT POPULATION
  x <- lt.results@population
  points(x@population$x, x@population$y, col = 2, pch = 20)
  #PLOT DETECTIONS
  x <- lt.results@ddf.data
  points(x@ddf.dat$x, x@ddf.dat$y, col = 5, pch = 20, cex = 1.5)
}

#' @rdname get.distance.data-methods
#' @export
setMethod(
  f="get.distance.data",
  signature="LT.Survey.Results",
  definition=function(object){
    #extracts and returns data.frame of distance data
    dist.data <- object@ddf.data@ddf.dat
    return(dist.data)
  }
)





