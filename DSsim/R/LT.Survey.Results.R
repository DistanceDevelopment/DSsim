
################################################################################
# CONSTRUCT CLASS AND DEFINE INITIALIZE AND VALIDITY
################################################################################

#' Virtual Class "LT.Survey.Results" 
#'
#' Class containing all the components relating to a single realisation of a
#' survey.
#' @name LT.Survey.Results-class
#' @docType class
#' @keywords classes
#' @export
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
################################################################################
# GENERIC METHODS
################################################################################

#' @rdname LT.Survey.Results-class
#' @aliases plot,LT.Survey.Results-method
setMethod(
  f="plot",
  signature="LT.Survey.Results",
  definition=function(x, y, ...){
    plot(x@region)
    plot(x@transects)
    plot(x@population)
    plot(x@ddf.data)
    invisible(x)
  }    
)  

#' @rdname get.distance.data-methods
#' @aliases get.distance.data,LT.Survey.Results-method
setMethod(
  f="get.distance.data",
  signature="LT.Survey.Results",
  definition=function(object){
    dist.data <- object@ddf.data@ddf.dat
    return(dist.data)
  }    
)  





