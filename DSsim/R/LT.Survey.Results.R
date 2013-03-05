
################################################################################
# CONSTRUCT CLASS AND DEFINE INITIALIZE AND VALIDITY
################################################################################

#Example class used only as a user interface not within run simulation

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
  definition=function(.Object, region, population, transects, ddf.data, obs.table = NULL, sample.table = NULL, region.table = NULL){
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





