################################################################################
# CONSTRUCT CLASS AND DEFINE INITIALIZE AND VALIDITY
################################################################################
setClass(Class = "Strata", 
         representation(strata.names = "character", strata.areas = "numeric"), 
         contains = "Region"
)

setMethod(
  f="get.area",
  signature="Strata",
  definition=function(object){
    return(object@strata.area)
  }
)

#setMethod(
#  f="initialize",
#  signature="Strata",
#  definition=function(.Object, region = NULL, region.name = character(0), area = numeric(0), shapefile = NULL, coords = list(), gaps = list()){
#    #Input pre-processing
#    if(length(region.name) == 0 & !is.null(region){
#      region.name <- region@region.name
#      area        <- region@area
#    }
#    if(is.null(coords) & !is.null(shapefile)){
#      polygons <- coords.from.shapefile(shapefile)
#      coords <- polygons$coords
#      gaps <- polygons$gaps
#      box <- shapefile$shp$shp[[1]]$box
#    }else{      
#    }
#    #if(units == "m"){
#    #  convert.units
#    #}else if(units != "km"){
#    #  stop("Unsupported units", .call = FALSE)
#    #}
#    if(length(area) == 0){
#      area <- calc.area(coords, gaps)
#    }
#    #Set slots
#    .Object@region.name <- region.name
#    .Object@area        <- area
#    .Object@box         <- box
#    .Object@coords      <- coords
#    .Object@gaps        <- gaps
#    #Check object is valid
#    validObject(.Object)
#    # return object
#    return(.Object) 
#  }
#)
