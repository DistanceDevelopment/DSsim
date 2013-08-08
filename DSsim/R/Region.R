#' Class "Region" 
#' 
#' Class \code{"Region"} is an S4 class containing descriptions of the 
#' study area. The polygons describing the region are found in the 
#' coords slot and any gaps are described as polygons in the gaps slot.
#'
#' @name Region-class
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{make.region(region.name = "region.name", shapefile = region.shapefile)} 
#' @keywords classes
#' @export
setClass(Class = "Region", 
         representation(region.name = "character", 
                        strata.name = "character",
                        units = "character", 
                        area = "numeric", 
                        box = "numeric", 
                        coords = "list", 
                        gaps = "list")
)

setMethod(
  f="initialize",
  signature="Region",
  definition=function(.Object, region.name = character(0), strata.name = character(0), units, area, shapefile = NULL, coords, gaps){
    #Input pre-processing
    boundbox <- numeric(0)
    if(length(coords) == 0 & !is.null(shapefile)){
      #if no coordinates have been supplied then it uses the shapefile
      polygons <- coords.from.shapefile(shapefile)
      coords <- polygons$coords
      gaps <- polygons$gaps
      boundbox <- get.bound.box(shapefile)
    }else if(length(coords) == 0 & is.null(shapefile)){
      #complains if neither the coordinates or the shapefile are supplied
      message("Error: You must provide either coordinates or a shapefile")
      return(NULL)
    }
    #calculates the strata areas
    if(length(area) == 0){
      area <- calc.area(coords, gaps)
    }
    #Set slots
    .Object@region.name <- region.name
    .Object@strata.name <- strata.name
    .Object@units       <- units
    .Object@area        <- area
    .Object@box         <- boundbox
    .Object@coords      <- coords
    .Object@gaps        <- gaps
    #Check object is valid
    validObject(.Object)
    # return object
    return(.Object) 
  }
)
setValidity("Region",
  function(object){
    if(length(which(object@area < 0)) > 0){
      return("negative area")
    }    
    if(length(object@coords) != length(object@gaps)){
      return("mismatch in coords and gaps length for strata")
    }
    #print(paste("length of coords: ",length(object@coords)))
    #print(object@strata.name) 
    if(length(object@coords) > 1 & length(object@coords) != length(object@strata.name)){
      return("Number of strata names differs to number of strata in the shapefile")
    }
    return(TRUE)
  }
)
################################################################################
# GENERIC METHODS
################################################################################
setGeneric(name = "get.area", def = function(object){standardGeneric ("get.area")})


setMethod(
  f="plot",
  signature="Region",
  definition=function(x, y, type = "l", add = FALSE, plot.units = character(0),...){
    plot.list <- function(list.coords, type, col = 1){
      lapply(list.coords, FUN = lines, type = type, col = col)
      invisible(list.coords)                          
    }
    #Set up plot
    if(length(plot.units) == 0){
      plot.units <- region@units
    }
    if(!add){      
      xlabel <- paste("X-coords (",plot.units[1],")", sep = "")
      ylabel <- paste("Y-coords (",plot.units[1],")", sep = "")
      plot(c(x@box[["xmin"]], x@box[["xmax"]]), c(x@box[["ymin"]], x@box[["ymax"]]), col = "white", xlab = xlabel, ylab = ylabel, main = x@region.name, yaxt = "n", xaxt = "n", ...)
      xticks <- axTicks(1)
      yticks <- axTicks(2)
      #Set up axes
      if(plot.units != region@units){
        #convert units
        if(region@units == "m" & plot.units == "km"){ 
          axis(1, at = xticks, labels = xticks/1000)
          axis(2, at = yticks, labels = yticks/1000)
        }else if(region@units == "km" & plot.units == "m"){
          axis(1, at = xticks, labels = xticks*1000)
          axis(2, at = yticks, labels = yticks*1000)
        }else{
          message("These units are not currently supported.")
        }
      }else{
        #no unit conversion needed
        axis(1, at = xticks, labels = xticks)
        axis(2, at = yticks, labels = yticks)
      }
    }
    lapply(x@coords, FUN = plot.list, type = type)
    lapply(x@gaps, FUN = plot.list, type = type, col = 1)
    invisible(x)
  }    
) 

setMethod(
  f="get.area",
  signature="Region",
  definition=function(object){
    return(object@area)
  }
)

#setMethod(f="$", signature="Region",
#                definition=function(x, name) {
#                  message("Checking other parts of the object first...")
#                  message("Area of this region is: ",x@area, sep="")
#                  region.name <- eval(parse(text = paste("x@", name, sep="")))                                       
#                  return(region.name)
#})







