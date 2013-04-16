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
         representation(region.name = "character", strata.name = "character", area = "numeric", box = "numeric", coords = "list", gaps = "list")
)

setMethod(
  f="initialize",
  signature="Region",
  definition=function(.Object, region.name = character(0), strata.name = character(0), area = numeric(0), shapefile = NULL, coords = list(), gaps = list()){
    #Input pre-processing
    boundbox <- numeric(0)
    if(length(coords) == 0 & !is.null(shapefile)){
      polygons <- coords.from.shapefile(shapefile)
      coords <- polygons$coords
      gaps <- polygons$gaps
      boundbox <- get.bound.box(shapefile)
    }
    #if(units == "m"){
    #  convert.units
    #}else if(units != "km"){
    #  stop("Unsupported units", .call = FALSE)
    #}
    if(length(area) == 0){
      area <- calc.area(coords, gaps)
    }
    #Set slots
    .Object@region.name <- region.name
    .Object@strata.name <- strata.name
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
  definition=function(x, y, type = "l", add = FALSE, ...){
    plot.list <- function(list.coords, type, col = 1){
      lapply(list.coords, FUN = lines, type = type, col = col)
      invisible(list.coords)
    }
    #Input pre-processing
    if(!add){
      plot(c(x@box[["xmin"]], x@box[["xmax"]]), c(x@box[["ymin"]], x@box[["ymax"]]), col = "white", xlab = "X-coords (units to be added)", ylab = "Y-coords (units to be added)", main = x@region.name, ...) 
    }
    lapply(x@coords, FUN = plot.list, type = type)
    lapply(x@gaps, FUN = plot.list, type = type, col = 8)
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

################################################################################
# ASSOCIATED METHODS
################################################################################

calc.area <- function(coords, gaps){
  list.area <- function(coords.list){
    matrix.coords <- lapply(coords.list, as.matrix)
    areas <- lapply(matrix.coords, areapl)
    areas <- unlist(areas)
    total.area <- sum(areas) 
    return(total.area)
  }
  require(splancs)
  
  gross.area <- unlist(lapply(coords, list.area))
  gap.area <- unlist(lapply(gaps, list.area))
  
#  temp.coords <- lapply(coords, as.matrix)
#  gross.area <- lapply(temp.coords, areapl)
#  gross.area <- as.numeric(gross.area)
#  gross.area <- sum(gross.area)
#  temp.gaps <- lapply(gaps, as.matrix)
#  gap.area <- lapply(temp.gaps, areapl)
#  gap.area <- as.numeric(gap.area)
#  gap.area <- sum(gap.area)
  
  net.area <- gross.area - gap.area
  return(net.area)
}

get.bound.box <- function(shapefile){
  bound.box <- shapefile$shp$shp[[1]]$box
  if(length(shapefile$shp$shp) == 1){
    return(bound.box)
  }
  for(strat in seq(along = shapefile$shp$shp)[-1]){
    bound.box <- rbind(bound.box, shapefile$shp$shp[[strat]]$box)  
  }     
  bound.box <- as.array(bound.box)
  dimnames(bound.box)[[1]] <- seq(along = shapefile$shp$shp)
  bound.box <- as.data.frame(bound.box)
  total.bound.box <- c(xmin = min(bound.box[,"xmin"]), ymin = min(bound.box[,"ymin"]), xmax = max(bound.box[,"xmax"]), ymax = max(bound.box[,"ymax"]))
  return(total.bound.box) 
}


