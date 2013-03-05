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
         representation(region.name = "character", area = "numeric", box = "numeric", coords = "list", gaps = "list")
)

setMethod(
  f="initialize",
  signature="Region",
  definition=function(.Object, region.name = character(0), area = numeric(0), shapefile = NULL, coords = list(), gaps = list()){
    #Input pre-processing
    boundbox <- numeric(0)
    if(length(coords) == 0 & !is.null(shapefile)){
      polygons <- coords.from.shapefile(shapefile)
      coords <- polygons$coords
      gaps <- polygons$gaps
      boundbox <- shapefile$shp$shp[[1]]$box
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
    if(object@area < 0){
      return("negative area")
    }else{
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
  definition=function(x, y, type = "l", ...){
    #Input pre-processing
    plot(c(x@box[["xmin"]], x@box[["xmax"]]), c(x@box[["ymin"]], x@box[["ymax"]]), col = "white", xlab = "X-coords (units to be added)", ylab = "Y-coords (units to be added)", main = x@region.name, ...) 
    lapply(x@coords, FUN = lines, type = type)
    lapply(x@gaps, FUN = lines, type = type, col = 8)
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

coords.from.shapefile <- function(shapefile){
  coords <- list()
  main.polygons <- list()
  gaps <- list()
  num.parts <- shapefile$shp$shp[[1]]$num.parts
  indexes <- c(shapefile$shp$shp[[1]]$parts, nrow(shapefile$shp$shp[[1]]$points))
  for(p in 1:num.parts){ 
    X <- shapefile$shp$shp[[1]]$points$X[(indexes[p]+1):indexes[p+1]]
    Y <- shapefile$shp$shp[[1]]$points$Y[(indexes[p]+1):indexes[p+1]]
    coords[[p]] <- data.frame(x = X, y = Y)
  }
  #check to see if any are gaps
  count.gap <- 0
  count.outer.poly <- 0
  for(p in 1:num.parts){
    gap <- is.gap(coords[p][[1]], coords[-p])
    if(gap){
      count.gap <- count.gap + 1
      gaps[[count.gap]] <- coords[p][[1]]        
    }else{
      count.outer.poly <- count.outer.poly + 1
      main.polygons[[count.outer.poly]] <- coords[p][[1]] 
    }
  }
  return(list(coords = main.polygons, gaps = gaps))
}           

is.gap <- function(poly, poly.list){
  #is.gap checks to see if a set of points belonging to one polygon are all inside any one of a list of polygons
  all.true <- function(pts.check){
    true.vals <- which(pts.check)
    if(length(true.vals) == length(pts.check)){
      return(TRUE)
    }else if(length(true.vals) == length(pts.check)-1){
      message("Warning two of the shapefiles have a shared boundary point.")
      return(TRUE)
    }else if(length(true.vals) > 0){
      stop(paste("Some of the polygons in the shapefile are intersecting.",sep = ""), call. = FALSE)    
    }else{
      return(FALSE)
    }
  }
  pts <- as.points(poly)
  pts.in <- array(NA, dim = c(nrow(pts),length(poly.list)))
  for(p in seq(along = poly.list)){
    pts.in[,p] <- inout(as.points(pts), poly.list[[p]], bound = FALSE)
  }
  poly.inside <- apply(pts.in, 2, FUN = all.true)
  if(length(which(poly.inside)) == 1){
    gap <- TRUE
  }else if(length(which(poly.inside)) > 1){
    stop(paste("One polygon is inside more than one other polygon. The simulation engine cannot deal with this scenario.",sep = ""), call. = FALSE)
  }else{
    gap <- FALSE
  }
  return(gap)
}

calc.area <- function(coords, gaps){
  temp.coords <- lapply(coords, as.matrix)
  temp.gaps <- lapply(gaps, as.matrix)
  gross.area <- lapply(temp.coords, areapl)
  gross.area <- as.numeric(gross.area)
  gross.area <- sum(gross.area)
  gap.area <- lapply(temp.gaps, areapl)
  gap.area <- as.numeric(gap.area)
  gap.area <- sum(gap.area)
  net.area <- gross.area - gap.area
  return(net.area)
}


