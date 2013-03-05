#' Class "Density" 
#' 
#' Class \code{"Density"} is an S4 class containing a grid which described
#' the density of individuals / clusters of a population.
#'
#' @name Density-class
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{make.density(region, x.space, y.space, constant)} 
#' @keywords classes
#' @export
setClass("Density", representation(region.name = "character", strata.name = "character", density.surface = "data.frame", x.space = "numeric", y.space = "numeric", used = "logical"))

setMethod(
  f="initialize",
  signature="Density",
  definition=function(.Object, region.name, strata.name = character(0), density.surface = NULL, x.space, y.space, constant = NULL, shapefile = NULL, density.gam = NULL, jit = 1){
    #Input pre-processing
    if(is.null(density.surface) & !is.null(constant)){
      density.surface <- get.surface.constant(region.name, x.space, y.space, constant, jit)
    }else{      
    }
    #Set slots
    .Object@region.name <- region.name
    .Object@strata.name <- strata.name
    .Object@density.surface <- density.surface
    .Object@x.space <- x.space
    .Object@y.space <- y.space
    .Object@used    <- FALSE
    #Check object is valid
    validObject(.Object)
    # return object
    return(.Object) 
  }
)
setValidity("Density",
  function(object){
    #check region object exists and is of the correct class
    #check strata object exists and is of the correct class
    
    return(TRUE)
  }
)
################################################################################
# ACCESSOR GENERIC METHODS
################################################################################ 
#setGeneric("get.region.name", function(object){standardGeneric ("get.region.name")})
#setGeneric("set.region.name", function(object, new.slot.value){standardGeneric ("set.region.name")})
#setGeneric("get.strata.name", function(object){standardGeneric ("get.strata.name")})
#setGeneric("set.strata.name", function(object, new.slot.value){standardGeneric ("set.strata.name")})
#setGeneric("get.density.surface", function(object){standardGeneric ("get.density.surface")})
#setGeneric("set.density.surface", function(object, new.slot.value){standardGeneric ("set.density.surface")})
#setGeneric("get.x.space", function(object){standardGeneric ("get.x.space")})
#setGeneric("set.x.space", function(object, new.slot.value){standardGeneric ("set.x.space")})
#setGeneric("get.y.space", function(object){standardGeneric ("get.y.space")})
#setGeneric("set.y.space", function(object, new.slot.value){standardGeneric ("set.y.space")})
#
#setMethod("get.density.surface","Density",
#  function(object){
#    return(object@density.surface)
#  }
#)
#setMethod("set.density.surface","Density",
#  function(object, new.slot.value){
#    object@slot
#    return(object)
#  }
#)

################################################################################
# OTHER GENERIC METHODS
################################################################################ 

setMethod("plot","Density",
  function(x, y, ...){
    density.surface <- x@density.surface
    zlim <- range(density.surface$density)
    zlen <- zlim[2] - zlim[1] + 1
    colorlut <- heat.colors(zlen) 
    col <- colorlut[density.surface$density-zlim[1]+1] 
    points(density.surface$x, density.surface$y, col = col, pch = 20)
  }
)

################################################################################
# ASSOCIATED METHODS
################################################################################
get.surface.constant <- function(region.name, x.space, y.space, constant, jit){
  region <- get(region.name)
  #Create a rectangular grid over the entire region
  no.x.ints <- ceiling((region@box[["xmax"]]-region@box[["xmin"]])/x.space)
  no.y.ints <- ceiling((region@box[["ymax"]]-region@box[["ymin"]])/y.space)
  x.adj <- (x.space*no.x.ints - (region@box[["xmax"]]-region@box[["xmin"]]))/2
  y.adj <- (y.space*no.y.ints - (region@box[["ymax"]]-region@box[["ymin"]]))/2
  x.vals <- seq(region@box[["xmin"]]-x.adj, region@box[["xmax"]]+x.adj, by = x.space)
  y.vals <- seq(region@box[["ymin"]]-y.adj, region@box[["ymax"]]+y.adj, by = y.space)
  temp.coords <- expand.grid(x.vals, y.vals)
  names(temp.coords) <- names(region@coords[[1]])
  #Find which grid points fall withing the region.
  to.keep <- in.polygons(temp.coords, region@coords, boundary = TRUE)
  gridpoints <- temp.coords[to.keep,] 
  to.discard <- in.polygons(gridpoints, region@gaps, boundary = FALSE)
  gridpoints <- gridpoints[!to.discard,]
  
  #temp.coords <- lapply(region@coords, FUN = as.matrix)
  #gridpoints <- lapply(temp.coords, FUN = gridpts, xs = x.space, ys = y.space)  
  #gridpoints <- gridpts(poly = as.matrix(region@coords), xs = x.space, ys = y.space)
  #gridpoints <- as.data.frame(gridpoints)
  #names(gridpoints) <- names(region@coords)
  
  #Create a buffer region                                                       #NEED TO IMPLEMENT JITTER FUNCTION HERE
  grid.up <- gridpoints
  grid.up$y <- grid.up$y + y.space
  grid.down <- gridpoints
  grid.down$y <-grid.down$y - y.space
  grid.right <- gridpoints
  grid.right$x <- grid.right$x + x.space
  grid.left <- gridpoints
  grid.left$x <- grid.left$x - x.space
  gridpoints <- rbind(gridpoints, grid.up, grid.down, grid.left, grid.right)
  gridpoints <- unique(gridpoints)
  #Add density
  density.surface <- cbind(gridpoints, density = rep(constant, nrow(gridpoints)))
  return(density.surface)
}



#For when there 
#point <- data.frame(x = c(1,5,2,10), y = c(1,0,2,10))
#centre <- data.frame(x = c(0,5), y = c(1,1), strata = c("A","B"))
#apply(point, 1, FUN = associate.strata, centre = centre)
associate.strata <- function(point, centre){
  delta.x <- point[["x"]] - centre[["x"]]
  delta.y <- point[["y"]] - centre[["y"]]
  sq.dx <- delta.x^2
  sq.dy <- delta.y^2
  dists <- sqrt(sq.dx+sq.dy)
  min.dist <- min(dists)
  strat <- as.character(centre$strata[which(dists == min.dist)])
  return(strat)
}




