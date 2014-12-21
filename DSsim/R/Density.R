#' @include generic.functions.R
NULL

#' Class "Density" 
#' 
#' Class \code{"Density"} is an S4 class containing a list of grids which
#' describe the density of individuals / clusters of a population. The list
#' contains one grid (\code{data.frame}) for each strata.
#'
#' @name Density-class
#' @docType class
#' @section Slots: 
#' \describe{
#'  \item{\code{region.name}}{Object of class \code{"character"}; the region
#'  name.}
#'  \item{\code{strata.name}}{Object of class \code{"character"}; the strata
#'  names}
#'  \item{\code{density.surface}}{Object of class \code{"list"}; list of 
#'  data.frames with the columns x, y and density. There must be one 
#'  data.frame for each strata.}
#'  \item{\code{x.space}}{Object of class \code{"numeric"}; The spacing 
#'  between gridpoints described in the density data.frames in the 
#'  x-direction.}
#'  \item{\code{y.space}}{Object of class \code{"numeric"}; The spacing 
#'  between gridpoints described in the density data.frames in the 
#'  y-direction.}
#'  \item{\code{units}}{Object of class \code{"numeric"}; The units of the
#'  grid points.}
#' }
#' @section Methods:
#' \describe{
#'  \item{\code{add.hotspot}}{\code{signature=(object = "Density")}: adds a hotspot based on a gaussian decay to the density
#'  surfaces.}
#' }
#' @keywords classes
#' @seealso \code{\link{make.density}}
#' @export
setClass("Density", representation(region.name = "character", strata.name = "character", density.surface = "list", x.space = "numeric", y.space = "numeric", units = "character"))

setMethod(
  f="initialize",
  signature="Density",
  definition=function(.Object, region, strata.name = character(0), density.surface = list(), x.space, y.space, constant = NULL, density.gam = NULL, jit = 1){
    #Input pre-processing
    if(length(density.surface) == 0){
      if(!is.null(constant)){
        #Create density surface with constant density within strata
        density.surface <- get.surface.constant(region, x.space, y.space, constant, jit)
      }else if(!is.null(density.gam)){
        #Create density surface from gam
        density.surface <- get.surface.gam(region, x.space, y.space, gam.model = density.gam)
      }else{
        density.surface <- list(data.frame(x = NULL, y = NULL, density = NULL))    
      }
    }
    #Set slots
    .Object@region.name <- region@region.name
    .Object@strata.name <- strata.name
    .Object@density.surface <- density.surface
    .Object@x.space <- x.space
    .Object@y.space <- y.space
    .Object@units <- region@units
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
    #check the density grid was created without problem
    some.strata.with.grids <- FALSE
    some.strata.with.no.grids <- FALSE
    for(i in seq(along = object@density.surface)){
      density.sum <- sum(object@density.surface[[i]]$density)
      #check there are some cells with non-zero density
      if(density.sum == 0){
        return("All strata must have some cells with non-zero density. Check that you have correctly specified your density grid. Large grid spacing may also generate this error.") 
      }
      if(nrow(object@density.surface[[1]]) > 0){  
        some.strata.with.grids <- TRUE
      }else{
        some.strata.with.no.grids <- TRUE  
      }  
    }
    if(some.strata.with.grids & some.strata.with.no.grids){
      return("The grid spacing needs to be smaller, not all strata have points in them")
    }else if(!some.strata.with.grids){
      return("There has been a problem generating the density grid. You must supply either a valid density surface, constant or valid density gam argument. DSM and formula are not currently suported")
    }
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

# @rdname add.hotspot-methods
# @aliases add.hotspot,Density-method
setMethod("add.hotspot","Density",
          function(object, centre, sigma, amplitude){
            density.surface <- object@density.surface    
            for(strat in seq(along = density.surface)){
              #Find distances from centre to each point on the density surface
              strata.surface <- density.surface[[strat]]
              dists <- sqrt((strata.surface$x-centre[1])^2 + (strata.surface$y-centre[2])^2) 
              #Calculate radial decay
              additive.values <- (exp(-dists^2/(2*sigma^2)))*amplitude
              #Add to surface
              strata.surface$density <- strata.surface$density+additive.values
              density.surface[[strat]] <- strata.surface
            }
            object@density.surface <- density.surface
            return(object)         
          }
)



#' @rdname Density-class
#' @aliases plot,Density-method
setMethod("plot","Density",
  function(x, y, add = FALSE, plot.units = character(0), ...){
    density.surface <- x@density.surface
    #Get all the x, y and density values across strata
    densities <- x.vals <- y.vals <- NULL
    for(strat in seq(along = density.surface)){
      densities <- c(densities, density.surface[[strat]]$density)
      x.vals <- c(x.vals, density.surface[[strat]]$x)
      y.vals <- c(y.vals, density.surface[[strat]]$y)
    }
    #Find the range of densities
    zlim <- range(densities) 
    #If the range < 1
    if(zlim[2] - zlim[1] < 1){
      #Multiply them by (1/minimum density)*10
      multiplier <- (1/zlim[1])*10
    }else{
      #otherwise no scaling
      multiplier <- 1
    }
    #Create the colour lookup
    zlim <- range(densities*multiplier)
    zlen <- zlim[2] - zlim[1] + 1
    colorlut <- heat.colors(zlen) 
    colorlut <- colorlut[length(colorlut):1]
    #Set up plot
    if(length(plot.units) == 0){
      plot.units <- x@units
    }
    if(!add){
      xlabel <- paste("X-coords (",plot.units[1],")", sep = "")
      ylabel <- paste("Y-coords (",plot.units[1],")", sep = "")
      plot(range(x.vals), range(y.vals), col = "white", xlab = xlabel, ylab = ylabel, main = x@region.name, yaxt = "n", xaxt = "n", ...)
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
          message("The requested conversion of units is not currently supported.")
        }
      }else{
        #no unit conversion needed
        axis(1, at = xticks, labels = xticks)
        axis(2, at = yticks, labels = yticks)
      }
    }
    #Add the points for each strata
    for(strat in seq(along = density.surface)){
      col <- colorlut[density.surface[[strat]]$density*multiplier-zlim[1]+1]
      points(density.surface[[strat]]$x, density.surface[[strat]]$y, col = col, pch = 20)
    }      
  }
)










