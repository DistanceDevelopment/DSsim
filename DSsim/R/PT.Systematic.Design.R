#' @include PT.Design.R

setClass(Class = "PT.Systematic.Design", 
         contains = "PT.Design"
)

setMethod(
  f="initialize",
  signature="PT.Systematic.Design",
  definition=function(.Object, region, spacing, design.axis, plus.sampling, path = character(0), ...){
    filenames <- character(0)
    file.index <- numeric(0)
    if(length(path) > 0){
      filenames  <- get.shapefile.names(path)
      file.index <- 1
    }
    #Set slots
    .Object@region.obj    <- region
    .Object@plus.sampling <- plus.sampling
    .Object@spacing       <- spacing
    .Object@design.axis   <- design.axis
    .Object@path          <- path
    .Object@filenames     <- filenames
    .Object@file.index    <- file.index
    #Check object is valid
    validObject(.Object)
    # return object
    return(.Object)
  }
)

setValidity("PT.Systematic.Design",
            function(object){
              if(length(object@path) > 1){
                return("You must only specify one path. All transect shapefiles must be in the same folder.")
              }
              if(any(ifelse(object@design.axis != 0, TRUE, FALSE))){
                if(length(object@path) == 0){
                  # warning only relevant if DSsim is generating the transects
                  warning("Only a design axis of 0 is currently implemented, other values will be ignored at present.", call. = FALSE, immediate. = TRUE)
                }
              }
              #Check that design axes are both 0
              return(TRUE)
            }
)

# GENERIC METHODS DEFINITIONS --------------------------------------------

#' @rdname generate.transects-methods
#' @param silent if TRUE does not report warnings about a single value for nested spacing with a multi strata region
#' @importFrom utils read.table
#' @importFrom fields cover.design
#' @export
setMethod(
  f="generate.transects",
  signature="PT.Systematic.Design",
  definition=function(object, region = NULL, index = NULL, silent = FALSE){
    if(is.null(region) | class(region) != "Region"){
      region <- object@region.obj
      region <- get(region, pos = 1)
      warning("Obtaining region object from the global environment.", call. = FALSE, immediate. = TRUE)
    }
    # Decide whether to read from file or generate survey
    if(length(object@path) == 0){
      read.from.file = FALSE
    }else if(length(object@path) > 0){
      read.from.file = TRUE
    }
    #Input pre-processing
    if(read.from.file){
      #Go to parent method to read from file
      point.transect <- callNextMethod()
      return(point.transect)
    }else{
      if(length(region@strata.name) > 0){
        strata.names <- region@strata.name
        strata.no <- length(region@strata.name)
      }else{
        strata.names <- region@region.name
        strata.no <- 1
      }
      #Main grid generation
      for (strat in seq(along = region@coords)) {
        spacing <- object@spacing[strat]
        start.x <- region@box[["xmin"]] + runif(1, 0, spacing)
        start.y <- region@box[["ymin"]] + runif(1, 0, spacing)
        x.vals <- seq(start.x, region@box[["xmax"]], by = spacing)
        y.vals <- seq(start.y, region@box[["ymax"]], by = spacing)
        temp.coords <- expand.grid(x.vals, y.vals)
        names(temp.coords) <- c("x","y")
        #keep everything within the polygon strata
        to.keep <-
          in.polygons(region@coords[[strat]], pts = temp.coords, boundary = TRUE)
        gridpoints <- temp.coords[to.keep, ]
        #Discard anything that lands in a gap
        to.discard <-
          in.polygons(region@gaps[[strat]], pts = gridpoints, boundary = TRUE)
        gridpoints <- gridpoints[!to.discard,]
        #Add strata ID
        gridpoints$strata <- rep(strata.names[strat], nrow(gridpoints))
        #nested.gridpoints$ac.simple <- rep(FALSE, nrow(nested.gridpoints))
        
        all.gridpoints <- gridpoints
        
        if (strat == 1) {
          transects <- all.gridpoints
        } else{
          transects <- rbind(transects, all.gridpoints)
        }
      }
    }
    #Add effort and rename
    sampler.info <- data.frame(ID = 1:nrow(transects), X = transects$x, Y = transects$y, region = transects$strata, effort = rep(1, nrow(transects)))
    point.transect <- new(Class = "Point.Transect", region = region, sampler.info = sampler.info)
    return(point.transect)
  }
)




