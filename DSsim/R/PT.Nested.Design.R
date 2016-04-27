#' @include PT.Design.R

setClass(Class = "PT.Nested.Design", 
         representation = representation(nested.space = "numeric"),
         contains = "PT.Design")

setMethod(
  f="initialize",
  signature="PT.Nested.Design",
  definition=function(.Object, region, spacing, nested.space, design.axis, plus.sampling, path = character(0), ...){
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
    .Object@nested.space  <- nested.space
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

setValidity("PT.Nested.Design",
            function(object){
              if(length(object@path) > 1){
                return("You must only specify one path. All transect shapefiles must be in the same folder.")
              }
              nested.space <- object@nested.space
              if(length(nested.space) == 0){
                return("You must supply a value for nested.space. This should be an integer value.")
              }else{
                if(!as.integer(nested.space) == nested.space){
                  return("The nested space value should be an integer specifying how many point in the main grid are between each nested point.")
                }
              }
              if(any(ifelse(object@design.axis != 0, TRUE, FALSE))){
                warning("Only a design axis of 0 is currently implemented, other values will be ignored at present.", call. = FALSE, immediate. = TRUE)
              }
              #Check that design axes are both 0
              return(TRUE)
            }
)

# GENERIC METHODS DEFINITIONS --------------------------------------------

#' @rdname generate.transects-methods
#' @importFrom utils read.table
#' @export
setMethod(
  f="generate.transects",
  signature="PT.Nested.Design",
  definition=function(object, read.from.file = FALSE, write.to.file = FALSE, region = NULL, index = NULL, silent = FALSE){
    if(is.null(region) | class(region) != "Region"){
      region <- object@region.obj
      region <- get(region, pos = 1)
    }
    #Input pre-processing
    if(read.from.file){
      #Go to parent method to read from file
      callNextMethod()
    }else{
      if(length(region@strata.name) > 0){
        strata.names <- region@strata.name
        strata.no <- length(region@strata.name)
      }else{
        strata.names <- region@region.name
        strata.no <- 1
      }
      if(strata.no != length(object@nested.space)){
        object@nested.space <- rep(object@nested.space[1], strata.no)
        if(!silent){
          warning("The number of nested space values did not match the number of strata. Only the first value will be used.", call. = FALSE, immediate. = TRUE)  
        }
      }
      #Main grid generation
      for (strat in seq(along = region@coords)) {
        #strat = 1
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
        
        #Generate nested grid
        no.point.space <- object@nested.space[strat]
        nested.spacing <- no.point.space*spacing
        start.x.offset <- sample(0:(no.point.space-1), 1, prob = rep(1/(no.point.space), (no.point.space)))
        start.y.offset <- sample(0:(no.point.space-1), 1, prob = rep(1/(no.point.space), (no.point.space)))
        nested.start.x <- start.x + start.x.offset*spacing
        nested.start.y <- start.y + start.y.offset*spacing
        nested.x.vals <- seq(nested.start.x, region@box[["xmax"]], by = nested.spacing)
        nested.y.vals <- seq(nested.start.y, region@box[["ymax"]], by = nested.spacing)
        nested.temp.coords <- expand.grid(nested.x.vals, nested.y.vals)
        names(nested.temp.coords) <- c("x","y")
        #keep everything within the polygon strata
        to.keep <-
          in.polygons(region@coords[[strat]], pts = nested.temp.coords, boundary = TRUE)
        nested.gridpoints <- nested.temp.coords[to.keep, ]
        #Discard anything that lands in a gap
        to.discard <-
          in.polygons(region@gaps[[strat]], pts = nested.gridpoints, boundary = TRUE)
        nested.gridpoints <- nested.gridpoints[!to.discard,]
        #Add strata ID
        nested.gridpoints$strata <- rep(strata.names[strat], nrow(nested.gridpoints))
        
        #Add ID to gridpoints
        gridpoints <- cbind(gridpoints, ID = 1:nrow(gridpoints))
        duplicate.ID <- merge(gridpoints, nested.gridpoints, by = c("x","y"))$ID
        gridpoints <- gridpoints[!gridpoints$ID %in% duplicate.ID,]
        #Remove ID column
        ID.col <- which(names(gridpoints) == "ID")
        gridpoints <- gridpoints[,-ID.col]
        #Add in column for which type of censor
        gridpoints$ac.simple <- rep(TRUE, nrow(gridpoints))
        nested.gridpoints$ac.simple <- rep(FALSE, nrow(nested.gridpoints))
        
        all.gridpoints <- rbind(gridpoints, nested.gridpoints)
        
        if (strat == 1) {
          transects <- all.gridpoints
        } else{
          transects <- rbind(transects, all.gridpoints)
        }
      }
    }
    #Add effort and rename
    sampler.info <- data.frame(X = transects$x, Y = transects$y, strata = transects$strata, ac.simple = transects$ac.simple, effort = rep(1, nrow(transects)))
    point.transect <- new(Class = "Point.Transect", region = region, sampler.info = sampler.info)
    return(point.transect)
  }
)

 
    

