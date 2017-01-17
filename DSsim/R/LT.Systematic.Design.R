#' @include LT.Design.R

setClass(Class = "LT.Systematic.Design", 
         contains = "LT.Design"
) 


setMethod(
  f="initialize",
  signature="LT.Systematic.Design",
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

setValidity("LT.Systematic.Design",
            function(object){
              if(length(object@path) > 1){
                return("You must only specify one path. All transect shapefiles must be in the same folder.")
              }
              if(any(ifelse(object@design.axis != 0, TRUE, FALSE)) & length(object@path) == 0){
                warning("Only a design axis of 0 is currently implemented, other values will be ignored at present.", call. = FALSE, immediate. = TRUE)
              }
              return(TRUE)
            }
)

# GENERIC METHODS DEFINITIONS --------------------------------------------

#' @rdname generate.transects-methods
#' @importFrom utils read.table
#' @importFrom stats runif
#' @importFrom sp Line Lines SpatialLines Polygon Polygons SpatialPolygons
#' @export
setMethod(
  f="generate.transects",
  signature="LT.Systematic.Design",
  definition=function(object, region = NULL, index = NULL, silent = FALSE){
    if(is.null(region) | class(region) != "Region"){
      warning(paste("Attempting to obtain region object from the global environment. Region name: ", region, sep = ""),  call. = FALSE, immediate. = TRUE)
      region <- object@region.obj
      region <- get(region, pos = 1)
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
      line.transect <- callNextMethod()
      return(line.transect)
    }else{
      # if(write.to.file){
      #   warning("Write to file not currently implemented", call. = FALSE, immediate. = TRUE)
      # }
      # Storage vectors
      ID <- start.X <- start.Y <- end.X <- end.Y <- tot.length <- numeric(0)
      region.name <- character(0)
      transect.ID <- 1
      # Get strata names
      if(length(region@strata.name) > 0){
        strata.names <- region@strata.name
        strata.no <- length(region@strata.name)
      }else{
        strata.names <- region@region.name
        strata.no <- 1
      }
      # For each strata
      for(strat in seq(along = region@coords)){   
        spacing <- object@spacing
        # Find x and y coordinate limits
        x.lim <- c(region@box[["xmin"]], region@box[["xmax"]])
        y.lim <- c(region@box[["ymin"]], region@box[["ymax"]])
        # Generate a start point between 0 and the spacing value
        start.space <- runif(1, 0, spacing)
        # Generate a sequence of x values with correct spacing till end of study region
        x.coords <- seq(start.space, x.lim[2], by = spacing)
        # Check there will be some lines
        if(length(x.coords) == 0){
          stop("There are no transects in the study region. Please check your design. ", call. = FALSE)
        }
        # Generate the lines from min to max y values for each x value
        sp.lines <- list()
        for(i in seq(along = x.coords)){
          coords <- matrix(c(x.coords[i], x.coords[i], y.lim[1], y.lim[2]), nrow = 2)
          next.line <- Line(coords)
          sp.lines[[i]] <- Lines(list(next.line), ID = i)
        }
        spat.lines <- SpatialLines(sp.lines)
        # Get the region polygons
        strat.poly <- region@coords[[strat]]
        strat.gap <- region@gaps[[strat]]
        temp.list.poly <- list()
        # Deal with the outer polygons
        for(poly in seq(along = strat.poly)){
          current.poly <- strat.poly[[poly]]
          temp <- Polygon(current.poly, hole = FALSE)
          temp.list.poly[[poly]] <- temp
        }
        # Deal with the gaps
        for(gap in seq(along = strat.gap)){
          current.poly <- strat.gap[[gap]]
          temp <- Polygon(current.poly, hole = TRUE)
          temp.list.poly[[poly+gap]] <- temp
        }
        # Make into correct format for rgeos
        polys.rgeos <- Polygons(temp.list.poly, ID = "region")
        region.coords <- SpatialPolygons(list(polys.rgeos))
        # Clip the lines to the polygon
        intersection <- rgeos::gIntersection(spat.lines, region.coords, byid = TRUE)
        # Extract coords and other sampler info
        clipped.lines <- intersection@lines
        for(i in seq(along= clipped.lines)){
          # Get the next transect
          next.line <- clipped.lines[[i]]@Lines
          # Store part lengths
          part.lengths <- numeric(0)
          # Transects may have multiple parts
          for(j in seq(along = next.line)){
            next.part <- next.line[[j]]@coords
            ID <- c(ID, transect.ID)
            region.name <- c(region.name, strata.names[strat])
            start.X <- c(start.X, next.part[,"x"][1])
            end.X <- c(end.X, next.part[,"x"][2])
            start.Y <- c(start.Y, next.part[,"y"][1])
            end.Y <- c(end.Y, next.part[,"y"][2])
            # Calculate length of line part
            part.lengths[j] <- sqrt((next.part[,"x"][2]-next.part[,"x"][1])^2 + (next.part[,"y"][2]-next.part[,"y"][1])^2)
          }
          # Add total length info
          tot.length <- c(tot.length, rep(sum(part.lengths), length(next.line)))
          # Increment transect ID counter
          transect.ID <- transect.ID + 1
        }
      }#loop over strata
      # Create samper table
      sampler.info <- data.frame(ID = ID, 
                                 start.X = start.X, 
                                 start.Y = start.Y, 
                                 end.X = end.X, 
                                 end.Y = end.Y, 
                                 length = tot.length, 
                                 region = region.name, 
                                 d7.length = rep(NA, length(ID)))
      # Create Line.Transect object
      line.transect <- new(Class = "Line.Transect", region = region, sampler.info = sampler.info)
      # Return object
      return(line.transect)
    }
  }
)

