#' @include LT.Design.R

setClass(Class = "LT.EqSpace.ZZ.Design", 
         contains = "LT.Design"
) 


# GENERIC METHODS DEFINITIONS --------------------------------------------

#' @rdname generate.transects-methods
#' @importFrom utils read.table
#' @importFrom stats runif
#' @importFrom sp Line Lines SpatialLines Polygon Polygons SpatialPolygons
#' @param complement logical indicating whether two sets of complimentary transects should be generated
#' @export
setMethod(
  f="generate.transects",
  signature="LT.EqSpace.ZZ.Design",
  definition=function(object, region = NULL, index = NULL, silent = FALSE, complement = FALSE){
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
      # Check that the number of spacing parameters is correct
      if(length(object@spacing) != strata.no){
        if(length(object@spacing) > 1){
          warning("Number of spacing values not equal to number of strata. Using first value only.", immediate. = TRUE, call. = FALSE)  
        }
        object@spacing <- rep(object@spacing[1], strata.no)
      }
      # Check that the number of design.axis parameters is correct
      if(length(object@design.axis) != strata.no){
        if(length(object@design.axis) > 1){
          warning("Number of spacing values not equal to number of strata. Using first value only.", immediate. = TRUE, call. = FALSE)  
        }
        object@design.axis <- rep(object@design.axis[1], strata.no)
      }
      # Store sampler info's for all strata
      sampler.info.list <- list()
      # For each strata
      for(strat in seq(along = region@coords)){
        # Storage vectors
        ID <- start.X <- start.Y <- end.X <- end.Y <- tot.length <- numeric(0)
        #Get design values for current strata
        spacing <- object@spacing[strat]
        angle.deg <- object@design.axis[strat]
        coords.names <- names(region@coords[[strat]][[1]])
        #First rotate the strata coords to lie along x axis
        design.axis.rad <- angle.deg/180*pi
        coords <- lapply(region@coords[[strat]], as.matrix)
        coords.t <- lapply(coords, t)
        #Calculate rotation angle theta
        theta <- ifelse(design.axis.rad == 0, 0, 2*pi-design.axis.rad)
        rot.mat <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), ncol = 2, byrow = FALSE)
        #Rotate coordinates
        rot.coords <- lapply(coords.t, function(coords){rot.mat %*% coords})
        rot.coords <- lapply(rot.coords, t)
        #Rotate gaps if there are any
        rot.gaps <- list()
        if(length(region@gaps[[strat]]) > 0){
          gaps <- lapply(region@gaps[[strat]], as.matrix)
          gaps.t <- lapply(gaps, t)
          rot.gaps <- lapply(gaps.t, function(coords){rot.mat %*% coords})
          rot.gaps <- lapply(rot.gaps, t)
        }
        #Find bounding box
        x.coords <- lapply(rot.coords, function(coords){coords[,1]})
        y.coords <- lapply(rot.coords, function(coords){coords[,2]})
        x.lim <- range(unlist(x.coords))
        y.lim <- range(unlist(y.coords))
        #Generate random start on x axis
        start.space <- runif(1, 0, spacing)
        x.coords <- seq(x.lim[1]-start.space, x.lim[2]+spacing, by = spacing)
        #Randomise zig or zag at start 
        random.start <- runif(1,0,1)
        if(random.start <=0.5){ random.start <- c(1,2)
        }else{ random.start <- c(2,1)}
        #Generate lines
        sp.lines <- list()
        if(complement){
          temp.ID <- 1
          for(i in 1:(length(x.coords)-1)){
            coords <- matrix(c(x.coords[i], x.coords[i+1], y.lim[random.start[1]], y.lim[random.start[2]]), nrow = 2)
            next.line <- Line(coords)
            sp.lines[[temp.ID]] <- Lines(list(next.line), ID = temp.ID)
            temp.ID <- temp.ID + 1
            coords <- matrix(c(x.coords[i], x.coords[i+1], y.lim[random.start[2]], y.lim[random.start[1]]), nrow = 2)
            next.line <- Line(coords)
            sp.lines[[temp.ID]] <- Lines(list(next.line), ID = temp.ID)
            temp.ID <- temp.ID + 1
          }  
        }else{
          odd <- TRUE
          for(i in 1:(length(x.coords)-1)){
            if(odd){
              coords <- matrix(c(x.coords[i], x.coords[i+1], y.lim[random.start[1]], y.lim[random.start[2]]), nrow = 2)  
            }else{
              coords <- matrix(c(x.coords[i], x.coords[i+1], y.lim[random.start[2]], y.lim[random.start[1]]), nrow = 2)
            }
            next.line <- Line(coords)
            sp.lines[[i]] <- Lines(list(next.line), ID = i)
            odd <- ifelse(odd, FALSE, TRUE)  
          }
        }
        spat.lines <- SpatialLines(sp.lines)
        #Now turn strata into sp polygons
        temp.list.poly <- list()
        # Deal with the outer polygons
        for(poly in seq(along = rot.coords)){
          current.poly <- data.frame(x = rot.coords[[poly]][,1], y = rot.coords[[poly]][,2])  
          temp <- Polygon(current.poly, hole = FALSE)
          temp.list.poly[[poly]] <- temp
        }
        #Deal with gaps
        for(gap in seq(along = rot.gaps)){
          current.gap <- data.frame(x = rot.gaps[[poly]][,1], y = rot.gaps[[poly]][,2])  
          temp <- Polygon(current.gap, hole = TRUE)
          temp.list.poly[[(poly+gap)]] <- temp
        }
        # Make into correct format for rgeos
        polys.rgeos <- Polygons(temp.list.poly, ID = strata.names[strat])
        region.coords <- SpatialPolygons(list(polys.rgeos))
        intersection <- rgeos::gIntersection(spat.lines, region.coords, byid = TRUE)
        # Extract coords and other sampler info
        clipped.lines <- intersection@lines
        transect.ID <- 1
        region.name <- character(0)
        for(i in seq(along = clipped.lines)){
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
        sampler.info <- data.frame(ID = ID, 
                                   start.X = start.X, 
                                   start.Y = start.Y, 
                                   end.X = end.X, 
                                   end.Y = end.Y, 
                                   length = tot.length, 
                                   region = region.name, 
                                   d7.length = rep(NA, length(ID)))
        #Now need to back rotate the sampler coords
        reverse.theta <- design.axis.rad
        rot.mat.rev <- matrix(c(cos(reverse.theta), sin(reverse.theta), -sin(reverse.theta), cos(reverse.theta)), ncol = 2, byrow = FALSE)
        rot.start.points <- matrix(c(sampler.info$start.X, sampler.info$start.Y), ncol = 2)
        rot.end.points <- matrix(c(sampler.info$end.X, sampler.info$end.Y), ncol = 2)
        start.points <- rot.mat.rev %*% t(rot.start.points)
        end.points <- rot.mat.rev %*% t(rot.end.points)
        start.points <- t(start.points)
        end.points <- t(end.points)
        sampler.info[,c("start.X","start.Y")] <- start.points
        sampler.info[,c("end.X","end.Y")] <- end.points
        sampler.info.list[[strat]] <- sampler.info
      }#do next strata
      #Combine data.frames
      sampler.info <- sampler.info.list[[1]]
      if(length(sampler.info.list) > 1){
        for(i in 2:length(sampler.info.list)){
          sampler.info <- rbind(sampler.info, sampler.info.list[[i]])  
        }
      }
      # Create Line.Transect object
      line.transect <- new(Class = "Line.Transect", region = region, sampler.info = sampler.info)
      # Return object
      return(line.transect)
    }
  }
)

