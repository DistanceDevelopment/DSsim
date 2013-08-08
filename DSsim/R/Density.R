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
setClass("Density", representation(region.name = "character", strata.name = "character", density.surface = "list", x.space = "numeric", y.space = "numeric", used = "logical"))


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
        message("Error: You must supply either the density surface, constant or density.gam argument. DSM and formula not currently supported.")
        return(NULL)      
      }
    }
    #Set slots
    .Object@region.name <- region@region.name
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
  function(x, y, add = FALSE, plot.units = character(0), ...){
    density.surface <- x@density.surface
    densities <- NULL
    x.vals <- NULL
    y.vals <- NULL
    for(strat in seq(along = density.surface)){
      densities <- c(densities, density.surface[[strat]]$density)
      x.vals <- c(x.vals, density.surface[[strat]]$x)
      y.vals <- c(y.vals, density.surface[[strat]]$y)
    }
    zlim <- range(densities) 
    if(zlim[2] - zlim[1] < 1){
      multiplier <- (1/zlim[1])*10
    }else{
      multiplier <- 1
    }
    zlim <- range(densities*multiplier)
    zlen <- zlim[2] - zlim[1] + 1
    colorlut <- heat.colors(zlen) 
    colorlut <- colorlut[length(colorlut):1]
    #Set up plot
    if(length(plot.units) == 0){
      plot.units <- region@units
    }
    if(!add){
      xlabel <- paste("X-coords (",plot.units[1],")", sep = "")
      ylabel <- paste("Y-coords (",plot.units[1],")", sep = "")
      plot(range(x.vals), range(y.vals), col = "white", xlab = xlabel, ylab = ylabel, main = x@region.name, yaxt = "n", xaxt = "n", ...)
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
    for(strat in seq(along = density.surface)){
      col <- colorlut[density.surface[[strat]]$density*multiplier-zlim[1]+1]
      points(density.surface[[strat]]$x, density.surface[[strat]]$y, col = col, pch = 20)
    }      
  }
)


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


################################################################################
# ASSOCIATED METHODS
################################################################################
#get.surface.constant <- function(region, x.space, y.space, constant, jit){
#  make.id.col <- function(matrix.row){
#    x <- matrix.row[1]
#    y <- matrix.row[2]
#    x.y <- paste(x,y, sep=",") 
#    return(x.y)
#  }
#  find.closest.strata <- function(point, grid, x.space, y.space){
#    point.left <- which(grid$x == point[["x"]] - x.space & grid$y == point[["y"]])
#    point.right <- which(grid$x == point[["x"]] + x.space & grid$y == point[["y"]])
#    point.down <- which(grid$x == point[["x"]] & grid$y == point[["y"]] - y.space)
#    point.up <- which(grid$x == point[["x"]] & grid$y == point[["y"]] + y.space)
#    point.left.up <- which(grid$x == point[["x"]] - x.space & grid$y == point[["y"]] + y.space)
#    point.right.up <- which(grid$x == point[["x"]] + x.space & grid$y == point[["y"]] + y.space)
#    point.left.down <- which(grid$x == point[["x"]] - x.space & grid$y == point[["y"]] - y.space)
#    point.right.down <- which(grid$x == point[["x"]] + x.space & grid$y == point[["y"]] - y.space)
#    strata <- c(grid[point.left,"strata.id"], grid[point.right,"strata.id"], grid[point.up,"strata.id"], grid[point.down,"strata.id"], grid[point.left.up,"strata.id"], grid[point.right.up,"strata.id"], grid[point.left.down,"strata.id"], grid[point.right.down,"strata.id"])
#    table.strata <- table(strata)
#    index <- which(table.strata == max(table.strata))
#    strata.id <- names(table.strata)[index]
#    if(length(strata.id) > 1){
#      strata.id <- strata.id[1]
#    }
#    return(strata.id)
#  }
#  
#  #region <- get(region.name)
#  #Create a rectangular grid over the entire region
#  no.x.ints <- ceiling((region@box[["xmax"]]-region@box[["xmin"]])/x.space)
#  no.y.ints <- ceiling((region@box[["ymax"]]-region@box[["ymin"]])/y.space)
#  x.adj <- (x.space*no.x.ints - (region@box[["xmax"]]-region@box[["xmin"]]))/2
#  y.adj <- (y.space*no.y.ints - (region@box[["ymax"]]-region@box[["ymin"]]))/2
#  x.vals <- seq(region@box[["xmin"]]-x.adj, region@box[["xmax"]]+x.adj, by = x.space)
#  y.vals <- seq(region@box[["ymin"]]-y.adj, region@box[["ymax"]]+y.adj, by = y.space)
#  temp.coords <- expand.grid(x.vals, y.vals)
#  names(temp.coords) <- names(region@coords[[1]][[1]])
#  #Find which grid points fall withing each strata.
#  to.keep <- lapply(region@coords, FUN = in.polygons, pts = temp.coords, boundary = TRUE)
#  #get strata ids and turn list into a
#  strata.id <- rep(NA, nrow(temp.coords))
#  for(i in seq(along = to.keep)){
#    strata.id <- ifelse(to.keep[[i]], i, strata.id)
#    if(i == 1){
#      to.keep.temp <- to.keep[[1]] 
#    }else{ 
#      to.keep.temp <- cbind(to.keep.temp, to.keep[[i]])
#    }
#  }
#  #check that points only occur in one strata
#  to.keep <- to.keep.temp
#  to.keep.check <- apply(to.keep, 1, FUN = sum)
#  if(length(which(to.keep.check > 1)) > 0){
#    message("Error: the density grid could not be completed there are overlapping polygons between strata.")
#    return(NULL)
#  }     
#  #Turn to.keep into one logical vector
#  to.keep <- ifelse(to.keep.check == 1, TRUE, FALSE)
#  #Extract the gridpoint coordinates that fall in one of the strata  
#  gridpoints <- cbind(temp.coords[to.keep,], strata.id = strata.id[to.keep]) 
#  to.discard <- lapply(region@gaps, FUN = in.polygons, pts = gridpoints[,1:2], boundary = FALSE)
#  #to.discard <- in.polygons(gridpoints, region@gaps, boundary = FALSE)
#  for(i in seq(along = to.discard)){
#    if(i == 1){
#      to.discard.temp <- to.discard[[1]] 
#    }else{ 
#      to.discard.temp <- cbind(to.discard.temp, to.discard[[i]])
#    }
#  }
#  to.discard <- to.discard.temp
#  to.discard <- apply(to.discard, 1, FUN = sum)
#  gridpoints <- gridpoints[to.discard == 0,]
#  
#  #temp.coords <- lapply(region@coords, FUN = as.matrix)
#  #gridpoints <- lapply(temp.coords, FUN = gridpts, xs = x.space, ys = y.space)  
#  #gridpoints <- gridpts(poly = as.matrix(region@coords), xs = x.space, ys = y.space)
#  #gridpoints <- as.data.frame(gridpoints)
#  #names(gridpoints) <- names(region@coords)
#  
#  #Create a buffer region                                                       #TRY EXPANDING THE AREA
#  grid.up <- gridpoints
#  grid.up$y <- grid.up$y + y.space
#  grid.down <- gridpoints
#  grid.down$y <-grid.down$y - y.space
#  grid.right <- gridpoints
#  grid.right$x <- grid.right$x + x.space
#  grid.left <- gridpoints
#  grid.left$x <- grid.left$x - x.space
#  gridpoints.new <- rbind(gridpoints, grid.up, grid.down, grid.left, grid.right)
#  gridpoints.new <- gridpoints.new[,1:2]
#  gridpoints.new <- unique(gridpoints.new)
#  if(!length(which(names(gridpoints)[1:2] == names(gridpoints.new)[1:2])) == 2){
#    message("Error: there is a mismatch with dataframe names.")
#  }
#  gridpoints <- cbind(id = apply(as.matrix(gridpoints), 1, FUN = make.id.col), gridpoints)
#  gridpoints.new <- cbind(id = apply(as.matrix(gridpoints.new), 1, FUN = make.id.col), gridpoints.new)
#  grid.temp <- merge(gridpoints[,c("id","strata.id")], gridpoints.new, by = "id", all = TRUE)
#  new.gridpoints <- rbind(grid.up, grid.down, grid.left, grid.right)
#  new.gridpoints <- cbind(id = apply(as.matrix(new.gridpoints), 1, FUN = make.id.col), new.gridpoints)
#  
#  outside.strata <- grid.temp[is.na(grid.temp$strata.id),]
#  inside.strata <- grid.temp[!is.na(grid.temp$strata.id),]
#  
#  coord.names <- names(region@coords[[1]][[1]])
#  
#  outside.strata.ids <- apply(as.matrix(outside.strata[,coord.names]),1 ,FUN = find.closest.strata, grid = inside.strata[,c(coord.names, "strata.id")], x.space = x.space, y.space = y.space)
#  outside.strata$strata.id <- outside.strata.ids
#
#  gridpoints <- rbind(inside.strata, outside.strata)[, c(coord.names, "strata.id")]
#  density.value <- rep(NA, nrow(gridpoints))
#  strata.id <- unique(gridpoints$strata.id)
#
#  for(st in seq(along = strata.id)){
#    density.value <- ifelse(gridpoints$strata.id == strata.id[st], constant[names(constant) == strata.id[st]], density.value)  
#  }
#  #Add density
#  density.surface <- cbind(gridpoints, density = density.value)
#  return(density.surface)
#}
#


#For when there 
#point <- data.frame(x = c(1,5,2,10), y = c(1,0,2,10))
#centre <- data.frame(x = c(0,5), y = c(1,1), strata = c("A","B"))
#apply(point, 1, FUN = associate.strata, centre = centre)
#associate.strata <- function(point, centre){
#  delta.x <- point[["x"]] - centre[["x"]]
#  delta.y <- point[["y"]] - centre[["y"]]
#  sq.dx <- delta.x^2
#  sq.dy <- delta.y^2
#  dists <- sqrt(sq.dx+sq.dy)
#  min.dist <- min(dists)
#  strat <- as.character(centre$strata[which(dists == min.dist)])
#  return(strat)
#}




