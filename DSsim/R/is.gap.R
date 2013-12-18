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
      print(poly)
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
