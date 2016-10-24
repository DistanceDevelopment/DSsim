#' @importFrom splancs inout as.points
in.polygons <- function(poly.list, pts, boundary){
#in.polygons checks to see if a set of points are in any one of a list of polygons
  any.true <- function(pts.check){
    true.vals <- which(pts.check)
    if(length(true.vals) > 0){
      return(TRUE)
    }else{
      return(FALSE)
    }
  }
  pts.in <- array(NA, dim = c(nrow(pts),length(poly.list)))
  for(p in seq(along = poly.list)){
    pts.in[,p] <- inout(as.points(pts), poly.list[[p]], bound = boundary)
  }
  pts.inside <- apply(pts.in, 1, FUN = any.true)
  return(pts.inside)
}
