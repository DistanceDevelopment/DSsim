get.ave.density <- function(density.surface, coords, gaps, x.space, y.space){
#Calculates the average density across the study region
  grid.cell.weighting <- function(centre, coords, gaps, x.space, y.space){
    #Get 4 corner points
    corners <- data.frame(x = (centre[["x"]]+c(x.space, x.space, -x.space, -x.space)), y = (centre[["y"]]+c(y.space, -y.space, y.space, -y.space)))
    #Check which have one or more that falls outside the strata
    in.strata <- in.polygons(poly.list = coords, pts = corners, boundary = TRUE)
    in.gaps <- in.polygons(poly.list = gaps, pts = corners, boundary = TRUE)
    if(length(which(in.strata)) == 4 & length(which(in.gaps)) == 0){
      return(1)
    }else{
      #populate with points
      x <- seq(centre[["x"]] - x.space, centre[["x"]] + x.space, length = 10) 
      y <- seq(centre[["y"]] - x.space, centre[["y"]] + x.space, length = 10)
      prop.points <- expand.grid(x = x, y = y)      
      #count what proportion are inside
      in.strata <- in.polygons(poly.list = coords, pts = prop.points, boundary = TRUE)
      in.gaps <- in.polygons(poly.list = gaps, pts = prop.points, boundary = TRUE)
      to.count <- ifelse(in.gaps, FALSE, in.strata)
      proportion.in.strata <- sum(to.count)/length(to.count)
      return(proportion.in.strata)
    }
  }
  x.space <- x.space/2
  y.space <- y.space/2  
  density.surface <- as.matrix(density.surface) 
  #takes those which are not entirely within the strata
  weighting <- apply(density.surface, 1, FUN = grid.cell.weighting, coords = coords, gaps = gaps, x.space = x.space, y.space = y.space)
  #calculate average density
  ave.density <- sum(weighting*density.surface[,"density"])/sum(weighting)
  return(ave.density)
}