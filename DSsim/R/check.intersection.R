#' @importFrom graphics lines
check.intersection.TP <- function(transect, point, display.diagnostics = FALSE){
  #find the gradient of the transect m=deltaY/deltaX
  transect.m <- (transect[["end.Y"]]-transect[["start.Y"]])/(transect[["end.X"]]-transect[["start.X"]]) 
  #If can't be calculated (e.g. start and end points the same)
  if(is.nan(transect.m)){
    return(FALSE)
  }
  if(transect[1] == transect[3] && transect[2] == transect[4]){
    return(FALSE) #start and end points are the same
  }
  #special cases when the transect is horizontal or vertical!
  if(transect.m == 0){
    if(transect[["start.X"]] < transect[["end.X"]]){
      intersects.with.transect <- ifelse((point[["x"]] >= transect[["start.X"]] & point[["x"]] <= transect[["end.X"]]), TRUE, FALSE)
      return(intersects.with.transect)
    }else if(transect[["start.X"]] > transect[["end.X"]]){
      intersects.with.transect <- ifelse((point[["x"]] <= transect[["start.X"]] & point[["x"]] >= transect[["end.X"]]), TRUE, FALSE)
      return(intersects.with.transect)
    }
  }else if(transect.m == Inf || transect.m == -Inf){
    if(transect[["start.Y"]] < transect[["end.Y"]]){
      intersects.with.transect <- ifelse((point[["y"]] >= transect[["start.Y"]] & point[["y"]] <= transect[["end.Y"]]), TRUE, FALSE)
      return(intersects.with.transect)
    }else if(transect[["start.Y"]] > transect[["end.Y"]]){
      intersects.with.transect <- ifelse((point[["y"]] <= transect[["start.Y"]] & point[["y"]] >= transect[["end.Y"]]), TRUE, FALSE)
      return(intersects.with.transect)
    }
  }
  #the gradient of the second line is perpendicular to the transect
  perpendicular.m <- -1/transect.m
  #calculate the angle the triangle has at the point of intersection
  #the triangle is drawn by taking a vertical line from the point until
  #the end point has the same y value as the intersection of the perpendicular
  #line with the transect. Now draw a horizontal line to the transect to complete
  #the triangle.
  the.angle <- atan(1/perpendicular.m)
  #calculate Opp (i.e. deltaX)
  delta.X <- transect[["p.dist"]]*sin(the.angle)
  #find x-coord at point of intersection
  new.x.coord <- point[["x"]]+delta.X
  #calculate Adj? (i.e. deltaY)
  delta.Y <- transect[["p.dist"]]*cos(the.angle)
  #find y-coord at point of intersection
  new.y.coord <- point[["y"]]+delta.Y
  #check new x-coord is in the right direction
  #c = y - mx
  transect.c <- transect[["start.Y"]] - transect.m*transect[["start.X"]]
  if(abs((transect.m*new.x.coord + transect.c)-new.y.coord) > 1.0e-3 & abs((transect.m*new.x.coord + transect.c)-new.y.coord) < delta.Y/2){
    warning("error tolerance not big enough to check intersection")
  }
  #Thorougly tested this function now so removing this warning (see tests)
#   if(delta.Y/2 < 1.0e-3){
#     message("Warning very small delta.Y")
#   }
  if(abs((transect.m*new.x.coord + transect.c)-new.y.coord) > min(1.0e-3, delta.Y/2)){     
    new.x.coord <- new.x.coord <- point[["x"]]-delta.X
    new.y.coord <- new.y.coord <- point[["y"]]-delta.Y 
  }
  if(display.diagnostics){      
    lines(c(point[["x"]], new.x.coord), c(point[["y"]], new.y.coord), col = 3, lty = 1) 
    new.m <- (new.y.coord-point[["y"]])/(new.x.coord-point[["x"]])
    #warning("Gradient.check: m between points = ", round(new.m, 3), ", perpendicular m = ",round(perpendicular.m,3))   
  }
  #check to see if it  within the x-range of the transect
  if(transect[["start.X"]] < transect[["end.X"]]){
    intersects.with.transect <- ifelse((new.x.coord >= transect[["start.X"]] & new.x.coord <= transect[["end.X"]]), TRUE, FALSE)
    return(intersects.with.transect)
  }else if(transect[["start.X"]] > transect[["end.X"]]){
    intersects.with.transect <- ifelse((new.x.coord <= transect[["start.X"]] & new.x.coord >= transect[["end.X"]]), TRUE, FALSE)
    return(intersects.with.transect)
  }   
}

check.intersection.PT <- function(point, transect, display.diagnostics = FALSE){
  #find the gradient of the transect m=deltaY/deltaX
  transect.m <- (transect[["end.Y"]]-transect[["start.Y"]])/(transect[["end.X"]]-transect[["start.X"]]) 
  #If can't be calculated (e.g. start and end points the same)
  if(is.nan(transect.m)){
    return(FALSE)
  }
  if(transect[1] == transect[3] && transect[2] == transect[4]){
    return(FALSE) #start and end points are the same
  }
  #special cases when the transect is horizontal or vertical!
  if(transect.m == 0){
    if(transect[["start.X"]] < transect[["end.X"]]){
      intersects.with.transect <- ifelse((point[["x"]] >= transect[["start.X"]] & point[["x"]] <= transect[["end.X"]]), TRUE, FALSE)
      return(intersects.with.transect)
    }else if(transect[["start.X"]] > transect[["end.X"]]){
      intersects.with.transect <- ifelse((point[["x"]] <= transect[["start.X"]] & point[["x"]] >= transect[["end.X"]]), TRUE, FALSE)
      return(intersects.with.transect)
    }
  }else if(transect.m == Inf || transect.m == -Inf){
    if(transect[["start.Y"]] < transect[["end.Y"]]){
      intersects.with.transect <- ifelse((point[["y"]] >= transect[["start.Y"]] & point[["y"]] <= transect[["end.Y"]]), TRUE, FALSE)
      return(intersects.with.transect)
    }else if(transect[["start.Y"]] > transect[["end.Y"]]){
      intersects.with.transect <- ifelse((point[["y"]] <= transect[["start.Y"]] & point[["y"]] >= transect[["end.Y"]]), TRUE, FALSE)
      return(intersects.with.transect)
    }
  }
  #the gradient of the second line is perpendicular to the transect
  perpendicular.m <- -1/transect.m
  #calculate the angle the triangle has at the point of intersection
  #the triangle is drawn by taking a vertical line from the point until
  #the end point has the same y value as the intersection of the perpendicular
  #line with the transect. Now draw a horizontal line to the transect to complete
  #the triangle.
  the.angle <- atan(1/perpendicular.m)
  #calculate Opp (i.e. deltaX)
  delta.X <- point[["p.dist"]]*sin(the.angle)
  #find x-coord at point of intersection
  new.x.coord <- point[["x"]]+delta.X
  #calculate Adj? (i.e. deltaY)
  delta.Y <- point[["p.dist"]]*cos(the.angle)
  #find y-coord at point of intersection
  new.y.coord <- point[["y"]]+delta.Y
  #check new x-coord is in the right direction
  #c = y - mx
  transect.c <- transect[["start.Y"]] - transect.m*transect[["start.X"]]
  if(abs((transect.m*new.x.coord + transect.c)-new.y.coord) > 1.0e-3 &  abs((transect.m*new.x.coord + transect.c)-new.y.coord) < delta.Y/2){
    warning("error tolerance not big enough to check intersection")
  }
  #Thorougly tested this function now so removing this warning (see inst folder for tests)
  #   if(delta.Y/2 < 1.0e-3){
  #     message("Warning very small delta.Y")
  #   }
  if(abs((transect.m*new.x.coord + transect.c)-new.y.coord) > min(1.0e-3, delta.Y/2)){     
    new.x.coord <- new.x.coord <- point[["x"]]-delta.X
    new.y.coord <- new.y.coord <- point[["y"]]-delta.Y 
  }
  if(display.diagnostics){      
    lines(c(point[["x"]], new.x.coord), c(point[["y"]], new.y.coord), col = 3, lty = 1) 
    new.m <- (new.y.coord-point[["y"]])/(new.x.coord-point[["x"]])
    #warning("Gradient.check: m between points = ", round(new.m, 3), ", perpendicular m = ",round(perpendicular.m,3))   
  }
  #check to see if it  within the x-range of the transect
  if(transect[["start.X"]] < transect[["end.X"]]){
    intersects.with.transect <- ifelse((new.x.coord >= transect[["start.X"]] & new.x.coord <= transect[["end.X"]]), TRUE, FALSE)
    return(intersects.with.transect)
  }else if(transect[["start.X"]] > transect[["end.X"]]){
    intersects.with.transect <- ifelse((new.x.coord <= transect[["start.X"]] & new.x.coord >= transect[["end.X"]]), TRUE, FALSE)
    return(intersects.with.transect)
  }   
}