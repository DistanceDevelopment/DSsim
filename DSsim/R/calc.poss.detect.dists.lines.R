#' @importFrom graphics points
calc.poss.detect.dists.lines <- function(population, survey, perp.truncation, plot = FALSE){
  transects <- survey@sampler.info
  individuals <- population@population
  #find all perpendicular distances at which animals may be detected 
  for(i in seq(along = individuals$object)){
    x.coord <- individuals[i,"x"]
    y.coord <- individuals[i,"y"]
    #find the angle between the transect and the vector from the animal to the start of the transect
    transect.angle <- atan2(transects[["end.Y"]]-transects[["start.Y"]], transects[["end.X"]]-transects[["start.X"]]) 
    animal.angle   <- atan2(y.coord-transects[["start.Y"]], x.coord-transects[["start.X"]])
    delta.angle <- abs(animal.angle-transect.angle)
    delta.angle <- (ifelse(delta.angle > pi, 2*pi - delta.angle, delta.angle))
    #calculate the distance from the transect start to the animal (the hypotenuse)
    hyp         <- sqrt((y.coord-transects[["start.Y"]])^2+(x.coord-transects[["start.X"]])^2)
    #calculate the perpendicular distance (the opposite side of the RA triangle)
    all.perp.dists  <- hyp*sin(delta.angle)   
    #check to see if the perpendicular line intersects the transect or not
    intersects.transects <- apply(cbind(transects[,c("start.X", "start.Y", "end.X", "end.Y", "length")], p.dist = all.perp.dists), 1, FUN = check.intersection.TP, point = data.frame(x = x.coord, y = y.coord), display.diagnostics = FALSE)
    #if they are within the range of the transect
    perp.dists <- ifelse(intersects.transects & all.perp.dists < perp.truncation, TRUE, FALSE)
    detect.dists <- data.frame(object = rep(individuals[i,"object"], length(transects$ID)), transect.ID = transects$ID, distance = all.perp.dists, available = perp.dists)
    #add on to larger dataframe
    if(i == 1){
      poss.detect.dists <- detect.dists
    }else{
      poss.detect.dists <- rbind(poss.detect.dists, detect.dists)
    }    
  }
  #Only keep animals that may be detected
  poss.detect.dists <- poss.detect.dists[poss.detect.dists$available,]
  #Add the x-y coords back in
  poss.detect.dists <- merge(poss.detect.dists, individuals, by="object") 
  #plot if desired 
  if(plot){ 
    transect.IDs <- sort(unique(poss.detect.dists$transect.ID))
    for(i in seq(along = transect.IDs)){
      points(poss.detect.dists$x[poss.detect.dists$transect.ID == transect.IDs[i]], poss.detect.dists$y[poss.detect.dists$transect.ID == transect.IDs[i]], col = i, pch = 20)
      points(poss.detect.dists$x[poss.detect.dists$transect.ID == transect.IDs[i]], poss.detect.dists$y[poss.detect.dists$transect.ID == transect.IDs[i]], col = i) 
    }
  }
  if(nrow(poss.detect.dists) > 0){
    index <- order(poss.detect.dists$object)
    poss.detect.dists <- poss.detect.dists[index,]
    row.names(poss.detect.dists) <- 1:nrow(poss.detect.dists)  
  }
  return(poss.detect.dists)
}