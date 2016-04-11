#' @importFrom graphics points
calc.poss.detect.dists.points <- function(population, survey, rad.truncation, plot = FALSE){
  transects <- survey@sampler.info
  individuals <- population@population
  #find all radial distances at which animals may be detected 
  for(i in seq(along = individuals$object)){
    x.coord <- individuals[i,"x"]
    y.coord <- individuals[i,"y"]
    transect.x <- transects$X
    transect.y <- transects$Y
    #calculate the radial distances from the transect to the animal (the hypotenuse)
    rad.dists <- sqrt((abs(x.coord - transect.x))^2+(abs(y.coord- transect.y))^2)         #if they are within the range of the transect
    available <- ifelse(rad.dists < rad.truncation, TRUE, FALSE)
    detect.dists <- data.frame(object = rep(individuals[i,"object"], length(transects$ID)), transect.ID = transects$ID, distance = rad.dists, available = available)
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
  return(poss.detect.dists)
}