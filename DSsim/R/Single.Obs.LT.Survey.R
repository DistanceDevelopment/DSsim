################################################################################
# CONSTRUCT CLASS AND DEFINE INITIALIZE AND VALIDITY
################################################################################

##' Class "LT.Survey"

setClass(Class = "Single.Obs.LT.Survey",
         contains = "LT.Survey"
)

setMethod(
  f="initialize",
  signature="Single.Obs.LT.Survey",
  definition=function(.Object, population, line.transect, rad.truncation, perp.truncation){
    #Input pre-processing
    #full.data <- calculate.distances(get(population), get(survey))
    #dist.data <- simulate.detection(full.data, pop@detectability)
    #Set slots
    #.Object@distance.data <- dist.data
    #.Object@full.data     <- full.data
    .Object@population    <- population
    .Object@line.transect <- line.transect
    .Object@radial.truncation <- rad.truncation
    .Object@perpendicular.truncation <- perp.truncation
    #Check object is valid
    validObject(.Object)
    # return object
    return(.Object) 
  }
)
setValidity("Single.Obs.LT.Survey",
  function(object){
    return(TRUE)
  }
)

################################################################################
# GENERIC METHODS
################################################################################

setMethod(
  f="simulate.survey",
  signature="Single.Obs.LT.Survey",
  definition=function(object, dht.tables = TRUE, ...){
    population <- object@population
    line.transect <- object@line.transect
    poss.distances <- calc.poss.detect.dists(population, line.transect, perp.truncation = object@perpendicular.truncation, rad.truncation = object@radial.truncation)
    n.in.covered <- nrow(poss.distances)
    dist.data <- simulate.detections(poss.distances, population@detectability)
    dist.data <- rename.duplicates(dist.data)
    ddf.data.obj <- new(Class = "Single.Obs.DDF.Data", data = dist.data)
    if(dht.tables){
      region.table <- create.region.table(object, ...)
      sample.table <- create.sample.table(object)
      obs.table <- data.frame(object = dist.data$object, Sample.Label = dist.data$transect.ID)
      obs.table <- merge(obs.table, sample.table@sample.table, by = "Sample.Label")[,c("object","Sample.Label","Region.Label")] 
      obs.table.obj <- new(Class = "Obs.Table", data = obs.table)
      return(list(ddf.data = ddf.data.obj, obs.table = obs.table.obj, sample.table = sample.table, region.table = region.table, n.in.covered = n.in.covered))
    }else{
      return(list(ddf.data = ddf.data.obj, n.in.covered = n.in.covered))
    }
  }    
) 

 

################################################################################
# ASSOCIATED METHODS
################################################################################
#calculate.distances <- function(population, survey){
#  if(class(survey) == "Line.Transect"){
#    dist.data <- calculate.possible.detection.distsances(population, survey, perp.truncation, rad.truncation, )
#  }else if(class(survey) == "Point.Transect"){
#    message("not currently implemented for PT")
#  }
#  return(dist.data)
#}




#find.possible.detections <- function(coords, transects, perp.truncation, rad.truncation){
#  x.coord <- coords[["x"]]
#  y.coord <- coords[["y"]]
#  #find the angle between the transect and the vector from the animal to the start of the transect
#  transect.angle <- atan2(transects[["end.Y"]]-transects[["start.Y"]], transects[["end.X"]]-transects[["start.X"]]) 
#  animal.angle   <- atan2(y.coord-transects[["start.Y"]], x.coord-transects[["start.X"]])
#  delta.angle <- abs(animal.angle-transect.angle)
#  delta.angle <- (ifelse(delta.angle > pi, 2*pi - delta.angle, delta.angle))
#  #calculate the distance from the transect start to the animal (the hypotenuse)
#  hyp         <- sqrt((y.coord-transects[["start.Y"]])^2+(x.coord-transects[["start.X"]])^2)
#  #calculate the perpendicular distance (the opposite side of the RA triangle)
#  all.perp.dists  <- hyp*sin(delta.angle)   
#  #check to see if the perpendicular line intersects the transect or not
#  intersects.transects <- apply(cbind(transects[,c("start.X", "start.Y", "end.X", "end.Y", "length")], p.dist = all.perp.dists), 1, FUN = check.intersection, point = data.frame(x = x.coord, y = y.coord))
#  #all.perp.dists
#  #intersects.transects
#  #test to see if the intersection is further away from the transect start than the end of the transect
#  #extrap.dist <- adjac - transects$length
#  #if it is exclude these transects from comparison [NOTE still an occasional problem the adj is shorter than the transect but in the opposite direction.]
#  
#  #possible.perp.dists <- perp.dists[intersects.transects]
#  #calculate distances to transect ends
#  dist.to.start <- sqrt((transects$start.X - coords[["x"]])^2 + (transects$start.Y - coords[["y"]])^2)
#  dist.to.end <- sqrt((transects$end.X - coords[["x"]])^2 + (transects$end.Y - coords[["y"]])^2)
#  available.from.start <- ifelse(!intersects.transects & dist.to.start <= rad.truncation, TRUE, FALSE)
#  available.from.end <- ifelse(!intersects.transects & dist.to.end <= rad.truncation, TRUE, FALSE)   
#  perp.dists <- ifelse(intersects.transects & all.perp.dists < perp.truncation, TRUE, FALSE)
#  #detect.dists <- array(c(rep(coords[["object.ID"]], length(transects$ID)), transects$ID, all.perp.dists, perp.dists, dist.to.start, dist.to.end), dim = c(length(transects$ID),6), dimnames = list(1:length(transects$ID), c("object.ID", "transect.ID", "p.dist", "available.from.pdist", "available.from.rdist.to.start", "available.from.rdist.to.end")))
#  detect.dists <- data.frame(object.ID = rep(coords[["object.ID"]], length(transects$ID)), transect.ID = transects$ID, p.dist = all.perp.dists, available.from.pdist = perp.dists, available.from.rdist.to.start = available.from.start, available.from.rdist.to.end = available.from.end)
#  return(detect.dists)   
#  
#  #min.dist.to.end <- calc.dists.to.transect.ends(coords, transects)
 # if(length(possible.perp.dists) > 0){
#    min.perp.dist <- min(possible.perp.dists)
#    if(min.perp.dist <= min.dist.to.end$distance){
#      ID <- transects$ID[min.perp.dist == perp.dists]
#    }else{
#      ID <- min.dist.to.end$ID
#      min.perp.dist <- perp.dists[transects$ID == min.dist.to.end$ID][1]
#    }
#  }else{
#    min.perp.dist <- perp.dists[transects$ID == min.dist.to.end$ID][1]
#    ID <- min.dist.to.end$ID
#  }  
#  return(array(c(coords[["object.ID"]], ID, min.perp.dist), dimnames = list(c("object.ID", "transect.ID", "p.dist"))))
#}
#
#calc.dists.to.transect.ends <- function(coords, transects){
#  #find the end point of the closest transect
#  dist.to.start <- sqrt((transects$start.X - coords[["x"]])^2 + (transects$start.Y - coords[["y"]])^2)
#  dist.to.end <- sqrt((transects$end.X - coords[["x"]])^2 + (transects$end.Y - coords[["y"]])^2)
#  min.to.start <- min(dist.to.start)
#  min.to.end <- min(dist.to.end)
#  index.start <- which(min.to.start == dist.to.start)
#  index.end <- which(min.to.end == dist.to.end)
#  #if one is closer than the other return that transect and distance 
#  if(min.to.start < min.to.end){
#    transect.ID <- transects$ID[index.start]
#    return(data.frame(ID = transect.ID, distance = min.to.start))
#  }else if(min.to.start > min.to.end){
#    transect.ID <- transects$ID[index.end]
#    return(data.frame(ID = transect.ID, distance = min.to.end))
#  }else if(min.to.start == min.to.end){
#    #check the end point is the same as the start point
#    min.dist <- min.to.start
#    start.point <- c(transects$start.X[index.start], transects$start.Y[index.start])
#    end.point <- c(transects$end.X[index.end], transects$end.Y[index.end])
#    if(start.point[1] == end.point[1] & start.point[2] == end.point[2]){
#      #find the closest transect by comparing the distances from the animal to 2 points 
#      #which are each one unit along the respective transects from their common end point
#      point <- start.point
#      angle.to.transectA <- atan2(point[2]-transects[["end.Y"]][index.start], point[1]-transects[["end.X"]][index.start])
#      angle.to.transectB <- atan2(point[2]-transects[["start.Y"]][index.end], point[1]-transects[["start.X"]][index.end])
#      angle.to.animal <- atan2(point[2]-coords[["x"]], point[1]-coords[["x"]])
#      delta.angleA <- abs(angle.to.animal-angle.to.transectA)
#      delta.angleA <- (ifelse(delta.angleA > pi, 2*pi - delta.angleA, delta.angleA)) 
#      delta.angleB <- abs(angle.to.animal-angle.to.transectB)
#      delta.angleB <- (ifelse(delta.angleB > pi, 2*pi - delta.angleB, delta.angleB))
#      #c^2 = a^2 + b^2 - 2abcosC (when a = 1: c^2 = b^2 - 2bcosC)
#      dist.to.transectA <- sqrt(min.dist^2 - 2*min.dist*cos(delta.angleA))
#      dist.to.transectB <- sqrt(min.dist^2 - 2*min.dist*cos(delta.angleB))
#      index <- ifelse(dist.to.transectA < dist.to.transectB, index.start, index.end)
#      transect.ID <- transects$ID[index]
#      return(data.frame(ID = transect.ID, distance = min.dist))
#    }else{
#      #if not randomly pick one
#      transect.ID <- sample(c(transects$ID[index.start],transects$ID[index.end]), 1, replace = FALSE)
#      return(data.frame(ID = transect.ID, distance = min.dist))
#    }   
#  }
#}  






   




