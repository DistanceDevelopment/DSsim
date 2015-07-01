get.sampler.info <- function(shapefile, region.obj, meta = NULL){
#Extracts the transect corrdinates and other information from the survey
#shapefiles
  ID <- start.X <- start.Y <- end.X <- end.Y <- tot.length <- d7.length <- region <- NULL
  for(samp in seq(along = shapefile$shp$shp)){
    #segs <- nrow(survey.shapefile$shp$shp[[samp]]$points)/2
    segs <- shapefile$shp$shp[[samp]]$num.parts
    temp.length <- 0 
    for(seg in seq(1:segs)){
      ID      <- c(ID, shapefile$shp$shp[[samp]]$record)
      start.X <- c(start.X, shapefile$shp$shp[[samp]]$points$X[2*seg-1])
      start.Y <- c(start.Y, shapefile$shp$shp[[samp]]$points$Y[2*seg-1])
      end.X   <- c(end.X, shapefile$shp$shp[[samp]]$points$X[2*seg])
      end.Y   <- c(end.Y, shapefile$shp$shp[[samp]]$points$Y[2*seg])
      temp.length  <- temp.length + sqrt((shapefile$shp$shp[[samp]]$points$X[2*seg] - shapefile$shp$shp[[samp]]$points$X[2*seg-1])^2 + 
                                         (shapefile$shp$shp[[samp]]$points$Y[2*seg] - shapefile$shp$shp[[samp]]$points$Y[2*seg-1])^2   )
    }
    tot.length <- c(tot.length, rep(temp.length, segs)) 
    d7.length <- c(d7.length, rep(NA, segs))
    region     <- c(region, rep(region.obj@region.name, segs))
  }
  
  #xtract strata transect info from meta
  if(!is.null(meta) & length(region.obj@strata.name) > 0){
    for(i in seq(along = ID)){
      if(length(meta[,1][meta[,2] == ID[i]]) > 0){
        region[i] <- meta[,3][meta[,2] == ID[i]]
        #d7.length[i] <- meta[,4][meta[,2] == ID[i]]
      }
    }
    #This should be coded into VB at some point
    region.names <- region.obj@strata.name[as.numeric(region)]
    sampler.info <- data.frame(ID = ID, start.X = start.X, start.Y = start.Y, end.X = end.X, end.Y = end.Y, length = tot.length, region = region.names, d7.length = d7.length)
  }else{
    #Get strata names for each transect - checks that both endpoints and mid point agree
    #*** Note: in plus sampling transect ends or some points will fall outside the strata polygons
    #ONLY USED IF THERE ARE MORE THAN ONE STRATA
    if(length(region.obj@strata.name) > 0){             
      start.point.coords <- data.frame(x = start.X, y = start.Y)
      end.point.coords <- data.frame(x = end.X, y = end.Y)
      mid.point.coords <- data.frame(x = (end.X + start.X)/2, y = (end.Y + start.Y)/2)
      strata.start <- lapply(region.obj@coords, FUN = in.polygons, pts = start.point.coords, boundary = TRUE) 
      strata.end <- lapply(region.obj@coords, FUN = in.polygons, pts = end.point.coords, boundary = TRUE) 
      strata.mid <- lapply(region.obj@coords, FUN = in.polygons, pts = mid.point.coords, boundary = TRUE) 
      strata.id <- rep(NA, nrow(start.point.coords))
      for(strat in seq(along = strata.start)){
        strata.temp <- cbind(start = strata.start[[strat]], end = strata.end[[strat]], mid = strata.mid[[strat]])
        strata.temp <- apply(strata.temp, 1, sum) 
        strata.id <- ifelse(strata.temp == 3, strat, strata.id)   
      }
      if(length(which(is.na(strata.id))) > 0){
       warning("Transect cannot be allocated to strata debug get.sampler.info (possible that part of a transect falls outwith study region)", call. = FALSE, immediate. = TRUE)
      return(NULL)
      }
      sampler.info <- data.frame(ID = ID, start.X = start.X, start.Y = start.Y, end.X = end.X, end.Y = end.Y, length = tot.length, region = region.obj@strata.name[strata.id], d7.length = d7.length)
    }else{
      #If there is only one strata all transect must be in that strata
      sampler.info <- data.frame(ID = ID, start.X = start.X, start.Y = start.Y, end.X = end.X, end.Y = end.Y, length = tot.length, region = region, d7.length = d7.length)
    }
  }
  return(sampler.info)
}