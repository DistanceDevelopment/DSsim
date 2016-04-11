get.point.sampler.info <- function(shapefile, region.obj, meta = NULL){
#Extracts the transect corrdinates and other information from the survey
#shapefiles
  ID <- X <- Y <- region <- NULL
  ID <- shapefile$shp$shp[,'record']
  X <- shapefile$shp$shp[,'x']
  Y <- shapefile$shp$shp[,'y']
  effort <- rep(1, length(ID))
  
  #If there are multiple strata
  if(length(region.obj@strata.name) > 0){
    if(!is.null(shapefile$dbf$dbf$Stratum)){
      #If there is information in the shapefile use that  
      strata.ID <- shapefile$dbf$dbf$Stratum[ID]
      strata.names <- region.obj@strata.name[strata.ID]
    }else if(!is.null(meta)){
      #Otherwise if there is information in the file meta.txt use that
      for(i in seq(along = ID)){
        if(length(meta[,1][meta[,2] == ID[i]]) > 0){
          region[i] <- meta[,3][meta[,2] == ID[i]]
        }
      }
      #This should be coded into VB at some point
      strata.names <- region.obj@strata.name[as.numeric(region)]
    }else{
      #As a last resort...
      #Get strata names for each transect - checks which strata the points are in
      #*** Note: in plus sampling transect will be outside polygons
      #ONLY USED IF THERE ARE MORE THAN ONE STRATA
      point.coords <- data.frame(x = X, y = Y)
      strata <- lapply(region.obj@coords, FUN = in.polygons, pts = point.coords, boundary = TRUE) 
      strata.id <- rep(NA, nrow(point.coords))
      for(strat in seq(along = strata)){
        strata.id <- ifelse(strata[[strat]], strat, strata.id)   
      }
      if(length(which(is.na(strata.id))) > 0){
       warning("Transect cannot be allocated to strata debug get.point.sampler.info (possible that a transect falls outwith study region)", call. = FALSE, immediate. = TRUE)
      return(NULL)
      }
      strata.names <- region.obj@strata.name[strata.id]
    }
    sampler.info <- data.frame(ID = ID, X = X, Y = Y, region = strata.names, effort = effort)
  }else{
    #If there is only one strata all transect must be in that strata
    region <- rep(region.obj@region.name, length(ID))
    sampler.info <- data.frame(ID = ID, X = X, Y = Y, region = region, effort = effort)
  }
  return(sampler.info)
}