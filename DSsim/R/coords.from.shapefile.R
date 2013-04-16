coords.from.shapefile <- function(shapefile){ 
  main.polygons <- list()
  gaps <- list()
  for(strat in seq(along = shapefile$shp$shp)){
    coords <- list()
    num.parts <- shapefile$shp$shp[[strat]]$num.parts
    indexes <- c(shapefile$shp$shp[[strat]]$parts, nrow(shapefile$shp$shp[[strat]]$points))
    for(p in 1:num.parts){ 
      X <- shapefile$shp$shp[[strat]]$points$X[(indexes[p]+1):indexes[p+1]]
      Y <- shapefile$shp$shp[[strat]]$points$Y[(indexes[p]+1):indexes[p+1]]
      coords[[p]] <- data.frame(x = X, y = Y)
    }
    #check to see if any are gaps
    count.gap <- 0
    count.outer.poly <- 0
    gaps.strat <- list()
    main.polygons.strat <- list()
    for(p in 1:num.parts){
      gap <- is.gap(coords[[p]], coords[-p])
      if(gap){
        count.gap <- count.gap + 1
        gaps.strat[[count.gap]] <- coords[p][[1]]        
      }else{
        count.outer.poly <- count.outer.poly + 1
        main.polygons.strat[[count.outer.poly]] <- coords[p][[1]] 
      }
    }
    gaps[[strat]] <- gaps.strat
    main.polygons[[strat]] <- main.polygons.strat
  }
  return(list(coords = main.polygons, gaps = gaps))
}
