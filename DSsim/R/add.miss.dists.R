add.miss.dists <- function(missed.dists, ddf.model){
  # ARGUMENTS
  #   missed.dists - data frame with same columns as provided to ddf
  #   ddf.model - ddf model object fitted to recorded distances
  #   obs.table - obs table to be provided to dht minus 
  # VALUE
  #   ddf object including missed.dists
  # AUTHOR: L Marshall 
  
  #Check single observer
  if(!is.null(ddf.model$mrmodel)){
    stop("Cannot currently deal with double observer models", call. = FALSE)  
  }
  
  #check data are not binned
  xmat <- ddf.model$ds$aux$ddfobj$xmat
  is.binned <- ddf.model$ds$aux$ddfobj$xmat$binned[1]
  if(is.binned){
    stop("Cannot currently deal with binned data", call. = FALSE)
  }
  
  #add new data
  ddf.data <- ddf.model$data
  new.ddf.data <- rbind(ddf.data, missed.dists)
  new.ddf.data <- new.ddf.data[order(new.ddf.data$object),]
  ddf.model$data <- new.ddf.data
  
  #add binned column and update xmat
  missed.dists$binned <- rep(is.binned, nrow(missed.dists))
  xmat.missed <- missed.dists[,names(xmat)]
  new.xmat <- rbind(xmat, xmat.missed)
  new.xmat <- new.xmat[order(new.xmat$object),]
  ddf.model$ds$aux$ddfobj$xmat <- new.xmat
  
  #calculate new fitted values
  ddf.model <- calculate.fitted(ddf.model)
  return(ddf.model)
}