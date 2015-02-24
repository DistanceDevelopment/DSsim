add.dist.error <- function(no.files, path, cv = NULL, distribution, beta.params = NULL){
  for(i in 1:no.files){
    load(paste(path,"dataset_",i,".robj", sep = ""))
    ddf.data <- dist.data$ddf@ddf.dat
    #Add normally distributed errors
    if(distribution == "Normal"){
      error <- rnorm(nrow(ddf.data), mean = 0, sd = cv*ddf.data$distance)
      new.distances <- ddf.data$distance + error
      if(range(new.distances)[1] < 0){
        warning("Distances less than 0 have been generatesd. The absolute value of these negative values will be used therefore the errors are no longer Normally-distributed.", call. = FALSE, immediate. = TRUE)
      }
      new.distances <- abs(new.distances)
    #Add Log-Normal errors
    }else if(distribution == "Log-Normal"){
      #Generate the new values from a log normal distribution
      #With log(mean) = log(original value)
      new.distances <- rep(NA, nrow(ddf.data))
      for(j in seq(along = new.distances)){
        new.distances[j] <- rlnorm(1, log(ddf.data$distance[j])-0.5*log(1+cv^2), sqrt(log(1+cv^2)))  
      }
    }else if(distribution == "Beta"){
      R <- 0.5 + rbeta(nrow(ddf.data), beta.params[1], beta.params[2])
      new.distances <- ddf.data$distance*R
    }
    ddf.data$distance <- new.distances
    dist.data$ddf@ddf.dat <- ddf.data
    save(dist.data, file = paste(path,"dataset_",i,".robj", sep = ""))
  }
}

