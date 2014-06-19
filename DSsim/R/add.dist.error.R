add.dist.error <- function(no.files, path, cv){
  for(i in 1:no.files){
    load(paste(path,"dataset_",i,".robj", sep = ""))
    ddf.data <- dist.data$ddf@ddf.dat
    error <- rnorm(nrow(ddf.data), mean = 0, sd = cv*ddf.data$distance)
    new.distances <- ddf.data$distance + error
    if(range(new.distances)[1] < 0){
      message("Warning distances less than 0 have been generatesd. The absolute value of these negative values will be used and the errors are no longer be normally distributed.")
    }
    new.distances <- abs(new.distances)
    ddf.data$distance <- new.distances
    dist.data$ddf@ddf.dat <- ddf.data
    save(dist.data, file = paste(path,"dataset_",i,".robj", sep = ""))
  }
}