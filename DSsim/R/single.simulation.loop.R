#' @importFrom utils flush.console
#single.simulation.loop <- function(i, object){
single.simulation.loop <- function(i, object, save.data, load.data, data.path = character(0)){
  # Input: i - integer representing the loop number
  #        object - an object of class Simulation
  #
  # Output: the updated Simulation object 
  #
  # Display to the user the progress of the simulation
  cat("\r", i, " out of ", object@reps,  " reps \r")
  flush.console()
  if(!load.data){
    #generate population
    population <- generate.population(object)
    #generate transects
    if(!object@single.transect.set){
      #Selects which set of transects to load
      object@design@file.index <- i
    }
    transects <- generate.transects(object)
    #make survey object
    if(inherits(object@design, "LT.Design")){
      survey <- new(Class = "Single.Obs.LT.Survey", population = population, line.transect = transects, perp.truncation = object@detectability@truncation)
    }else if(inherits(object@design, "PT.Design")){
      survey <- new(Class = "Single.Obs.PT.Survey", population = population, point.transect = transects, rad.truncation = object@detectability@truncation)
    }
  }
  if(load.data){
    #load data
    load(paste(data.path,"dataset_",i,".robj", sep = ""))
    cat("\r Analysing dataset: ", i, "\r")
    ddf.data <- dist.data$ddf 
    obs.table <- dist.data$obs.table
    sample.table <- dist.data$sample.table
    region.table <- dist.data$region.table
    n.in.covered <- dist.data$n.in.covered 
  }else{
    #simulate survey
    survey.data <- create.survey.results(object = survey, dht.tables = TRUE, region = object@region)
    ddf.data <- survey.data$ddf.data 
    obs.table <- survey.data$obs.table
    sample.table <- survey.data$sample.table
    region.table <- survey.data$region.table
    n.in.covered <- survey.data$n.in.covered 
    if(save.data){
      dist.data <- list(ddf = ddf.data, obs.table = obs.table, sample.table = sample.table, region.table = region.table, n.in.covered = n.in.covered)  
      save(dist.data, file = paste(data.path,"dataset_",i,".robj", sep = ""))
    }
  }
  #Find how many animals were in the covered region
  if(length(object@ddf.analyses[[1]]@truncation) > 0){
    n.in.covered <- length(which(n.in.covered <= object@ddf.analyses[[1]]@truncation))  
  }else{
    n.in.covered <- length(n.in.covered)
  }
  #analyse survey if there are data to analyse
  if(nrow(ddf.data@ddf.dat[!is.na(ddf.data@ddf.dat$distance),]) >= 20){
    ddf.results <- run.analysis(object, ddf.data)
  }else{
    warning("There are too few data points (<20) to be analysed, skipping this iteration.", call. = FALSE, immediate. = TRUE)
    ddf.results <- NULL
  }
  #Check at least one model worked
  if(!is.null(ddf.results)){
    #Store ddf results
    object@results$Detection <- store.ddf.results(object@results$Detection, ddf.results, i, n.in.covered)
    #Check to see if the stratification is to be modified for analysis
    analysis.strata <- object@ddf.analyses[[1]]@analysis.strata
    if(nrow(analysis.strata) > 0){
      new.tables <- modify.strata.for.analysis(analysis.strata, obs.table, sample.table, region.table) 
      obs.table <- new.tables$obs.table
      sample.table <- new.tables$sample.table
      region.table <- new.tables$region.table
    }
    #Check if there are missing distances
    miss.dists <- any(is.na(ddf.data@ddf.dat$distance))
    if(miss.dists){
      # Add the missing distance observations in to ddf object
      missing.dists <- ddf.data@ddf.dat[is.na(ddf.data@ddf.dat$distance),]
      # NA's break dht
      missing.dists$distance <- rep(-1, nrow(missing.dists))
      if(is.null(missing.dists$detected)){
        missing.dists$detected <- rep(1, nrow(missing.dists))  
      }
      ddf.results <- add.miss.dists(missing.dists, ddf.results)
    }
    #Compute density / abundance estimates
    compute.dht = TRUE
    if(compute.dht){
      dht.results <- dht(ddf.results, region.table@region.table, sample.table@sample.table, obs.table@obs.table)
      object@results <- store.dht.results(object@results, dht.results, i, object@population.description@size, ddf.data@ddf.dat, obs.table@obs.table)         }
  }
  object@results$filename <- object@design@filenames[object@design@file.index] 
  return(object@results)
}

