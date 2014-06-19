<<<<<<< HEAD
single.simulation.loop <- function(i, object){
# Input: i - integer representing the loop number
#        object - an object of class Simulation
#
# Output: the updated Simulation object 
#
=======
single.simulation.loop <- function(i, object, save.data = FALSE, load.data = FALSE, data.path = character(0)){
>>>>>>> Binned-Data
    #generate population
    population <- generate.population(object)
    #generate transects
    if(!object@single.transect.set){
      object@design@file.index <- i
    }
    transects <- generate.transects(object)
    #make survey object
    if(object@double.observer){
      message("Double observer simulations not supported at present")#move this to the checking of the simulation object
    }else{
      if(inherits(object@design, "LT.Design")){
        survey <- new(Class = "Single.Obs.LT.Survey", population = population, line.transect = transects, perp.truncation = object@detectability@truncation)
      }
    }
<<<<<<< HEAD
    #simulate survey
    survey.data <- create.survey.results(object = survey, dht.tables = TRUE, region = object@region)
    ddf.data <- survey.data$ddf.data 
    obs.table <- survey.data$obs.table
    sample.table <- survey.data$sample.table
    region.table <- survey.data$region.table
    n.in.covered <- survey.data$n.in.covered 
=======
    if(load.data){
      #load data
      load(paste(data.path,"dataset_",i,".robj", sep = ""))
      ddf.data <- dist.data$ddf 
      obs.table <- dist.data$obs.table
      sample.table <- dist.data$sample.table
      region.table <- dist.data$region.table
      n.in.covered <- dist.data$n.in.covered 
    }else{
      #simulate survey
      survey.data <- simulate.survey(object = survey, dht.table = TRUE, region = object@region)
      ddf.data <- survey.data$ddf.data 
      obs.table <- survey.data$obs.table
      sample.table <- survey.data$sample.table
      region.table <- survey.data$region.table
      n.in.covered <- survey.data$n.in.covered 
      if(save.data){
        dist.data <- list(ddf = ddf.data, obs.table = obs.table, sample.table = sample.table, region.table = region.table, n.in.covered = n.in.covered)  
        save(dist.data, file = paste("dataset_",i,".robj", sep = ""))
      }
    }
>>>>>>> Binned-Data
    #analyse survey
    ddf.results <- run.analysis(object, ddf.data)
    #Find how many animals were in the covered region
    if(length(object@ddf.analyses[[1]]@truncation) > 0){
      n.in.covered <- length(which(n.in.covered <= object@ddf.analyses[[1]]@truncation))  
    }else{
      n.in.covered <- length(n.in.covered)
    }
    #Store ddf results
    object@results$Detection <- store.ddf.results(object@results$Detection, ddf.results, i, n.in.covered)
    #Compute density / abundance estimates
    compute.dht = TRUE
    if(compute.dht){
      dht.results <- dht(ddf.results, region.table@region.table, sample.table@sample.table, obs.table@obs.table)
      object@results <- store.dht.results(object@results, dht.results, i, object@population.description@size)                                           
    }
    object@results$filename <- object@design@filenames[object@design@file.index] 
    return(object@results)
}

