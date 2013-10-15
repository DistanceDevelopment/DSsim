single.simulation.loop <- function(i, object){
# Input: i - integer representing the loop number
#        object - an object of class Simulation
#
# Output: the updated Simulation object 
#
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
    #simulate survey
    survey.data <- create.survey.results(object = survey, dht.tables = TRUE, region = object@region)
    ddf.data <- survey.data$ddf.data 
    obs.table <- survey.data$obs.table
    sample.table <- survey.data$sample.table
    region.table <- survey.data$region.table
    n.in.covered <- survey.data$n.in.covered 
    #analyse survey
    ddf.results <- run.analysis(object, ddf.data)
    object@results$Detection <- store.ddf.results(object@results$Detection, ddf.results, i, n.in.covered)
    compute.dht = TRUE
    if(compute.dht){
      dht.results <- dht(ddf.results, region.table@region.table, sample.table@sample.table, obs.table@obs.table)
      object@results <- store.dht.results(object@results, dht.results, i, object@population.description@size)                                           
    }
    object@results$filename <- object@design@filenames[object@design@file.index] 
    return(object@results)
}

