#' Class "Simulation" 
#' 
#' Class \code{"Simulation"} is an S4 class containing descriptions of the 
#' region, population, survey design and analyses the user wishes to investigate.
#' Once the simulation has been run the N.D.Estimates will contain multiple 
#' estimates of abundance and density obtained by repeatedly generating 
#' populations, simulating the survey and completing the analyses. 
#'
#' @name Simulation-class
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{make.simulation(reps, double.observer, region.obj, design.obj, 
#' population.description.obj, detectability.obj, ddf.analyses.list)} 
#' @keywords classes
#' @export
setClass("Simulation", representation(reps = "numeric",
                                      double.observer = "logical",                                        
                                      region = "Region",
                                      design = "Survey.Design",
                                      population.description = "Population.Description",
                                      detectability = "Detectability",
                                      ddf.analyses = "list", 
                                      dsm.analysis = "DSM.Analysis",                                          
                                      ddf.param.ests = "array",
                                      results = "list")) 
                                      
setMethod(
  f="initialize",
  signature="Simulation",
  definition=function(.Object, reps, double.observer = FALSE, region, design, population.description, detectability, ddf.analyses, results){
    #Set slots
    .Object@reps            <- reps
    .Object@double.observer <- double.observer    
    .Object@region          <- region
    .Object@design          <- design
    .Object@population.description <- population.description
    .Object@detectability   <- detectability
    .Object@ddf.analyses    <- ddf.analyses
    .Object@results          <- results
    #Check object is valid
    validObject(.Object)
    # return object
    return(.Object) 
  }
)

setValidity("Simulation",
  function(object){
    if(object@double.observer){
      message("Double observer simulations not supported at present")
      return(FALSE)
    }
    design <- object@design
    transects.from.file <- ifelse(length(design@path) == 1, TRUE, FALSE)
    if(transects.from.file){
      no.files <- length(design@filenames)
      if(object@reps > no.files){
        message("You have specified a higher number of repetitions than you have provided transect shapefiles at: ", design@path)
        return(FALSE)
      }    
    }     
    return(TRUE)
  }
)

################################################################################
# ACCESSOR GENERIC METHODS
################################################################################ 
#setGeneric("get.region.name", function(object){standardGeneric ("get.region.name")})
#setGeneric("set.region.name", function(object, new.slot.value){standardGeneric ("set.region.name")})


################################################################################
# GENERIC METHODS
################################################################################


setMethod(
  f="Summary",
  signature="Simulation",
  definition=function(x, ..., na.rm = FALSE){
    cat("\nRegion: ", x@region@region.name)       
    cat("\n\nSummary Statistics")
    cat("\n\nMean values across bootstrap repetitions\n\n")
    print(x@results$summary[,,"mean"])
    cat("\nStandard deviations of values across bootstrap repetitions\n\n")
    print(x@results$summary[,,"sd"])
    cat("\n     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    cat("\nTrue N (over entire study area):", x@population.description@N, "\n")
    cat("\nN Estimates\n")
    cat("\nMean values across bootstrap repetitions\n\n")
    print(x@results$N[,,"mean"])
    cat("\nStandard deviations of values across bootstrap repetitions\n\n")
    print(x@results$N[,,"sd"])
    cat("\n     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    cat("\nTrue D (over entire study area):", x@population.description@N/sum(x@region@area), "\n")
    cat("\nD Estimates\n")
    cat("\nMean values across bootstrap repetitions\n\n")
    print(x@results$D[,,"mean"])
    cat("\nStandard deviations of values across bootstrap repetitions\n\n")
    print(x@results$D[,,"sd"])  
    cat("\n     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    cat("\n\nDetection Function Values\n")
    cat("\nMean values across bootstrap repetitions\n\n")
    print(x@results$Detection[,,"mean"])
    cat("\nStandard deviations of values across bootstrap repetitions\n\n")
    print(x@results$Detection[,,"sd"])     
    invisible(x)
  }    
)
  
setMethod(
  f="plot",
  signature="Simulation",
  definition=function(x, ...){
    message("not currently implemented")
    #hist(x@results[,1], xlab = "Estimate of N")
    #abline(v=x@population.description@N, col=2, lwd = 2)
    invisible(x)
  }    
)

setMethod(
  f="generate.population",
  signature="Simulation",
  definition=function(object, ...){
    population <- generate.population(object = object@population.description, detectability = object@detectability, region.obj = object@region)
    return(population)
  }    
)

setMethod(
  f="generate.transects",
  signature="Simulation",
  definition=function(object, read.from.file = TRUE, write.to.file = FALSE){
    region <- object@region
    transect <- generate.transects(object@design, region = region)
    return(transect)
  }    
) 

setMethod(
  f="simulate.survey",
  signature="Simulation",
  definition=function(object, dht.tables = TRUE, ...){       
    population <- generate.population(object) 
    transects  <- generate.transects(object)
    if(object@double.observer){
      message("Double observer simulations not supported at present")#move this to the checking of the simulation object
    }else{
      if(inherits(object@design, "LT.Design")){
        survey <- new(Class = "Single.Obs.LT.Survey", population = population, line.transect = transects, rad.truncation = object@detectability@rad.truncation, perp.truncation = object@detectability@perp.truncation)
      }
    }
    survey.data <- simulate.survey(object = survey, dht.table = dht.tables, region = object@region)
    ddf.data <- survey.data$ddf.data 
    if(dht.tables){
      obs.table <- survey.data$obs.table
      sample.table <- survey.data$sample.table
      region.table <- survey.data$region.table
      survey.results <- new(Class = "LT.Survey.Results", region = object@region, population = population, transects = transects, ddf.data = ddf.data, obs.table = obs.table, sample.table = sample.table, region.table = region.table)
      #return(list(survey = survey, ddf.data = ddf.data, obs.table = obs.table, sample.table = sample.table, region.table = region.table))
    }else{
      survey.results <- new(Class = "LT.Survey.Results", region = object@region, population = population, transects = transects, ddf.data = ddf.data)
      #return(list(survey = survey, ddf.data = ddf.data))
    }
    return(survey.results)     
  }    
)

setMethod(
  f="run.analysis",
  signature=c("Simulation","LT.Survey.Results"),
  definition=function(object, data, dht = TRUE){
    require(mrds)
    dist.data <- survey.results@ddf.data
    ddf.analyses <- object@ddf.analyses
    criteria <- NULL
    results <- list()
    for(a in seq(along = ddf.analyses)){
      results[[a]] <- run.analysis(ddf.analyses[[a]], ddf.dat = dist.data)
      criteria <- c(criteria, results[[a]]$criterion)
    }
    best.model <- which(criteria == min(criteria))
    if(dht){
      dht.results <- dht(results[[best.model]], survey.results@region.table@region.table, survey.results@sample.table@sample.table, survey.results@obs.table@obs.table)
      return(list(ddf = results[[best.model]], dht = dht.results))
    }
    #ddf.result.list <- list(ddf.result = ddf.result)
    #object@ddf.result <- ddf.result.list
    return(list(ddf = results[[best.model]]))
  }    
)

setMethod(
  f="run.analysis",
  signature=c("Simulation","DDF.Data"),
  definition=function(object, data, dht = TRUE){
    require(mrds)
    ddf.analyses <- object@ddf.analyses
    criteria <- NULL
    results <- list()
    for(a in seq(along = ddf.analyses)){
      results[[a]] <- run.analysis(ddf.analyses[[a]], ddf.dat = data)
      criteria <- c(criteria, results[[a]]$criterion)
    }
    best.model <- which(criteria == min(criteria))
    return(results[[best.model]])
  }    
)


setMethod(
  f="run",
  signature="Simulation",
  definition=function(object){
    require(mrds)
    require(splancs)
    #set the transect index to 1
    orig.file.index <- object@design@file.index
    object@design@file.index <- 1
    for(i in 1:object@reps){
      message("file index = ", object@design@file.index, ", filename = ", object@design@filenames[object@design@file.index])
      #generate population
      population <- generate.population(object)
      #generate transects
      transects <- generate.transects(object)
      #make survey object
      if(object@double.observer){
        message("Double observer simulations not supported at present")#move this to the checking of the simulation object
      }else{
        if(inherits(object@design, "LT.Design")){
          survey <- new(Class = "Single.Obs.LT.Survey", population = population, line.transect = transects, rad.truncation = object@detectability@rad.truncation, perp.truncation = object@detectability@perp.truncation)
        }
      }
      #simulate survey
      survey.data <- simulate.survey(object = survey, dht.table = TRUE, region = object@region)
      ddf.data <- survey.data$ddf.data 
      obs.table <- survey.data$obs.table
      sample.table <- survey.data$sample.table
      region.table <- survey.data$region.table 
      #analyse survey
      ddf.results <- run.analysis(simulation, ddf.data)
      object@results$Detection <- store.ddf.results(object@results$Detection, ddf.results, i)
      compute.dht = TRUE
      if(compute.dht){
        dht.results <- dht(ddf.results, region.table@region.table, sample.table@sample.table, obs.table@obs.table)
        object@results <- store.dht.results(object@results, dht.results, i)                                           
      }
      object@design@file.index <- object@design@file.index + 1
    }
  object@results <- add.summary.results(object@results)
  object@design@file.index <- orig.file.index
  return(object)  
  }  
)







