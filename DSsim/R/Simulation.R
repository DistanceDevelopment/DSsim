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
                                      region  = "Region",
                                      design = "Survey.Design",
                                      population.description  = "Population.Description",
                                      detectability    = "Detectability",
                                      ddf.analyses   = "list", 
                                      dsm.analysis = "DSM.Analysis",                                          
                                      ddf.param.ests   = "array",
                                      N.D.Estimates    = "array")) 
                                      
setMethod(
  f="initialize",
  signature="Simulation",
  definition=function(.Object, reps, double.observer = FALSE, region, design, population.description, detectability, ddf.analyses){
    #Input pre-processing
    N.D.Estimates <- array(NA, dim = c(reps, 3), dimnames = list(1:reps, c("N estimate", "N lcl", "N ucl")))
    #Set slots
    .Object@reps            <- reps
    .Object@double.observer <- double.observer    
    .Object@region          <- region
    .Object@design          <- design
    .Object@population.description <- population.description
    .Object@detectability   <- detectability
    .Object@ddf.analyses    <- ddf.analyses
    .Object@N.D.Estimates   <- N.D.Estimates
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
    cat("\nTrue N:", x@population.description@N, "\n")
    if(length(which(!is.na(x@N.D.Estimates))) > 0){
      print(apply(x@N.D.Estimates, 2, mean))
    }
    invisible(x)
  }    
)
  
setMethod(
  f="hist",
  signature="Simulation",
  definition=function(x, ...){
    hist(x@N.D.Estimates[,1], xlab = "Estimate of N")
    abline(v=x@population.description@N, col=2, lwd = 2)
    invisible(x)
  }    
)

setMethod(
  f="plot",
  signature="Region",
  definition=function(x, y, type = "l", ...){
    #Input pre-processing
    plot(c(x@box[["xmin"]], x@box[["xmax"]]), c(x@box[["ymin"]], x@box[["ymax"]]), col = "white", xlab = "X-coords (units to be added)", ylab = "Y-coords (units to be added)", main = x@region.name, ...) 
    lapply(x@coords, FUN = lines, type = type)
    lapply(x@gaps, FUN = lines, type = type, col = 8)
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
      #make survey
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
      dht = TRUE
      if(dht){
        dht.results <- dht(ddf.results, region.table@region.table, sample.table@sample.table, obs.table@obs.table)                                           
      }
      #store results 
      object@N.D.Estimates[i,] <- as.matrix(dht.results$individuals$N[c("Estimate", "lcl", "ucl")])
      object@design@file.index <- object@design@file.index + 1
    }
  object@design@file.index <- orig.file.index
  return(object)  
  }  
)







