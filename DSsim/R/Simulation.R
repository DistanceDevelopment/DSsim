#' @include Detectability.R
#' @include Population.Description.R
#' @include Survey.Design.R
#' @include Region.R
#' @include DSM.Analysis.R
#' @include generic.functions.R
#' @include LT.Survey.Results.R
#' @include DDF.Data.R

#' Class "Simulation"
#'
#' Class \code{"Simulation"} is an S4 class containing descriptions of the
#' region, population, survey design and analyses the user wishes to investigate.
#' Once the simulation has been run the N.D.Estimates will contain multiple
#' estimates of abundance and density obtained by repeatedly generating
#' populations, simulating the survey and completing the analyses.
#'
#' @name Simulation-class
#' @title S4 Class "Simulation"
#' @slot reps Object of class \code{"numeric"}; the number of
#'  times the simulation should be repeated.
#' @slot single.transect.set Object of class \code{"logical"}; if
#'  \code{TRUE} the same set of transects are used in each repetition.
#' @slot double.observer Object of class \code{"logical"}; whether
#'  a double observer protocol is being used. Not currently implemented.
#' @slot region Object of class \code{"Region"}; the survey region.
#' @slot design Object of class \code{"Survey.Design"}; the
#'  survey design.
#' @slot population.description Object of class \code{"Population.Description"}; 
#' the population.description.
#' @slot detectability Object of class \code{"Detectability"}; a
#'  description of the detectability of the population.
#' @slot ddf.analyses Object of class \code{"list"}; a list of
#'  objects of class DDF.Analysis. These are fitted and the one with the
#'  minimum criteria is selected and used in predicting N and D.
#' @slot dsm.analysis Object of class \code{"DSM.Analysis"}; Not
#'  yet implemented.
#' @slot ddf.param.ests Object of class \code{"array"}; stores the
#'  parameters associated with the detection function.
#' @slot results A \code{"list"} of \code{"arrays"}; stores
#'  the estimated of abundance and density as well as other measures
#'  of interest.
#' @section Methods:
#' \describe{
#'  \item{\code{add.hotspot}}{\code{signature=(object = "Simulation")}: adds
#'  a hotspot based on a gaussian decay to the density surface.}
#'  \item{\code{summary}}{\code{signature=(object = "Simulation")}: produces
#'  a summary of the simulation and its results.}
#'  \item{\code{generate.population}}{\code{signature = (object =
#'  "Simulation")}: generates a single instance of a population.}
#'  \item{\code{generate.transects}}{\code{signature = (object =
#'  "Simulation")}: generates a single set of transects.}
#'  \item{\code{create.survey.results}}{\code{signature = (object =
#'  "Simulation")}: carries out the simulation process as far as generating
#'  the distance data and returns an object containing the population,
#'  transects and data.}
#'  \item{\code{run.analysis}}{\code{signature = c(object =
#'  "Simulation", data = "LT.Survey.Results")}: returns the ddf analysis
#'  results from the models in the simulation fitted to the data in the
#'  LT.Survey.Results object.}
#'  \item{\code{run.analysis}}{\code{signature = c(object =
#'  "Simulation", data = "DDF.Data")}: returns the ddf analysis
#'  results from the models in the simulation fitted to the data in the
#'  DDF.Data object.}
#'  \item{\code{run}}{\code{signature = (object = "Simulation")}: runs
#'  the whole simulation for the specified number of repetitions.}
#' }
#' @keywords classes
#' @rdname Simulation-class
#' @seealso \code{\link{make.simulation}}
setClass("Simulation", representation(reps = "numeric",
                                      single.transect.set = "logical",
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
  definition=function(.Object, reps, single.transect.set = FALSE, double.observer = FALSE, region, design, population.description, detectability, ddf.analyses, results){
    #Set slots
    .Object@reps            <- reps
    .Object@single.transect.set <- single.transect.set
    .Object@double.observer <- double.observer
    .Object@region          <- region
    .Object@design          <- design
    .Object@population.description <- population.description
    .Object@detectability   <- detectability
    .Object@ddf.analyses    <- ddf.analyses
    .Object@results         <- results
    #Check object is valid
    validObject(.Object)
    # return object
    return(.Object)
  }
)

setValidity("Simulation",
  function(object){
    if(object@double.observer){
      warning("Double observer simulations not supported at present", call. = TRUE, immediate. = TRUE)
      return(FALSE)
    }
    design <- object@design
    transects.from.file <- ifelse(length(design@path) == 1, TRUE, FALSE)
    if(transects.from.file & !object@single.transect.set){
      no.files <- length(design@filenames)
      if(object@reps > no.files){
        message("You have specified a higher number of repetitions than you have provided transect shapefiles at: ", design@path)
        return(FALSE)
      }
    }else if(!transects.from.file){
      message("The generation of transects is not currently implemented in R")
      return(FALSE)
    }
    return(TRUE)
  }
)

# GENERIC METHODS DEFINITIONS --------------------------------------------

#' summary
#' 
#' Provides a summary of the simulation results.
#' 
#' @param object object of class Simulation
#' @param description.summary logical indicating whether an
#'  explanation of the summary should be included
#' @param ... not implemented
#' @rdname summary.Simulation-methods
#' @importFrom stats na.omit
#' @export
setMethod(
  f="summary",
  signature="Simulation",
  definition=function(object, description.summary = TRUE, ...){
    if(description.summary){
      description.summary()
    }
    reps <- dim(object@results$individuals$N)[3]-2
    #Calculate true values
    strata.names <- object@region@strata.name
    strata.order <- NULL
    #Deal with any grouping of strata
    analysis.strata <- object@ddf.analyses[[1]]@analysis.strata
    if(nrow(analysis.strata) > 0){
      #get strata names
      sub.strata.names <- strata.names
      strata.names <- unique(analysis.strata$analysis.id)
      #sum areas of sub strata
      areas <- N <- rep(NA, length(strata.names))
      for(strat in seq(along = strata.names)){
        #Get sub strata names
        sub.strata <- analysis.strata$design.id[which(analysis.strata$analysis.id == strata.names[strat])]
        #Find their index
        index <- which(sub.strata.names %in% sub.strata)
        areas[strat] <- sum(object@region@area[index]) 
        N[strat] <- sum(object@population.description@N[index])
      }
      #Add on totals
      areas <- c(areas, sum(areas))
      N <- c(N, sum(N))
    #Otherwise process areas and Population size as normal
    }else{
      #Re ordering in the same way as the results tables (think dht arranges them)
      for(strat in seq(along = strata.names)){
        strata.order <- c(strata.order, which(strata.names == dimnames(object@results$individuals$N)[[1]][strat]))
      }
      N <- object@population.description@N
      if(length(strata.names) > 0){
        N <- N[strata.order]
        N <- c(N, sum(N))
        areas <- c(object@region@area[strata.order], sum(object@region@area))
        strata.names <- c(strata.names[strata.order], "Pooled")
        
      }else{
        areas <- object@region@area
      }
    }
    if(is.null(object@results$clusters)){
      #If there are no clusters
      true.N.individuals <- N
      true.D.individuals <- true.N.individuals/areas
    }else{
      #If there are clusters
      true.N.clusters <- N
      #calculate expected cluster size
      size.table <- object@population.description@size.table
      true.expected.s <- sum(size.table$size*size.table$prob)
      #calculate expected number of individuals
      true.N.individuals <- true.N.clusters*true.expected.s
      true.D.individuals <- true.N.individuals/areas
      true.D.clusters <- true.N.clusters/areas
    }

    #Create summary tables
    capture <- array(NA, dim = c(reps, length(true.N.individuals)))
    capture.D <- array(NA, dim = c(reps, length(true.D.individuals)))
    zero.n <- array(NA, dim = c(reps, length(true.N.individuals)))
    for(strat in seq(along = true.N.individuals)){
      for(i in 1:reps){
        capture[i, strat] <- ifelse(object@results$individuals$N[strat, "lcl", i] < true.N.individuals[strat] & object@results$individuals$N[strat, "ucl", i] > true.N.individuals[strat], TRUE, FALSE)
        capture.D[i, strat] <- ifelse(object@results$individuals$D[strat, "lcl", i] < true.D.individuals[strat] & object@results$individuals$D[strat, "ucl", i] > true.D.individuals[strat], TRUE, FALSE)
        zero.n[i, strat] <- ifelse(object@results$individuals$summary[strat, "n", i] == 0, TRUE, FALSE)
      }
    }
    #Calculates the percentage of times the true value is whithin the confidence intervals
    percent.capture <- (apply(capture, 2, sum, na.rm = TRUE)/nrow(na.omit(capture)))*100
    percent.capture.D <- (apply(capture.D, 2, sum, na.rm = TRUE)/nrow(na.omit(capture)))*100
    zero.n <- apply(zero.n, 2, sum)
    individual.summary <- data.frame(mean.Cover.Area = object@results$individuals$summary[,"CoveredArea","mean"],
                                     mean.Effort = object@results$individuals$summary[,"Effort","mean"],
                                     mean.n = object@results$individuals$summary[,"n","mean"],
                                     no.zero.n = zero.n,
                                     mean.ER = object@results$individuals$summary[,"ER","mean"],
                                     mean.se.ER = object@results$individuals$summary[,"se.ER","mean"],
                                     sd.mean.ER = object@results$individuals$summary[,"ER","sd"])
    individual.N <- data.frame(Truth = true.N.individuals,
                                   mean.Estimate = object@results$individuals$N[,"Estimate","mean"],
                                   percent.bias = (object@results$individuals$N[,"Estimate","mean"] - true.N.individuals)/true.N.individuals*100,
                                   #lcl = object@results$individuals$N[,"lcl","mean"],
                                   #ucl = object@results$individuals$N[,"ucl","mean"],
                                   CI.coverage.prob = percent.capture/100,
                                   mean.se = object@results$individuals$N[,"se","mean"],
                                   sd.of.means = object@results$individuals$N[,"Estimate","sd"])
    individual.D <- data.frame(Truth = true.D.individuals,
                                   mean.Estimate = object@results$individuals$D[,"Estimate","mean"],
                                   percent.bias = (object@results$individuals$D[,"Estimate","mean"] - true.D.individuals)/true.D.individuals*100,
                                   #lcl = object@results$individuals$N[,"lcl","mean"],
                                   #ucl = object@results$individuals$N[,"ucl","mean"],
                                   CI.coverage.prob = percent.capture.D/100,
                                   mean.se = object@results$individuals$D[,"se","mean"],
                                   sd.of.means = object@results$individuals$D[,"Estimate","sd"])

    if(!is.null(object@results$clusters)){
      capture <- array(NA, dim = c(reps, length(true.N.individuals)))
      capture.D <- array(NA, dim = c(reps, length(true.D.individuals)))
      zero.n <- array(NA, dim = c(reps, length(true.N.individuals)))
      for(strat in seq(along = true.N.clusters)){
        for(i in 1:reps){
          capture[i, strat] <- ifelse(object@results$clusters$N[strat, "lcl", i] < true.N.clusters[strat] & object@results$clusters$N[strat, "ucl", i] > true.N.clusters[strat], TRUE, FALSE)
          capture.D[i, strat] <- ifelse(object@results$clusters$D[strat, "lcl", i] < true.D.clusters[strat] & object@results$clusters$D[strat, "ucl", i] > true.D.clusters[strat], TRUE, FALSE)
          zero.n[i, strat] <- ifelse(object@results$clusters$summary[strat, "n", i] == 0, TRUE, FALSE)
        }
      }
      percent.capture <- (apply(capture, 2, sum, na.rn = TRUE)/nrow(na.omit(capture)))*100
      percent.capture.D <- (apply(capture.D, 2, sum, na.rm = TRUE)/nrow(na.omit(capture.D)))*100
      zero.n <- apply(zero.n, 2, sum)
      cluster.summary <- data.frame(mean.Cover.Area = object@results$clusters$summary[,"CoveredArea","mean"],
                                       mean.Effort = object@results$clusters$summary[,"Effort","mean"],
                                       mean.n = object@results$clusters$summary[,"n","mean"],
                                       no.zero.n = zero.n,
                                       mean.k = object@results$clusters$summary[,"k","mean"],
                                       mean.ER = object@results$clusters$summary[,"ER","mean"],
                                       mean.se.ER = object@results$clusters$summary[,"se.ER","mean"],
                                       sd.mean.ER = object@results$clusters$summary[,"ER","sd"])
      cluster.N <- data.frame(Truth = true.N.clusters,
                                     mean.Estimate = object@results$clusters$N[,"Estimate","mean"],
                                     percent.bias = (object@results$clusters$N[,"Estimate","mean"] - true.N.clusters)/true.N.clusters*100,
                                     #lcl = object@results$clusters$N[,"lcl","mean"],
                                     #ucl = object@results$clusters$N[,"ucl","mean"],
                                     CI.coverage.prob = percent.capture/100,
                                     mean.se = object@results$clusters$N[,"se","mean"],
                                     sd.of.means = object@results$clusters$N[,"Estimate","sd"])
      cluster.D <- data.frame(Truth = true.D.clusters,
                                     mean.Estimate = object@results$clusters$D[,"Estimate","mean"],
                                     percent.bias = abs(object@results$clusters$D[,"Estimate","mean"] - true.D.clusters)/true.D.clusters*100,
                                     #lcl = object@results$clusters$N[,"lcl","mean"],
                                     #ucl = object@results$clusters$N[,"ucl","mean"],
                                     CI.coverage.prob = percent.capture.D/100,
                                     mean.se = object@results$clusters$D[,"se","mean"],
                                     sd.of.means = object@results$clusters$D[,"Estimate","sd"])
      expected.size <- data.frame(Truth = true.expected.s,
                                  mean.Expected.S = object@results$expected.size[,"Expected.S","mean"],
                                  percent.bias = abs(true.expected.s - object@results$expected.size[,"Expected.S","mean"])/true.expected.s*100,
                                  mean.se.ExpS = object@results$expected.size[,"se.Expected.S","mean"],
                                  sd.mean.ExpS = object@results$expected.size[,"Expected.S","sd"])
      clusters <- list(summary = cluster.summary, N = cluster.N, D = cluster.D)
    }
    detection <- data.frame(mean.observed.Pa = object@results$Detection[,"True.Pa","mean"],
                            mean.estimate.Pa = object@results$Detection[,"Pa","mean"],
                            sd.estimate.Pa = object@results$Detection[,"Pa","sd"],
                            mean.ESW = object@results$Detection[,"ESW","mean"],
                            sd.ESW = object@results$Detection[,"ESW","sd"])
    #Find how many iterations failed
    no.fails <- length(which(is.na(object@results$Detection[1,1,1:object@reps])))
    #print(individual.N.est)
    individuals <- list(summary = individual.summary, N = individual.N, D = individual.D)
    if(!is.null(object@results$clusters)){
      summary.x <- new(Class = "Simulation.Summary", region.name = object@region@region.name, total.reps = object@reps, failures = no.fails, individuals = individuals, clusters = clusters, expected.size = expected.size, detection = detection)
    }else{
      summary.x <- new(Class = "Simulation.Summary", region.name = object@region@region.name, total.reps = object@reps, failures = no.fails, individuals = individuals, detection = detection)
    }
    return(summary.x)
  }
)

#' show
#' 
#' Not currently implemented
#' 
#' @param object object of class Simulation
#' @rdname show.Simulation-methods
#' @export
setMethod(
  f="show",
  signature="Simulation",
  definition=function(object){
    message("show not currently implemented")
    invisible(object)
  }
)


# @rdname Simulation-class
# @export
#setMethod(
#  f="plot",
#  signature="Simulation",
#  definition=function(x, ...){
#    message("not currently implemented")
#    invisible(x)
#  }
#)

#' @rdname generate.population-methods
#' @export
setMethod(
  f="generate.population",
  signature="Simulation",
  definition=function(object, ...){
    population <- generate.population(object = object@population.description, detectability = object@detectability, region.obj = object@region)
    return(population)
  }
)

#' @rdname generate.transects-methods
#' @export
setMethod(
  f="generate.transects",
  signature="Simulation",
  definition=function(object, read.from.file = TRUE, write.to.file = FALSE, region = NULL){
    region <- object@region
    transect <- generate.transects(object@design, region = region)
    return(transect)
  }
)

#' @rdname create.survey.results-methods
#' @export
setMethod(
  f="create.survey.results",
  signature="Simulation",
  definition=function(object, dht.tables = FALSE, ...){
    population <- generate.population(object)
    transects  <- generate.transects(object)
    if(object@double.observer){
      #move this to the checking of the simulation object
      message("Double observer simulations not supported at present")
    }else{
      if(inherits(object@design, "LT.Design")){
        survey <- new(Class = "Single.Obs.LT.Survey", population = population, line.transect = transects, perp.truncation = object@detectability@truncation)
      }
    }
    survey.data <- create.survey.results(object = survey, dht.tables = dht.tables, region = object@region)
    ddf.data <- survey.data$ddf.data
    if(dht.tables){
      obs.table <- survey.data$obs.table
      sample.table <- survey.data$sample.table
      region.table <- survey.data$region.table
      survey.results <- new(Class = "LT.Survey.Results", region = object@region, population = population, transects = transects, ddf.data = ddf.data, obs.table = obs.table, sample.table = sample.table, region.table = region.table)
    }else{
      obs.table <- new(Class = "Obs.Table")
      sample.table <- new(Class = "Sample.Table")
      region.table <- new(Class = "Region.Table")
      survey.results <- new(Class = "LT.Survey.Results", region = object@region, population = population, transects = transects, ddf.data = ddf.data, obs.table = obs.table, sample.table = sample.table, region.table = region.table)
    }
    return(survey.results)
  }
)


#' @rdname run.analysis-methods
#' @export
setMethod(
  f="run.analysis",
  signature=c("Simulation","LT.Survey.Results"),
  definition=function(object, data, dht = TRUE){
    #dist.data <- survey.results@ddf.data
    best.model <- run.analysis(object, data@ddf.data)
    #If ddf has converged and dht it TRUE
    if(dht & !is.null(best.model)){
      #Calculate density/abundance
      dht.results <- dht(best.model, data@region.table@region.table, data@sample.table@sample.table, data@obs.table@obs.table)
      return(list(ddf = best.model, dht = dht.results))
    }
    return(list(ddf = best.model))
  }
)

#' @rdname run.analysis-methods
#' @importFrom stats na.omit
#' @export
setMethod(
  f="run.analysis",
  signature=c("Simulation","DDF.Data"),
  definition=function(object, data, dht = TRUE){
    ddf.analyses <- object@ddf.analyses
    criteria <- NULL
    results <- list()
    for(a in seq(along = ddf.analyses)){
      results[[a]] <- run.analysis(ddf.analyses[[a]], data)
      if(!is.na(results[[a]][1])){
        criteria <- c(criteria, results[[a]]$criterion)
      }else{
        criteria <- c(criteria, NA)
      }
    }
    #check that at least one model worked
    if(length(which(!is.na(criteria))) > 0){
      best.model <- which(criteria == min(na.omit(criteria)))
      return(results[[best.model]])
    }else{
      return(NULL)
    }
  }
)



#' @rdname run-methods
#' @importFrom parallel detectCores makeCluster clusterEvalQ parLapply stopCluster
#' @export
setMethod(
  f="run",
  signature="Simulation",
  definition=function(object, run.parallel = FALSE, max.cores = NA, save.data = FALSE, load.data = FALSE, data.path = character(0)){
    #Note options save.data, load.data, data.path are not implemented in simulations run in parallel.
    #check the data.path ends in "/"
    if(length(data.path) > 0){
      temp.path <- strsplit(data.path, split = "")
      if(temp.path[length(temp.path)] != "/"){
        data.path <- paste(data.path, "/", sep = "")
      }
      rm(temp.path)
    }
    #set the transect index to 1
    orig.file.index <- object@design@file.index
    object@design@file.index <- 1
    if(run.parallel & requireNamespace('parallel', quietly = TRUE)){
      # counts the number of cores you have
      nCores <- getOption("cl.cores", detectCores())
      if(!is.na(max.cores)){
        nCores <- min(nCores - 1, max.cores)
      }
      # intitialise the cluster
      myCluster <- makeCluster(nCores)
      clusterEvalQ(myCluster, {
        require(DSsim)
      })
      results <- parLapply(myCluster, X = as.list(1:object@reps), fun = single.simulation.loop, object = object, save.data = save.data, load.data = load.data, data.path = data.path)
      object <- accumulate.PP.results(simulation = object, results = results)
      stopCluster(myCluster)
    }else{
      #Check that it wasn't trying to run parallel
      if(run.parallel){
        warning("Could not run in parallel, library(parallel) is not installed.")
      }
      #otherwise loop
      for(i in 1:object@reps){
        object@results <- single.simulation.loop(i, object, save.data = save.data, load.data = load.data, data.path = data.path)
      }
    }
    object@results <- add.summary.results(object@results)
    object@design@file.index <- orig.file.index
    return(object)
  }
)







