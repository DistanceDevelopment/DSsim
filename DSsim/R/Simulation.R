#' @include Detectability.R
#' @include Population.Description.R
#' @include Survey.Design.R
#' @include Region.R
#' @include DSM.Analysis.R
#' @include generic.functions.R
#' @include LT.Survey.Results.R
#' @include Survey.Results.R
#' @include DDF.Data.R

#' @title Class "Simulation"
#'
#' @description Class \code{"Simulation"} is an S4 class containing descriptions of the
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
#' @slot warnings A \code{"list"} to store warnings and error messages encountered
#'  during runtime.
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
                                      results = "list",
                                      warnings = "list"))

setMethod(
  f="initialize",
  signature="Simulation",
  definition=function(.Object, reps = 10, single.transect.set = FALSE, double.observer = FALSE, region = make.region(), design = make.design(), population.description = make.population.description(), detectability = make.detectability(), ddf.analyses = make.ddf.analysis.list(), results = list()){
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
    .Object@warnings        <- list()
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
              if(class(design) == "PT.Nested.Design"){
                if(object@detectability@truncation > object@ddf.analyses[[1]]@truncation){
                  warning("Please be aware that your truncation distance for analysis is less than that used to generate the data (defined in detectability). This will introduce bias into your estimates.", call. = FALSE, immediate. = TRUE)
                }
              }
              transects.from.file <- ifelse(length(design@path) == 1, TRUE, FALSE)
              if(transects.from.file & !object@single.transect.set){
                no.files <- length(design@filenames)
                if(object@reps > no.files){
                  message("You have specified a higher number of repetitions than you have provided transect shapefiles at: ", design@path)
                  return(FALSE)
                }
              }else if(!transects.from.file){
                if(!(class(object@design) %in% c("PT.Nested.Design", "PT.Systematic.Design", "LT.Systematic.Design"))){
                  message("The generation of transects is only implemented in R for nested and systematic point transect designs as well as systematic parallel line transect designs.")
                  return(FALSE)  
                }
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
#' @param ... can specify if you want the maximum number of iterations to be used where at least one model converged (use.max.reps = TRUE) or only use iterations where all models converged (use.max.reps = FALSE)
#' @rdname summary.Simulation-methods
#' @importFrom stats na.omit qlnorm qnorm
#' @export
setMethod(
  f="summary",
  signature="Simulation",
  definition=function(object, description.summary = TRUE, ...){
    if(description.summary){
      description.summary()
    }
    #Check if simulation object is up-to-date
    object <- dssim.update(object, warn = FALSE)
    #Get number of reps
    reps <- object@reps
    #Check for additional arguments
    additional.args <- names(list(...))
    if (!("use.max.reps" %in% additional.args)){
      #By default exclude all iterations where one or more models failed to converge
      use.max.reps = FALSE
    }else{
      use.max.reps = list(...)$use.max.reps
    }
    #Get index of iterations to use
    model.count <- length(object@ddf.analyses)
    #These will use max iters by default
    results <- object@results
    #Make backwards compatible
    if("SuccessfulModels" %in% dimnames(object@results$Detection)[[2]]){
      if(use.max.reps){
        rep.index <- which(object@results$Detection[1,"SuccessfulModels",1:reps] > 0)
      }else{
        rep.index <- which(object@results$Detection[1,"SuccessfulModels",1:reps] == model.count)
        results <- add.summary.results(results, length(object@ddf.analyses), use.max.reps = use.max.reps)
      }  
    }else{
      #Otherwise get all repetitions where estimated abundance is not NA
      rep.index <- which(!is.na(results$individuals$N[1,"Estimate",1:reps]))
      use.max.reps = TRUE
    }
    #Create function to calculate RMSE
    calc.RMSE <- function(x, reps){ 
      true.x <- x[(reps+1)]
      x <- na.omit(x[1:reps])
      reps.success <- length(x)
      return( sqrt( sum((x-true.x)^2) / reps.success ))
    }
    #Calculate true values
    strata.names <- object@region@strata.name
    strata.order <- NULL
    #Deal with any grouping of strata
    analysis.strata <- try(object@ddf.analyses[[1]]@analysis.strata, silent = TRUE)
    if(class(analysis.strata) == "try-error"){
      analysis.strata <- data.frame()
    }
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
        strata.order <- c(strata.order, which(strata.names == dimnames(results$individuals$N)[[1]][strat]))
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
    if(is.null(results$clusters)){
      #If there are no clusters
      true.N.individuals <- N
      true.D.individuals <- true.N.individuals/areas
    }else{
      #If there are clusters
      true.N.clusters <- N
      #calculate expected cluster size
      true.expected.s <- numeric()
      size.list <- object@population.description@covariates[["size"]]
      for(i in seq(along = size.list)){
        if(class(size.list[[i]]) == "data.frame"){
          true.expected.s[i] <- sum(size.list[[i]]$level*size.list[[i]]$prob)
        }else{
          size.dist <- size.list[[i]]
          dist <- size.dist[[1]]
          dist.param <- size.dist[[2]]
          # if(dist == "ztruncpois"){
          #   temp <- rtpois(99999, mean = dist.param$mean)
          #   true.expected.s[i] <- mean(temp)
          # }else{
          true.expected.s[i] <- switch(dist,
                                       "normal" = dist.param$mean,
                                       "poisson" = dist.param$lambda,
                                       "ztruncpois" = dist.param$mean,
                                       "lognormal" = exp(dist.param$meanlog + 0.5 * dist.param$sdlog^2))
          #}
        }
      }
      if(length(size.list) > 1){
        true.expected.s <- c(true.expected.s, sum(true.N.clusters[1:length(true.expected.s)]*true.expected.s)/sum(true.N.clusters[1:length(true.expected.s)]))  
      }
      #calculate expected number of individuals
      true.N.individuals <- true.N.clusters*true.expected.s  
      true.D.individuals <- true.N.individuals/areas
      true.D.clusters <- true.N.clusters/areas
    }
    
    #Create summary tables
    included.reps <- length(rep.index)
    capture <- array(NA, dim = c(included.reps, length(true.N.individuals)))
    capture.D <- array(NA, dim = c(included.reps, length(true.D.individuals)))
    zero.n <- array(NA, dim = c(included.reps, length(true.N.individuals)))
    for(strat in seq(along = true.N.individuals)){
      for(i in seq(along = rep.index)){
        capture[i, strat] <- ifelse(results$individuals$N[strat, "lcl", i] < true.N.individuals[strat] & results$individuals$N[strat, "ucl", i] > true.N.individuals[strat], TRUE, FALSE)
        capture.D[i, strat] <- ifelse(results$individuals$D[strat, "lcl", i] < true.D.individuals[strat] & results$individuals$D[strat, "ucl", i] > true.D.individuals[strat], TRUE, FALSE)
        zero.n[i, strat] <- ifelse(results$individuals$summary[strat, "n", i] == 0, TRUE, FALSE)
      }
    }
    #Calculates the percentage of times the true value is whithin the confidence intervals
    percent.capture <- (apply(capture, 2, sum, na.rm = TRUE)/nrow(na.omit(capture)))*100
    percent.capture.D <- (apply(capture.D, 2, sum, na.rm = TRUE)/nrow(na.omit(capture)))*100
    zero.n <- apply(zero.n, 2, sum, na.rm = TRUE)
    if(length(true.N.individuals) == 1){
      RMSE.N = apply(cbind(t(as.matrix(results$individuals$N[, "Estimate", rep.index])), true.N.individuals), 1, calc.RMSE, reps = reps)
      RMSE.D = apply(cbind(t(as.matrix(results$individuals$D[, "Estimate", rep.index])), true.D.individuals), 1, calc.RMSE, reps = reps)
    }else{
      RMSE.N = apply(cbind(results$individuals$N[, "Estimate", rep.index], true.N.individuals), 1, calc.RMSE, reps = reps) 
      RMSE.D = apply(cbind(results$individuals$D[, "Estimate", rep.index], true.D.individuals), 1, calc.RMSE, reps = reps)
    }
    individual.summary <- data.frame(mean.Cover.Area = results$individuals$summary[,"CoveredArea","mean"],
                                     mean.Effort = results$individuals$summary[,"Effort","mean"],
                                     mean.n = results$individuals$summary[,"n","mean"],
                                     mean.n.miss.dist = ifelse("n.miss.dist" %in% dimnames(results$individuals$summary)[[2]], results$individuals$summary[,"n.miss.dist","mean"], NA),
                                     no.zero.n = zero.n,
                                     mean.ER = results$individuals$summary[,"ER","mean"],
                                     mean.se.ER = results$individuals$summary[,"se.ER","mean"],
                                     sd.mean.ER = results$individuals$summary[,"ER","sd"])
    # Remove unnecessary columns
    if(all(is.na(individual.summary$mean.n.miss.dist))){
      # To keep CRAN check happy!
      eval(parse(text = paste("individual.summary <- subset(individual.summary, select = -mean.n.miss.dist)")))  
    }else if(all(individual.summary$mean.n.miss.dist == 0)){
      # To keep CRAN check happy!
      eval(parse(text = paste("individual.summary <- subset(individual.summary, select = -mean.n.miss.dist)")))
    }
    if(all(individual.summary$no.zero.n == 0)){
      eval(parse(text = paste("individual.summary <- subset(individual.summary, select = -no.zero.n)")))
    }
    individual.N <- data.frame(Truth = true.N.individuals,
                               mean.Estimate = results$individuals$N[,"Estimate","mean"],
                               percent.bias = (results$individuals$N[,"Estimate","mean"] - true.N.individuals)/true.N.individuals*100,
                               RMSE = RMSE.N,
                               CI.coverage.prob = percent.capture/100,
                               mean.se = results$individuals$N[,"se","mean"],
                               sd.of.means = results$individuals$N[,"Estimate","sd"])
    individual.D <- data.frame(Truth = true.D.individuals,
                               mean.Estimate = results$individuals$D[,"Estimate","mean"],
                               percent.bias = (results$individuals$D[,"Estimate","mean"] - true.D.individuals)/true.D.individuals*100,
                               RMSE = RMSE.D,
                               CI.coverage.prob = percent.capture.D/100,
                               mean.se = results$individuals$D[,"se","mean"],
                               sd.of.means = results$individuals$D[,"Estimate","sd"])
    
    if(!is.null(results$clusters)){
      capture <- array(NA, dim = c(included.reps, length(true.N.clusters)))
      capture.D <- array(NA, dim = c(included.reps, length(true.D.clusters)))
      zero.n <- array(NA, dim = c(included.reps, length(true.N.clusters)))
      for(strat in seq(along = true.N.clusters)){
        for(i in seq(along = rep.index)){
          capture[i, strat] <- ifelse(results$clusters$N[strat, "lcl", i] < true.N.clusters[strat] & results$clusters$N[strat, "ucl", i] > true.N.clusters[strat], TRUE, FALSE)
          capture.D[i, strat] <- ifelse(results$clusters$D[strat, "lcl", i] < true.D.clusters[strat] & results$clusters$D[strat, "ucl", i] > true.D.clusters[strat], TRUE, FALSE)
          zero.n[i, strat] <- ifelse(results$clusters$summary[strat, "n", i] == 0, TRUE, FALSE)
        }
      }
      percent.capture <- (apply(capture, 2, sum, na.rm = TRUE)/nrow(na.omit(capture)))*100
      percent.capture.D <- (apply(capture.D, 2, sum, na.rm = TRUE)/nrow(na.omit(capture.D)))*100
      zero.n <- apply(zero.n, 2, sum, na.rm = TRUE)
      if(length(true.N.clusters) == 1){
        RMSE.N = apply(cbind(t(as.matrix(results$clusters$N[, "Estimate", rep.index])), true.N.clusters), 1, calc.RMSE, reps = reps)
        RMSE.D = apply(cbind(t(as.matrix(results$clusters$D[, "Estimate", rep.index])), true.D.clusters), 1, calc.RMSE, reps = reps)
      }else{
        RMSE.N = apply(cbind(results$clusters$N[, "Estimate", rep.index], true.N.clusters), 1, calc.RMSE, reps = reps) 
        RMSE.D = apply(cbind(results$clusters$D[, "Estimate", rep.index], true.D.clusters), 1, calc.RMSE, reps = reps)
      }
      cluster.summary <- data.frame(mean.Cover.Area = results$clusters$summary[,"CoveredArea","mean"],
                                    mean.Effort = results$clusters$summary[,"Effort","mean"],
                                    mean.n = results$clusters$summary[,"n","mean"],
                                    mean.n.miss.dist = ifelse("n.miss.dist" %in% dimnames(results$clusters$summary)[[2]], results$clusters$summary[,"n.miss.dist","mean"], NA),
                                    no.zero.n = zero.n,
                                    mean.k = results$clusters$summary[,"k","mean"],
                                    mean.ER = results$clusters$summary[,"ER","mean"],
                                    mean.se.ER = results$clusters$summary[,"se.ER","mean"],
                                    sd.mean.ER = results$clusters$summary[,"ER","sd"])
      # Remove unnecessary columns
      if(all(is.na(cluster.summary$mean.n.miss.dist))){
        eval(parse(text = paste("cluster.summary <- subset(cluster.summary,select = -mean.n.miss.dist)")))  
      }else if(all(cluster.summary$mean.n.miss.dist == 0)){
        eval(parse(text = paste("cluster.summary <- subset(cluster.summary,select = -mean.n.miss.dist)")))
      }
      if(all(cluster.summary$no.zero.n == 0)){
        eval(parse(text = paste("cluster.summary <- subset(cluster.summary,select = -no.zero.n)")))
      }
      cluster.N <- data.frame(Truth = true.N.clusters,
                              mean.Estimate = results$clusters$N[,"Estimate","mean"],
                              percent.bias = (results$clusters$N[,"Estimate","mean"] - true.N.clusters)/true.N.clusters*100,
                              RMSE = RMSE.N,
                              #lcl = results$clusters$N[,"lcl","mean"],
                              #ucl = results$clusters$N[,"ucl","mean"],
                              CI.coverage.prob = percent.capture/100,
                              mean.se = results$clusters$N[,"se","mean"],
                              sd.of.means = results$clusters$N[,"Estimate","sd"])
      cluster.D <- data.frame(Truth = true.D.clusters,
                              mean.Estimate = results$clusters$D[,"Estimate","mean"],
                              percent.bias = (results$clusters$D[,"Estimate","mean"] - true.D.clusters)/true.D.clusters*100,
                              RMSE = RMSE.D,
                              #lcl = results$clusters$N[,"lcl","mean"],
                              #ucl = results$clusters$N[,"ucl","mean"],
                              CI.coverage.prob = percent.capture.D/100,
                              mean.se = results$clusters$D[,"se","mean"],
                              sd.of.means = results$clusters$D[,"Estimate","sd"])
      expected.size <- data.frame(Truth = true.expected.s,
                                  mean.Expected.S = results$expected.size[,"Expected.S","mean"],
                                  percent.bias = abs(true.expected.s - results$expected.size[,"Expected.S","mean"])/true.expected.s*100,
                                  mean.se.ExpS = results$expected.size[,"se.Expected.S","mean"],
                                  sd.mean.ExpS = results$expected.size[,"Expected.S","sd"])
      clusters <- list(summary = cluster.summary, N = cluster.N, D = cluster.D)
    }
    detection <- data.frame(mean.observed.Pa = results$Detection[,"True.Pa","mean"],
                            mean.estimate.Pa = results$Detection[,"Pa","mean"],
                            sd.estimate.Pa = results$Detection[,"Pa","sd"],
                            mean.ESW = results$Detection[,"ESW","mean"],
                            sd.ESW = results$Detection[,"ESW","sd"])
    #Find how many iterations failed
    no.fails <- reps - included.reps
    #print(individual.N.est)
    individuals <- list(summary = individual.summary, N = individual.N, D = individual.D)
    #Model selection table
    tab.model.selection <- table(results$Detection[,"SelectedModel",rep.index])
    #Create detectabilty summary
    detectability.summary <- list(key.function = object@detectability@key.function, scale.param = object@detectability@scale.param, shape.param = object@detectability@shape.param, cov.param = object@detectability@cov.param, truncation = object@detectability@truncation)
    #Create analysis summary
    analysis.summary <- list(dsmodels = list(), criteria = object@ddf.analyses[[1]]@criteria, truncation = object@ddf.analyses[[1]]@truncation)
    #Create design summary
    design.type <- switch(class(object@design),
                          LT.Systematic.Design = "Systematic Parallel Line Transect",
                          LT.EqAngle.ZZ.Design = "Equal Angle Zigzag Line Transect",
                          LT.EqSpace.ZZ.Design = "Equal Spaced Zigzag Line Transect",
                          LT.Random.Design = "Random Parallel Line Transect",
                          LT.User.Specified.Design = "Subjective Line Transect",
                          PT.Systematic.Design = "Systematic Point Transect",
                          PT.Nested.Design = "Systematic Nested Point Transect",
                          PT.Random.Design = "Random Point Transect",
                          LT.SegmentedTrack.Design = "Segmented Track Line Transect",
                          LT.SegmentedGrid.Design = "Segmented Grid Line Transect")
    slots <- slotNames(object@design)
    design.parameters <- list()
    count <- 1
    for(i in seq(along = slots)){
      if(slots[i] %in% c("design.axis", "spacing", "plus.sampling", "nested.space")){
        if(!(length(slot(object@design, slots[i])) == 0)){
          design.parameters[[slots[i]]] <- slot(object@design, slots[i])  
        }
      }
    }
    design.summary <- new(Class = "Design.Summary", design.type = design.type, design.parameters = design.parameters)
    #Create population summary
    
    if(object@ddf.analyses[[1]]@binned.data){
      analysis.summary$cutpoints <- object@ddf.analyses[[1]]@cutpoints
    }
    if(nrow(analysis.strata) > 0){
      analysis.summary$analysis.strata <- object@ddf.analyses[[1]]@analysis.strata
    }
    for(i in seq(along = object@ddf.analyses)){
      analysis.summary$dsmodels[[i]] <- object@ddf.analyses[[i]]@dsmodel
    }
    #For backwards compatibility
    pop.covars <- try(object@population.description@covariates, silent = TRUE)
    pop.covars <- ifelse(class(pop.covars) == "try-error", list(), pop.covars)
    #Create simulation summary object
    if(!is.null(results$clusters)){
      summary.x <- new(Class = "Simulation.Summary", region.name = object@region@region.name, strata.name = object@region@strata.name, total.reps = object@reps, failures = no.fails, use.max.reps = use.max.reps, individuals = individuals, clusters = clusters, expected.size = expected.size, population.covars = pop.covars, detection = detection, model.selection = tab.model.selection, design.summary = design.summary, detectability.summary = detectability.summary, analysis.summary = analysis.summary)
    }else{
      summary.x <- new(Class = "Simulation.Summary", region.name = object@region@region.name, strata.name = object@region@strata.name, total.reps = object@reps, failures = no.fails, use.max.reps = use.max.reps, individuals = individuals, population.covars = pop.covars, detection = detection, model.selection = tab.model.selection, design.summary = design.summary, detectability.summary = detectability.summary, analysis.summary = analysis.summary)
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
    summary <- summary(object, description.summary = FALSE)
    show(summary)
  }
)

#' histogram.N.ests
#' 
#' Plots a histogram of the estimates abundances
#' 
#' @param x object of class Simulation
#' @param ... optional parameters to pass to the generic hist function in graphics
#' @rdname histogram.N.ests-methods
#' @importFrom graphics hist abline
#' @export
histogram.N.ests <- function(x, ...){
  reps <- x@reps
  sum.sim <- summary(x, description.summary = FALSE)
  if(sum.sim@failures == reps){
    warning("None of the simulation repetitions were successful, cannot plot histogram of estimates.", immediate. = TRUE, call. = TRUE)
  }else{
    index <- dim(x@results$individuals$N)[1]
    true.N <- sum(x@population.description@N)
    if(!is.null(x@results$clusters)){
      ests <- x@results$clusters$N[index, "Estimate", 1:reps]
      hist(ests, main = "Histogram of Estimates", xlab = "Estimated Abundance of Clusters", ...) 
    }else{
      ests <- x@results$individuals$N[index, "Estimate", 1:reps]
      hist(ests, main = "Histogram of Estimates", xlab = "Estimated Abundance of Individuals", ...)  
    }
    abline(v = true.N, col = 2, lwd = 3, lty = 2)
  }
  invisible(x)
}

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
  definition=function(object, region = NULL){
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
      }else if(inherits(object@design, "PT.Design")){
        survey <- new(Class = "Single.Obs.PT.Survey", population = population, point.transect = transects, rad.truncation = object@detectability@truncation)
      }
    }
    survey.data <- create.survey.results(object = survey, dht.tables = dht.tables, region = object@region)
    ddf.data <- survey.data$ddf.data
    if(dht.tables){
      obs.table <- survey.data$obs.table
      sample.table <- survey.data$sample.table
      region.table <- survey.data$region.table
      survey.results <- new(Class = "Survey.Results", region = object@region, population = population, transects = transects, ddf.data = ddf.data, obs.table = obs.table, sample.table = sample.table, region.table = region.table)
    }else{
      obs.table <- new(Class = "Obs.Table")
      sample.table <- new(Class = "Sample.Table")
      region.table <- new(Class = "Region.Table")
      survey.results <- new(Class = "Survey.Results", region = object@region, population = population, transects = transects, ddf.data = ddf.data, obs.table = obs.table, sample.table = sample.table, region.table = region.table)
    }
    return(survey.results)
  }
)


#' @rdname run.analysis-methods
#' @export
setMethod(
  f="run.analysis",
  signature=c("Simulation","Survey.Results"),
  definition=function(object, data, dht = FALSE){
    analysis.results <- run.analysis(object, data@ddf.data)
    best.model <- analysis.results$best.model
    object@warnings <- analysis.results$warnings
    num.successful.models <- analysis.results$num.successful.models
    #If dht is true but tables have not been provided
    if(dht & nrow(data@region.table@region.table) == 0){
      warning("dht tables have not been provided please re-run create.survey.results with dht.tables = TRUE if you would like density/abundance estimates in addition to ddf results.", immediate. = TRUE, call. = FALSE)
      dht = FALSE
    }
    #If ddf has converged and dht is TRUE
    if(dht & !is.null(best.model)){
      #Check if there are missing distances
      ddf.dat <- data@ddf.data@ddf.dat
      miss.dists <- any(is.na(ddf.dat$distance))
      if(miss.dists){
        # Add the missing distance observations in to ddf object
        missing.dists <- ddf.dat[is.na(ddf.dat$distance),]
        # NA's break dht
        missing.dists$distance <- rep(-1, nrow(missing.dists))
        if(is.null(missing.dists$detected)){
          missing.dists$detected <- rep(1, nrow(missing.dists))  
        }
        best.model <- add.miss.dists(missing.dists, best.model)
      }
      #Calculate density/abundance
      dht.options <- list()
      # if it is a point transect design
      if(inherits(object@design, "PT.Design")){
        dht.options$ervar <- "P3"  
      }
      dht.results <- dht(best.model, data@region.table@region.table, data@sample.table@sample.table, data@obs.table@obs.table, options = dht.options)
      return(list(ddf = best.model, dht = dht.results, warnings = object@warnings, num.successful.models = num.successful.models))
    }
    return(list(ddf = best.model, warnings = object@warnings, num.successful.models = num.successful.models))
  }
)

#' @rdname run.analysis-methods
#' @importFrom stats na.omit
#' @export
setMethod(
  f="run.analysis",
  signature=c("Simulation","DDF.Data"),
  definition=function(object, data, dht = FALSE){
    ddf.analyses <- object@ddf.analyses
    criteria <- NULL
    results <- list()
    point <- inherits(object@design, "PT.Design")
    for(a in seq(along = ddf.analyses)){
      temp <- run.analysis(ddf.analyses[[a]], data, point = point, warnings = object@warnings)
      #run analysis returns the result and any warnings
      results[[a]] <- temp$ddf.result
      object@warnings <- temp$warnings
      if(!is.na(results[[a]][1])){
        #Get information to calculate selection criteria
        lnl <- results[[a]]$lnl 
        k <- length(results[[a]]$par)
        n <- nrow(results[[a]]$data)
        criterion <- object@ddf.analyses[[1]]@criteria
        selection.criterion.value <- switch(criterion,
                                            AIC  = 2*k-2*lnl,
                                            aic  = 2*k-2*lnl,
                                            AICc = 2*k-2*lnl+(2*k*(k+1))/(n-k-1),
                                            BIC  = k*log(n)-2*lnl,
                                            bic  = k*log(n)-2*lnl)
        criteria <- c(criteria, selection.criterion.value)
      }else{
        criteria <- c(criteria, NA)
      }
    }
    #check that at least one model worked
    num.successful.models <- length(which(!is.na(criteria)))
    if(num.successful.models > 0){
      best.model.index <- which(criteria == min(na.omit(criteria)))
      best.model <- results[[best.model.index]]
      best.model$model.index <- best.model.index
      #If there is more than one sucessful model find the delta criteria to the 
      #second best model.
      if(num.successful.models > 1){
        sorted.criteria <- sort(criteria, na.last = NA)
        delta.criteria <- sorted.criteria[2] - sorted.criteria[1]
        best.model$delta.criteria <- delta.criteria
      }
      return(list(best.model = best.model, warnings = object@warnings, num.successful.models = num.successful.models))
    }else{
      return(list(best.model = NULL, warnings = object@warnings, num.successful.models = 0))
    }
  }
)



#' @rdname run-methods
#' @importFrom parallel detectCores makeCluster clusterEvalQ parLapply stopCluster
#' @export
setMethod(
  f="run",
  signature="Simulation",
  definition=function(object, run.parallel = FALSE, max.cores = NA, save.data = FALSE, load.data = FALSE, data.path = character(0), counter = TRUE, progress.file = ""){
    #Check to see if the Simulation object is up-to-date
    object <- dssim.update(object)
    #Reset results arrays
    object@results <- create.results.arrays(object@reps, object@region, object@ddf.analyses, object@population.description)
    #reset the error/warning message
    test <- try(object@warnings, silent = TRUE)
    if(class(test) == "list"){
      object@warnings$message <- list()
      object@warnings$counter <- list()  
    }
    #Note options save.data, load.data, data.path are not implemented in simulations run in parallel.
    #check the data.path ends in "/"
    if(length(data.path) > 0){
      temp.path <- strsplit(data.path, split = "")
      if(temp.path[length(temp.path)] != "/"){
        # if not add it
        data.path <- paste(data.path, "/", sep = "")
      }
      rm(temp.path)
    }
    #set the transect index to 1
    orig.file.index <- object@design@file.index
    object@design@file.index <- 1
    if(run.parallel){
      if(!requireNamespace('parallel', quietly = TRUE) | !requireNamespace('pbapply', quietly = TRUE)){
        warning("Could not run in parallel, library(pbapply) or library(parallel) is not installed.", immediate. = TRUE, call. = FALSE)  
        run.parallel = FALSE
      }else{
        # counts the number of cores you have
        nCores <- getOption("cl.cores", detectCores()) - 1
        if(!is.na(max.cores)){
          nCores <- min(nCores, max.cores)
        }
        if(nCores <= 1){
          warning("Could not run in parallel only one core available/requested (DSsim limits running in parallel to 1 less than the number of cores on the machine).", immediate. = TRUE, call. = FALSE)
          run.parallel = FALSE
        }  
      }
    }
    if(run.parallel){
      # intitialise the cluster
      myCluster <- makeCluster(nCores)
      clusterEvalQ(myCluster, {
        require(DSsim)
        require(shapefiles)
      })
      on.exit(stopCluster(myCluster))
      if(counter){
        if(progress.file != ""){
          # Set up progress file
          cat(0, file = progress.file) 
          results <- pbapply::pblapply(X = as.list(1:object@reps), FUN = single.simulation.loop, object = object, save.data = save.data, load.data = load.data, data.path = data.path, cl = myCluster, counter = TRUE, progress.file = progress.file, in.parallel = TRUE) 
        }else{
          results <- pbapply::pblapply(X= as.list(1:object@reps), FUN = single.simulation.loop, object = object, save.data = save.data, load.data = load.data, data.path = data.path, cl = myCluster, counter = FALSE) 
        }
      }else{
        results <- parLapply(myCluster, X = as.list(1:object@reps), fun = single.simulation.loop, object = object, save.data = save.data, load.data = load.data, data.path = data.path, counter = FALSE) 
      }
      #Extract results and warnings
      sim.results <- sim.warnings <- list()
      for(i in seq(along = results)){
        sim.results[[i]] <- results[[i]]$results
        sim.warnings[[i]] <- results[[i]]$warnings
      }
      object <- accumulate.PP.results(simulation = object, results = sim.results)
      object@warnings <- accumulate.warnings(sim.warnings)
      stopCluster(myCluster)
      on.exit()
    }
    if(!run.parallel){
      #otherwise loop
      for(i in 1:object@reps){
        results <- single.simulation.loop(i, object, save.data = save.data, load.data = load.data, data.path = data.path, counter = counter, progress.file = progress.file)
        object@results <- results$results
        object@warnings <- results$warnings
      }
    }
    object@results <- add.summary.results(object@results, length(object@ddf.analyses))
    object@design@file.index <- orig.file.index
    #Process warnings
    test <- try(object@warnings, silent = TRUE)
    if(class(test) == "list"){
      if(length(object@warnings$message) > 0){
        message("Summary of warnings and errors:")
        for(i in seq(along = object@warnings$message)){
          message(paste(object@warnings$message[[i]], " (occurred ", object@warnings$counter[[i]], " times)"))
        }  
        message("-----")
      }
    }
    return(object)
  }
)







