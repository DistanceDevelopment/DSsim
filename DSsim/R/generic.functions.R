#' S4 generic method to generate an instance of a population
#'
#' Uses the population description and detectability details to generate an 
#' instance of the population. Note that if the first argument supplied is 
#' of class Population.Description rather than class Simulation then a second 
#' argument \code{detectabity} must also be supplied and must be an object of 
#' class Detectability.
#'
#' @param object an object of class Simulation or Population.Description
#' @param detectability object of class Detectability (optional - only
#'   required if object is of class Population.Description) 
#' @usage generate.population(object, ...)
#' @return an object of class Population
#' @export
#' @docType methods
#' @rdname generate.population-methods
setGeneric("generate.population", function(object, ...){standardGeneric ("generate.population")})

#' S4 generic method to generate an instance of a design
#'
#' Uses the Survey.Design details to generate transects. Currenty this 
#' involves loading a survey shapefile from the path specified in the 
#' Survey.Design object and can only work with line transect designs.
#'
#' @param object an object of class Simulaiton or a class which inherits from 
#'   Survey.Design
#' @param read.from.file if the transect details should be read in from file 
#'   (currently must be TRUE)
#' @param write.to.file not currently implemented
#' @param region optional only required if object is of class Survey.Design.
#' @usage generate.transects(object, read.from.file, write.to.file, region)
#' @return an object of class Line.Transect
#' @export
#' @docType methods
#' @rdname generate.transects-methods
setGeneric("generate.transects", function(object, read.from.file = TRUE, write.to.file = FALSE, region = NULL, ...){standardGeneric ("generate.transects")})

#' S4 generic method to generate a region table
#'
#' This function is called internally to generate a region table required to
#' estimate abundance /  density via the Hortvitz-Thompson estimator.
#'
#' @param object an object of a class inheriting from Survey
#' @param region an object of class Region
#' @usage create.region.table(object, region)
#' @return an object of class Region.Table
#' @docType methods
#' @rdname create.region.table-methods
setGeneric(name = "create.region.table", def = function(object, region){standardGeneric ("create.region.table")})

#' S4 generic method to generate a sample table
#'
#' This function is called internally to generates a sample table required to
#' estimate abundance /  density via the Hortvitz-Thompson estimator.
#'
#' @param object an object of a class inheriting from Survey
#' @usage create.region.table(object, region)
#' @return an object of class Sample.Table
#' @docType methods
#' @rdname create.sample.table-methods
setGeneric(name = "create.sample.table", def = function(object){standardGeneric ("create.sample.table")}) 

#' S4 generic method to simulate a survey
#'
#' Simulates the process by which individuals / clusters are detected. 
#' Currently this is only implemented for line transect surveys. It returns an 
#' object of class LT.Survey.Results which contains a population, a set of 
#' transects, distance data and if requested region, sample and obs tables. 
#' 
#' This object can be displayed using \code{plot()} or the distance data
#' extracted using \code{get.distance.data()}. You can then investigate 
#' fitting models to this data.
#'
#' @param object an object of class Simulation
#' @param dht.tables logical value indicating whether or the data tables for 
#'   Hortvitz-Thompson estimation are required.
#' @usage create.survey.results(object, dht.tables, ...)
#' @return an object of class LT.Survey.Results
#' @export
#' @docType methods
#' @rdname create.survey.results-methods
#' @examples
#' \dontrun{
#' survey.results <- create.survey.results(simulation, dht.table = TRUE)
#' 
#' plot(survey.results)
#' }
#' 
setGeneric(name = "create.survey.results", def = function(object, dht.tables = FALSE, ...){standardGeneric ("create.survey.results")})

#' S4 generic method to extract distance data
#'
#' Extracts distance data from a Survey.Results object
#'
#' @param object an object of class LT.Survey.Results
#' @usage get.distance.data(object)
#' @return a data.frame describing the distance data
#' @export
#' @docType methods
#' @rdname get.distance.data-methods
#' @seealso \code{\link{create.survey.results}}
setGeneric(name = "get.distance.data", def = function(object){standardGeneric ("get.distance.data")})

#' S4 generic method to add a hotspot to the density grid
#'
#' Uses a Gaussian decay around a central location to add a hotspot to the 
#' density grid.
#'
#' @param object an object of class Density or Simulation
#' @param centre an x,y-coordinate giving the centre of the hotspot
#' @param sigma a value giving the scale parameter for a gaussian decay
#' @param amplitude the height of the hotspot at its centre
#' @return the updated Density or Simulation object
#' @export
#' @docType methods
#' @rdname add.hotspot-methods
#' @seealso \code{\link{make.density}}
setGeneric(name = "add.hotspot", def = function(object, centre, sigma, amplitude){standardGeneric ("add.hotspot")})

#' S4 generic method to run a simulation
#' 
#' Runs the simulation and returns the simulation object with results. If
#' running in parallel and max.cores is not specified it will default to using
#' one less than the number of cores / threads on your machine.
#'
#' @param object an object of class Simulation
#' @param logical option to use multiple processors
#' @param integer maximum number of cores to use, if not specified then
#' one less than the number available will be used.
#' @usage run(object, ...)
#' @return an object of class simulation which now includes the results
#' @export
#' @docType methods
#' @rdname run-methods
#' @seealso \code{\link{make.simulation}}
setGeneric(name = "run", def = function(object, run.parallel = FALSE, max.cores = NA){standardGeneric ("run")})

#' S4 generic method to run analyses
#'
#' This method carries out an analysis of distance sampling data. This method
#' is provided to allow the user to perform diagnostics of the analyses used
#' in the simulation. The data argument can be obtained by a call to
#' \code{simulate.survey(object, dht.table = TRUE)}. Note if the first object
#' supplied is of class DDf.Analysis then the second argument must be of class
#' DDf.Data. The data argument may be of either class for an object argument
#' of class Simulation.
#'
#' @param object an object of class Simulation or DDF.Analysis
#' @param data an object of class Survey.Results or DDF.Data
#' @usage run.analysis(object, data, ...)
#' @return a list containing an S3 ddf object and optionally an S3 dht object relating to the model with the miminum criteria.
#' @export
#' @docType methods
#' @rdname run.analysis-methods
setGeneric(name = "run.analysis", def = function(object, data, ...){standardGeneric ("run.analysis")})


if (!isGeneric("plot")){
  setGeneric(name = "plot", def = function(x, y, ...){standardGeneric("plot")})
}

if (!isGeneric("summary")){
  setGeneric(name = "summary", def = function(object, ...){standardGeneric("summary")})
}

if (!isGeneric("show")){
  setGeneric(name = "show", def = function(object){standardGeneric("show")})
}


