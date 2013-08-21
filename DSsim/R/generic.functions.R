#' S4 generic method to generate an instance of a population
#'
#' Uses the population description and detectability details to generate an 
#' instance of the population. Note that if the first argument supplied is 
#' of class Population.Description rather than class Simulation then a second 
#' argument \code{detectabity} must also be supplied and must be an object of 
#' class Detectability.
#'
#' @param object an object of class Simulation or Population.Description
#' @param detectability object of class detectability (optional - only
#'   required if object is of class Population.Description) 
#' @usage generate.population(object, ...)
#' @return an object of class Population
#' @export
#' @docType methods
setGeneric("generate.population", function(object, ...){standardGeneric ("generate.population")})

#' S4 generic method to generate an instance of a design
#'
#' Uses the Survey.Design details to generate transects. Currenty this 
#' involves loading a survey shapefile from the path specified in the 
#' Survey.Design object.
#'
#' @param object an object of class Simulaiton or a class which inherits from 
#'   Survey.Design
#' @param read.from.file if the transect details should be read in from file 
#'   (currently must be TRUE)
#' @param write.to.file not currently implemented
#' @param region optional only required if object is of class Survey.Design.
#' @usage generate.transects(object, read.from.file, write.to.file, region)
#' @return an object of class Population
#' @export
#' @docType methods
setGeneric("generate.transects", function(object, read.from.file = TRUE, write.to.file = FALSE, region = NULL, ...){standardGeneric ("generate.transects")})

#' S4 generic method to generate a region table
#'
#' Generates a region table required to estimate abundance /  density via the
#' Hortvitz-Thompson estimator.
#'
#' @param object an object of a class inheriting from Survey
#' @param object an object of class Region
#' @usage create.region.table(object, region)
#' @return an object of class Region.Table
#' @docType methods
setGeneric(name = "create.region.table", def = function(object, region){standardGeneric ("create.region.table")})

#' S4 generic method to generate a sample table
#'
#' Generates a sample table required to estimate abundance /  density via the
#' Hortvitz-Thompson estimator.
#'
#' @param object an object of a class inheriting from Survey
#' @usage create.region.table(object, region)
#' @return an object of class Region.Table
#' @docType methods
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
#' @usage simulate.survey(object, dht.tables, ...)
#' @return an object of class LT.Survey.Results
#' @export
#' @docType methods
setGeneric(name = "simulate.survey", def = function(object, dht.tables = FALSE, ...){standardGeneric ("simulate.survey")})


#' S4 generic method to extract distance data
#'
#' Extracts distance data from a Survey.Results object
#'
#' @param object an object of class LT.Survey.Results
#' @usage get.distance.data(object)
#' @return a data.frame describing the distance data
#' @export
#' @docType methods
setGeneric(name = "get.distance.data", def = function(object){standardGeneric ("get.distance.data")})

#' S4 generic method to displays a summary of a Simulation object
#'
#' If the simulation has been run this will include a summary of the results.
#'
#' @param object an object of class simulation
#' @usage summary(object, ...)
#' @export
#' @author Laura Marshall 
#' @docType methods
setGeneric("summary")

#' setGeneric(name = "summary", def = function(object, ...){standardGeneric ("summary")})


#' S4 generic method to add a hotspot to the density grid
#'
#' Uses a Gaussian decay around a central location to add a hotspot to the 
#' density grid.
#'
#' @param object an object of class Density or Simulation
#' @param centre an x,y-coordinate giving the centre of the hotspot
#' @param sigma a value giving the scale parameter for a gaussian decay
#' @param amplitude the height of the hotspot at its centre
#' @usage add.hotspot(object, centre, sigma, amplitude)
#' @return the updated Density or Simulation object
#' @export
#' @docType methods
setGeneric(name = "add.hotspot", def = function(object, centre, sigma, amplitude){standardGeneric ("add.hotspot")})


#' S4 generic method to run a simulation
#' 
#' Runs the simulation and returns the simulation object with results. If
#' running in parallel and max.cores is not specified it will default to using
#' one less than the number of cores / threads on your machine.
#'
#' @param object an object of class simulation
#' @param logical option to use multiple processors
#' @param integer maximum number of processors to use
#' @usage run(object, ...)
#' @return an object of class simulation which now includes the results
#' @export
#' @docType methods
setGeneric(name = "run", def = function(object, run.parallel = FALSE, max.cores = NA){standardGeneric ("run")})


#' S4 generic method to run analyses
#'
#' This method carries out an analysis of distance sampling data. This method
#' is provided to allow the user to perform diagnostic of the analysis used
#' in the simulation. The data argument can be obtained by a call to
#' \code{simulate.survey(object, dht.table = TRUE)}.
#'
#' @param object an object of class Simulation
#' @param data an object of class Survey.Results
#' @usage run.analysis(object, data, ...)
#' @return a list containing an S3 ddf object and optionally an S3 dht object relating to the model with the miminum criteria.
#' @export
#' @docType methods
setGeneric(name = "run.analysis", def = function(object, data, ...){standardGeneric ("run.analysis")})


