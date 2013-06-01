




#' S4 generic method to run a simulation
#'
#' @param object an object of class simulation
#' @usage run(object, ...)
#' @return an object of class simulation which now includes the results
#' @export
#' @docType methods
setGeneric(name = "run", def = function(object, ...){standardGeneric ("run")})

#' S4 generic method to run analyses
#'
#' This method carries out an analysis of distance sampling data. This method
#' is provided to allow the user to perform diagnostic of the analysis used
#' in the simulation. The data argument can be obtained by a call to
#' \code{simulate.survey(object, dht.table = TRUE)}.
#'
#' @param object an object of class simulation
#' @param data an object of class Survey.Results
#' @usage run.analysis(object, data, ...)
#' @return a list containing an S3 ddf object and optionally an S3 dht object of
#'    relating to the model with the miminum criteria.
#' @export
#' @docType methods
setGeneric(name = "run.analysis", def = function(object, data, ...){standardGeneric ("run.analysis")})

#' S4 generic get method 
#'
#' Accesses the information on the selection criteria
#'
#' @param object an object of class simulation or DDF.Analysis
#' @usage get.criteria(object, ...)
#' @return the name of the criteria used for model selection
#' @export
#' @docType methods
setGeneric(name = "get.criteria", def = function(object,  ...){standardGeneric ("get.criteria")})

#' S4 generic method to generate an instance of a population
#'
#' Uses the population description and detectability details to generate an 
#' instance of the population. 
#'
#' Note that if the first argument supplied is of class Population.Description
#' rather than simulation then a second argument \code{detectabity} must also 
#' be supplied and must be an object of class Detectability.
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
#' Uses the Survey.Design details to generate transects. 
#'
#' @param object an object of class Simulaiton or a class which inherits from 
#'   Survey.Design
#' @param read.from.file if the transect details should be read in from file 
#'   (currently must be TRUE)
#' @param write.to.file not currently implemented
#' @param region optional if object is of class Survey.Design. If left blank
#'   the method will load the object from the global environment based on the 
#'   region object name inside the Survey.Design object.
#' @usage generate.transects(object, read.from.file, write.to.file, region)
#' @return an object of class Population
#' @export
#' @docType methods
setGeneric("generate.transects", function(object, read.from.file = TRUE, write.to.file = FALSE, region = NULL){standardGeneric ("generate.transects")})

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
#'
#' @param object an object of class Simulation
#' @param dht.tables logical value indicating whether or the data tables for 
#'   Hortvitz-Thompson estimation are required.
#' @usage simulate.survey(object, dht.tables, ...)
#' @return an object of class Region.Table
#' @export
#' @docType methods
setGeneric(name = "simulate.survey", def = function(object, dht.tables, ...){standardGeneric ("simulate.survey")})


#' S4 generic method to access the selection criteria
#'
#' Provides the selection criteria value
#'
#' @param object an object of class Simulation or DDF.Analysis
#' @usage get.criteria(object)
#' @return the model selection criteria value
#' @docType methods
setGeneric(name = "get.criteria", def = function(object){standardGeneric ("get.criteria")})


#' S4 generic method to simulate a survey
#'
#' Simulates the process by which individuals / clusters are detected.
#'
#' @param object an object of class Simulation
#' @param dht.tables logical value indicating whether or the data tables for 
#'   Hortvitz-Thompson estimation are required.
#' @usage simulate.survey(object, dht.tables, ...)
#' @return an object of class Region.Table
#' @export
#' @docType methods
#setGeneric(name = "get.table", def = function(object){standardGeneric ("get.table")})


#' S4 generic method to display a histogram of Abundance estimates
#'
#' Displays a histogram of the abundance estimates. Truth is indicated by
#' the red line
#'
#' @param x an object of class simulation
#' @usage hist(x, ...)
#' @export
#' @author Laura Marshall 
#' @docType methods
setGeneric(name = "hist", def = function(x, ...){standardGeneric ("hist")})
#' S4 generic method to displays a summary of the simulation
#'
#' If the simulation has been run this will include a summary of the results
#'
#' @param object an object of class simulation
#' @usage summary(object, ...)
#' @export
#' @author Laura Marshall 
#' @docType methods
#' setGeneric(name = "summary", def = function(object, ...){standardGeneric ("summary")})

#' S4 generic method to access the selection criteria
#'
#' Provides the selection criteria value
#'
#' @param object an object of class Simulation or DDF.Analysis
#' @usage get.criteria(object)
#' @return the model selection criteria value
#' @docType methods
setGeneric(name = "add.hotspot", def = function(object, centre, radius, amplitude){standardGeneric ("add.hotspot")})

