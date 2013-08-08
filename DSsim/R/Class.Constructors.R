#' Creates an object of class Region
#'
#' This creates an instance of the Region class. If the user supplied a 
#' shapefile all information will be extracted from here. Otherwise the user
#' needs to specify a list of polygons describing the areas of interest (coords) and 
#' optionally a list of polygons describing the areas to be excluded (gaps).
#' 
#' @param region.name the region name
#' @param strata.name the region name
#' @param area the area of the region (optional - if not supplied it will be 
#'   calculated for you if you supply a shapefile)
#' @param shapefile a shapefile of the region
#' @param coords list of polygons describing the areas of interest
#' @param gaps list of polygons describing the areas to be excluded
#' @return object of either class Region 
#' @export
#' @author Laura Marshall
#'
make.region <- function(region.name, strata.name = character(0), no.strata = numeric(0), units, area = numeric(0), shapefile = NULL, coords = list(), gaps = list()){
    region <- new(Class="Region", region.name = region.name, strata.name = strata.name, units = units, area = area, shapefile = shapefile, coords = coords, gaps = gaps)
  return(region)
}
  
#' Creates an object of a class which inherits from class Survey.Design
#'
#' Currently surveys are only generated within the GIS in Distance therefore
#' if you are running a simulation in R you will need to get Distance to 
#' generate all the surveys as shapefiles in advance and supply the path to
#' these files.
#'
#' The \code{design.details} argument should specify a list of either 1
#' or 2 elements. These options are described in the table below:
#'
#' \tabular{lll}{ Transect Type \tab Design Details \tab              \tab \cr 
#'                Line          \tab Parallel       \tab Systematic   \tab \cr
#'                Line          \tab Parallel       \tab Random       \tab \cr
#'                Line          \tab Zigzag         \tab Equal Angle  \tab \cr
#'                Line          \tab Zigzag         \tab Equal Spaced \tab \cr
#'                Point         \tab Systematic     \tab              \tab \cr
#'                Point         \tab Random         \tab              \tab \cr}
#'                                                                                     
#' @param transect.type character variable specifying either "Line" or "Point"
#' @param design.details a list describing the type of design. See details.
#' @param region the name of the Region object where the survey is to be carried out.
#' @param design.axis - user may provide details but not currently used
#' @param spacing - user may provide details but not currently used
#' @param plus.sampling - user may provide details but not currently used
#' @param path file pathway giving the location of the survey shapefiles 
#' @return object of a class which inherits from class Survey.Design 
#' @export
#' @author Laura Marshall
#'
make.design <- function(transect.type, design.details, region.obj, design.axis = numeric(0), spacing = numeric(0), angle = numeric(0), plus.sampling = logical(0), path = character(0)){
  region <- global.name <- deparse(substitute(region.obj))
  #if(class(region) != "character"){
  #  message("Error: the region argument is not of class character. Only the object name should be provided using quotes.")
  #  return(NULL)
  #}             
  design <- NULL
  if(transect.type %in% c("Line", "line", "Line Transect", "line transect")){
    if(length(design.details) == 1){
      if(length(design.details) == 1 & design.details %in% c("user specified", "User Specified")){
        design <- new(Class = "LT.User.Specified.Design", region = region, design.axis = design.axis, spacing = spacing, plus.sampling = plus.sampling, path = path)
      }
    }else if(length(design.details) == 2){
      if(design.details[1] %in% c("ZZ", "ZigZag", "Zigzag", "zigzag")){
        if(design.details[2] %in% c("Equal Spaced", "ES", "equal spaced", "Equal spaced")){
          design <- new(Class = "LT.EqSpace.ZZ.Design", region = region, design.axis = design.axis, spacing = spacing, plus.sampling = plus.sampling, path = path)
        }
        if(design.details[2] %in% c("Equal Angle", "equal angle", "Equal angle")){               #error in this no spacing only angle
          design <- new(Class = "LT.EqAngle.ZZ.Design", region = region, design.axis = design.axis, angle = angle, plus.sampling = plus.sampling, path = path)
        }
      }
      if(design.details[1] %in% c("P", "Parallel", "parallel")){
        if(design.details[2] %in% c("Random", "random")){
          design <- new(Class = "LT.Random.Design", region = region, design.axis = design.axis, spacing = spacing, plus.sampling = plus.sampling, path = path)
        }
        if(design.details[2] %in% c("Systematic", "systematic")){
          design <- new(Class = "LT.Systematic.Design", region = region, design.axis = design.axis, spacing = spacing, plus.sampling = plus.sampling, path = path)
        }
      }
    }
  }
  if(is.null(design)){
    message("Apologies, this design type is not supported at present.")
  }
  return(design)
}

#' Creates an object of class Density
#' 
#' The user has the option to create a grid describing the density of the 
#' objects and pass this in giving the x and y spacings used in the creation
#' of this grid.
#'
#' Alternatively the user can specify a constant density and x, y spacings
#' and this grid will be generated automatically.
#'
#' Further options will be added to allow the creation of non-constant surfaces
#' within this method.
#'
#' @param region the name of the Region object in which this population exists.
#' @param density.surface a dataframe describing the density with columns
#'   x, y,  and density.
#' @param x.space the intervals in the grid in the x direction
#' @param y.space the intervals in the grid in the y direction
#' @param constant a value describing a constant density across the surface.
#' @param shapefile - not yet implemented
#' @param density.gam - not yet implemented
#' @param jit a value descibing how many positions the grid should be moved to 
#'   ensure that the grid covers the entire survey region.
#' @return object of either class Density 
#' @export
#' @author Laura Marshall                         
#'
make.density <- function(region.obj, density.surface = list(), x.space, y.space, constant = numeric(0), density.gam = NULL, dsm = NULL, formula = NULL){
  if(!is.null(constant)){
    if(length(region@strata.name) > 0 & length(constant) != length(region@strata.name)){
      message("Error: the length of the constant vector does not correspond to the number of strata")
    }
  }
  density <- new(Class = "Density", region = region, strata.name = region@strata.name, density.surface = density.surface, x.space = x.space, y.space = y.space, constant = constant, density.gam = density.gam, jit = 1)
 return(density)
}

#' Creates an object of class Population.Description
#'
#' @param N the number of individuals / clusters in a population
#' @param density.obj an object of class Density describing the density of the
#'   individuals / clusters.
#' @param region the name of the Region object in which this population exists.
#' @param cluster.size.table - data.frame with two columns size (giving the 
#'   cluster sizes) and prob (giving the probability that that size of cluster
#'   appears). prob must sum to 1. 
#' @param size.min - not yet implemented
#' @param size.max - not yet implemented
#' @param size.lambda - not yet implemented
#' @param gen.by.N a logical value. If TRUE the population is generated from the
#'   value in N otherwise it is generated from the density description (not yet
#'   implemented)
#' @return object of either class Density 
#' @export
#' @author Laura Marshall
#'
make.population.description <- make.pop.description <- function(region.obj, density.obj, cluster.size.table = data.frame(NULL), size.distribution = character(0), size.param = numeric(0), N = numeric(0), fixed.N = TRUE, average.D = numeric(0)){
  if(nrow(cluster.size.table) > 0 | length(size.distribution) > 0){   
    cluster.size <- TRUE
  }else{
    cluster.size <- FALSE
  }
  pop.description <- new(Class = "Population.Description", N = N, density = density.obj, region.obj = region.obj, size.table = cluster.size.table, size = cluster.size, gen.by.N = fixed.N)
  return(pop.description)
}

#' Creates an object of class Detectablility
#'
#' @param key.function specifies shape of the detection function (either 
#'   half-normal or hazard rate)
#' @param scale.param parameters associated with the detection function
#' @param shape.param parameters associated with the detection function
#' @param adjustment - not yet implemented
#' @param adj.param - not yet implemented
#' @param covariates - not yet implemented
#' @param cov.param - not yet implemented
#' @param perp.truncation the maximum perpendicular distance at which objects
#'   may be detected. Must be specified for line transect surveys.
#' @param rad.truncation  the maximum radial distance at which objects may be
#'   detected. Must be specified for both line transect and point transect 
#'   surveys.
#' @return object of either class Detectablility 
#' @export
#' @author Laura Marshall
#'
make.detectability <- function(key.function, scale.param, shape.param = numeric(0), covariates = character(0), cov.param = numeric(0), truncation){
  detectability <- new(Class = "Detectability", key.function = key.function, scale.param = scale.param, shape.param = shape.param, covariates = covariates, cov.param = cov.param, truncation = truncation)
  return(detectability)
}

#' Creates an object of a class which inherits from class DDF.Analysis
#'
#' @param dsmodel distance sampling model for the detection function (see
#'   \code{?ddf} for further details)
#' @param mrmodel mark-recapture model specification (see \code{?ddf} for further
#'   details.
#' @param criteria model selection criteria (AIC, AICc, BIC) - only AIC implemented
#'   at present.
#' @return object of either class DS.Analysis or MRDS.Analysis 
#' @export
#' @author Laura Marshall
#' @seealso \code{ddf} in \code{library(mrds)},
#'
make.ddf.analysis.list <- function(dsmodel, mrmodel = NULL, method, criteria){
  ddf.analyses <- list()
  if(method == "ds"){
    for(a in seq(along = dsmodel)){
      ddf.analyses[[a]] <- new(Class = "DS.Analysis", dsmodel = dsmodel[[a]], criteria = criteria)
    }
  }else{
    message("Double observer methods are not yet implemented")
    return(NULL)
  }
  return(ddf.analyses)
}

#' Creates an object of class Simulation
#'
#' @param reps number of times the simulation should be repeated
#' @param single.transect.set logical specifying whether the transects should
#'   be kept the same throughout the simulation.
#' @param double.observer logical value indicating TRUE for a double observer
#'   mark recapture survey or FALSE for normal distance sampling.
#' @param region.obj an object of class Region
#' @param design.obj an object of class Survey.Design
#' @param population.description.obj an object of class Population.Description
#' @param detectability.obj and object of class Detectabolity
#' @param ddf.analyses.list a list of objects of class DS.Analysis
#' @return object of class Simulation 
#' @export
#' @author Laura Marshall
#'
make.simulation <- function(reps, single.transect.set = FALSE, double.observer = FALSE, region.obj, design.obj, population.description.obj, detectability.obj, ddf.analyses.list){
  #Make the results arrays and store in a list
  no.strata <- ifelse(length(region.obj@strata.name) > 0, length(region.obj@strata.name)+1, 1) 
  if(length(region.obj@strata.name) > 0){
    strata.name <- c(sort(region.obj@strata.name), "Total")
  }else{
    strata.name <- region.obj@region.name
  }
  individuals <- list(summary = array(NA, dim = c(no.strata, 7, reps+2), dimnames = list(strata.name, c("Area", "CoveredArea", "Effort", "n", "ER", "se.ER", "cv.ER"), c(1:reps,"mean","sd"))), 
                  N = array(NA, dim = c(no.strata, 6, reps+2), dimnames = list(strata.name, c("Estimate", "se", "cv", "lcl", "ucl", "df"), c(1:reps,"mean","sd"))), 
                  D = array(NA, dim = c(no.strata, 6, reps+2), dimnames = list(strata.name, c("Estimate", "se", "cv", "lcl", "ucl", "df"), c(1:reps,"mean", "sd"))))
  detection = array(NA, dim = c(1, 4, reps+2), dimnames = list("Pooled", c("True.Pa", "Pa", "ESW", "f(0)"), c(1:reps,"mean","sd")))
  #create additional arrays if animals are in clusters
  if(population.description.obj@size){
    clusters <- list(summary = array(NA, dim = c(no.strata, 8, reps+2), dimnames = list(strata.name, c("Area", "CoveredArea", "Effort", "n", "k", "ER", "se.ER", "cv.ER"), c(1:reps,"mean","sd"))), 
                    N = array(NA, dim = c(no.strata, 6, reps+2), dimnames = list(strata.name, c("Estimate", "se", "cv", "lcl", "ucl", "df"), c(1:reps,"mean","sd"))), 
                    D = array(NA, dim = c(no.strata, 6, reps+2), dimnames = list(strata.name, c("Estimate", "se", "cv", "lcl", "ucl", "df"), c(1:reps,"mean", "sd"))))
    expected.size <- array(NA, dim = c(no.strata, 3, reps+2), dimnames = list(strata.name, c("Expected.S", "se.Expected.S", "cv.Expected.S"), c(1:reps,"mean","sd")))
    results <- list(individuals = individuals, clusters = clusters, expected.size = expected.size, Detection = detection)
  }else{
    results <- list(individuals = individuals, Detection = detection)
  }
  #create a simulation object
  simulation <- new(Class = "Simulation", reps = reps, single.transect.set = single.transect.set, double.observer = double.observer, region = region.obj, design = design.obj, population.description = population.description.obj, detectability = detectability.obj, ddf.analyses = ddf.analyses.list, results = results)
  return(simulation)
}





