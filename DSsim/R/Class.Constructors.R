# @include Simulation.R
# @include DS.Analysis.R
# @include Population.Description.R
# @include Density.R
# @include LT.Systematic.Design.R
# @include LT.Random.Design.R
# @include LT.EqAngle.ZZ.Design.R
# @include LT.EqSpace.ZZ.Design.R
# @include LT.User.Specified.Design.R
# @include Region.R


#' Creates a Region object
#'
#' This creates an instance of the Region class. If the user supplied a 
#' shapefile all information will be extracted from here. Otherwise the user
#' needs to specify a list of polygons describing the areas of interest 
#' (coords) and optionally a list of polygons describing the areas to 
#' be excluded (gaps). If area is not specified it will be calculated.
#' 
#' @param region.name the region name
#' @param strata.name the region name
#' @param units the units given as a character (either 'm' or 'km')
#' @param area the area of the region (optional - if not supplied it will be 
#'   calculated for you if you supply a shapefile)
#' @param shapefile a shapefile of the region
#' @param coords list of polygons describing the areas of interest
#' @param gaps list of polygons describing the areas to be excluded
#' @return object of class Region 
#' @export
#' @author Laura Marshall
#' @examples
#' coords <- gaps <- list()
#' coords[[1]] <- list(data.frame(x = c(0,1000,1000,0,0), y = c(0,0,
#'  1000,1000,0)))
#' gaps[[1]] <- list(data.frame(x = c(400,600,500,350,400), y = c(100,
#'  250,600,120,100)))
#' 
#' region <- make.region(region.name = "study.area", units = "m", 
#'  coords = coords, gaps = gaps)
#' plot(region)
#' 
make.region <- function(region.name, strata.name = character(0), units, area = numeric(0), shapefile = NULL, coords = list(), gaps = list()){
    region <- new(Class="Region", region.name = region.name, strata.name = strata.name, units = units, area = area, shapefile = shapefile, coords = coords, gaps = gaps)
  return(region)
}
  
#' Creates a Survey.Design object
#'
#' Currently surveys are only generated within the GIS in Distance. If you 
#' are running a simulation in R you will need to get Distance to 
#' generate all the surveys as shapefiles in advance and supply the path to
#' the directory which contains these shapefiles and only these shapefiles.
#'
#' The \code{design.details} argument should specify a character vector of either 1
#' or 2 elements. These options are described in the table below:
#'
#' \tabular{lll}{ Transect Type \tab Design Details \tab               \cr 
#'                Line          \tab Parallel       \tab Systematic    \cr
#'                Line          \tab Parallel       \tab Random        \cr
#'                Line          \tab Zigzag         \tab Equal Angle   \cr
#'                Line          \tab Zigzag         \tab Equal Spaced  \cr
#'                Line          \tab User Specified \tab               \cr
#'                Point         \tab Systematic     \tab               \cr
#'                Point         \tab Random         \tab               \cr}
#'                                                                                     
#' @param transect.type character variable specifying either "Line" or "Point"
#' @param design.details a character vector describing the type of design. See details section.
#' @param region.obj the name of the Region object where the survey is to be carried out.
#' @param design.axis user may provide the angle of the design axis but not currently used
#' @param spacing user may provide the systematic design spacing but but not currently used
#' @param angle user may provide the design angle (only relevant in equal angle zigzag designs) but not currently used
#' @param plus.sampling logical vaule indicating whether a plus sampling protocol is used but not currently used
#' @param path pathway giving the location of the folder of survey shapefiles 
#' @return object of a class which inherits from class Survey.Design 
#' @export
#' @author Laura Marshall
#' @examples
#' \dontrun{
#' data(transects.shp)
#' #Edit the pathway below to point to an empty folder where the
#' #transect shapefile will be saved
#' shapefile.pathway <- "C:/..."
#' write.shapefile(transects.shp, paste(shapefile.pathway,"/transects_1",
#'  sep = ""))
#' 
#' parallel.design <- make.design(transect.type = "Line", 
#'  design.details = c("Parallel","Systematic"), region = region, 
#'  design.axis = 0, spacing = 100, plus.sampling =FALSE, 
#'  path = shapefile.pathway)
#' }
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
          stop("This design is not currently supported")
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
    stop("Apologies, this design type is not supported at present. You should use the line transect user specified design options.", call. = FALSE)
  }
  return(design)
}

#' Creates a Density object
#' 
#' The user has the option to create a grid describing the density of the 
#' objects and pass this in giving the x and y spacings used in the creation
#' of this grid. Alternatively the user can specify a constant density and x, 
#' y spacings and this grid will be generated automatically. The user may 
#' also supply a \code{mgcv gam} object and x, y spacings and the density grid will 
#' be created from these.
#'
#' @param region.obj the Region object in which the density grid will be created
#' @param density.surface Object of class \code{list}; list of 
#'  data.frames with the columns x, y and density. There must be one 
#'  data.frame for each strata.
#' @param x.space the intervals in the grid in the x direction
#' @param y.space the intervals in the grid in the y direction
#' @param constant a value describing a constant density across the surface.
#' @param density.gam \code{gam} object created using \code{mgcv}
#' @param dsm not currently implemented
#' @param formula not currently implemented
#' @return object of class Density 
#' @export
#' @author Laura Marshall     
#' @seealso \code{\link{make.region}}  
#' @examples                  
#' \dontrun{
#' pop.density <- make.density(region.obj = region, x.space = 10, 
#'  y.space = 10, constant = 0.5) 
#'  
#' pop.density <- add.hotspot(pop.density, centre = c(50, 200), 
#'  sigma = 100, amplitude = 0.1)
#' pop.density <- add.hotspot(pop.density, centre = c(500, 700), 
#'  sigma = 900, amplitude = 0.05)
#' pop.density <- add.hotspot(pop.density, centre = c(300, 100), 
#'  sigma = 100, amplitude = -0.15)
#' 
#' #New plot features
#' plot(pop.density)
#' plot(region, add = TRUE)
#' 
#' #Old style plotting
#' plot(pop.density, contours = FALSE, old.style = TRUE)
#' plot(region, add = TRUE)
#' 
#' }
make.density <- function(region.obj, density.surface = list(), x.space, y.space, constant = numeric(0), density.gam = NULL, dsm = NULL, formula = NULL){
  if(!is.null(constant)){
    if(length(region.obj@strata.name) > 0 & length(constant) != length(region.obj@strata.name)){
      stop("The length of the constant vector does not correspond to the number of strata", call. = FALSE)
    }
  }
  density <- new(Class = "Density", region = region.obj, strata.name = region.obj@strata.name, density.surface = density.surface, x.space = x.space, y.space = y.space, constant = constant, density.gam = density.gam, jit = 1)
 return(density)
}

#' Creates a Population.Description object
#' 
#' Creates an object which describes a population. The values in this object 
#' will be used to create instances of the population
#'
#' @param region.obj the Region object in which this population exists.
#' @param density.obj the Density object describing the distribution of the individuals / clusters.
#' @param cluster.size.table - data.frame with two columns size (giving the cluster sizes) and prob (giving the probability that a cluster is of that size). prob must sum to 1. 
#' @param size.distribution - not yet implemented
#' @param size.param - not yet implemented
#' @param N the number of individuals / clusters in a population
#' @param fixed.N a logical value. If TRUE the population is generated from the value of N otherwise it is generated from the density description.
#' @param average.D not currently implemented.
#' @return object of class Population.Description 
#' @export
#' @author Laura Marshall
#' @seealso \code{\link{make.region}}, \code{\link{make.density}}
#' @examples
#' \dontrun{
#' pop.description <- make.population.description(N = 1000, 
#'  density.obj = pop.density, region = region, fixed.N = TRUE)
#'  }
make.population.description <- make.pop.description <- function(region.obj, density.obj, cluster.size.table = data.frame(NULL), size.distribution = character(0), size.param = numeric(0), N = numeric(0), fixed.N = TRUE, average.D = numeric(0)){
  if(nrow(cluster.size.table) > 0 | length(size.distribution) > 0){   
    cluster.size <- TRUE
  }else{
    cluster.size <- FALSE
  }
  pop.description <- new(Class = "Population.Description", N = N, density = density.obj, region.obj = region.obj, size.table = cluster.size.table, size = cluster.size, gen.by.N = fixed.N)
  return(pop.description)
}

#' Creates a Detectablility object
#' 
#' The detectability of the population is described by the values in this 
#' class.
#'
#' @param key.function specifies shape of the detection function (either 
#'   half-normal "hn" or hazard rate "hr")
#' @param scale.param parameter value associated with the detection function
#' @param shape.param parameter value associated with the detection function
#' @param covariates not yet implemented
#' @param cov.param not yet implemented
#' @param truncation the maximum perpendicular (or radial) distance at which 
#'   objects may be detected from a line (or point) transect.
#' @return object of class Detectablility 
#' @export
#' @author Laura Marshall
#' @examples
#' detect <- make.detectability(key.function = "hn", scale.param = 15,
#'  truncation = 30) 
make.detectability <- function(key.function, scale.param, shape.param = numeric(0), covariates = character(0), cov.param = numeric(0), truncation){
  detectability <- new(Class = "Detectability", key.function = key.function, scale.param = scale.param, shape.param = shape.param, covariates = covariates, cov.param = cov.param, truncation = truncation)
  return(detectability)
}

#' Creates a list of DDF.Analysis objects
#' 
#' This method creates a list of DDF.Analysis objects each of which describes 
#' a model to fit to the distance data. The simulation will fit each of these 
#' models to the data generated in the simulation and select the model with 
#' the minimum criteria value.
#'
#' @param dsmodel list of distance sampling model formula specifying the detection function (see \code{?ddf} for further details)
#' @param mrmodel not yet implemented
#' @param method character only "ds" normal distance sampling currently implemented
#' @param criteria character model selection criteria (AIC, AICc, BIC) - only AIC implemented at present.
#' @param truncation numeric truncation distance for analyses
#' @param binned.data logical whether the data should be analsed in bins
#' @param cutpoints gives the cutpoints of the binned data
#' @return list of objects of class DDF.Analysis 
#' @export
#' @author Laura Marshall
#' @seealso \code{ddf} in \code{library(mrds)}
#' @examples
#' ddf.analyses <- make.ddf.analysis.list(dsmodel = list(~cds(key = "hn",
#'  formula = ~1),~cds(key = "hr", formula = ~1)), method = "ds", 
#'  criteria = "AIC")
#'
make.ddf.analysis.list <- function(dsmodel, mrmodel = NULL, method, criteria = "AIC", truncation = numeric(0), binned.data = FALSE, cutpoints = numeric(0)){
  ddf.analyses <- list()
  if(method == "ds"){
    for(a in seq(along = dsmodel)){
      ddf.analyses[[a]] <- new(Class = "DS.Analysis", dsmodel = dsmodel[[a]], criteria = criteria, truncation = truncation, binned.data = binned.data, cutpoints = cutpoints)
    }
  }else{
    stop("Double observer methods are not yet implemented", call. = FALSE)
  }
  return(ddf.analyses)
}

#' Creates a Simulation object
#' 
#' This creates a simulation object which groups together all the objects 
#' needed to complete the simulation. 
#'
#' @param reps number of times the simulation should be repeated
#' @param single.transect.set logical specifying whether the transects should
#'   be kept the same throughout the simulation.
#' @param double.observer not currently implemented.
#' @param region.obj an object of class Region
#' @param design.obj an object of class Survey.Design
#' @param population.description.obj an object of class Population.Description
#' @param detectability.obj and object of class Detectabolity
#' @param ddf.analyses.list a list of objects of class DDF.Analysis
#' @return object of class Simulation 
#' @export
#' @author Laura Marshall
#' @examples
#' coords <- gaps <- list()
#' coords[[1]] <- list(data.frame(x = c(0,1000,1000,0,0), y = c(0,0,
#'  1000,1000,0)))
#' gaps[[1]] <- list(data.frame(x = c(400,600,500,350,400), y = c(100,
#'  250,600,120,100)))
#' 
#' region <- make.region(region.name = "study.area", units = "m", 
#'  coords = coords, gaps = gaps)
#' plot(region)
#' 
#' \dontrun{
#' data(transects.shp)
#' #Edit the pathway below to point to an empty folder where the
#' #transect shapefile will be saved
#' shapefile.pathway <- "C:/..."
#' write.shapefile(transects.shp, paste(shapefile.pathway,"/transects_1",
#'  sep = ""))
#' 
#' parallel.design <- make.design(transect.type = "Line", 
#'  design.details = c("Parallel","Systematic"), region = region, 
#'  design.axis = 0, spacing = 100, plus.sampling =FALSE, 
#'  path = shapefile.pathway)
#' 
#' pop.density <- make.density(region.obj = region, x.space = 10, 
#'  y.space = 10, constant = 0.5) 
#' pop.density <- add.hotspot(pop.density, centre = c(50, 200), 
#'  sigma = 100, amplitude = 0.1)
#' pop.density <- add.hotspot(pop.density, centre = c(500, 700), 
#'  sigma = 900, amplitude = 0.05)
#' pop.density <- add.hotspot(pop.density, centre = c(300, 100), 
#'  sigma = 100, amplitude = -0.15)
#' 
#' plot(pop.density)
#' plot(region, add = TRUE)
#' 
#' pop.description <- make.population.description(N = 1000, 
#'  density.obj = pop.density, region = region, fixed.N = TRUE)
#' 
#' detect <- make.detectability(key.function = "hn", scale.param = 15,
#'  truncation = 30) 
#' 
#' ddf.analyses <- make.ddf.analysis.list(dsmodel = list(~cds(key = "hn",
#'  formula = ~1),~cds(key = "hr", formula = ~1)), method = "ds", 
#'  criteria = "AIC")
#' 
#' my.simulation <- make.simulation(reps = 10, single.transect.set = TRUE,
#'  region.obj = region, design.obj = parallel.design, 
#'  population.description.obj = pop.description, 
#'  detectability.obj = detect, ddf.analyses.list = ddf.analyses)
#' 
#' survey.results <- create.survey.results(my.simulation, dht.table = TRUE)
#' 
#' plot(survey.results)
#' 
#' my.simulation <- run(my.simulation)
#' 
#' summary(my.simulation)
#' }
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





