#' @include Simulation.R
#' @include Region.R
#' @include Population.Description.R
#' @include Density.R
#' @include Survey.Design.R
#' @include PT.Design.R
#' @include LT.Design.R
#' @include LT.Systematic.Design.R
#' @include LT.Random.Design.R
#' @include LT.EqAngle.ZZ.Design.R
#' @include LT.EqSpace.ZZ.Design.R
#' @include LT.User.Specified.Design.R
#' @include PT.Random.Design.R
#' @include PT.Systematic.Design.R
#' @include PT.Nested.Design.R
#' @include DS.Analysis.R

#' @title Creates a Region object
#' @description This creates an instance of the Region class. If the user supplied a 
#' shapefile all information will be extracted from here. Otherwise the user
#' needs to specify a list of polygons describing the areas of interest 
#' (coords) and optionally a list of polygons describing the areas to 
#' be excluded (gaps). If area is not specified it will be calculated.
#' @param region.name the region name
#' @param strata.name the strata names. If not supplied when there are 2 or mode strata default names of "A", "B", "C"... will be assigned.
#' @param units the units given as a character (either 'm' or 'km')
#' @param area the area of the region (optional - if not supplied it will be 
#'   calculated for you)
#' @param shapefile a shapefile of the study region. These can be loaded using the \code{read.shapefile} function in the shapefiles library.
#' @param coords A list with one element per strata. Each element in the list is a list of dataframes describing the polygon coordinates. This allows multiple regions in each strata. The corrdinates should start and finish with the same point. By default DSsim will create a rectangular study region 2000 m by 500 m.
#' @param gaps A list with one element per strata. Each element in the list is a list of dataframes describing the polygon coordinates. This allows multiple gaps in each strata. The corrdinates should start and finish with the same point.
#' @param check.LinkID boolean to check the order of the LinkID value in the attribute table. This is important if this shapefile was used in Distance to create the survey shapefiles as Distance would have re-ordered the strata in this way. Failing to re-order the strata will mean that the strata in DSsim will not match the transect strata ID values created by Distance. If you have created your surveys outside Distance you can turn this option off.
#' @return object of class Region 
#' @export
#' @author Laura Marshall
#' @examples
#' # A basic study region of 2000m by 500m is created using the defaults
#' region <- make.region()
#' plot(region)
#' 
#' # Here is an example of a study region with gaps
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
make.region <- function(region.name = "region", 
                        strata.name = character(0), 
                        units = "m", 
                        area = numeric(0), 
                        shapefile = NULL, 
                        coords = coords <- list(list(data.frame(x = c(0, 0 , 2000, 2000, 0), y = c(0, 500, 500, 0, 0)))), 
                        gaps = list(), 
                        check.LinkID = TRUE){
  # If the user hasn't specified gaps set up an empty list
  if(length(gaps) == 0){
    for(i in seq(along = coords)){
      gaps[[i]] <- list()
    }
  }
  # If there is more than one strata and the user has not specified strata.name
  if(length(coords) > 1 & length(strata.name) == 0){
    no.strata <- length(coords)
    if(no.strata < 27){
      strata.name <- LETTERS[1:no.strata]   
    }else{
      stop("Too many strata (>26) for strata names to be assigned default names.", call. = FALSE)
    }
  }
  # Call to make the region object
  region <- new(Class="Region", region.name = region.name, strata.name = strata.name, units = units, area = area, shapefile = shapefile, coords = coords, gaps = gaps, check.LinkID = check.LinkID)
  return(region)
}
  
#' @title  Creates a Survey.Design object
#' @description Currently surveys are only generated within the GIS in Distance. If you 
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
#'                Point         \tab Random         \tab               \cr
#'                Point         \tab Nested         \tab               \cr}
#'                                                                                     
#' @param transect.type character variable specifying either "Line" or "Point"
#' @param design.details a character vector describing the type of design. See details section.
#' @param region.obj the character name of the Region object where the survey is to be carried out.
#' @param design.axis user may provide the angle of the design axis but not currently used
#' @param spacing user may provide the systematic design spacing but but not currently used
#' @param nested.space the number of spaces between nested points. If spacing = 1 then all points on the systematic design will be sepected.
#' @param no.complex the number of complex detectors to distribute based on simple random sampling of the systematic grid of detectors.
#' @param angle user may provide the design angle (only relevant in equal angle zigzag designs) but not currently used
#' @param plus.sampling logical vaule indicating whether a plus sampling protocol is used but not currently used
#' @param path pathway giving the location of the folder of survey shapefiles 
#' @return object of a class which inherits from class Survey.Design 
#' @export
#' @author Laura Marshall
#' @examples
#' # DSsim can generate a systematic grid of point transects
#' design <- make.design("point")
#' 
#' # The easiest way to generate the transect is by creating a simulation
#' sim <- make.simulation(design.obj = make.design("point"))
#' transects <- generate.transects(sim)
#' plot(make.region())
#' plot(transects)
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
#' }
make.design <- function(transect.type = "line", design.details = "default", region.obj = "region", design.axis = 0, spacing = 100, nested.space = numeric(0), no.complex = numeric(0), angle = numeric(0), plus.sampling = logical(0), path = character(0)){
  # Set the design details if spcified as "default"
  if(design.details[1] == "default"){
    if(transect.type %in% c("line", "Line", "Line Transect", "line transect")){
      design.details <- c("parallel", "systematic")
    }else if(transect.type %in% c("Point", "point", "Point Transect", "point transect")){
      design.details <- "systematic"
    }else{
      stop("Incorrect transect.type specified.", call. = FALSE)
    }
  }
  # Get region object name only
  region <- region.obj
  if(class(region) != "character"){
    if(class(region) == "Region"){
      region <- global.name <- deparse(substitute(region.obj))
    }else{
      stop("Please supply the name of the region object to the region.obj argument.", call. = FALSE)  
    }
  } 
  # 
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
        if(design.details[2] %in% c("Equal Angle", "equal angle", "Equal angle")){            
          design <- new(Class = "LT.EqAngle.ZZ.Design", region = region, design.axis = design.axis, spacing = numeric(0), plus.sampling = plus.sampling, path = path)
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
      if(design.details[1] %in% c("Segmented", "segmented")){
        if(design.details[2] %in% c("Grid", "grid")){
          design <- new(Class = "LT.SegmentedGrid.Design", region = region, design.axis = design.axis, spacing = spacing, plus.sampling = plus.sampling, path = path)
        }
        if(design.details[2] %in% c("Track", "track")){
          design <- new(Class = "LT.SegmentedTrack.Design", region = region, design.axis = design.axis, spacing = spacing, plus.sampling = plus.sampling, path = path)
        }
      }
    }
  }else if(transect.type %in% c("Point", "point", "Point Transect", "point transect")){
    if(length(design.details) > 1){
      warning("Only one design details option required for point transects, subsequent options will be ignored.")
    }
    if(design.details[1] %in% c("Systematic", "systematic")){
      design <- new(Class = "PT.Systematic.Design", region = region, design.axis = design.axis, spacing = spacing, plus.sampling = plus.sampling, path = path)  
    }
    if(design.details[1] %in% c("Random", "random")){
      design <- new(Class = "PT.Random.Design", region = region, design.axis = design.axis, spacing = spacing, plus.sampling = plus.sampling, path = path)
    }
    if(design.details[1] %in% c("Nested", "nested")){
      design <- new(Class = "PT.Nested.Design", region = region, design.axis = design.axis, spacing = spacing, nested.space = nested.space, no.complex = no.complex, plus.sampling = plus.sampling, path = path)
    }
  }
  if(is.null(design)){
    stop("Apologies, this design type is not supported at present. You should use the line transect user specified design options.", call. = FALSE)
  }
  return(design)
}

#' @title Creates a Density object
#' @description 
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
#' @param buffer the width of the buffer region for generating the density grid. If not supplied DSsim will use the maximum value provided for the x.space or y.space.
#' @param constant a value describing a constant density across the surface.
#' @param density.gam \code{gam} object created using \code{mgcv}
#' @param dsm not currently implemented
#' @param formula not currently implemented
#' @return object of class Density 
#' @export
#' @author Laura Marshall     
#' @seealso \code{\link{make.region}}  
#' @examples  
#' # A simple density surface can be created within a rectangular region using 
#' # the default values:
#' density <- make.density()
#' plot(density)
#' plot(make.region(), add = TRUE)
#'                 
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
make.density <- function(region.obj = make.region(), density.surface = list(), x.space = 5, y.space = NULL, buffer = numeric(0), constant = numeric(0), density.gam = NULL, dsm = NULL, formula = NULL){
  # Find the number of strata
  no.strata <- length(region.obj@strata.name)
  # Check the user has supplied the correct number of consants
  if(length(constant) > 0){
    if(no.strata > 0 & length(constant) != length(region.obj@strata.name)){
      stop("The length of the constant vector does not correspond to the number of strata", call. = FALSE)
    }
  }else{
    if(no.strata == 0){
      constant <- 1
    }else{
      constant <- rep(1, no.strata)
    }
  }
  # Check if the user has supplied a y.space value
  if(is.null(y.space)){
    # If not set it equal to x.space
    y.space <- x.space
  }
  # Make density object
  density <- new(Class = "Density", region = region.obj, strata.name = region.obj@strata.name, density.surface = density.surface, x.space = x.space, y.space = y.space, constant = constant, density.gam = density.gam, buffer = buffer)
 return(density)
}

#' @title Creates a Population.Description object
#' @description 
#' Creates an object which describes a population. The values in this object 
#' will be used to create instances of the population
#' 
#' @details #' The \code{covariates} argument should specify a list with one named 
#' element per covariate. If specifying the covariate values via a distribution
#' this should be done in the form of a list. The first element should be one of 
#' the following: 'normal', 'poisson', 'lognormal'. The corresponding parameters
#' that you must supply are detailed below. These should be added to a named list
#' (each element named with the parameter name) containing the parameter values.
#' See examples for implementation.
#'
#' \tabular{lll}{ Distribution  \tab Parameters  \tab         \cr 
#'                normal        \tab mu          \tab sigma   \cr
#'                poisson       \tab lambda      \tab Random  \cr
#'                lognormal     \tab mu          \tab sigma   \cr
#'               }
#'
#' @param region.obj the Region object in which this population exists.
#' @param density.obj the Density object describing the distribution of the individuals / clusters.
#' @param covariates Named list with one named entry per individual level covariate.
#' Cluster sizes can be defined here. Each list entry will either be a data.frame 
#' containing 2 columns, the first the level (level) and the second the probability 
#' (prob). The cluster size entry in the list must be named 'size'. Alternatively the
#' list element may be another list specifying the distribution in the first element
#' and a named list in the second element with the distribution parameters. 
#' @param N the number of individuals / clusters in a population
#' @param fixed.N a logical value. If TRUE the population is generated from the value of N otherwise it is generated from the density description.
#' @param average.D not currently implemented.
#' @return object of class Population.Description 
#' @export
#' @author Laura Marshall
#' @seealso \code{\link{make.region}}, \code{\link{make.density}}
#' @examples
#' # An example population can be created from the default values
#' pop.desc <- make.population.description()
#' 
#' # To view an instance of this population
#' pop <- generate.population(pop.desc, make.detecability(), make.region())
#' plot(pop)
#' 
#' \dontrun{
#' pop.description <- make.population.description(N = 1000, 
#'  density.obj = pop.density, region = region, fixed.N = TRUE)
#'  }
make.population.description <- make.pop.description <- function(region.obj = make.region(), density.obj = make.density(), covariates = list(), N = numeric(0), fixed.N = TRUE, average.D = numeric(0)){
  # Get the number of strata
  no.strata <- ifelse(length(region.obj@strata.name) > 0, length(region.obj@strata.name), 1)
  # Check covariate input
  covariates <- check.covariates(covariates, no.strata)
  # Check population size input
  if(fixed.N){
    if(length(N) == 0){
      N <- rep(1000, no.strata)  
    }else if(length(N) != no.strata){
      stop("You have not supplied the correct number of constants for population size N for each strata", call. = FALSE)
    }
  }
  pop.description <- new(Class = "Population.Description", N = N, density = density.obj, region.obj = region.obj, size.table = cluster.size.table, size = cluster.size, gen.by.N = fixed.N)
  return(pop.description)
}

#' @title Creates a Detectablility object
#' @description 
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
#' # The default values create a detectability object as follows:
#' detect <- make.detectability(key.function = "hn", scale.param = 25,
#'  truncation = 75) 
make.detectability <- function(key.function = "hn", scale.param = 25, shape.param = numeric(0), covariates = character(0), cov.param = numeric(0), truncation = 75){
  detectability <- new(Class = "Detectability", key.function = key.function, scale.param = scale.param, shape.param = shape.param, covariates = covariates, cov.param = cov.param, truncation = truncation)
  return(detectability)
}

#' @title Creates a list of DDF.Analysis objects
#' @description 
#' This method creates a list of DDF.Analysis objects each of which describes 
#' a model to fit to the distance data. The simulation will fit each of these 
#' models to the data generated in the simulation and select the model with 
#' the minimum criteria value.
#' 
#' @details By default this function creates a half-normal detection
#'  function model \code{dsmodel = list(~cds(key = "hn",
#'  formula = ~1))} with a truncation distance of 75. 
#'
#' @param dsmodel list of distance sampling model formula specifying the detection function (see \code{?ddf} for further details)
#' @param mrmodel not yet implemented
#' @param method character only "ds" normal distance sampling currently implemented
#' @param criteria character model selection criteria (AIC, AICc, BIC) - only AIC implemented at present.
#' @param analysis.strata Dataframe with two columns ("design.id" and 
#' "analysis.id"). The former gives the strata names as defined in the
#' design (i.e. the region object) the second specifies how they should 
#' be grouped (into less strata) for the analyses
#' @param truncation numeric truncation distance for analyses
#' @param binned.data logical whether the data should be analsed in bins
#' @param cutpoints gives the cutpoints of the binned data
#' @return list of objects of class DDF.Analysis 
#' @export
#' @author Laura Marshall
#' @seealso \code{ddf} in \code{library(mrds)}
#' @examples
#' # A simple half-normal "ds" model can be created using the default values
#' ddf.analyses <- make.ddf.analysis.list()
#' 
#' ddf.analyses <- make.ddf.analysis.list(dsmodel = list(~cds(key = "hn",
#'  formula = ~1),~cds(key = "hr", formula = ~1)), method = "ds", 
#'  criteria = "AIC")
#'
make.ddf.analysis.list <- function(dsmodel = list(~cds(key = "hn", formula = ~1)), mrmodel = NULL, method = "ds", criteria = "AIC", analysis.strata = data.frame(), truncation = 75, binned.data = FALSE, cutpoints = numeric(0)){
  ddf.analyses <- list()
  if(method == "ds"){
    for(a in seq(along = dsmodel)){
      ddf.analyses[[a]] <- new(Class = "DS.Analysis", dsmodel = dsmodel[[a]], criteria = criteria, analysis.strata, truncation = truncation, binned.data = binned.data, cutpoints = cutpoints)
    }
  }else{
    stop("Double observer methods are not yet implemented", call. = FALSE)
  }
  return(ddf.analyses)
}

#' @title Creates a Simulation object
#' @description This creates a simulation with all the information necessary for DSsim
#' to generate a population, create or read in transects, simulate the survey process
#' and fit detection functions and estimate density / abundance. This function can be
#' used by itself based on default values to create a simple line transect example, see
#' Examples below. To create more comples simulations it is advisable to define the 
#' different parts of the simulation individually before grouping them together. See
#' the Arguments for links to the functions which make the definitions for the 
#' individual simulation components. Example simulations can also be found at
#' <https://github.com/DistanceDevelopment/DSsim/wiki>.
#' @details The \code{make.simulation} function is now set up so that by
#'  default (with the exception of specifying point transects rather than
#'   line) it can run a simple simulation example. See examples.
#' @param reps number of times the simulation should be repeated
#' @param single.transect.set logical specifying whether the transects should
#'   be kept the same throughout the simulation.
#' @param double.observer not currently implemented.
#' @param region.obj an object of class Region created by a call to
#'  \link{make.region}
#' @param design.obj an object of class Survey.Design created by a call to
#'  \link{make.design}
#' @param population.description.obj an object of class Population.Description
#'  created by a call to \link{make.population.description}
#' @param detectability.obj and object of class Detectabolity created by a call to
#'  \link{make.detectability}
#' @param ddf.analyses.list a list of objects of class DDF.Analysis created by 
#'  a call to\link{make.ddf.analysis.list}
#' @return object of class Simulation 
#' @export
#' @author Laura Marshall
#' @examples
#' \dontrun{
#' # A basic line transect simulation example
#' sim <- make.simulation()
#' check.sim.setup(sim)
#' sim <- run(sim) 
#' summary(sim)
#' 
#' # A basic point transect simulation example
#' sim <- make.simulation(design.obj = make.design("point"))
#' check.sim.setup(sim)
#' sim <- run(sim) 
#' summary(sim)
#' # Note % bias levels will vary due to low number of repetitions 
#' # set by default in these examples
#' 
#' # To increase the number of repetitions
#' sim <- make.simulation(reps = 100)
#' sim <- run(sim) 
#' summary(sim)
#' }
#'
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
make.simulation <- function(reps = 10, single.transect.set = FALSE, double.observer = FALSE, region.obj = make.region(), design.obj = make.design(), population.description.obj = make.population.description(), detectability.obj = make.detectability(), ddf.analyses.list = make.ddf.analysis.list()){
  #Make the results arrays and store in a list
  no.strata <- ifelse(length(region.obj@strata.name) > 0, length(region.obj@strata.name)+1, 1) 
  #Check to see if the strata are grouped in the analyses
  new.strata.names <- NULL
  if(nrow(ddf.analyses.list[[1]]@analysis.strata) > 0){
    new.strata.names <- unique(ddf.analyses.list[[1]]@analysis.strata$analysis.id)  
  }else{
    new.strata.names <- NULL
  }
  if(length(region.obj@strata.name) > 0){
    if(!is.null(new.strata.names)){
      strata.name <- c(sort(new.strata.names), "Total")
      no.strata <- length(strata.name)
    }else{
      strata.name <- c(sort(region.obj@strata.name), "Total")  
    }
  }else{
    strata.name <- region.obj@region.name
  }
  individuals <- list(summary = array(NA, dim = c(no.strata, 8, reps+2), dimnames = list(strata.name, c("Area", "CoveredArea", "Effort", "n", "n.miss.dist", "ER", "se.ER", "cv.ER"), c(1:reps,"mean","sd"))), 
                  N = array(NA, dim = c(no.strata, 6, reps+2), dimnames = list(strata.name, c("Estimate", "se", "cv", "lcl", "ucl", "df"), c(1:reps,"mean","sd"))), 
                  D = array(NA, dim = c(no.strata, 6, reps+2), dimnames = list(strata.name, c("Estimate", "se", "cv", "lcl", "ucl", "df"), c(1:reps,"mean", "sd"))))
  detection = array(NA, dim = c(1, 6, reps+2), dimnames = list("Pooled", c("True.Pa", "Pa", "ESW", "f(0)", "SelectedModel", "DeltaCriteria"), c(1:reps,"mean","sd")))
  #create additional arrays if animals are in clusters
  if(population.description.obj@size){
    clusters <- list(summary = array(NA, dim = c(no.strata, 9, reps+2), dimnames = list(strata.name, c("Area", "CoveredArea", "Effort", "n", "n.miss.dist", "k", "ER", "se.ER", "cv.ER"), c(1:reps,"mean","sd"))), 
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





