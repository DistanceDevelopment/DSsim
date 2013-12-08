\docType{class}
\name{Simulation-class}
\alias{plot}
\alias{plot,Simulation-method}
\alias{print}
\alias{print,Simulation-method}
\alias{show}
\alias{show,Simulation-method}
\alias{Simulation-class}
\alias{summary}
\alias{summary,Simulation-method}
\title{Class "Simulation"}
\description{
  Class \code{"Simulation"} is an S4 class containing
  descriptions of the region, population, survey design and
  analyses the user wishes to investigate. Once the
  simulation has been run the N.D.Estimates will contain
  multiple estimates of abundance and density obtained by
  repeatedly generating populations, simulating the survey
  and completing the analyses.
}
\section{Objects from the Class}{
  Objects can be created by calls of the form
  \code{make.simulation(reps, double.observer, region.obj,
  design.obj, population.description.obj,
  detectability.obj, ddf.analyses.list)}
}

\section{Slots}{
  \describe{ \item{\code{reps}}{Object of class
  \code{"numeric"}; the number of times the simulation
  should be repeated.}
  \item{\code{single.transect.set}}{Object of class
  \code{"logical"}; if \code{TRUE} the same set of
  transects are used in each repetition.}
  \item{\code{double.observer}}{Object of class
  \code{"logical"}; whether a double observer protocol is
  being used. Not currently implemented.}
  \item{\code{region}}{Object of class \code{"Region"}; the
  survey region.} \item{\code{design}}{Object of class
  \code{"Survey.Design"}; the survey design.}
  \item{\code{population.description}}{Object of class
  \code{"Population.Description"}; the
  population.description.}
  \item{\code{detectability}}{Object of class
  \code{"Detectability"}; a description of the
  detectability of the population.}
  \item{\code{ddf.analyses}}{Object of class \code{"list"};
  a list of object of class DDF.Analysis. These are fitted
  and the one with the minimum criteria is selected and
  used in predicting N and D.}
  \item{\code{dsm.analysis}}{Object of class
  \code{"DSM.Analysis"}; Not yet implemented.}
  \item{\code{ddf.param.ests}}{Object of class
  \code{"array"}; stores the parameters associated with the
  detection function.} \item{\code{results}}{Object of
  class \code{"logical"}; stores the estimated of abundance
  and density as well as other measures of interest.} }
}

\section{Methods}{
  \describe{
  \item{\code{add.hotspot}}{\code{signature=(object =
  "Simulation")}: adds a hotspot based on a gaussian decay
  to the density surface.}
  \item{\code{summary}}{\code{signature=(object =
  "Simulation")}: produces a summary of the simulation and
  its results.}
  \item{\code{generate.population}}{\code{signature =
  (object = "Simulation")}: generates a single instance of
  a population.}
  \item{\code{generate.transects}}{\code{signature =
  (object = "Simulation")}: generates a single set of
  transects.}
  \item{\code{create.survey.results}}{\code{signature =
  (object = "Simulation")}: carries out the simulation
  process as far as generating the distance data and
  returns an object containing the population, transects
  and data.} \item{\code{run.analysis}}{\code{signature =
  c(object = "Simulation", data = "LT.Survey.Results")}:
  returns the ddf analysis results from the models in the
  simulation fitted to the data in the LT.Survey.Results
  object.} \item{\code{run.analysis}}{\code{signature =
  c(object = "Simulation", data = "DDF.Data")}: returns the
  ddf analysis results from the models in the simulation
  fitted to the data in the DDF.Data object.}
  \item{\code{run}}{\code{signature = (object =
  "Simulation")}: runs the whole simulation for the
  specified number of repetitions.} }
}
\examples{
coords <- gaps <- list()
coords[[1]] <- list(data.frame(x = c(0,1000,1000,0,0), y = c(0,0,1000,1000,0)))
gaps[[1]] <- list(data.frame(x = c(400,600,500,350,400), y = c(100,250,600,120,100)))

region <- make.region(region.name = "study.area", units = "m", coords = coords, gaps = gaps)
plot(region)

\dontrun{
data(transects.shp)
shapefile.pathway <- "C:/..."
write.shapefile(transects.shp, shapefile.pathway)

parallel.design <- make.design(transect.type = "Line", design.details = c("Parallel","Systematic"), region = region, design.axis = 0, spacing = 100, plus.sampling =FALSE, path = shapefile.pathway)
}

pop.density <- make.density(region.obj = region, x.space = 10, y.space = 10, constant = 0.5)
pop.density <- add.hotspot(pop.density, centre = c(50, 200), sigma = 100, amplitude = 0.1)
pop.density <- add.hotspot(pop.density, centre = c(500, 700), sigma = 900, amplitude = 0.05)
pop.density <- add.hotspot(pop.density, centre = c(300, 100), sigma = 100, amplitude = -0.15)

plot(pop.density)
plot(region, add = TRUE)

pop.description <- make.population.description(N = 1000, density.obj = pop.density, region = region, fixed.N = TRUE)

detect <- make.detectability(key.function = "hn", scale.param = 15, truncation = 30)

ddf.analyses <- make.ddf.analysis.list(dsmodel = list(~cds(key = "hn", formula = ~1),~cds(key = "hr", formula = ~1)), method = "ds", criteria = "AIC")

\dontrun{
simulation <- make.simulation(reps = 10, single.transect.set = TRUE, region.obj = region, design.obj = parallel.design, population.description.obj = pop.description, detectability.obj = detect, ddf.analyses.list = ddf.analyses)

survey.results <- create.survey.results(simulation, dht.table = TRUE)

plot(survey.results)

my.simulation <- run(my.simulation)

summary(my.simulation)
}
}
\keyword{classes}

