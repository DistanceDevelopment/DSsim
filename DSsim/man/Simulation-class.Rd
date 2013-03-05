\docType{class}
\name{Simulation-class}
\alias{Simulation-class}
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
\keyword{classes}

