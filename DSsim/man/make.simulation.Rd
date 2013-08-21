\name{make.simulation}
\alias{make.simulation}
\title{Creates a Simulation object}
\usage{
  make.simulation(reps, single.transect.set = FALSE,
    double.observer = FALSE, region.obj, design.obj,
    population.description.obj, detectability.obj,
    ddf.analyses.list)
}
\arguments{
  \item{reps}{number of times the simulation should be
  repeated}

  \item{single.transect.set}{logical specifying whether the
  transects should be kept the same throughout the
  simulation.}

  \item{double.observer}{not currently implemented.}

  \item{region.obj}{an object of class Region}

  \item{design.obj}{an object of class Survey.Design}

  \item{population.description.obj}{an object of class
  Population.Description}

  \item{detectability.obj}{and object of class
  Detectabolity}

  \item{ddf.analyses.list}{a list of objects of class
  DDF.Analysis}
}
\value{
  object of class Simulation
}
\description{
  This creates a simulation object which groups together
  all the objects needed to complete the simulation.
}
\author{
  Laura Marshall
}

