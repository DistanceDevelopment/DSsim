\name{make.simulation}
\alias{make.simulation}
\title{Creates an object of class Simulation}
\usage{
  make.simulation(reps, double.observer = FALSE,
    region.obj, design.obj, population.description.obj,
    detectability.obj, ddf.analyses.list)
}
\arguments{
  \item{reps}{number of times the simulation should be
  repeated}

  \item{double.observer}{logical value indicating TRUE for
  a double observer mark recapture survey or FALSE for
  normal distance sampling.}

  \item{region.obj}{an object of class Region}

  \item{design.obj}{an object of class Survey.Design}

  \item{population.description.obj}{an object of class
  Population.Description}

  \item{detectability.obj}{and object of class
  Detectabolity}

  \item{ddf.analyses.list}{a list of objects of class
  DS.Analysis}
}
\value{
  object of class Simulation
}
\description{
  Creates an object of class Simulation
}
\author{
  Laura Marshall
}

