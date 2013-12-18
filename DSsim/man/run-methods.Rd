\docType{methods}
\name{run}
\alias{run}
\alias{run,Simulation-method}
\title{S4 generic method to run a simulation}
\arguments{
  \item{object}{an object of class Simulation}

  \item{logical}{option to use multiple processors}

  \item{integer}{maximum number of processors to use}
}
\value{
  an object of class simulation which now includes the
  results
}
\description{
  Runs the simulation and returns the simulation object
  with results. If running in parallel and max.cores is not
  specified it will default to using one less than the
  number of cores / threads on your machine.
}
\seealso{
  \code{\link{make.simulation}}
}

