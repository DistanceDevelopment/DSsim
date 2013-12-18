\docType{methods}
\name{run.analysis}
\alias{run.analysis}
\alias{run.analysis,DDF.Analysis,DDF.Data-method}
\alias{run.analysis,Simulation,DDF.Data-method}
\alias{run.analysis,Simulation,LT.Survey.Results-method}
\title{S4 generic method to run analyses}
\arguments{
  \item{object}{an object of class Simulation or
  DDF.Analysis}

  \item{data}{an object of class Survey.Results or
  DDF.Data}
}
\value{
  a list containing an S3 ddf object and optionally an S3
  dht object relating to the model with the miminum
  criteria.
}
\description{
  This method carries out an analysis of distance sampling
  data. This method is provided to allow the user to
  perform diagnostics of the analyses used in the
  simulation. The data argument can be obtained by a call
  to \code{simulate.survey(object, dht.table = TRUE)}. Note
  if the first object supplied is of class DDf.Analysis
  then the second argument must be of class DDf.Data. The
  data argument may be of either class for an object
  argument of class Simulation.
}

