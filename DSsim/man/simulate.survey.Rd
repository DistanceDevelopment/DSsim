\docType{methods}
\name{simulate.survey}
\alias{simulate.survey}
\title{S4 generic method to simulate a survey}
\arguments{
  \item{object}{an object of class Simulation}

  \item{dht.tables}{logical value indicating whether or the
  data tables for Hortvitz-Thompson estimation are
  required.}
}
\value{
  an object of class LT.Survey.Results
}
\description{
  Simulates the process by which individuals / clusters are
  detected. Currently this is only implemented for line
  transect surveys. It returns an object of class
  LT.Survey.Results which contains a population, a set of
  transects, distance data and if requested region, sample
  and obs tables.
}
\details{
  This object can be displayed using \code{plot()} or the
  distance data extracted using \code{get.distance.data()}.
  You can then investigate fitting models to this data.
}

