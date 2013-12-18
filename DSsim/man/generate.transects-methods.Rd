\docType{methods}
\name{generate.transects}
\alias{generate.transects}
\alias{generate.transects,LT.Design-method}
\alias{generate.transects,Simulation-method}
\title{S4 generic method to generate an instance of a design}
\arguments{
  \item{object}{an object of class Simulaiton or a class
  which inherits from Survey.Design}

  \item{read.from.file}{if the transect details should be
  read in from file (currently must be TRUE)}

  \item{write.to.file}{not currently implemented}

  \item{region}{optional only required if object is of
  class Survey.Design.}
}
\value{
  an object of class Line.Transect
}
\description{
  Uses the Survey.Design details to generate transects.
  Currenty this involves loading a survey shapefile from
  the path specified in the Survey.Design object and can
  only work with line transect designs.
}

