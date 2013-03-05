\docType{methods}
\name{generate.transects}
\alias{generate.transects}
\title{S4 generic method to generate an instance of a design}
\arguments{
  \item{object}{an object of class Simulaiton or a class
  which inherits from Survey.Design}

  \item{read.from.file}{if the transect details should be
  read in from file (currently must be TRUE)}

  \item{write.to.file}{not currently implemented}

  \item{region}{optional if object is of class
  Survey.Design. If left blank the method will load the
  object from the global environment based on the region
  object name inside the Survey.Design object.}
}
\value{
  an object of class Population
}
\description{
  Uses the Survey.Design details to generate transects.
}

