\docType{methods}
\name{add.hotspot}
\alias{add.hotspot}
\alias{add.hotspot,Density-method}
\title{S4 generic method to add a hotspot to the density grid}
\arguments{
  \item{object}{an object of class Density or Simulation}

  \item{centre}{an x,y-coordinate giving the centre of the
  hotspot}

  \item{sigma}{a value giving the scale parameter for a
  gaussian decay}

  \item{amplitude}{the height of the hotspot at its centre}
}
\value{
  the updated Density or Simulation object
}
\description{
  Uses a Gaussian decay around a central location to add a
  hotspot to the density grid.
}
\seealso{
  \code{\link{make.density}}
}

