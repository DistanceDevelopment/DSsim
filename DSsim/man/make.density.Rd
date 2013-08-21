\name{make.density}
\alias{make.density}
\title{Creates a Density object}
\usage{
  make.density(region.obj, density.surface = list(),
    x.space, y.space, constant = numeric(0),
    density.gam = NULL, dsm = NULL, formula = NULL)
}
\arguments{
  \item{region.obj}{the Region object in which the density
  grid will be created}

  \item{density.surface}{a dataframe describing the density
  with columns x, y, and density.}

  \item{x.space}{the intervals in the grid in the x
  direction}

  \item{y.space}{the intervals in the grid in the y
  direction}

  \item{constant}{a value describing a constant density
  across the surface.}

  \item{density.gam}{- not yet implemented}

  \item{dsm}{not currently implemented}

  \item{formula}{not currently implemented}
}
\value{
  object of class Density
}
\description{
  The user has the option to create a grid describing the
  density of the objects and pass this in giving the x and
  y spacings used in the creation of this grid.
}
\details{
  Alternatively the user can specify a constant density and
  x, y spacings and this grid will be generated
  automatically.

  The user may also supply a mgcv gam object and x, y
  spacings and the density grid will be created from these.
}
\author{
  Laura Marshall
}

