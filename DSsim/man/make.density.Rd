\name{make.density}
\alias{make.density}
\title{Creates an object of class Density}
\usage{
  make.density(region, strata = character(0),
    density.surface = NULL, x.space, y.space,
    constant = NULL, shapefile = NULL, density.gam = NULL,
    jit = 1)
}
\arguments{
  \item{region}{the name of the Region object in which this
  population exists.}

  \item{density.surface}{a dataframe describing the density
  with columns x, y, and density.}

  \item{x.space}{the intervals in the grid in the x
  direction}

  \item{y.space}{the intervals in the grid in the y
  direction}

  \item{constant}{a value describing a constant density
  across the surface.}

  \item{shapefile}{- not yet implemented}

  \item{density.gam}{- not yet implemented}

  \item{jit}{a value descibing how many positions the grid
  should be moved to ensure that the grid covers the entire
  survey region.}
}
\value{
  object of either class Density
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

  Further options will be added to allow the creation of
  non-constant surfaces within this method.
}
\author{
  Laura Marshall
}

