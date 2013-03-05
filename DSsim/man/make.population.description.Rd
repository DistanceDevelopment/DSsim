\name{make.population.description}
\alias{make.population.description}
\title{Creates an object of class Population.Description}
\usage{
  make.population.description(N, density.obj, region,
    cluster.size = logical(0), size.min = numeric(0),
    size.max = numeric(0), size.lambda = numeric(0),
    gen.by.N = TRUE)
}
\arguments{
  \item{N}{the number of individuals / clusters in a
  population}

  \item{density.obj}{an object of class Density describing
  the density of the individuals / clusters.}

  \item{region}{the name of the Region object in which this
  population exists.}

  \item{cluster.size}{- not yet implemented}

  \item{size.min}{- not yet implemented}

  \item{size.max}{- not yet implemented}

  \item{size.lambda}{- not yet implemented}

  \item{gen.by.N}{a logical value. If TRUE the population
  is generated from the value in N otherwise it is
  generated from the density description (not yet
  implemented)}
}
\value{
  object of either class Density
}
\description{
  Creates an object of class Population.Description
}
\author{
  Laura Marshall
}

