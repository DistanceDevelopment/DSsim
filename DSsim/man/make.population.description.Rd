\name{make.population.description}
\alias{make.population.description}
\title{Creates a Population.Description object}
\usage{
  make.population.description(region.obj, density.obj,
    cluster.size.table = data.frame(NULL),
    size.distribution = character(0),
    size.param = numeric(0), N = numeric(0),
    fixed.N = TRUE, average.D = numeric(0))
}
\arguments{
  \item{region.obj}{the Region object in which this
  population exists.}

  \item{density.obj}{the Density object describing the
  distribution of the individuals / clusters.}

  \item{cluster.size.table}{- data.frame with two columns
  size (giving the cluster sizes) and prob (giving the
  probability that a cluster is of that size). prob must
  sum to 1.}

  \item{size.distribution}{- not yet implemented}

  \item{size.param}{- not yet implemented}

  \item{N}{the number of individuals / clusters in a
  population}

  \item{fixed.N}{a logical value. If TRUE the population is
  generated from the value of N otherwise it is generated
  from the density description.}

  \item{average.D}{not currently implemented.}
}
\value{
  object of class Population.Description
}
\description{
  Creates an object which describes a population. The
  values in this object will be used to create instances of
  the population
}
\examples{
coords <- gaps <- list()
coords[[1]] <- list(data.frame(x = c(0,1000,1000,0,0), y = c(0,0,
 1000,1000,0)))
gaps[[1]] <- list(data.frame(x = c(400,600,500,350,400), y = c(100,
 250,600,120,100)))

region <- make.region(region.name = "study.area", units = "m",
 coords = coords, gaps = gaps)

pop.density <- make.density(region.obj = region, x.space = 10,
 y.space = 10, constant = 0.5)
pop.density <- add.hotspot(pop.density, centre = c(50, 200),
 sigma = 100, amplitude = 0.1)
pop.density <- add.hotspot(pop.density, centre = c(500, 700),
 sigma = 900, amplitude = 0.05)
pop.density <- add.hotspot(pop.density, centre = c(300, 100),
 sigma = 100, amplitude = -0.15)

plot(pop.density)
plot(region, add = TRUE)

pop.description <- make.population.description(N = 1000,
 density.obj = pop.density, region = region, fixed.N = TRUE)
}
\author{
  Laura Marshall
}

