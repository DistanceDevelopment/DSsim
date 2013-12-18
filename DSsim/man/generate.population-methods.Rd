\docType{methods}
\name{generate.population}
\alias{generate.population}
\alias{generate.population,Population.Description-method}
\alias{generate.population,Simulation-method}
\title{S4 generic method to generate an instance of a population}
\arguments{
  \item{object}{an object of class Simulation or
  Population.Description}

  \item{detectability}{object of class Detectability
  (optional - only required if object is of class
  Population.Description)}
}
\value{
  an object of class Population
}
\description{
  Uses the population description and detectability details
  to generate an instance of the population. Note that if
  the first argument supplied is of class
  Population.Description rather than class Simulation then
  a second argument \code{detectabity} must also be
  supplied and must be an object of class Detectability.
}

