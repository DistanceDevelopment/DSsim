\docType{methods}
\name{generate.population}
\alias{generate.population}
\title{S4 generic method to generate an instance of a population}
\arguments{
  \item{object}{an object of class Simulation or
  Population.Description}

  \item{detectability}{object of class detectability
  (optional - only required if object is of class
  Population.Description)}
}
\value{
  an object of class Population
}
\description{
  Uses the population description and detectability details
  to generate an instance of the population.
}
\details{
  Note that if the first argument supplied is of class
  Population.Description rather than simulation then a
  second argument \code{detectabity} must also be supplied
  and must be an object of class Detectability.
}

