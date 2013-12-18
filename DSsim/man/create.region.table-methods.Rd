\docType{methods}
\name{create.region.table}
\alias{create.region.table}
\alias{create.region.table,Survey-method}
\title{S4 generic method to generate a region table}
\arguments{
  \item{object}{an object of a class inheriting from
  Survey}

  \item{region}{an object of class Region}
}
\value{
  an object of class Region.Table
}
\description{
  This function is called internally to generate a region
  table required to estimate abundance / density via the
  Hortvitz-Thompson estimator.
}

