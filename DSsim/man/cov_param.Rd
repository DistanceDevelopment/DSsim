\name{param.list}
\alias{param.list}
\docType{data}
\title{
  Parameter Estimates from Covariate Simulation
}
\description{
  This is a list of 2 2D arrays. Each array contains parameter estimates from the covariate 
  simulations described in the vignette. These simulations generate data based on sex affecting
  detectaibility, they then fit a model to these data with sex as a covariate to see if the 
  original parameters can be estimated from the data. This process was repeated 999 times for 
  5 different truncation values to see if truncation distance affects the parameter estimates.
}
\usage{data("cov_param")}
\format{
  The format is: List of 2 $sigma, $sex.male ...
  \describe{
    \item{\code{sigma}}{a numeric 2D array}
    \item{\code{sex.male}}{a numeric 2D array}
  }
}
\examples{
data(cov_param)
}
\keyword{datasets}
