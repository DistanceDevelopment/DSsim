\name{covmod.summary.list}
\alias{covmod.summary.list}
\docType{data}
\title{
  Covariate Truncation Simulation Summaries
}
\description{
  This is a list of 5 simulation summaries. Data were generated using a systematic line transect 
  design and a mixture of two half normal detection functions describing detectability, one for 
  males and one for females. Each of the five simulation summaries corresponds to a different 
  analysis truncation distance when fitting the detection function model in which detectability
  was modelled as a function of sex. These simulations investigate whether truncation distance 
  affects the accuracy and precision of the estimates of abundance 
  / density.
}
\usage{data("covmod_summary")}
\format{
  The format is: List of 5
  \describe{
    \item{\code{t200}}{a simulation summary (truncation = 200)}
    \item{\code{t400}}{a simulation summary (truncation = 400)}
    \item{\code{t600}}{a simulation summary (truncation = 600)}
    \item{\code{t800}}{a simulation summary (truncation = 800)}
    \item{\code{t1000}}{a simulation summary (truncation = 1000)}
  }
}
\examples{
data(covmod_summary)
covmod.summary.list$t200
}
\keyword{datasets}
