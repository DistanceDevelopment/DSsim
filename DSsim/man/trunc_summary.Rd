\name{summary.list}
\alias{summary.list}
\docType{data}
\title{
  Truncation Simulation Summaries
}
\description{
  This is a list of 3 simulation summaries. Data were generated using a systematic line transect 
  design and a single half normal detection function describing detectability. Each of the three
  simulation summaries corresponds to a different analysis truncation distance, to test if this
  affects the accuracy and precision of the estimates of abundance / density.
}
\usage{data("trunc_summary")}
\format{
  The format is: List of 5
  \describe{
    \item{\code{t200}}{a simulation summary (truncation = 200)}
    \item{\code{t400}}{a simulation summary (truncation = 400)}
    \item{\code{t600}}{a simulation summary (truncation = 600)}
  }
}
\examples{
data(trunc_summary)
summary.list$t200
}
\keyword{datasets}
