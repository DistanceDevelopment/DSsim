\name{LT.Survey-class}
\alias{LT.Survey-class}
\title{Virtual Class "LT.Survey" extends class "Survey"}
\description{
  Virtual Class \code{"LT.Survey"} is an S4 class
  containing a population and a set of transects.
}
\section{Slots}{
  \describe{ \item{\code{line.transect}}{Object of class
  \code{"Line.Transect"}; the transects.}
  \item{\code{perpendicular.truncation}}{Object of class
  \code{"numeric"}; the maximum distance from the transect
  at which animals may be detected.} }
}

\section{Methods}{
  \describe{
  \item{\code{create.sample.table}}{\code{signature=(object
  = "LT.Survey", ...)}: creates a sample table for
  \code{dht}.} }
}
\seealso{
  \code{\link{make.design}}
}
\keyword{classes}

