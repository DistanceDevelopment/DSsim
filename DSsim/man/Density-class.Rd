\docType{class}
\name{Density-class}
\alias{Density-class}
\alias{plot,Density-method}
\title{Class "Density"}
\description{
  Class \code{"Density"} is an S4 class containing a list
  of grids which describe the density of individuals /
  clusters of a population. The list contains one grid
  (\code{data.frame}) for each strata.
}
\section{Slots}{
  \describe{ \item{\code{region.name}}{Object of class
  \code{"character"}; the region name.}
  \item{\code{strata.name}}{Object of class
  \code{"character"}; the strata names}
  \item{\code{density.surface}}{Object of class
  \code{"list"}; list of data.frames with the columns x, y
  and density. There must be one data.frame for each
  strata.} \item{\code{x.space}}{Object of class
  \code{"numeric"}; The spacing between gridpoints
  described in the density data.frames in the x-direction.}
  \item{\code{y.space}}{Object of class \code{"numeric"};
  The spacing between gridpoints described in the density
  data.frames in the y-direction.} }
}

\section{Methods}{
  \describe{
  \item{\code{add.hotspot}}{\code{signature=(object =
  "Density")}: adds a hotspot based on a gaussian decay to
  the density surfaces.} }
}
\seealso{
  \code{\link{make.density}}
}
\keyword{classes}

