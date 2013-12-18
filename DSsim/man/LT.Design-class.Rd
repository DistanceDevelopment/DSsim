\docType{class}
\name{LT.Design-class}
\alias{LT.Design-class}
\title{Virtual Class "LT.Design" extends Class "Survey.Design"}
\description{
  Virtual Class \code{"LT.Design"} is an S4 class detailing
  the type of line transect design.
}
\section{Slots}{
  \describe{ \item{\code{design.axis}}{Object of class
  \code{"numeric"}; the angle of the design axis.}
  \item{\code{spacing}}{Object of class \code{"numeric"};
  the spacing of the design.} }
}

\section{Methods}{
  \describe{
  \item{\code{generate.transects}}{\code{signature=(object
  = "LT.Design", ...)}: loads a set of transects from a
  shapefile.} }
}
\seealso{
  \code{\link{make.design}}
}
\keyword{classes}

