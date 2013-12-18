\docType{class}
\name{DDF.Analysis-class}
\alias{DDF.Analysis-class}
\title{Class "DDF.Analysis"}
\description{
  Class \code{"DDF.Analysis"} is an S4 class describing a
  detection function which is to be fitted to the data.
}
\section{Slots}{
  \describe{ \item{\code{dsmodel}}{Object of class
  \code{"formula"}; describing the detection function
  model.} \item{\code{criteria}}{Object of class
  \code{"character"}; describes which model delection
  croteria to use ("AIC","AICc","BIC").}
  \item{\code{ddf.result}}{Object of class \code{"list"};
  object of S3 class ddf.} }
}

\section{Methods}{
  \describe{
  \item{\code{run.analysis}}{\code{signature=c(object =
  "DDF.Analysis", data = "DDF.Data")}: runs the analysis
  described in the object on the data provided.} }
}
\seealso{
  \code{\link{make.ddf.analysis.list}}
}
\keyword{classes}

