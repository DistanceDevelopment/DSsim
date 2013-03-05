\name{make.ddf.analysis}
\alias{make.ddf.analysis}
\title{Creates an object of a class which inherits from class DDF.Analysis}
\usage{
  make.ddf.analysis(dsmodel, mrmodel = NULL, criteria)
}
\arguments{
  \item{dsmodel}{distance sampling model for the detection
  function (see \code{?ddf} for further details)}

  \item{mrmodel}{mark-recapture model specification (see
  \code{?ddf} for further details.}

  \item{criteria}{model selection criteria (AIC, AICc, BIC)
  - only AIC implemented at present.}
}
\value{
  object of either class DS.Analysis or MRDS.Analysis
}
\description{
  Creates an object of a class which inherits from class
  DDF.Analysis
}
\author{
  Laura Marshall
}
\seealso{
  \code{ddf} in \code{library(mrds)},
}

