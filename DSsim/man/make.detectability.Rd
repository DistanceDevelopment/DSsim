\name{make.detectability}
\alias{make.detectability}
\title{Creates an object of class Detectablility}
\usage{
  make.detectability(key.function, scale.param,
    shape.param = numeric(0), adjustment = character(0),
    adj.param = numeric(0), covariates = character(0),
    cov.param = numeric(0), perp.truncation = numeric(0),
    rad.truncation)
}
\arguments{
  \item{key.function}{specifies shape of the detection
  function (either half-normal or hazard rate)}

  \item{scale.param}{parameters associated with the
  detection function}

  \item{shape.param}{parameters associated with the
  detection function}

  \item{adjustment}{- not yet implemented}

  \item{adj.param}{- not yet implemented}

  \item{covariates}{- not yet implemented}

  \item{cov.param}{- not yet implemented}

  \item{perp.truncation}{the maximum perpendicular distance
  at which objects may be detected. Must be specified for
  line transect surveys.}

  \item{rad.truncation}{the maximum radial distance at
  which objects may be detected. Must be specified for both
  line transect and point transect surveys.}
}
\value{
  object of either class Detectablility
}
\description{
  Creates an object of class Detectablility
}
\author{
  Laura Marshall
}

