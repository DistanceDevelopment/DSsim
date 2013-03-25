\name{make.design}
\alias{make.design}
\title{Creates an object of a class which inherits from class Survey.Design}
\usage{
  make.design(transect.type, design.details, region,
    design.axis = numeric(0), spacing = numeric(0),
    plus.sampling = logical(0), path = character(0))
}
\arguments{
  \item{transect.type}{character variable specifying either
  "Line" or "Point"}

  \item{design.details}{a list describing the type of
  design. See details.}

  \item{region}{the name of the Region object where the
  survey is to be carried out.}

  \item{design.axis}{- user may provide details but not
  currently used}

  \item{spacing}{- user may provide details but not
  currently used}

  \item{plus.sampling}{- user may provide details but not
  currently used}

  \item{path}{file pathway giving the location of the
  survey shapefiles}
}
\value{
  object of a class which inherits from class Survey.Design
}
\description{
  Currently surveys are only generated within the GIS in
  Distance therefore if you are running a simulation in R
  you will need to get Distance to generate all the surveys
  as shapefiles in advance and supply the path to these
  files.
}
\details{
  The \code{design.details} argument should specify a list
  of either 1 or 2 elements. These options are described in
  the table below:

  \tabular{lll}{ Transect Type \tab Design Details \tab \cr
  Line \tab Parallel \tab Systematic \cr Line \tab Parallel
  \tab Random \cr Line \tab Zigzag \tab Equal Angle \cr
  Line \tab Zigzag \tab Equal Spaced \cr Point \tab
  Systematic \tab \cr Point \tab Random \tab \cr}
}
\author{
  Laura Marshall
}

