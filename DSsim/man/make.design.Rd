\name{make.design}
\alias{make.design}
\title{Creates a Survey.Design object}
\usage{
  make.design(transect.type, design.details, region.obj,
    design.axis = numeric(0), spacing = numeric(0),
    angle = numeric(0), plus.sampling = logical(0),
    path = character(0))
}
\arguments{
  \item{transect.type}{character variable specifying either
  "Line" or "Point"}

  \item{design.details}{a character vector describing the
  type of design. See details section.}

  \item{region.obj}{the name of the Region object where the
  survey is to be carried out.}

  \item{design.axis}{user may provide the angle of the
  design axis but not currently used}

  \item{spacing}{user may provide the systematic design
  spacing but but not currently used}

  \item{angle}{user may provide the design angle (only
  relevant in equal angle zigzag designs) but not currently
  used}

  \item{plus.sampling}{logical vaule indicating whether a
  plus sampling protocol is used but not currently used}

  \item{path}{pathway giving the location of the folder of
  survey shapefiles}
}
\value{
  object of a class which inherits from class Survey.Design
}
\description{
  Currently surveys are only generated within the GIS in
  Distance. If you are running a simulation in R you will
  need to get Distance to generate all the surveys as
  shapefiles in advance and supply the path to the
  directory which contains these shapefiles and only these 
  shapefiles.
}
\details{
  The \code{design.details} argument should specify a
  character vector of either 1 or 2 elements. These options
  are described in the table below:

  \tabular{lll}{ Transect Type \tab Design Details \tab \cr
  Line \tab Parallel \tab Systematic \cr Line \tab Parallel
  \tab Random \cr Line \tab Zigzag \tab Equal Angle \cr
  Line \tab Zigzag \tab Equal Spaced \cr Line \tab User
  Specified \tab \cr Point \tab Systematic \tab \cr Point
  \tab Random \tab \cr}
}
\examples{
\dontrun{
data(transects.shp)
#Edit the pathway below to point to an empty folder where the
#transect shapefile will be saved
shapefile.pathway <- "C:/..."
write.shapefile(transects.shp, paste(shapefile.pathway,"/transects_1", 
  sep = ""))

parallel.design <- make.design(transect.type = "Line",
 design.details = c("Parallel","Systematic"), region = region,
 design.axis = 0, spacing = 100, plus.sampling =FALSE,
 path = shapefile.pathway)
}
}
\author{
  Laura Marshall
}

