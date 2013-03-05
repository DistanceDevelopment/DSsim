\name{make.region}
\alias{make.region}
\title{Creates an object of class Region}
\usage{
  make.region(region.name, area = numeric(0),
    shapefile = NULL, coords = list(), gaps = list())
}
\arguments{
  \item{region.name}{the region name.}

  \item{area}{the area of the region (optional - if not
  supplied it will be calculated for you if you supply a
  shapefile)}

  \item{shapefile}{a shapefile of the region}

  \item{coords}{list of polygons describing the areas of
  interest}

  \item{gaps}{list of polygons describing the areas to be
  excluded}
}
\value{
  object of either class Region
}
\description{
  This creates an instance of the Region class. If the user
  supplied a shapefile all information will be extracted
  from here. Otherwise the user needs to specify a list of
  polygons describing the areas of interest (coords) and
  optionally a list of polygons describing the areas to be
  excluded (gaps).
}
\author{
  Laura Marshall
}

