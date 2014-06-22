\name{make.ddf.analysis.list}
\alias{make.ddf.analysis.list}
\title{Creates a list of DDF.Analysis objects}
\usage{
  make.ddf.analysis.list(dsmodel, mrmodel = NULL, method,
    criteria, truncation = numeric(0), binned.data = FALSE,
    cutpoints = numeric(0))
}
\arguments{
  \item{dsmodel}{list of distance sampling model formula
  specifying the detection function (see \code{?ddf} for
  further details)}

  \item{mrmodel}{not yet implemented}

  \item{method}{only "ds" normal distance sampling
  currently implemented}

  \item{criteria}{model selection criteria (AIC, AICc, BIC)
  - only AIC implemented at present.}

  \item{\code{truncation}}{The truncation distance to be 
  applied in the analyses.}
  
  \item{\code{binned.data}}{Logical value indicating 
  Whether the data should be binned prior to analysis.}
  
  \item{\code{cutpoints}}{Numeric vector supplying the
  cutpoints to be used to bin the data.}
}
\value{
  list of objects of class DDF.Analysis
}
\description{
  This method creates a list of DDF.Analysis objects each
  of which describes a model to fit to the distance data.
  The simulation will fit each of these models to the data
  generated in the simulation and select the model with the
  minimum criteria value.
}
\examples{
ddf.analyses <- make.ddf.analysis.list(dsmodel = list(~cds(key = "hn",
 formula = ~1),~cds(key = "hr", formula = ~1)), method = "ds",
 criteria = "AIC")
}
\author{
  Laura Marshall
}
\seealso{
  \code{ddf} in \code{library(mrds)}
}

