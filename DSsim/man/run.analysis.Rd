\docType{methods}
\name{run.analysis}
\alias{run.analysis}
\title{S4 generic method to run analyses}
\arguments{
  \item{object}{an object of class simulation}

  \item{data}{an object of class Survey.Results}
}
\value{
  a list containing an S3 ddf object and optionally an S3
  dht object of relating to the model with the miminum
  criteria.
}
\description{
  This method carries out an analysis of distance sampling
  data. This method is provided to allow the user to
  perform diagnostic of the analysis used in the
  simulation. The data argument can be obtained by a call
  to \code{simulate.survey(object, dht.table = TRUE)}.
}

