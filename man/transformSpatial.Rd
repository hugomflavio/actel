% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load.R
\name{transformSpatial}
\alias{transformSpatial}
\title{Process spatial elements}
\usage{
transformSpatial(spatial, bio, dot_list, first.array = NULL)
}
\arguments{
\item{bio}{A table with the tags and biometrics of the studied animals.}

\item{first.array}{Either NULL or the top level array in the study area.}
}
\value{
The stations, release sites and array order.
}
\description{
Creates a list containing multiple spatial elements required
throughout the analyses
}
\keyword{internal}
