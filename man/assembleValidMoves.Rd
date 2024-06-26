% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/movements.R
\name{assembleValidMoves}
\alias{assembleValidMoves}
\title{Wrapper for simplifyMovements}
\usage{
assembleValidMoves(movements, bio, discard.first, speed.method, dist.mat)
}
\arguments{
\item{movements}{A list of movements for each tag.}

\item{bio}{The biometrics table.}

\item{discard.first}{A threshold amount of time (in hours) that must pass after
release for the respective detections to be valid. Set to 0 to discard only
the release-to-first-detection calculations.}

\item{speed.method}{Can take two forms: 'last to first' or 'last to last'.
If 'last to first' (default), the last detection on the previous array is matched
to the first detection on the target array to perform the calculations.
If 'last to last', the last detection on the previous array is matched to the
last detection on the target array to perform the calculations.
If 'first to first', the first detection on the previous array is matched to the
first detection on the target array to perform the calculations.}

\item{dist.mat}{A matrix of the distances between the deployed ALS.}
}
\value{
A list of valid movements
}
\description{
Wrapper for simplifyMovements
}
\keyword{internal}
