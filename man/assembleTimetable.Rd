% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/migration.R
\name{assembleTimetable}
\alias{assembleTimetable}
\title{Create the timetable}
\usage{
assembleTimetable(
  secmoves,
  valid.moves,
  all.moves,
  spatial,
  dot_list,
  bio,
  tz,
  dist.mat,
  speed.method,
  success.arrays
)
}
\arguments{
\item{secmoves}{the section-movements}

\item{valid.moves}{the valid array movements}

\item{all.moves}{all array movements}

\item{spatial}{The spatial list.}

\item{dot_list}{A list containing information on the study area configuration.}

\item{bio}{The biometrics table.}

\item{tz}{The time zone of the study area. Must match one of the values
present in \code{\link[base]{timezones}}.}

\item{dist.mat}{A matrix of the distances between the deployed ALS.}

\item{speed.method}{Can take two forms: 'last to first' or 'last to last'.
If 'last to first' (default), the last detection on the previous array is matched
to the first detection on the target array to perform the calculations.
If 'last to last', the last detection on the previous array is matched to the
last detection on the target array to perform the calculations.
If 'first to first', the first detection on the previous array is matched to the
first detection on the target array to perform the calculations.}

\item{success.arrays}{The arrays that mark the end of the study area. If a
tag crosses one of these arrays, the respective animal is considered to have
successfully migrated through the study area.}
}
\value{
A data frame containing the entering and leaving timestamps
for each section per target tag
}
\description{
Crawls trough the movement events of each tag to find when it entered and
left each section of the study area.
}
\keyword{internal}
