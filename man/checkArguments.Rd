% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check.R
\name{checkArguments}
\alias{checkArguments}
\title{Check argument quality}
\usage{
checkArguments(
  dp,
  tz,
  min.total.detections,
  min.per.event,
  max.interval,
  speed.method = c("last to first", "last to last", "first to first"),
  speed.warning,
  speed.error,
  start.time,
  stop.time,
  report,
  auto.open,
  save.detections,
  jump.warning,
  jump.error,
  inactive.warning,
  inactive.error,
  exclude.tags,
  override,
  print.releases,
  detections.y.axis = c("auto", "stations", "arrays"),
  replicates = NULL,
  section.warning,
  section.error,
  section.order = NULL,
  timestep = c("days", "hours"),
  back.warning = Inf,
  back.error = Inf
)
}
\arguments{
\item{dp}{A preloaded datapack (or NULL if no data was preloaded).}

\item{tz}{The time zone of the study area. Must match one of the values
present in \code{\link[base]{timezones}}.}

\item{min.total.detections}{Minimum number of times a tag must have
been detected during the study period for the detections to be considered true
and not just random noise. Defaults to 2.}

\item{min.per.event}{Minimum number of detections an event must have to be
deemed valid. For analyses with both array and section events, a vector of
two values can be provided. If only one value is provided, the same threshold
applies for both types of events. Defaults to 1.}

\item{max.interval}{The number of minutes that must pass between detections
for a new event to be created. Defaults to 60.}

\item{speed.method}{Can take two forms: 'last to first' or 'last to last'.
If 'last to first' (default), the last detection on the previous array is matched
to the first detection on the target array to perform the calculations.
If 'last to last', the last detection on the previous array is matched to the
last detection on the target array to perform the calculations.
If 'first to first', the first detection on the previous array is matched to the
first detection on the target array to perform the calculations.}

\item{speed.warning}{If a tag moves at a speed equal or greater than
\code{speed.warning} (in metres per second), a warning is issued. If left
NULL (default), no warnings are issued. Must be equal to or lower than \code{speed.error}}

\item{speed.error}{If a tag moves at a speed equal or greater than
\code{speed.error} (in metres per second), user intervention is suggested.
If left NULL (default), user intervention is never suggested.}

\item{start.time}{Detection data prior to the timestamp set in
\code{start.time} (in YYYY-MM-DD HH:MM:SS format) is not considered during
the analysis.}

\item{stop.time}{Detection data posterior to the timestamp set in
\code{stop.time} (in YYYY-MM-DD HH:MM:SS format) is not considered during
the analysis.}

\item{report}{Logical. Should an HTML report be created at the end of the
analysis? NOTE: Setting report to TRUE will generate an HTML file in the current
directory. Additionally, if auto.open = TRUE (default), the web browser will
automatically be launched to open the report once the function terminates.}

\item{auto.open}{Logical: Should the report be automatically opened once the
analysis is over? Defaults to TRUE. NOTE: If report = TRUE and auto.open = TRUE,
the web browser will automatically be launched to open the report once the
function terminates.}

\item{save.detections}{Logical: Should the processed detections be saved for
future runs?}

\item{jump.warning}{If a tag crosses a number of arrays equal or greater
than \code{jump.warning} without being detected, a warning is issued. Defaults
to 2. To disable jump warnings, set to Inf. Must be equal to or lower than \code{jump.error}.}

\item{jump.error}{If a tag crosses a number of arrays equal or greater than
\code{jump.error} without being detected, user intervention is suggested.
Defaults to 3. To disable user intervention suggestions, set to Inf.}

\item{inactive.warning}{If a tag spends a number of days equal or greater
than \code{inactive.warning} in a given array at the tail of the respective
detections, a warning is issued. If left NULL (default), no warnings are
issued. Must be equal to or lower than \code{innactive.error}.}

\item{inactive.error}{If a tag spends a number of days equal or greater than
\code{inactive.error} in a given array at the tail of the respective
detections, user intervention is suggested. If left NULL (default), user
intervention is never suggested.}

\item{exclude.tags}{A vector of tags that should be excluded from the
detection data before any analyses are performed. Intended to be used if
stray tags from a different code space but with the same signal as a target
tag are detected in the study area.}

\item{override}{A vector of signals for which the user intends to manually
define which movement events are valid and invalid.}

\item{print.releases}{Logical: Should the release sites be printed in the
study area diagrams?}

\item{detections.y.axis}{The type of y axis desired for the individual
detection plots. While the argument defaults to "auto", it can be hard-set
to one of "stations" or "arrays".}

\item{replicates}{A list containing, for each array to which intra-array
efficiency is to be calculated: The standard names of the stations to be
used as a replicate. See the vignettes for more details.}

\item{section.warning}{If a tag has section movement events with less or equal to
\code{section.warning} detections, a warning is issued. Defaults to 1.
To disable section warnings, set to 0. Must be equal to or greater than \code{section.error}.}

\item{section.error}{If a tag has section movement events with less or equal to
\code{section.error} detections, user intervention is suggested.
Defaults to 1. To disable user intervention suggestions, set to 0.}

\item{section.order}{A vector containing the order by which sections should
be aligned in the results.}

\item{timestep}{The resolution desired for the residency calculations.
One of "days" (default) or "hours".}

\item{back.warning}{If a tag moves backwards a number of arrays equal or
greater than \code{back.warning}, a warning is issued. Defaults
to 2. To disable back warnings, set to Inf.
Must be equal to or lower than \code{back.error}.}

\item{back.error}{If a tag moves backwards a number of arrays equal or
greater than \code{back.error}, user intervention is suggested.
Defaults to 3. To disable user intervention suggestions, set to Inf.}
}
\value{
updated parameters
}
\description{
Check argument quality
}
\keyword{internal}
