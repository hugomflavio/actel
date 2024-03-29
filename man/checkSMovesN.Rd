% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check.R
\name{checkSMovesN}
\alias{checkSMovesN}
\title{Verify number of detections in section movements}
\usage{
checkSMovesN(
  secmoves,
  tag,
  section.warning,
  section.error,
  GUI,
  save.tables.locally,
  n
)
}
\arguments{
\item{secmoves}{the section movements list.}

\item{tag}{The tag being analysed.}

\item{section.warning}{If a tag has section movement events with less or equal to
\code{section.warning} detections, a warning is issued. Defaults to 1.
To disable section warnings, set to 0. Must be equal to or greater than \code{section.error}.}

\item{section.error}{If a tag has section movement events with less or equal to
\code{section.error} detections, user intervention is suggested.
Defaults to 1. To disable user intervention suggestions, set to 0.}

\item{GUI}{One of "needed", "always" or "never". If "needed", a new window is
opened to inspect the movements only if the movements table is too big to be
displayed in R's console. If "always", a graphical interface is always created
when the possibility to invalidate events emerges. If "never", a graphical
interface is never invoked. In this case, if the table to be displayed does
not fit in R's console, a temporary file will be saved and the user will be
prompted to open and examine that file. Defaults to "needed".}

\item{save.tables.locally}{Logical: If a table must be temporarily stored into a file
for user inspection, should it be saved in the current working directory, or
in R's temporary folder?}

\item{n}{A string indicating the overall progress.}
}
\value{
A list of section movements with updated 'Valid' columns
}
\description{
Verify number of detections in section movements
}
\keyword{internal}
