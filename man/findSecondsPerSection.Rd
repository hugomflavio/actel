% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/residency.R
\name{findSecondsPerSection}
\alias{findSecondsPerSection}
\title{Calculate number of seconds at each location per day}
\usage{
findSecondsPerSection(res, frame, the.range, num.step)
}
\arguments{
\item{res}{a residency list}

\item{the.range}{the first and last start.time for the specific tag}

\item{num.step}{the size of the block}
}
\value{
A data frame containing the number of seconds spent at each location for a specific timeframe
}
\description{
Calculate number of seconds at each location per day
}
\keyword{internal}
