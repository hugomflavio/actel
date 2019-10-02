#' Create a default Workspace
#'
#' Produces the files and folders required to run the package, which the user can use as a template.
#'
#' @export
#' 
createWorkspace <- function() {
  spatial <- data.frame(Station.Name = c("Example station1", "Example station2", "Example station3", "Example release1", "Example release2"), Receiver = c("123001", "123002", "331", NA, NA), 
  Latitude = c(8.411, 8.521, 8.402, 8.442, 8.442), Longitude = c(40.411, 40.521, 40.402, 40.442, 40.442), Array = c("River1", "River1", "River2", "River1", "River2"), Type = c("Hydrophone", 
    "Hydrophone", "Hydrophone", "Release", "Release"))
  write.csv(spatial, "spatial.csv", row.names = FALSE)
  biometrics <- data.frame(Release.date = c("2018-02-01 10:05:00", "2018-02-01 10:10:00", "2018-02-01 10:15:00"), Serial.nr = c("12340001", "12501034", "19340301"), Signal = c(1, 1034, 301), 
  Length.mm = c(150, 160, 170), Weight.g = c(40, 60, 50), Array = c("Wild", "Hatchery", "Wild"), Release.site = c("Example release1", "Example release1", "Example release2"))
  write.csv(biometrics, "biometrics.csv", row.names = FALSE)
  if (!dir.exists("detections")) 
  dir.create("detections")
}

#' Create an example Workspace
#'
#' Creates a ready-to-run workspace with example data.
#'
#' @export
#' 
exampleWorkspace <- function() {
  if (!dir.exists("exampleWorkspace")) 
    dir.create("exampleWorkspace")
  write.csv(example.spatial, "exampleWorkspace/spatial.csv", row.names = FALSE)
  write.csv(example.biometrics, "exampleWorkspace/biometrics.csv", row.names = FALSE)
  if (!dir.exists("exampleWorkspace/detections")) 
    dir.create("exampleWorkspace/detections")
  my.list <- split(example.detections, example.detections$Receiver)
  for (i in names(my.list)) {
    write.csv(my.list[[i]], paste("exampleWorkspace/detections/", i, ".csv", sep = ""), row.names = FALSE)
  }
  cat("The example workspace is now ready. To run the analysis on the example data, run:\n
\tresults <- actel(path = 'exampleWorkspace', sections = c('River','Fjord','Sea'), success.arrays = 'Sea1',
\t\tminimum.detections = 2, maximum.time = 60, speed.method = 'last to first', if.last.skip.section = TRUE,
\t\ttz.study.area = 'Europe/Copenhagen', start.timestamp = NULL, end.timestamp = NULL, report = TRUE, redraw = TRUE, 
\t\toverride = NULL, exclude.tags = NULL, debug = FALSE, cautious.assignment = TRUE, replicate = NULL)\n
And follow the instructions as they come. Once finished, explore the object 'results' for the output.\n")
}

#' Example spatial data
#'
#' A dataset containing the positions of the deployed ALS and release site.
#'
#' @format A data frame with 18 rows and 6 variables:
#' \describe{
#'   \item{Station.Name}{The name of the ALS or release site}
#'   \item{Receiver}{The ALS deployed (leave empty if the row is a release site)}
#'   \item{Latitude}{The latitude of the ALS or release site}
#'   \item{Longitude}{The longitude of the ALS or release site}
#'   \item{Group}{The Array to which the ALS belongs, or the first ALS array downstream of the release site.}
#'   \item{Type}{The type of spatial object (must be either Hydrophone or Release)}
#' }
#' @source Data collected by the authors.
"example.spatial"

#' Example biometric data
#'
#' A dataset containing the tag numbers and respective biometrics of 60 fish.
#'
#' @format A data frame with 60 rows and 7 variables:
#' \describe{
#'   \item{Release.date}{The date and time of release}
#'   \item{Release.site}{The release site}
#'   \item{Serial.nr}{The serial number of the implanted tag}
#'   \item{Signal}{The Signal emitted by the implanted tag}
#'   \item{Group}{The group of the tagged fish}
#'   \item{Total.Length.mm}{The length of the tagged fish}
#'   \item{Mass.g}{The mass of the tagged fish}
#' }
#' @source Data collected by the authors.
"example.biometrics"

#' Example detection data
#'
#' A dataset containing the detections of the deployed ALS, for the 60 fish.
#'
#' @format A data frame with 14549 rows and 4 variables:
#' \describe{
#'   \item{Date.and.Time.UTC}{The date and time of the detections}
#'   \item{Receiver}{The ALS serial number}
#'   \item{Transmitter}{The detected tag code space and signal}
#'   \item{Station.Name}{The name given to the ALS}
#' }
#' @source Data collected by the authors.
"example.detections"
