#' Create a Default Workspace
#'
#' Produces the files and folders required to run the package, which the user can use as a template.
#' 
#' @param dir The name of the target directory. Will be created if not present
#'
#' @export
#' 
createWorkspace <- function(dir = "actel_workspace") {
  if (!dir.exists(dir)) 
    dir.create(dir)
  spatial <- data.frame(
    Station.Name = c("Example station1", "Example station2", "Example station3", "Example release1", "Example release2"),
    Latitude = c(8.411, 8.521, 8.402, 8.442, 8.442),
    Longitude = c(40.411, 40.521, 40.402, 40.442, 40.442), 
    Array = c("River1", "River1", "River2", "River1", "River2"), 
    Type = c("Hydrophone", "Hydrophone", "Hydrophone", "Release", "Release"))

  biometrics <- data.frame(
    Release.date = c("2018-02-01 10:05:00", "2018-02-01 10:10:00", "2018-02-01 10:15:00"), 
    Serial.nr = c("12340001", "12501034", "19340301"), 
    Signal = c(1, 1034, 301), 
    Length.mm = c(150, 160, 170), 
    Weight.g = c(40, 60, 50), 
    Group = c("Wild", "Hatchery", "Wild"), 
    Release.site = c("Example release1", "Example release1", "Example release2"))

  deployments <- data.frame(
    Receiver = c("123001", "123002", "331"), 
    Station.Name = c("Example station1", "Example station2", "Example station3"),
    Start = c("2018-01-25 12:00:00", "2018-01-25 12:00:00", "2018-01-25 12:00:00"),
    Stop = c("2018-04-03 12:00:00", "2018-04-03 12:00:00", "2018-04-03 12:00:00"))

  write.csv(spatial, paste(dir, "spatial.csv", sep ="/"), row.names = FALSE)
  write.csv(biometrics, paste(dir, "biometrics.csv", sep ="/"), row.names = FALSE)
  write.csv(deployments, paste(dir, "deployments.csv", sep ="/"), row.names = FALSE)

  if (!dir.exists(paste(dir, "detections", sep ="/"))) 
    dir.create(paste(dir, "detections", sep ="/"))
  message(paste0("M: Workspace files created in folder '", dir,"'."))
}

#' Start \code{actel} Example
#'
#' Creates a ready-to-run workspace with example data.
#' 
#' @param spatial,biometrics,detections,deployments Datasets provided with the package.
#'
#' @export
#' 
exampleWorkspace <- function(spatial = example.spatial, biometrics = example.biometrics, detections = example.detections, deployments = example.deployments) {
  if (!dir.exists("exampleWorkspace")) 
    dir.create("exampleWorkspace")
  write.csv(spatial, "exampleWorkspace/spatial.csv", row.names = FALSE)
  write.csv(biometrics, "exampleWorkspace/biometrics.csv", row.names = FALSE)
  write.csv(deployments, "exampleWorkspace/deployments.csv", row.names = FALSE)
  if (!dir.exists("exampleWorkspace/detections")) 
    dir.create("exampleWorkspace/detections")
  my.list <- split(detections, detections$Receiver)
  for (i in names(my.list)) {
    write.csv(my.list[[i]], paste0("exampleWorkspace/detections/", i, ".csv"), row.names = FALSE)
  }
  message("M: The example workspace is now ready. To run the analysis on the example data, run:\n
  results <- migration(path = 'exampleWorkspace', sections = c('River', 'Fjord', 'Sea'), 
  \t\t     success.arrays = 'Sea1', tz = 'Europe/Copenhagen')\n
And follow the instructions as they come. Once finished, explore the object 'results' for the output.")
}

#' Example spatial data
#'
#' A dataset containing the positions of the deployed ALS and release site.
#'
#' @format A data frame with 18 rows and 6 variables:
#' \describe{
#'   \item{Station.Name}{The name of the ALS or release site}
#'   \item{Latitude}{The latitude of the ALS or release site}
#'   \item{Longitude}{The longitude of the ALS or release site}
#'   \item{Array}{The Array to which the ALS belongs, or the first ALS array downstream of the release site.}
#'   \item{Type}{The type of spatial object (must be either Hydrophone or Release)}
#' }
#' @source Data collected by the authors.
#' 
#' @keywords internal
#' 
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
#' 
#' @keywords internal
#' 
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
#' 
#' @keywords internal
#' 
"example.detections"

#' Example deployment data
#'
#' A dataset containing the deployed receivers.
#'
#' @format A data frame with 17 rows and 4 variables:
#' \describe{
#'   \item{Receiver}{The receiver serial number}
#'   \item{Station.Name}{The name given to the receiver}
#'   \item{Start}{The date and time of the deployment}
#'   \item{Stop}{The date and time of the retrieval}
#' }
#' @source Data collected by the authors.
#' 
#' @keywords internal
#' 
"example.deployments"
