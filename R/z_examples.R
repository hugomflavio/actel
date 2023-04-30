#' Deprecated function.
#' 
#' Use blankWorkspace instead.
#'
#' @inheritParams blankWorkspace
#'
#' @examples
#' \donttest{
#' # createWorkspace is deprecated. Use blankWorkspace instead.
#' }
#'
#' @return No return value, called for side effects
#'
#' @export
#'
createWorkspace <- function(dir, force = FALSE) { # nocov start
  .Deprecated("blankWorkspace")
  blankWorkspace(dir = dir, force = force)
} # nocov end

#' Create a Blank Workspace
#'
#' Produces template files and folders required to run the \code{\link{explore}},
#' \code{\link{migration}} and \code{\link{residency}} functions.
#'
#' @param dir The name of the target directory. Will be created if not present.
#' @param force logical. Defaults to FALSE. Prevents deploying files in a directory that already exists without explicit permission.
#'
#' @examples
#' \donttest{
#' # running blankWorkspace deploys template
#' # files to a directory specified by the user
#' blankWorkspace(paste0(tempdir(), "/blankWorkspace_example"))
#' }
#'
#' @return No return value, called for side effects
#'
#' @export
#'
blankWorkspace <- function(dir, force = FALSE) {
  if (missing(dir))
    stop("Please specify a target directory.", call. = FALSE)

  if (!dir.exists(dir)) {
    dir.create(dir)
  } else {
    if (force)
      warning("The specified directory already exists, but force = TRUE. Deploying template files. Data loss may occur.", call. = FALSE, immediate. = TRUE)
    else
      stop("The specified directory already exists! Stopping to avoid accidental data loss. To continue regardless, run again with force = TRUE.", call. = FALSE)
  }


  spatial <- data.frame(
    Station.name = c("Example station1", "Example station2", "Example station3", "Example release1", "Example release2"),
    Latitude = c(8.411, 8.521, 8.402, 8.442, 8.442),
    Longitude = c(40.411, 40.521, 40.402, 40.442, 40.442),
    Array = c("A1", "A1", "A2", "A1", "A2"),
    Section = c("River", "River", "River", "", ""),
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
    Station.name = c("Example station1", "Example station2", "Example station3"),
    Start = c("2018-01-25 12:00:00", "2018-01-25 12:00:00", "2018-01-25 12:00:00"),
    Stop = c("2018-04-03 12:00:00", "2018-04-03 12:00:00", "2018-04-03 12:00:00"))

  write.csv(spatial, paste(dir, "spatial.csv", sep ="/"), row.names = FALSE)
  write.csv(biometrics, paste(dir, "biometrics.csv", sep ="/"), row.names = FALSE)
  write.csv(deployments, paste(dir, "deployments.csv", sep ="/"), row.names = FALSE)

  if (!dir.exists(paste(dir, "detections", sep ="/")))
    dir.create(paste(dir, "detections", sep ="/"))

  message(paste0("M: Template files created in folder '", dir,"'."))
}

#' Deploy Example Data
#'
#' Creates a ready-to-run workspace with example data.
#'
#' @inheritParams blankWorkspace
#'
#' @examples
#' \donttest{
#' # deploy a minimal dataset to try actel!
#' exampleWorkspace(paste0(tempdir(), "/exampleWorkspace_example"))
#' }
#'
#' @return No return value, called for side effects.
#'
#' @export
#'
exampleWorkspace <- function(dir, force = FALSE) {
  if (missing(dir))
    stop("Please specify a target directory.", call. = FALSE)

  if (!dir.exists(dir)) {
    dir.create(dir)
  } else {
    if (force)
      warning("The specified directory already exists, but force = TRUE. Deploying example files. Data loss may occur.", call. = FALSE, immediate. = TRUE)
    else
      stop("The specified directory already exists! Stopping to avoid accidental data loss. To continue regardless, run again with force = TRUE.", call. = FALSE)
  }


  write.csv(example.spatial, paste(dir, "spatial.csv", sep ="/"), row.names = FALSE)
  write.csv(example.biometrics, paste(dir, "biometrics.csv", sep ="/"), row.names = FALSE)
  write.csv(example.deployments, paste(dir, "deployments.csv", sep ="/"), row.names = FALSE)
  if (!dir.exists(paste(dir, "detections", sep ="/")))
    dir.create(paste(dir, "detections", sep ="/"))
  my.list <- split(example.detections, example.detections$Receiver)
  for (i in names(my.list)) {
    write.csv(my.list[[i]], paste0(dir, "/detections/", i, ".csv"), row.names = FALSE)
  }
  message("M: The example workspace is now ready. To run the analysis on the example data, run:\n
  # move into the newly created folder
  setwd('", dir, "')\n
  # Run analysis. Note: This will open an analysis report on your web browser.
  results <- explore(tz = 'Europe/Copenhagen', report = TRUE)\n
Once finished, explore the html report and the object 'results' for the output.")
}

#' Example spatial data
#'
#' A dataset containing the positions of the deployed hydrophone stations and release sites.
#'
#' @format A data frame with 18 rows and 6 variables:
#' \describe{
#'   \item{Station.name}{The name of the hydrophone station or release site}
#'   \item{Latitude}{The latitude of the hydrophone station or release site in WGS84}
#'   \item{Longitude}{The longitude of the hydrophone station or release site in WGS84}
#'   \item{x}{The x coordinate of the hydrophone station or release site in EPSG 32632}
#'   \item{y}{The y coordinate of the hydrophone station or release site in EPSG 32632}
#'   \item{Array}{If documenting a hydrophone station, the array to which the station belongs. 
#' If documenting a release site, the first array(s) where the fish is expected to be detected.}
#'   \item{Section}{The Section to which the hydrophone station belongs (irrelevant for the release sites).}
#'   \item{Type}{The type of spatial object (must be either 'Hydrophone' or 'Release')}
#' }
#' @source Data collected by the authors.
#'
#' @keywords internal
#'
#' @name example.spatial
NULL

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
#' @name example.biometrics
NULL

#' Example detection data
#'
#' A dataset containing the detections of the deployed ALS, for the 60 fish.
#'
#' @format A data frame with 14549 rows and 4 variables:
#' \describe{
#'   \item{Timestamp}{The date and time of the detections}
#'   \item{Receiver}{The ALS serial number}
#'   \item{CodeSpace}{The code space of the detected tag}
#'   \item{Signal}{The signal of the detected tag}
#'   \item{Sensor.Value}{The data recorded by the sensor (dummy values)}
#'   \item{Sensor.Unit}{The unit of the sensor data}
#' }
#' @source Data collected by the authors.
#'
#' @keywords internal
#'
#' @name example.detections
NULL

#' Example deployment data
#'
#' A dataset containing the deployed receivers.
#'
#' @format A data frame with 17 rows and 4 variables:
#' \describe{
#'   \item{Receiver}{The receiver serial number}
#'   \item{Station.name}{The name given to the receiver}
#'   \item{Start}{The date and time of the deployment}
#'   \item{Stop}{The date and time of the retrieval}
#' }
#' @source Data collected by the authors.
#'
#' @keywords internal
#'
#' @name example.deployments
NULL

#' Example distances matrix
#'
#' A matrix containing the distances between stations and release sites
#'
#' @format A 18 * 18 matrix
#' @source Data collected by the authors.
#'
#' @keywords internal
#'
#' @name example.distances
NULL

#' Example migration results
#'
#' A list with the results of a migration analysis ran on the example data. 
#' Note: Many objects were trimmed to reduce package size. Use exampleWorkspace()
#' To run an analysis on the example data and obtain a full results object.
#'
#' @format A list of outputs from migration()
#' @source Data collected by the authors.
#'
#' @keywords internal
#'
'example.results'

#' Example residency results
#'
#' A list with residency results to append to example.results, so some examples can run.
#'
#' @format A list of outputs from residency()
#' @source Data collected by the authors.
#'
#' @keywords internal
#'
'additional.residency.results'
