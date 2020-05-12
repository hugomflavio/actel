#' Create a Default Workspace
#'
#' Produces the files and folders required to run the package, which the user can use as a template.
#' Note: This function will create a new sub-directory in the current directory and write files into it.
#' 
#' @param dir The name of the target directory. Will be created if not present
#'
#' @examples
#' \dontrun{
#' # running createWorkspace deploys template files for an analysis
#' # by default, createWorkspace creates a directory called 'actel_workspace'
#' # but this can be changed with the argument 'dir'.
#' createWorkspace(dir = "my_new_folder")
#' }
#' 
#' @return No return value, called for side effects
#' 
#' @export
#' 
createWorkspace <- function(dir = "actel_workspace") {
  if (interactive() & dir.exists(dir)) {
    warning("The folder ", dir, " already exists. Continuing may overwrite some of its contents.", call. = FALSE, immediate. = TRUE)
    decision <- readline("Proceed? (y/N) ")
    if (decision != "y" & decision != "Y")
      stop("Function stopped by user command.\n", call. = FALSE)
  }

  if (!dir.exists(dir))
    dir.create(dir)

  spatial <- data.frame(
    Station.name = c("Example station1", "Example station2", "Example station3", "Example release1", "Example release2"),
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
    Station.name = c("Example station1", "Example station2", "Example station3"),
    Start = c("2018-01-25 12:00:00", "2018-01-25 12:00:00", "2018-01-25 12:00:00"),
    Stop = c("2018-04-03 12:00:00", "2018-04-03 12:00:00", "2018-04-03 12:00:00"))

  write.csv(spatial, paste(dir, "spatial.csv", sep ="/"), row.names = FALSE)
  write.csv(biometrics, paste(dir, "biometrics.csv", sep ="/"), row.names = FALSE)
  write.csv(deployments, paste(dir, "deployments.csv", sep ="/"), row.names = FALSE)

  if (!dir.exists(paste(dir, "detections", sep ="/"))) 
    dir.create(paste(dir, "detections", sep ="/"))
  message(paste0("M: Workspace files created in folder '", dir,"'."))
}

#' Deploy Example Data
#'
#' Creates a ready-to-run workspace with example data.
#' Note: This function will create a new sub-directory called 'exampleWorkspace' 
#' in the current directory and write files into it.
#' 
#' @param spatial,biometrics,detections,deployments Example datasets provided with the package.
#'
#' @examples
#' \dontrun{
#' # deploy a minimal dataset to try actel!
#' exampleWorkspace()
#' 
#' # you can then move into the newly created folder
#' setwd("exampleWorkspace")
#' 
#' # and run the example analysis
#' results <- explore(tz = 'Europe/Copenhagen', report = TRUE)
#' 
#' # Have a look at the results and the html report.
#' names(results)
#' 
#' # you can also try running the migration and residency analyses
#' m.results <- migration(tz = 'Europe/Copenhagen', 
#'  sections = c('River', 'Fjord', 'Sea'), report = TRUE)
#' 
#' r.results <- migration(tz = 'Europe/Copenhagen', 
#'  sections = c('River', 'Fjord', 'Sea'), report = TRUE)
#' }
#' 
#' @return No return value, called for side effects.
#' 
#' @export
#' 
exampleWorkspace <- function(spatial = example.spatial, biometrics = example.biometrics, detections = example.detections, deployments = example.deployments) {
  if (interactive() & dir.exists("exampleWorkspace")) {
    warning("The folder exampleWorkspace already exists. Continuing may overwrite some of its contents.", call. = FALSE, immediate. = TRUE)
    decision <- readline("Proceed? (y/N) ")
    if (decision != "y" & decision != "Y")
      stop("Function stopped by user command.\n", call. = FALSE)
  }

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
  # move into the newly created folder
  setwd('exampleWorkspace')\n
  # Run analysis. Note: This will open an analysis report on your web browser.
  results <- explore(tz = 'Europe/Copenhagen', report = TRUE)\n
Once finished, explore the html report and the object 'results' for the output.")
}

#' Example spatial data
#'
#' A dataset containing the positions of the deployed ALS and release site.
#'
#' @format A data frame with 18 rows and 6 variables:
#' \describe{
#'   \item{Station.name}{The name of the ALS or release site}
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
#'   \item{Station.name}{The name given to the receiver}
#'   \item{Start}{The date and time of the deployment}
#'   \item{Stop}{The date and time of the retrieval}
#' }
#' @source Data collected by the authors.
#' 
#' @keywords internal
#' 
"example.deployments"

#' Example distances matrix
#'
#' A matrix containing the distances between stations and release sites
#'
#' @format A 18 * 18 matrix
#' @source Data collected by the authors.
#' 
#' @keywords internal
#' 
"example.distances"
