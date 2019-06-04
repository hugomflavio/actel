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
