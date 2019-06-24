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
  cat("The example worskpace is now ready. To run the analysis on the example data, run:\n
\tresults <- actel(path = 'exampleWorkspace', sections = c('River','Fjord','Sea'), success.arrays = 'Sea1',
\t\tminimum.detections = 2, maximum.time = 60, speed.method = 'last to first', if.last.skip.section = TRUE,
\t\ttz.study.area = 'Europe/Copenhagen', start.timestamp = NULL, end.timestamp = NULL, report = TRUE, redraw = TRUE, 
\t\toverride = NULL, exclude.tags = NULL, debug = FALSE, cautious.assignment = TRUE, replicate = NULL)\n
And follow the instructions as they come. Once finished, explore the object 'results' for the output.\n")
}
