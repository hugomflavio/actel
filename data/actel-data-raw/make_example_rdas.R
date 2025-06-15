library("actel")
example.detections <- read.csv("example.detections.csv")

example.biometrics <- actel:::loadBio("biometrics.csv",
                                      tz = "Europe/Copenhagen")

example.spatial <- actel::loadSpatial("spatial.csv")
example.spatial <- example.spatial[,-ncol(example.spatial)]

example.distances <- as.matrix(read.csv("distances.csv", row.names = 1))

example.deployments <- actel:::loadDeployments("deployments.csv",
                                               tz = "Europe/Copenhagen")

x <- preload(spatial = example.spatial,
             biometrics = example.biometrics,
             tz = "Europe/Copenhagen",
             detections = example.detections,
             deployments = example.deployments,
             distances = example.distances)

example.results <- migration(datapack = x)
n
n

aux <- residency(datapack = x)
n
n
n

keep_these <- c("time.ratios", "time.positions",
                "global.ratios", "group.ratios")
additional.residency.results <- aux[keep_these]

usethis::use_data(
  example.detections, 
  example.distances, 
  example.biometrics, 
  example.spatial, 
  example.deployments, overwrite = TRUE, internal = TRUE)

usethis::use_data(example.results, overwrite = TRUE)

usethis::use_data(additional.residency.results, overwrite = TRUE)
