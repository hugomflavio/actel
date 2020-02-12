exampleWorkspace()
setwd("exampleWorkspace")
write.csv(example.distances, "distances.csv")

sections <- c("River", "Fjord", "Sea")

study.data <- suppressWarnings(loadStudyData(tz = "Europe/Copenhagen", start.time = NULL, 
	stop.time = NULL, sections = sections, exclude.tags = NULL))
detections.list <- study.data$detections.list
bio <- study.data$bio
spatial <- study.data$spatial
dist.mat <- study.data$dist.mat
invalid.dist <- study.data$invalid.dist
arrays <- study.data$arrays

moves <- groupMovements(detections.list = detections.list[1:2], bio = bio, spatial = spatial,
    speed.method = "last to first", max.interval = 60, tz = "Europe/Copenhagen", 
    dist.mat = dist.mat, invalid.dist = invalid.dist)

aux <- names(moves)
moves <- lapply(names(moves), function(fish) {
    speedReleaseToFirst(fish = fish, bio = bio, movements = moves[[fish]],
                        dist.mat = dist.mat, invalid.dist = invalid.dist)
  })
names(moves) <- aux
rm(aux)

xmoves <- moves
attributes(xmoves[[1]])$p.type <- "Manual"
xmoves[[1]]$Valid[18] <- FALSE
vm <- xmoves
vm[[1]] <- vm[[1]][-18, ]

timetable <- assembleTimetable(vm = vm, all.moves = xmoves, sections = sections, 
  arrays = arrays, dist.mat = dist.mat, invalid.dist = invalid.dist, speed.method = "last to first", 
  if.last.skip.section = TRUE, success.arrays = "Sea1")

status.df <- assembleOutput(timetable = timetable, bio = bio, spatial = spatial, 
  sections = sections, dist.mat = dist.mat, invalid.dist = invalid.dist, tz = "Europe/Copenhagen")

test_that("assembleGroupOverview is working as expected", {
  output <- assembleSectionOverview(status.df = status.df, sections = sections)
  expect_equal(rownames(output), as.character(unique(bio$Group)))
  expect_equal(output$Total, c(30, 30))
  expect_equal(output$Disap..in.River, c(28, 30))
  expect_equal(output$Migrated.to.Fjord, c(2, 0))
  expect_equal(output$Disap..in.Fjord, c(0, 0))
  expect_equal(output$Migrated.to.Sea, c(2, 0))
  expect_equal(output$Disap..in.Sea, c(1, 0))
  expect_equal(output$Succeeded, c(1, 0))
})

setwd("..")
unlink("exampleWorkspace", recursive = TRUE)
rm(list = ls())
