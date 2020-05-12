skip_on_cran()

tests.home <- getwd()
setwd(tempdir())

exampleWorkspace()
setwd("exampleWorkspace")
write.csv(example.distances, "distances.csv")

sections <- c("River", "Fjord", "Sea")

study.data <- suppressWarnings(loadStudyData(tz = "Europe/Copenhagen", start.time = NULL, 
	stop.time = NULL, sections = sections, exclude.tags = NULL))
# n
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

test_that("assembleTimetable correctly extracts fish information", {
  xmoves <- moves
  attributes(xmoves[[1]])$p.type <- "Manual"
  xmoves[[1]]$Valid[18] <- FALSE
  vm <- xmoves
  vm[[1]] <- vm[[1]][-18, ]

  output <- assembleTimetable(vm = vm, all.moves = xmoves, sections = sections, 
    arrays = arrays, dist.mat = dist.mat, invalid.dist = invalid.dist, speed.method = "last to first", 
    if.last.skip.section = TRUE, success.arrays = "Sea1")
  timetable <<- output

  expect_equal(colnames(output), c('Time.until.River', 'Speed.to.River', 'First.station.River', 'Arrived.River', 
    'Time.in.River', 'Speed.in.River', 'Last.station.River', 'Left.River', 'Time.until.Fjord', 'Speed.to.Fjord', 
    'First.station.Fjord', 'Arrived.Fjord', 'Time.in.Fjord', 'Speed.in.Fjord', 'Last.station.Fjord', 'Left.Fjord', 
    'Time.until.Sea', 'Speed.to.Sea', 'First.station.Sea', 'Arrived.Sea', 'Time.in.Sea', 'Speed.in.Sea', 
    'Last.station.Sea', 'Left.Sea', 'Very.last.array', 'Status', 'Valid.detections', 'Invalid.detections', 
    'Backwards.movements', 'Max.cons.back.moves', 'P.type', 'Transmitter'))
  expect_equal(output$Arrived.River[1], as.character(moves[[1]]$First.time[1]))
  expect_equal(output$Left.River[2], as.character(moves[[2]]$Last.time[6]))
  expect_equal(output$Arrived.Fjord[1], as.character(moves[[1]]$First.time[7]))
  expect_equal(output$Left.Fjord[2], as.character(moves[[2]]$Last.time[12]))
  expect_equal(output$Arrived.Sea[1], NA_character_)
  expect_equal(output$Left.Sea[2], as.character(moves[[2]]$Last.time[13]))
  expect_equal(output$Invalid.detections, c(11, 0))
  expect_equal(output$Very.last.array, c("Fjord2", "Sea1"))
  expect_equal(output$Status, c("Disap. in Sea", "Succeeded"))
  expect_equal(output$P.type, c("Manual", "Auto"))
  expect_equal(output$Transmitter, names(moves))
  expect_equal(output$Time.until.River, c(NA, NA)) # Time.until first section is only filled in during assembleOutput
  expect_equal(output$Time.in.River, c(94010, 133524))
  expect_equal(output$Time.in.Fjord, c(1368315, 929074))
  expect_equal(output$Time.in.Sea, c(NA, 33))

  to.match <- output
  output <- assembleTimetable(vm = vm, all.moves = xmoves, sections = sections, 
    arrays = arrays, dist.mat = dist.mat, invalid.dist = invalid.dist, speed.method = "first to first", 
    if.last.skip.section = FALSE, success.arrays = "Sea1")
  expect_equal(colnames(output), c('Time.until.River', 'Speed.to.River', 'First.station.River', 'Arrived.River', 
    'Time.in.River', 'Last.station.River', 'Left.River', 'Time.until.Fjord', 'Speed.to.Fjord', 
    'First.station.Fjord', 'Arrived.Fjord', 'Time.in.Fjord', 'Last.station.Fjord', 'Left.Fjord', 
    'Time.until.Sea', 'Speed.to.Sea', 'First.station.Sea', 'Arrived.Sea', 'Time.in.Sea', 
    'Last.station.Sea', 'Left.Sea', 'Very.last.array', 'Status', 'Valid.detections', 'Invalid.detections', 
    'Backwards.movements', 'Max.cons.back.moves', 'P.type', 'Transmitter'))
  expect_false(any(to.match$Speed.to.Fjord == output$Speed.to.Fjord))
  expect_true(to.match$Speed.to.Sea[2] != output$Speed.to.Sea[2])
  expect_equal(output$Status, c("Disap. in Fjord", "Succeeded"))

  output <- assembleTimetable(vm = vm, all.moves = xmoves, sections = sections, 
    arrays = arrays, dist.mat = dist.mat, invalid.dist = TRUE, speed.method = "first to first", 
    if.last.skip.section = FALSE, success.arrays = "Sea1")
  expect_equal(colnames(output), c('Time.until.River', 'First.station.River', 'Arrived.River', 
    'Time.in.River', 'Last.station.River', 'Left.River', 'Time.until.Fjord', 
    'First.station.Fjord', 'Arrived.Fjord', 'Time.in.Fjord', 'Last.station.Fjord', 'Left.Fjord', 
    'Time.until.Sea', 'First.station.Sea', 'Arrived.Sea', 'Time.in.Sea', 
    'Last.station.Sea', 'Left.Sea', 'Very.last.array', 'Status', 'Valid.detections', 'Invalid.detections', 
    'Backwards.movements', 'Max.cons.back.moves', 'P.type', 'Transmitter'))

  xmoves <- moves
  xmoves[[1]]$Array[7] <- "River5"
  xmoves[[1]]$Array[8] <- "River4"
  xmoves[[1]]$Array[11] <- "Fjord2"
  xmoves[[2]]$Valid <- FALSE
  vm <- xmoves[1]
  vm[[1]] <- vm[[1]][-18, ]

  output <- assembleTimetable(vm = vm, all.moves = xmoves, sections = sections, 
    arrays = arrays, dist.mat = dist.mat, invalid.dist = invalid.dist, speed.method = "first to first", 
    if.last.skip.section = FALSE, success.arrays = "Sea1")
  expect_equal(output$Backwards.movements, c(3, 0))
  expect_equal(output$Max.cons.back.moves, c(2, 0))

  xmoves[[2]] <- xmoves[[2]][1, ]
  output <- assembleTimetable(vm = vm, all.moves = xmoves, sections = sections, 
    arrays = arrays, dist.mat = dist.mat, invalid.dist = invalid.dist, speed.method = "first to first", 
    if.last.skip.section = FALSE, success.arrays = "Sea1")
  expect_equal(output$Backwards.movements, c(3, 0))
  expect_equal(output$Max.cons.back.moves, c(2, 0))
})

test_that("assembleOutput correctly combines the timetable and the biometrics", {
  output <- assembleOutput(timetable = timetable, bio = bio, spatial = spatial, 
    sections = sections, dist.mat = dist.mat, invalid.dist = invalid.dist, tz = "Europe/Copenhagen")
  expect_equal(colnames(output), c('Transmitter', 'Release.date', 'Release.site', 'Serial.nr', 'Signal', 
    'Group', 'Total.Length.mm', 'Mass.g', 'Time.until.River', 'Speed.to.River', 'First.station.River', 
    'Arrived.River', 'Time.in.River', 'Speed.in.River', 'Last.station.River', 'Left.River', 'Time.until.Fjord', 
    'Speed.to.Fjord', 'First.station.Fjord', 'Arrived.Fjord', 'Time.in.Fjord', 'Speed.in.Fjord', 
    'Last.station.Fjord', 'Left.Fjord', 'Time.until.Sea', 'Speed.to.Sea', 'First.station.Sea', 
    'Arrived.Sea', 'Time.in.Sea', 'Speed.in.Sea', 'Last.station.Sea', 'Left.Sea', 'Very.last.array', 
    'Status', 'Valid.detections', 'Invalid.detections', 'Backwards.movements', 'Max.cons.back.moves', 'P.type'))
  expect_false(all(is.na(output$Time.until.River)))
  expect_false(all(is.na(output$Speed.to.River)))
  expect_equal(class(output$Arrived.River), c("POSIXct", "POSIXt"))
  expect_equal(class(output$Time.in.River), "difftime")
  expect_equal(class(output$Left.River), c("POSIXct", "POSIXt"))
  expect_equal(class(output$Arrived.Fjord), c("POSIXct", "POSIXt"))
  expect_equal(class(output$Time.in.Fjord), "difftime")
  expect_equal(class(output$Left.Fjord), c("POSIXct", "POSIXt"))
  expect_equal(class(output$Arrived.Sea), c("POSIXct", "POSIXt"))
  expect_equal(class(output$Time.in.Sea), "difftime")
  expect_equal(class(output$Left.Sea), c("POSIXct", "POSIXt"))
  expect_false(any(is.na(output$Valid.detections)))
  expect_false(any(is.na(output$All.detections)))
  expect_false(any(is.na(output$Status)))
  expect_equal(levels(output$Very.last.array), c("Release", names(arrays)))
  expect_equal(levels(output$Status), c("Disap. in River", "Disap. in Fjord", "Disap. in Sea", "Succeeded"))
  expect_equal(unique(output$P.type), c("Manual", "Auto", "Skipped"))
})

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
setwd(tests.home)
rm(list = ls())
