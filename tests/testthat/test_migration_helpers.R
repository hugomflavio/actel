skip_on_cran()
oldtz <- Sys.getenv('TZ', unset = NA)
Sys.setenv(TZ = 'UTC')

tests.home <- getwd()
setwd(tempdir())

exampleWorkspace("exampleWorkspace", force = TRUE)
setwd("exampleWorkspace")
write.csv(example.distances, "distances.csv")

study.data <- suppressWarnings(loadStudyData(tz = "Europe/Copenhagen", start.time = NULL,
	stop.time = NULL, exclude.tags = NULL))
detections.list <- study.data$detections.list
bio <- study.data$bio
spatial <- study.data$spatial
dist.mat <- study.data$dist.mat
arrays <- study.data$arrays

moves <- groupMovements(detections.list = detections.list[1:2], bio = bio, spatial = spatial,
    speed.method = "last to first", max.interval = 60, tz = "Europe/Copenhagen", dist.mat = dist.mat)

aux <- names(moves)
moves <- lapply(names(moves), function(tag) {
    speedReleaseToFirst(tag = tag, bio = bio, movements = moves[[tag]],
                        dist.mat = dist.mat, speed.method = "last to first")
  })
names(moves) <- aux
rm(aux)

test_that("assembleTimetable correctly extracts tag information", {
  xmoves <- moves
  attributes(xmoves[[1]])$p.type <- "Manual"
  xmoves[[1]]$Valid[18] <- FALSE
  vm <- xmoves
  vm[[1]] <- vm[[1]][-18, ]

  secmoves <- lapply(seq_along(vm), function(i) {
    tag <- names(vm)[i]
    appendTo("debug", paste0("debug: Compiling valid section movements for tag ", tag,"."))
    output <- sectionMovements(movements = vm[[i]], spatial = spatial, valid.dist = attributes(dist.mat)$valid)
    return(output)
  })
  names(secmoves) <- names(vm)

  output <- assembleTimetable(secmoves = secmoves, valid.moves = vm, all.moves = xmoves, spatial = spatial,
    arrays = arrays, dist.mat = dist.mat, speed.method = "last to first",
    if.last.skip.section = TRUE, success.arrays = "A9", bio = bio, tz = "Europe/Copenhagen")
  timetable <<- output

  expect_equal(colnames(output), c('Times.entered.River', 'Average.time.until.River', 'Average.speed.to.River',
    'First.array.River', 'First.station.River', 'First.arrived.River', 'Average.time.in.River',
    'Average.speed.in.River', 'Last.array.River', 'Last.station.River', 'Last.left.River',
    'Total.time.in.River', 'Times.entered.Fjord', 'Average.time.until.Fjord', 'Average.speed.to.Fjord',
    'First.array.Fjord', 'First.station.Fjord', 'First.arrived.Fjord', 'Average.time.in.Fjord',
    'Average.speed.in.Fjord', 'Last.array.Fjord', 'Last.station.Fjord', 'Last.left.Fjord',
    'Total.time.in.Fjord', 'Times.entered.Sea', 'Average.time.until.Sea', 'Average.speed.to.Sea',
    'First.array.Sea', 'First.station.Sea', 'First.arrived.Sea', 'Average.time.in.Sea', 'Average.speed.in.Sea',
    'Last.array.Sea', 'Last.station.Sea', 'Last.left.Sea', 'Total.time.in.Sea', 'Very.last.array',
    'Very.last.time', 'Status', 'Valid.detections', 'Valid.events', 'Invalid.detections',
    'Invalid.events', 'Backwards.movements', 'Max.cons.back.moves', 'P.type', 'Transmitter'))
  expect_equal(output$First.arrived.River[1], moves[[1]]$First.time[1])
  expect_equal(output$Last.left.River[2], moves[[2]]$Last.time[6])
  expect_equal(output$First.arrived.Fjord[1], moves[[1]]$First.time[7])
  expect_equal(output$Last.left.Fjord[2], moves[[2]]$Last.time[12])
  expect_equal(as.numeric(output$First.arrived.Sea[1]), NA_real_)
  expect_equal(output$Last.left.Sea[2], moves[[2]]$Last.time[13])
  expect_equal(output$Invalid.detections, c(11, 0))
  expect_equal(output$Very.last.array, c("A8", "A9"))
  expect_equal(output$Status, c("Disap. in Sea", "Succeeded"))
  expect_equal(output$P.type, c("Manual", "Auto"))
  expect_equal(output$Transmitter, names(moves))
  expect_equal(as.numeric(output$Average.time.until.River), c(12.323, 13.917)) # Time.until first section is only filled in during assembleOutput
  expect_equal(as.numeric(output$Total.time.in.River), c(1.088, 1.545))
  expect_equal(as.numeric(output$Total.time.in.Fjord), c(15.837, 10.753))
  expect_equal(as.numeric(output$Total.time.in.Sea), c(NA, 0.55))
})

test_that("assembleTimetable correctly handles speed methods and invalid.dist", {
  moves.ff <- groupMovements(detections.list = detections.list[1:2], bio = bio, spatial = spatial,
      speed.method = "last to last", max.interval = 60, tz = "Europe/Copenhagen", dist.mat = dist.mat)

  aux <- names(moves.ff)
  xmoves <- lapply(names(moves.ff), function(tag) {
      speedReleaseToFirst(tag = tag, bio = bio, movements = moves.ff[[tag]],
                          dist.mat = dist.mat, speed.method = "last to last")
    })
  names(xmoves) <- aux
  rm(aux)

  xmoves.ff <- moves.ff
  attributes(xmoves.ff[[1]])$p.type <- "Manual"
  xmoves.ff[[1]]$Valid[18] <- FALSE
  vm.ff <- xmoves.ff
  vm.ff[[1]] <- vm.ff[[1]][-18, ]

  secmoves.ff <- lapply(seq_along(vm.ff), function(i) {
    tag <- names(vm.ff)[i]
    appendTo("debug", paste0("debug: Compiling valid section movements for tag ", tag,"."))
    output <- sectionMovements(movements = vm.ff[[i]], spatial = spatial, valid.dist = attributes(dist.mat)$valid)
    return(output)
  })
  names(secmoves.ff) <- names(vm.ff)

  output <- assembleTimetable(secmoves = secmoves.ff, valid.moves = vm.ff, all.moves = xmoves.ff, spatial = spatial,
    arrays = arrays, dist.mat = dist.mat, speed.method = "last to last",
    if.last.skip.section = FALSE, success.arrays = "A9", bio = bio, tz = "Europe/Copenhagen")
  expect_equal(colnames(output), c('Times.entered.River', 'Average.time.until.River', 'Average.speed.to.River',
    'First.array.River', 'First.station.River', 'First.arrived.River', 'Average.time.in.River',
    'Last.array.River', 'Last.station.River', 'Last.left.River', 'Total.time.in.River', 'Times.entered.Fjord',
    'Average.time.until.Fjord', 'Average.speed.to.Fjord', 'First.array.Fjord', 'First.station.Fjord',
    'First.arrived.Fjord', 'Average.time.in.Fjord', 'Last.array.Fjord', 'Last.station.Fjord', 'Last.left.Fjord',
    'Total.time.in.Fjord', 'Times.entered.Sea', 'Average.time.until.Sea', 'Average.speed.to.Sea',
    'First.array.Sea', 'First.station.Sea', 'First.arrived.Sea', 'Average.time.in.Sea', 'Last.array.Sea',
    'Last.station.Sea', 'Last.left.Sea', 'Total.time.in.Sea', 'Very.last.array', 'Very.last.time', 'Status',
    'Valid.detections', 'Valid.events', 'Invalid.detections', 'Invalid.events', 'Backwards.movements',
    'Max.cons.back.moves', 'P.type', 'Transmitter'))
  expect_false(any(timetable$Average.speed.to.Fjord == output$Average.speed.to.Fjord))
  expect_true(timetable$Average.speed.to.Sea[2] != output$Average.speed.to.Sea[2])
  expect_equal(output$Status, c("Disap. in Fjord", "Succeeded"))

  xdist <- dist.mat
  attributes(xdist)$valid <- FALSE
  output <- assembleTimetable(secmoves = secmoves.ff, valid.moves = vm.ff, all.moves = xmoves.ff, spatial = spatial,
    arrays = arrays, dist.mat = xdist, speed.method = "last to last",
    if.last.skip.section = FALSE, success.arrays = "A9", bio = bio, tz = "Europe/Copenhagen")
  expect_equal(colnames(output), c('Times.entered.River', 'Average.time.until.River',
    'First.array.River', 'First.station.River', 'First.arrived.River', 'Average.time.in.River',
    'Last.array.River', 'Last.station.River', 'Last.left.River', 'Total.time.in.River',
    'Times.entered.Fjord', 'Average.time.until.Fjord', 'First.array.Fjord', 'First.station.Fjord',
    'First.arrived.Fjord', 'Average.time.in.Fjord', 'Last.array.Fjord', 'Last.station.Fjord',
    'Last.left.Fjord', 'Total.time.in.Fjord', 'Times.entered.Sea', 'Average.time.until.Sea',
    'First.array.Sea', 'First.station.Sea', 'First.arrived.Sea', 'Average.time.in.Sea',
    'Last.array.Sea', 'Last.station.Sea', 'Last.left.Sea', 'Total.time.in.Sea', 'Very.last.array',
    'Very.last.time', 'Status', 'Valid.detections', 'Valid.events', 'Invalid.detections',
    'Invalid.events', 'Backwards.movements', 'Max.cons.back.moves', 'P.type', 'Transmitter'))

  xmoves.ff <- moves.ff
  xmoves.ff[[1]]$Array[7] <- "A5"
  xmoves.ff[[1]]$Array[8] <- "A4"
  xmoves.ff[[1]]$Array[11] <- "A8"
  xmoves.ff[[2]]$Valid <- FALSE
  vm.ff <- xmoves.ff[1]
  vm.ff[[1]] <- vm.ff[[1]][-18, ]
  secmoves.ff <- lapply(seq_along(vm.ff), function(i) {
    tag <- names(vm.ff)[i]
    appendTo("debug", paste0("debug: Compiling valid section movements for tag ", tag,"."))
    output <- sectionMovements(movements = vm.ff[[i]], spatial = spatial, valid.dist = attributes(dist.mat)$valid)
    return(output)
  })
  names(secmoves.ff) <- names(vm.ff)

  output <- assembleTimetable(secmoves = secmoves.ff, valid.moves = vm.ff, all.moves = xmoves.ff, spatial = spatial,
    arrays = arrays, dist.mat = dist.mat, speed.method = "last to last",
    if.last.skip.section = FALSE, success.arrays = "A9", bio = bio, tz = "Europe/Copenhagen")
  expect_equal(output$Backwards.movements, c(3))
  expect_equal(output$Max.cons.back.moves, c(2))

  xmoves.ff[[2]] <- xmoves.ff[[2]][1, ]
  output <- assembleTimetable(secmoves = secmoves.ff, valid.moves = vm.ff, all.moves = xmoves.ff, spatial = spatial,
    arrays = arrays, dist.mat = dist.mat, speed.method = "last to last",
    if.last.skip.section = FALSE, success.arrays = "A9", bio = bio, tz = "Europe/Copenhagen")
  expect_equal(output$Backwards.movements, c(3))
  expect_equal(output$Max.cons.back.moves, c(2))
})

test_that("assembleOutput correctly combines the timetable and the biometrics", {
  output <- assembleOutput(timetable = timetable, bio = bio, spatial = spatial,
    dist.mat = dist.mat, tz = "Europe/Copenhagen")
  expect_equal(colnames(output), c('Transmitter', 'Release.date', 'Release.site', 'Serial.nr', 'Signal',
    'Group', 'Total.Length.mm', 'Mass.g', 'Times.entered.River', 'Average.time.until.River',
    'Average.speed.to.River', 'First.array.River', 'First.station.River', 'First.arrived.River',
    'Average.time.in.River', 'Average.speed.in.River', 'Last.array.River', 'Last.station.River',
    'Last.left.River', 'Total.time.in.River', 'Times.entered.Fjord', 'Average.time.until.Fjord',
    'Average.speed.to.Fjord', 'First.array.Fjord', 'First.station.Fjord', 'First.arrived.Fjord',
    'Average.time.in.Fjord', 'Average.speed.in.Fjord', 'Last.array.Fjord', 'Last.station.Fjord',
    'Last.left.Fjord', 'Total.time.in.Fjord', 'Times.entered.Sea', 'Average.time.until.Sea',
    'Average.speed.to.Sea', 'First.array.Sea', 'First.station.Sea', 'First.arrived.Sea',
    'Average.time.in.Sea', 'Average.speed.in.Sea', 'Last.array.Sea', 'Last.station.Sea', 'Last.left.Sea',
    'Total.time.in.Sea', 'Very.last.array', 'Very.last.time', 'Status', 'Valid.detections', 'Valid.events',
    'Invalid.detections', 'Invalid.events', 'Backwards.movements', 'Max.cons.back.moves', 'P.type'))
  expect_equal(class(output$Total.time.in.Sea), "difftime")
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

secmoves <- lapply(seq_along(vm), function(i) {
  tag <- names(vm)[i]
  appendTo("debug", paste0("debug: Compiling valid section movements for tag ", tag,"."))
  output <- sectionMovements(movements = vm[[i]], spatial = spatial, valid.dist = attributes(dist.mat)$valid)
  return(output)
})
names(secmoves) <- names(vm)

timetable <- assembleTimetable(secmoves = secmoves, valid.moves = vm, all.moves = xmoves, spatial = spatial,
  arrays = arrays, dist.mat = dist.mat, speed.method = "last to first",
  if.last.skip.section = TRUE, success.arrays = "A9", bio = bio, tz = "Europe/Copenhagen")

status.df <- assembleOutput(timetable = timetable, bio = bio, spatial = spatial,
  dist.mat = dist.mat, tz = "Europe/Copenhagen")

test_that("assembleGroupOverview is working as expected", {
  output <- assembleSectionOverview(status.df = status.df, spatial = spatial)
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

if (is.na(oldtz)) Sys.unsetenv("TZ") else Sys.setenv(TZ = oldtz)

rm(list = ls())
