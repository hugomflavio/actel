skip_on_cran()

tests.home <- getwd()
setwd(tempdir())
exampleWorkspace()
setwd("exampleWorkspace")
write.csv(example.distances, "distances.csv")

study.data <- suppressWarnings(loadStudyData(tz = "Europe/Copenhagen", start.time = NULL, 
	stop.time = NULL, sections = NULL, exclude.tags = NULL))
# n
detections.list <- study.data$detections.list
bio <- study.data$bio
spatial <- study.data$spatial
dist.mat <- study.data$dist.mat
invalid.dist <- study.data$invalid.dist

output <- groupMovements(detections.list = detections.list[1:2], bio = bio, spatial = spatial,
    speed.method = "last to first", max.interval = 60, tz = "Europe/Copenhagen", 
    dist.mat = dist.mat, invalid.dist = invalid.dist)

test_that("groupMovements assigns correct names to objects", {
	expect_equal(names(output), c("R64K-4451", "R64K-4453"))
	expect_equal(colnames(output[[1]]), c('Array', 'Detections', 'First.station', 'Last.station', 'First.time', 'Last.time', 'Time.travelling', 'Time.in.array', 'Average.speed.m.s', 'Valid'))
})

test_that("groupMovements assigns timestamps correctly", {
	m <- output$`R64K-4451`
	d <- detections.list$`R64K-4451`
	expect_equal(m$First.time[1], d$Timestamp[1])
	expect_equal(m$Last.time[1], d$Timestamp[m$Detections[1]])
	for (i in 2:nrow(m)) {
		expect_equal(m$First.time[i], d$Timestamp[sum(m$Detections[1:(i - 1)]) + 1])
		expect_equal(m$Last.time[i], d$Timestamp[sum(m$Detections[1:i])])
	}
})

test_that("groupMovements assigns arrays correctly", {
	m <- output$`R64K-4451`
	d <- detections.list$`R64K-4451`
	expect_equal(m$Array[1], as.character(d$Array[1]))
	expect_equal(m$Array[1], as.character(d$Array[m$Detections[1]]))
	for (i in 2:nrow(m)) {
		expect_equal(m$Array[i], as.character(d$Array[sum(m$Detections[1:(i - 1)]) + 1]))
		expect_equal(m$Array[i], as.character(d$Array[sum(m$Detections[1:i])]))
	}
})

test_that("groupMovements only uses dist.mat if it is valid", {
	aux <- groupMovements(detections.list = detections.list[1:2], bio = bio, spatial = spatial,
    speed.method = "last to first", max.interval = 60, tz = "Europe/Copenhagen", 
    dist.mat = dist.mat, invalid.dist = TRUE)
	expect_equal(names(aux), c("R64K-4451", "R64K-4453"))
	expect_equal(colnames(aux[[1]]), c('Array', 'Detections', 'First.station', 'Last.station', 'First.time', 'Last.time', 'Time.travelling', 'Time.in.array', 'Valid'))
})

test_that("groupMovements can handle unknown detections", {
	d <- detections.list[1:2]
	levels(d[[1]]$Array) <- c(levels(d[[1]]$Array), "Unknown")
	d[[1]]$Array[1] <- "Unknown"
	levels(d[[1]]$Standard.name) <- c(levels(d[[1]]$Standard.name), "Ukn.")
	d[[1]]$Standard.name[1] <- "Ukn."
	expect_warning(aux <- groupMovements(detections.list = d, bio = bio, spatial = spatial,
	  	speed.method = "last to first", max.interval = 60, tz = "Europe/Copenhagen", 
	  	dist.mat = dist.mat, invalid.dist = invalid.dist),
		"Movement events at 'Unknown' locations have been rendered invalid.", fixed = TRUE)
	expect_equal(names(aux), c("R64K-4451", "R64K-4453"))
	expect_equal(colnames(aux[[1]]), c('Array', 'Detections', 'First.station', 'Last.station', 'First.time', 'Last.time', 'Time.travelling', 'Time.in.array', 'Average.speed.m.s', 'Valid'))
	expect_equal(aux[[1]]$Array[1], "Unknown")
	expect_equal(aux[[1]]$First.station[1], "Ukn.")
	expect_equal(aux[[1]]$Last.station[1], "Ukn.")
	expect_equal(aux[[1]]$Average.speed.m.s[1], NA_real_)
	expect_equal(aux[[1]]$Average.speed.m.s[2], NA_real_)
	moves <<- aux
})

test_that("Switching speed.method leads to different speed results.", {
	aux <- groupMovements(detections.list = detections.list[1:2], bio = bio, spatial = spatial,
	    speed.method = "last to last", max.interval = 60, tz = "Europe/Copenhagen", 
	    dist.mat = dist.mat, invalid.dist = invalid.dist)
	expect_true(aux[[1]]$Average.speed.m.s[3] != moves[[1]]$Average.speed.m.s[3])
})

test_that("Movement events with one detection have '0:00' residency time.", {
	d <- detections.list[1:2]
	d[[1]] <- d[[1]][-c(2:12, 14:17), ] 
	aux <- groupMovements(detections.list = d, bio = bio, spatial = spatial,
	    speed.method = "last to last", max.interval = 60, tz = "Europe/Copenhagen", 
	    dist.mat = dist.mat, invalid.dist = invalid.dist)
	# First event
	expect_equal(aux[[1]]$Detections[1], 1)
	expect_equal(aux[[1]]$Time.in.array[1], "0:00")
	# Following events
	expect_equal(aux[[1]]$Detections[2], 1)
	expect_equal(aux[[1]]$Time.in.array[2], "0:00")
})

test_that("speedReleaseToFirst can handle unknown events", {
  aux <- names(moves)
  output <- lapply(names(moves), function(fish) {
      speedReleaseToFirst(fish = fish, bio = bio, movements = moves[[fish]],
                          dist.mat = dist.mat, invalid.dist = invalid.dist,
                          speed.method = "last to last")
    })
  names(output) <- aux
  rm(aux)
  expect_equal(output[[1]]$Time.travelling[1], "295:44")
  expect_equal(output[[1]]$Average.speed.m.s[1], NA_real_)
  expect_equal(output[[2]]$Time.travelling[1], "334:00")
  expect_equal(output[[2]]$Average.speed.m.s[1], 0.001759)
})

test_that("speedReleaseToFirst can handle a first detection previous to release", {
  xbio <- bio
  xbio$Release.date[4] <- xbio$Release.date[4] + (40 * 24 * 3600)
  aux <- names(moves)
  output <- lapply(names(moves), function(fish) {
      speedReleaseToFirst(fish = fish, bio = xbio, movements = moves[[fish]],
                          dist.mat = dist.mat, invalid.dist = invalid.dist,
                          speed.method = "last to last")
    })
  names(output) <- aux
  rm(aux)
  expect_equal(output[[2]]$Time.travelling[1], NA_character_)
  expect_equal(output[[2]]$Average.speed.m.s[1], NA_real_)
})

test_that("movementTimes correctly handles events with one detection.", {
	xmoves <- output[[1]]
	xmoves$Detections <- 1
	output <- movementTimes(xmoves)
	expect_equal(unique(output$Time.in.array), "0:00")
})

setwd("..")
unlink("exampleWorkspace", recursive = TRUE)
setwd(tests.home)
rm(list = ls())
