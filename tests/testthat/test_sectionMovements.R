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


test_that("sectionMovements correctly compresses array movements", {
	output <- sectionMovements(movements = moves[[1]], spatial = spatial, valid.dist = attributes(dist.mat)$valid)
	expect_equal(colnames(output), c('Section', 'Events', 'Detections', 'First.array', 'First.station', 'Last.array', 'Last.station', 'First.time', 'Last.time', 'Time.travelling', 'Time.in.section', 'Speed.in.section.m.s', 'Valid'))
	expect_equal(output$Section, c("River", "Fjord", "Sea"))
	expect_equal(output$Events, c(6, 11, 1))
	expect_equal(output$First.array, c("A1", "A7", "A9"))
	expect_equal(output$First.station, c("St.2", "St.11", "St.15"))
	expect_equal(output$Last.array, c("A6", "A8", "A9"))
	expect_equal(output$Last.station, c("St.7", "St.14", "St.15"))
	expect_equal(output$First.time, moves[[1]]$First.time[c(1, 7, 18)])
	expect_equal(output$Last.time, moves[[1]]$Last.time[c(6, 17, 18)])
	expect_equal(output$Time.travelling, moves[[1]]$Time.travelling[c(1, 7, 18)])
	expect_equal(output$Time.in.section, c("26:06:50", "380:05:15", "0:04:27"))

	output <- sectionMovements(movements = moves[[1]], spatial = spatial, valid.dist = FALSE)
	expect_equal(colnames(output), c('Section', 'Events', 'Detections', 'First.array', 'First.station', 'Last.array', 'Last.station', 'First.time', 'Last.time', 'Time.travelling', 'Time.in.section', 'Valid'))

	xmoves <- moves[[1]]
	xmoves$Array[4] <- "A9"
	output <- sectionMovements(movements = xmoves, spatial = spatial, valid.dist = FALSE)
	expect_equal(output$Section, c("River", "Sea", "River", "Fjord", "Sea"))
	expect_equal(output$Events, c(3, 1, 2, 11, 1))	
})

test_that("sectionMovements returns NULL if all events are invalid", {
	xmoves <- moves[[1]]
	xmoves$Valid <- FALSE
	expect_equal(sectionMovements(movements = xmoves, spatial = spatial), NULL)
})

test_that("checkLinearity throws warning only if movements are not ordered", {
	aux <- sectionMovements(movements = moves[[1]], spatial = spatial, valid.dist = attributes(dist.mat)$valid)

  tryCatch(checkLinearity(secmoves = aux, tag = "test", spatial = spatial, arrays = arrays, GUI = "never"),
    warning = function(w) stop("A warning was issued where it should not have been."))

  xspatial <- spatial
  xspatial$array.order <- spatial$array.order[3:1]
  aux <- sectionMovements(movements = moves[[1]], spatial = xspatial, valid.dist = attributes(dist.mat)$valid)

	expect_warning(checkLinearity(secmoves = aux, tag = "test", spatial = xspatial, arrays = arrays, GUI = "never", n = "(1/1)"),
		"Inter-section backwards movements were detected for tag test (1/1) and the last events are not ordered!", fixed = TRUE)

	xmoves <- moves[[1]]
	xmoves$Array[4] <- "A9"
	aux <- sectionMovements(movements = xmoves, spatial = spatial, valid.dist = FALSE)
	expect_warning(output <- checkLinearity(secmoves = aux, tag = "test", spatial = spatial, arrays = arrays, GUI = "never", n = "(1/1)"),
		"Inter-section backwards movements were detected for tag test (1/1).", fixed = TRUE)
	expect_equal(output$Valid, c(TRUE, TRUE, TRUE, TRUE, TRUE))
})
# n
# n

test_that("updateValidity correctly transfers invalid events.", {
	xmoves <- moves[[1]]
	xmoves$Array[4] <- "A9"
	secmoves <- sectionMovements(movements = xmoves, spatial = spatial, valid.dist = FALSE)
	secmoves$Valid[3:4] <- FALSE

	expect_message(output <- updateValidity(arrmoves = list(test = xmoves), secmoves = list(test = secmoves)),
		"M: Rendering 13 array movement(s) invalid for tag test as the respective section movements were discarded by the user.", fixed = TRUE)
	expect_type(output, "list")
	expect_equal(names(output), "test")
	expect_equal(sum(!output[[1]]$Valid), 13)
})

test_that("checkSMovesN throws warning only if movements are not ordered", {
	aux <- sectionMovements(movements = moves[[1]], spatial = spatial, valid.dist = attributes(dist.mat)$valid)

  tryCatch(checkSMovesN(secmoves = aux, tag = "test", section.warning = 1, section.error = 1, GUI = "never"),
    warning = function(w) stop("A warning was issued where it should not have been."))

	expect_warning(checkSMovesN(secmoves = aux, tag = "test", section.warning = 15, section.error = 0, GUI = "never", n = "(1/1)"),
		"Section movements with 15 or less detections are present for tag test (1/1).", fixed = TRUE)
})

setwd("..")
unlink("exampleWorkspace", recursive = TRUE)
setwd(tests.home)

if (is.na(oldtz)) Sys.unsetenv("TZ") else Sys.setenv(TZ = oldtz)

rm(list = ls())

