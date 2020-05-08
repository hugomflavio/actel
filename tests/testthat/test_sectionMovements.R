skip_on_cran()

exampleWorkspace()
setwd("exampleWorkspace")
write.csv(example.distances, "distances.csv")
study.data <- suppressWarnings(loadStudyData(tz = "Europe/Copenhagen", start.time = NULL, 
	stop.time = NULL, sections = c("River", "Fjord", "Sea"), exclude.tags = NULL))
detections.list <- study.data$detections.list
bio <- study.data$bio
spatial <- study.data$spatial
dist.mat <- study.data$dist.mat
invalid.dist <- study.data$invalid.dist
arrays <- study.data$arrays
sections <- study.data$sections

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


test_that("sectionMovements correctly compresses array movements", {
	output <- sectionMovements(movements = moves[[1]], sections = sections, invalid.dist = invalid.dist)
	expect_equal(colnames(output), c('Section', 'Events', 'Detections', 'First.array', 'Last.array', 'First.time', 'Last.time', 'Time.travelling', 'Time.in.section', 'Speed.in.section.m.s', 'Valid'))
	expect_equal(output$Section, sections)
	expect_equal(output$Events, c(6, 11, 1))
	expect_equal(output$First.array, c("River1", "Fjord1", "Sea1"))
	expect_equal(output$Last.array, c("River6", "Fjord2", "Sea1"))
	expect_equal(output$First.time, moves[[1]]$First.time[c(1, 7, 18)])
	expect_equal(output$Last.time, moves[[1]]$Last.time[c(6, 17, 18)])
	expect_equal(output$Time.travelling, moves[[1]]$Time.travelling[c(1, 7, 18)])
	expect_equal(output$Time.in.section, c("26:06", "380:05", "0:04"))

	output <- sectionMovements(movements = moves[[1]], sections = sections, invalid.dist = TRUE)
	expect_equal(colnames(output), c('Section', 'Events', 'Detections', 'First.array', 'Last.array', 'First.time', 'Last.time', 'Time.travelling', 'Time.in.section', 'Valid'))

	xmoves <- moves[[1]]
	xmoves$Array[4] <- "Sea1"
	output <- sectionMovements(movements = xmoves, sections = sections, invalid.dist = TRUE)
	expect_equal(output$Section, c("River", "Sea", "River", "Fjord", "Sea"))
	expect_equal(output$Events, c(3, 1, 2, 11, 1))	
})

test_that("sectionMovements returns NULL if all events are invalid", {
	xmoves <- moves[[1]]
	xmoves$Valid <- FALSE
	expect_equal(sectionMovements(movements = xmoves, sections = sections), NULL)
})

test_that("checkLinearity throws warning only if movements are not ordered", {
	aux <- sectionMovements(movements = moves[[1]], sections = sections, invalid.dist = invalid.dist)

  tryCatch(checkLinearity(secmoves = aux, fish = "test", sections = sections, arrays = arrays, GUI = "never"), 
    warning = function(w) stop("A warning was issued where it should not have been."))

	expect_warning(checkLinearity(secmoves = aux, fish = "test", sections = rev(sections), arrays = arrays, GUI = "never"),
		"Inter-section backwards movements were detected for fish test and the last events are not ordered!", fixed = TRUE)

	xmoves <- moves[[1]]
	xmoves$Array[4] <- "Sea1"
	aux <- sectionMovements(movements = xmoves, sections = sections, invalid.dist = TRUE)
	expect_warning(output <- checkLinearity(secmoves = aux, fish = "test", sections = sections, arrays = arrays, GUI = "never"),
		"Inter-section backwards movements were detected for fish test.", fixed = TRUE)
	expect_equal(output$Valid, c(TRUE, TRUE, FALSE, FALSE, TRUE))
})

test_that("updateValidity correctly transfers invalid events.", {
	xmoves <- moves[[1]]
	xmoves$Array[4] <- "Sea1"
	aux <- sectionMovements(movements = xmoves, sections = sections, invalid.dist = TRUE)
	secmoves <- suppressWarnings(checkLinearity(secmoves = aux, fish = "test", sections = sections, arrays = arrays, GUI = "never"))
	expect_message(output <- updateValidity(arrmoves = list(test = xmoves), secmoves = list(test = secmoves)),
		"M: Rendering 13 array movement(s) invalid for fish test as the respective section movements were discarded by the user.", fixed = TRUE)
	expect_type(output, "list")
	expect_equal(names(output), "test")
	expect_equal(sum(!output[[1]]$Valid), 13)
})


test_that("checkSMovesN throws warning only if movements are not ordered", {
	aux <- sectionMovements(movements = moves[[1]], sections = sections, invalid.dist = invalid.dist)

  tryCatch(checkSMovesN(secmoves = aux, fish = "test", section.minimum = 1, GUI = "never"),
    warning = function(w) stop("A warning was issued where it should not have been."))

	expect_warning(checkSMovesN(secmoves = aux, fish = "test", section.minimum = 15, GUI = "never"),
		"Section movements with less than 15 detections are present for fish test.", fixed = TRUE)
})

setwd("..")
unlink("exampleWorkspace", recursive = TRUE)
rm(list = ls())

