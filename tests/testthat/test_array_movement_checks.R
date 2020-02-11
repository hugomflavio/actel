exampleWorkspace()
setwd("exampleWorkspace")
write.csv(example.distances, "distances.csv")

study.data <- suppressWarnings(loadStudyData(tz = "Europe/Copenhagen", start.time = NULL, 
	stop.time = NULL, sections = NULL, exclude.tags = NULL))
detections.list <- study.data$detections.list
bio <- study.data$bio
spatial <- study.data$spatial
dist.mat <- study.data$dist.mat
invalid.dist <- study.data$invalid.dist
dotmat <- study.data$dotmat

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

test_that("checkMinimumN reacts as expected", {
	xmoves <- moves
	xmoves[[1]] <- xmoves[[1]][1, ]
	xmoves[[1]]$Detections <- 1
  expect_warning(output <- checkMinimumN(movements = xmoves[[1]], fish = "test", minimum.detections = 2),
  	"Fish test only has one movement event (River1) with 1 detections. Considered invalid.", fixed = TRUE)
  expect_false(output$Valid)
  output <- checkMinimumN(movements = xmoves[[2]], fish = "test", minimum.detections = 2)
  expect_true(all(output$Valid))
})

test_that("checkImpassables reacts as expected", {
 	xdotmat <- dotmat
 	xdotmat["River1", "River2"] <- NA
  expect_error(
  	expect_warning(output <- checkImpassables(movements = moves[[1]], fish = "test", dotmat = xdotmat, GUI = "never"),
  		"Fish test made an impassable jump: It is not possible to go from array River1 to River2.", fixed = TRUE), 	
		"Preventing analysis from entering interactive mode in a non-interactive session.", fixed = TRUE)
})

test_that("checkJumpDistance reacts as expected", {
	# jump from release to first event
	xmoves <- moves[[1]]
	xmoves$Array[1] <- "River3"
	expect_warning(checkJumpDistance(movements = xmoves, release = "River1", fish = "test", dotmat = dotmat, 
	                          jump.warning = 1, jump.error = Inf, GUI = "never"),
	"Fish test jumped through 2 arrays from release to first valid event (Release -> River3).", fixed = TRUE)

	# jump from release because first event is invalid
	xmoves <- moves[[1]]
	xmoves$Valid[1] <- FALSE
	expect_warning(checkJumpDistance(movements = xmoves, release = "River1", fish = "test", dotmat = dotmat, 
	                          jump.warning = 1, jump.error = Inf, GUI = "never"),
	"Fish test jumped through 1 array from release to first valid event (Release -> River2).", fixed = TRUE)

	# jump from first to second event
	xmoves <- moves[[1]]
	xmoves$Array[2] <- "River3"
	expect_warning(checkJumpDistance(movements = xmoves, release = "River1", fish = "test", dotmat = dotmat, 
	                          jump.warning = 1, jump.error = Inf, GUI = "never"),
	"Fish test jumped through 1 array in valid events 1 -> 2 (River1 -> River3)", fixed = TRUE)
	
	# jump from first to third because second is invalid
	xmoves <- moves[[1]]
	xmoves$Array[2] <- "Unknown"
	xmoves$Valid[2] <- FALSE
	expect_warning(checkJumpDistance(movements = xmoves, release = "River1", fish = "test", dotmat = dotmat, 
	                          jump.warning = 1, jump.error = Inf, GUI = "never"),
	"Fish test jumped through 1 array in valid events 1 -> 2 (River1 -> River3)", fixed = TRUE)

	# Impassable jump exception
	xdotmat <- dotmat
 	xdotmat["River1", "River2"] <- NA
	expect_error(checkJumpDistance(movements = moves[[1]], release = "River1", fish = "test", dotmat = xdotmat, 
	                          jump.warning = 1, jump.error = Inf, GUI = "never"),
	"There are unresolved impassable jumps in the movements.", fixed = TRUE)

	# jump.error is not automatically tested because it triggers user interaction.
})


test_that("checkSpeeds reacts as expected.", {
	# speed warning from release
	xmoves <- moves[[1]]
	xmoves$Average.speed.m.s[1] <- 3
	expect_warning(output <- checkSpeeds(movements = xmoves, fish = "test", valid.movements = xmoves, 
    speed.warning = 3, speed.error = Inf, GUI = "never"),
	"Fish test had an average speed of 3 m/s from release to first valid event (Release -> River1)", fixed = TRUE)
	expect_equal(output, xmoves)

	# speed warning between movements
	expect_warning(output <- checkSpeeds(movements = moves[[1]], fish = "test", valid.movements = moves[[1]], 
    speed.warning = 2, speed.error = Inf, GUI = "never"),
	"Fish test had an average speed of 2.12 m/s from valid event 4 to 5 (River4 -> River5)", fixed = TRUE)
	expect_equal(output, moves[[1]])

	# no warnings, runs smoothly
	output <- checkSpeeds(movements = moves[[1]], fish = "test", valid.movements = moves[[1]], 
    speed.warning = Inf, speed.error = Inf, GUI = "never")
	expect_equal(output, moves[[1]])
})

test_that("checkInactiveness reacts as expected.", {
	xmoves <- moves[[1]][-c(17, 18), ]
	# With distances
  expect_warning(output <- checkInactiveness(movements = xmoves, fish = "test", detections.list = detections.list[[1]], 
		inactive.warning = 1, inactive.error = Inf, dist.mat = dist.mat, invalid.dist = invalid.dist, GUI = "never"),
  "Fish test was detected 292 times at stations less than 1.5 km apart in array 'Fjord1' (St.9, St.10, St.11), over 2.57 days and then disappeared. Could it be inactive?", fixed = TRUE)
  expect_equal(output, xmoves)

  # Without distances
  expect_warning(output <- checkInactiveness(movements = xmoves, fish = "test", detections.list = detections.list[[1]], 
		inactive.warning = 1, inactive.error = Inf, dist.mat = dist.mat, invalid.dist = TRUE, GUI = "never"),
  "Fish test was detected 292 times at three or less stations of array 'Fjord1' (St.9, St.10, St.11) over 2.57 days and then disappeared. Could it be inactive?", fixed = TRUE)
  expect_equal(output, xmoves)

  # no warnings
  output <- checkInactiveness(movements = xmoves, fish = "test", detections.list = detections.list[[1]], 
		inactive.warning = Inf, inactive.error = Inf, dist.mat = dist.mat, invalid.dist = TRUE, GUI = "never")
  expect_equal(output, xmoves)

  # internal code option for no shifts
	xmoves <- moves[[1]][-c(1:6, 17, 18), ]
  output <- checkInactiveness(movements = xmoves, fish = "test", detections.list = detections.list[[1]], 
		inactive.warning = Inf, inactive.error = Inf, dist.mat = dist.mat, invalid.dist = TRUE, GUI = "never")
  expect_equal(output, xmoves)
})

test_that("simplifyMovements works as expected.", {
	# no invalid events
  output <- simplifyMovements(movements = moves[[1]], fish = "R64K-4451", bio = bio, 
    speed.method = "last to first", dist.mat = dist.mat, invalid.dist = invalid.dist)
  expect_equal(output, moves[[1]])
  # invalid events
  xmoves <- moves[[1]]
  xmoves$Valid <- rep(c(TRUE, FALSE), 9)
  # With dist.mat
  output <- simplifyMovements(movements = xmoves, fish = "R64K-4451", bio = bio, 
    speed.method = "last to first", dist.mat = dist.mat, invalid.dist = invalid.dist)
  expect_equal(nrow(output), 9)
  expect_equal(output$Array, xmoves$Array[(1:9 * 2) - 1])
  expect_equal(output$Detections, xmoves$Detections[(1:9 * 2) - 1])
  A <- output$Average.speed.m.s
  A <- A[!is.na(A)]
  B <- xmoves$Average.speed.m.s[(1:9 * 2) - 1]
  B <- B[!is.na(B)]
  expect_false(all(A == B))

  # Without dist.mat
  output <- simplifyMovements(movements = xmoves, fish = "R64K-4451", bio = bio, 
    speed.method = "last to first", dist.mat = dist.mat, invalid.dist = TRUE)
  expect_equal(nrow(output), 9)
  expect_equal(output$Array, xmoves$Array[(1:9 * 2) - 1])
  expect_equal(output$Detections, xmoves$Detections[(1:9 * 2) - 1])
  expect_equal(output$Average.speed.m.s, xmoves$Average.speed.m.s[(1:9 * 2) - 1])

  # Fully invalid
  xmoves <- moves[[1]]
  xmoves$Valid <- FALSE
  output <- simplifyMovements(movements = xmoves, fish = "R64K-4451", bio = bio, 
    speed.method = "last to first", dist.mat = dist.mat, invalid.dist = invalid.dist)
  expect_equal(output, NULL)
})

test_that("validateDetections works as expected.", {
  xmoves <- moves[[1]]
  xmoves$Valid <- rep(c(TRUE, FALSE), 9)
  vm <- list("R64K-4451" = simplifyMovements(movements = xmoves, fish = "R64K-4451", bio = bio, 
    speed.method = "last to first", dist.mat = dist.mat, invalid.dist = invalid.dist))
  vd <- validateDetections(detections.list = detections.list, movements = vm)[[2]]
  expect_equal(sum(moves[[1]]$Detections), nrow(detections.list[[1]]))
  expect_equal(sum(vm[[1]]$Detections), nrow(vd[[1]]))
  expect_equal(attributes(vd)$actel, "valid.detections")
  expect_equal(length(vd), length(vm))
})

setwd("..")
unlink("exampleWorkspace", recursive = TRUE)
rm(list = ls())

