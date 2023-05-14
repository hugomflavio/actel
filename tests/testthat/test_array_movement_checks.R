skip_on_cran()
oldtz <- Sys.getenv('TZ', unset = NA)
Sys.setenv(TZ = 'UTC')

tests.home <- getwd()
setwd(tempdir())
exampleWorkspace("exampleWorkspace", force = TRUE)
setwd("exampleWorkspace")
write.csv(example.distances, "distances.csv")

study.data <- suppressWarnings(loadStudyData(tz = "Europe/Copenhagen", start.time = NULL,
	stop.time = NULL, section.order = NULL, exclude.tags = NULL))
detections.list <- study.data$detections.list
bio <- study.data$bio
spatial <- study.data$spatial
dist.mat <- study.data$dist.mat
dotmat <- study.data$dotmat
arrays <- study.data$arrays
paths <- study.data$paths

moves <- groupMovements(detections.list = detections.list[1:2], bio = bio, spatial = spatial,
    speed.method = "last to first", max.interval = 60, tz = "Europe/Copenhagen", dist.mat = dist.mat)

aux <- names(moves)
moves <- lapply(names(moves), function(tag) {
    speedReleaseToFirst(tag = tag, bio = bio, movements = moves[[tag]],
                        dist.mat = dist.mat, speed.method = "last to first")
  })
names(moves) <- aux
rm(aux)

test_that("checkMinimumN reacts as expected", {
	xmoves <- moves
	xmoves[[1]] <- xmoves[[1]][1, ]
	xmoves[[1]]$Detections <- 1
  expect_warning(output <- checkMinimumN(movements = xmoves[[1]], tag = "test", min.total.detections = 2, min.per.event = 2, n = "(1/1)"),
  	"Tag test (1/1) has less than 2 detections in total. Discarding this tag.", fixed = TRUE)
  expect_warning(output <- checkMinimumN(movements = xmoves[[1]], tag = "test", min.total.detections = 1, min.per.event = 2, n = "(1/1)"),
    "Tag test (1/1) has array movement events with less than 2 detections. Invalidating those events.", fixed = TRUE)
  expect_false(output$Valid)
  output <- checkMinimumN(movements = xmoves[[2]], tag = "test", min.total.detections = 1, min.per.event = 1)
  expect_true(all(output$Valid))
})

if (interactive()) {
  # ONLY RUN THIS PART IF YOU CAN MANUALLY CONTROL THE CONSOLE INPUT. USE THE VALUES PROVIDED BELOW
  xdotmat <- dotmat
  xdotmat["A7", "A8"] <- NA
  expect_warning(output <- checkImpassables(movements = moves[[1]], tag = "R64K-4451", bio = bio, 
      spatial = spatial, dotmat = xdotmat, GUI = "never", n = "(1/1)"),
    "Tag R64K-4451 (1/1) made an impassable jump in events 16 to 17: It is not possible to go from array A7 to A8.", fixed = TRUE)
  # 17
  # y
  # n
  expect_warning(output <- checkImpassables(movements = moves[[1]], tag = "R64K-4451", bio = bio, 
      spatial = spatial, dotmat = xdotmat, GUI = "never", n = "(1/1)"),
"The last interaction did not solve the impassable problem! See remaining problems below.
         You can also press ESC to abort the current run and alter your spatial.txt file.", fixed = TRUE)
  # 17
  # y
  # n

} else {
  test_that("checkImpassables reacts as expected", {
   	xdotmat <- dotmat
   	xdotmat["A7", "A8"] <- NA
      expect_error(
      	expect_warning(output <- checkImpassables(movements = moves[[1]], tag = "R64K-4451", bio = bio, 
            spatial = spatial, dotmat = xdotmat, GUI = "never", n = "(1/1)"),
      		"Tag R64K-4451 (1/1) made an impassable jump in events 16 to 17: It is not possible to go from array A7 to A8.", fixed = TRUE), 	
    		"Preventing analysis from entering interactive mode in a non-interactive session.", fixed = TRUE)
  })
  # 17
  # y
  # n
}

test_that("checkJumpDistance reacts as expected", {
	# jump from release to first event
	xmoves <- moves[[1]]
	xmoves$Array[1] <- "A3"
	expect_warning(checkJumpDistance(movements = xmoves, bio = bio, tag = "R64K-4451", dotmat = dotmat, paths = paths, n = "(1/1)",
                                   arrays = arrays, spatial = spatial, jump.warning = 1, jump.error = Inf, GUI = "never",
                                   detections = detections.list[["R64K-4451"]], save.tables.locally = FALSE),
	"Tag R64K-4451 (1/1) jumped through 2 arrays from release to first valid event (Release -> A3).", fixed = TRUE)

	# jump from release because first event is invalid
	xmoves <- moves[[1]]
	xmoves$Valid[1] <- FALSE
	expect_warning(checkJumpDistance(movements = xmoves, bio = bio, tag = "R64K-4451", dotmat = dotmat, paths = paths, n = "(1/1)",
                                   arrays = arrays, spatial = spatial, jump.warning = 1, jump.error = Inf, GUI = "never",
                                   detections = detections.list[["R64K-4451"]], save.tables.locally = FALSE),
	"Tag R64K-4451 (1/1) jumped through 1 array from release to first valid event (Release -> A2).", fixed = TRUE)

	# jump from first to second event
	xmoves <- moves[[1]]
	xmoves$Array[2] <- "A3"
	expect_warning(checkJumpDistance(movements = xmoves, bio = bio, tag = "R64K-4451", dotmat = dotmat, paths = paths, n = "(1/1)", 
                                   arrays = arrays, spatial = spatial, jump.warning = 1, jump.error = Inf, GUI = "never",
                                   detections = detections.list[["R64K-4451"]], save.tables.locally = FALSE),
	"Tag R64K-4451 (1/1) jumped through 1 array in valid events 1 -> 2 (A1 -> A3)", fixed = TRUE)
	
	# jump from first to third because second is invalid
	xmoves <- moves[[1]]
	xmoves$Array[2] <- "Unknown"
	xmoves$Valid[2] <- FALSE
	expect_warning(checkJumpDistance(movements = xmoves, bio = bio, tag = "R64K-4451", dotmat = dotmat, paths = paths, n = "(1/1)", 
                                   arrays = arrays, spatial = spatial, jump.warning = 1, jump.error = Inf, GUI = "never",
                                   detections = detections.list[["R64K-4451"]], save.tables.locally = FALSE),
	"Tag R64K-4451 (1/1) jumped through 1 array in valid events 1 -> 2 (A1 -> A3)", fixed = TRUE)

	# Impassable jump exception at release
	xdotmat <- dotmat
 	xdotmat["A1", "A2"] <- NA
  xmoves <- moves[[1]][-1, ]
	expect_error(checkJumpDistance(movements = xmoves, bio = bio, tag = "R64K-4451", dotmat = xdotmat, paths = paths, n = "(1/1)", 
                                   arrays = arrays, spatial = spatial, jump.warning = 1, jump.error = Inf, GUI = "never",
                                   detections = detections.list[["R64K-4451"]], save.tables.locally = FALSE),
	"There are unresolved impassable jumps in the movements (Found at release).", fixed = TRUE)

  # Impassable jump exception elsewhere
  expect_error(checkJumpDistance(movements = moves[[1]], bio = bio, tag = "R64K-4451", dotmat = xdotmat, paths = paths, n = "(1/1)", 
                                   arrays = arrays, spatial = spatial, jump.warning = 1, jump.error = Inf, GUI = "never",
                                   detections = detections.list[["R64K-4451"]], save.tables.locally = FALSE),
  "There are unresolved impassable jumps in the movements (Found during moves).", fixed = TRUE)
	# jump.error is not automatically tested because it triggers user interaction.
})


test_that("checkSpeeds reacts as expected.", {
	# speed warning from release
	xmoves <- moves[[1]]
	xmoves$Average.speed.m.s[1] <- 3
	expect_warning(output <- checkSpeeds(movements = xmoves, tag = "R64K-4451", valid.movements = xmoves,
    speed.warning = 3, speed.error = Inf, GUI = "never", n = "(1/1)"),
	"Tag R64K-4451 (1/1) had an average speed of 3 m/s from release to first valid event (Release -> A1)", fixed = TRUE)
	expect_equal(output, xmoves)

	# speed warning between movements
	expect_warning(output <- checkSpeeds(movements = moves[[1]], tag = "R64K-4451", valid.movements = moves[[1]],
    speed.warning = 2, speed.error = Inf, GUI = "never", n = "(1/1)"),
	"Tag R64K-4451 (1/1) had an average speed of 2.12 m/s from valid event 4 to 5 (A4 -> A5)", fixed = TRUE)
	expect_equal(output, moves[[1]])

	# no warnings, runs smoothly
	output <- checkSpeeds(movements = moves[[1]], tag = "test", valid.movements = moves[[1]],
    speed.warning = Inf, speed.error = Inf, GUI = "never", n = "(1/1)")
	expect_equal(output, moves[[1]])
})

test_that("checkInactiveness reacts as expected.", {
  xmoves <- moves[[1]][-c(17, 18), ]
  # With distances
  expect_warning(output <- checkInactiveness(movements = xmoves, tag = "R64K-4451", detections = detections.list[[1]],
    inactive.warning = 1, inactive.error = Inf, dist.mat = dist.mat, GUI = "never", n = "(1/1)"),
  "Tag R64K-4451 (1/1) was detected 292 times at stations less than 1.5 km apart in array 'A7' (St.9, St.10, St.11), over 2.57 days and then disappeared. Could it be inactive?", fixed = TRUE)
  expect_equal(output, xmoves)

  # Without distances
  xdist <- dist.mat
  attributes(xdist)$valid <- FALSE
  expect_warning(output <- checkInactiveness(movements = xmoves, tag = "R64K-4451", detections = detections.list[[1]],
    inactive.warning = 1, inactive.error = Inf, dist.mat = xdist, GUI = "never", n = "(1/1)"),
  "Tag R64K-4451 (1/1) was detected 292 times at three or less stations of array 'A7' (St.9, St.10, St.11) over 2.57 days and then disappeared. Could it be inactive?", fixed = TRUE)
  expect_equal(output, xmoves)

  # no warnings
  output <- checkInactiveness(movements = xmoves, tag = "R64K-4451", detections = detections.list[[1]],
    inactive.warning = Inf, inactive.error = Inf, dist.mat = xdist, GUI = "never", n = "(1/1)")
  expect_equal(output, xmoves)

  # internal code option for no shifts
  xmoves <- moves[[1]][-c(1:6, 17, 18), ]
  output <- checkInactiveness(movements = xmoves, tag = "R64K-4451", detections = detections.list[[1]],
    inactive.warning = Inf, inactive.error = Inf, dist.mat = xdist, GUI = "never", n = "(1/1)")
  expect_equal(output, xmoves)
})

test_that("checkFirstDetBackFromRelease reacts as expected.", {
  tryCatch(x <- checkFirstDetBackFromRelease(movements = moves[[1]], tag = "R64K-4451", bio = bio, spatial = spatial, arrays = arrays, GUI = "never", n = "(1/1)"),
    warning = function(w) stop("A warning was issued where it should not have been."))

  xspatial <- spatial
  xspatial$release.sites$Array <- "A2"
  expect_warning(checkFirstDetBackFromRelease(movements = moves[[1]], tag = "R64K-4451", bio = bio, spatial = xspatial, arrays = arrays, GUI = "never", n = "(1/1)"),
    "Tag R64K-4451 (1/1) was detected in an array that is not after its release site! Opening relevant data for inspection.\nExpected first array: A2", fixed = TRUE)
  
  xmoves <- moves[[1]]
  xmoves$Valid <- FALSE
  tryCatch(output <- checkFirstDetBackFromRelease(movements = xmoves, tag = "R64K-4451", bio = bio, spatial = xspatial, arrays = arrays, GUI = "never", n = "(1/1)"),
    warning = function(w) stop("A warning was issued where it should not have been."))
  expect_equal(output, xmoves)
})
# n

test_that("simplifyMovements works as expected.", {
	# no invalid events
  output <- simplifyMovements(movements = moves[[1]], tag = "R64K-4451", bio = bio, discard.first = NULL,
    speed.method = "last to first", dist.mat = dist.mat)
  expect_equal(output, moves[[1]])
  # invalid events
  xmoves <- moves[[1]]
  xmoves$Valid <- rep(c(TRUE, FALSE), 9)
  # With dist.mat
  output <- simplifyMovements(movements = xmoves, tag = "R64K-4451", bio = bio, discard.first = NULL,
    speed.method = "last to first", dist.mat = dist.mat)
  expect_equal(nrow(output), 9)
  expect_equal(output$Array, xmoves$Array[(1:9 * 2) - 1])
  expect_equal(output$Detections, xmoves$Detections[(1:9 * 2) - 1])
  A <- output$Average.speed.m.s
  A <- A[!is.na(A)]
  B <- xmoves$Average.speed.m.s[(1:9 * 2) - 1]
  B <- B[!is.na(B)]
  expect_false(all(A == B))

  # Without dist.mat
  xdist <- dist.mat
  attributes(xdist)$valid <- FALSE
  output <- simplifyMovements(movements = xmoves, tag = "R64K-4451", bio = bio, discard.first = NULL,
    speed.method = "last to first", dist.mat = xdist)
  expect_equal(nrow(output), 9)
  expect_equal(output$Array, xmoves$Array[(1:9 * 2) - 1])
  expect_equal(output$Detections, xmoves$Detections[(1:9 * 2) - 1])
  expect_equal(output$Average.speed.m.s, xmoves$Average.speed.m.s[(1:9 * 2) - 1])

  # Fully invalid
  xmoves <- moves[[1]]
  xmoves$Valid <- FALSE
  output <- simplifyMovements(movements = xmoves, tag = "R64K-4451", bio = bio, discard.first = NULL,
    speed.method = "last to first", dist.mat = xdist)
  expect_equal(output, NULL)
})

test_that("validateDetections works as expected.", {
  xmoves <- moves[[1]]
  xmoves$Valid <- rep(c(TRUE, FALSE), 9)
  vm <- list("R64K-4451" = simplifyMovements(movements = xmoves, tag = "R64K-4451", bio = bio, discard.first = NULL,
    speed.method = "last to first", dist.mat = dist.mat))
  vd <- validateDetections(detections.list = detections.list, movements = vm)[[2]]
  expect_equal(sum(moves[[1]]$Detections), nrow(detections.list[[1]]))
  expect_equal(sum(vm[[1]]$Detections), nrow(vd[[1]]))
  expect_equal(attributes(vd)$actel, "valid.detections")
  expect_equal(length(vd), length(vm))
})

setwd("..")
unlink("exampleWorkspace", recursive = TRUE)
setwd(tests.home)

if (is.na(oldtz)) Sys.unsetenv("TZ") else Sys.setenv(TZ = oldtz)

rm(list = ls())
