skip_on_cran()

tests.home <- getwd()
setwd(tempdir())

exampleWorkspace("exampleWorkspace")
setwd("exampleWorkspace")

bio <- read.csv("biometrics.csv")
deployments <- read.csv("deployments.csv")
spatial <- read.csv("spatial.csv")
detections <- example.detections
colnames(detections)[1] <- "Timestamp"
detections$CodeSpace <- extractCodeSpaces(detections$Transmitter)
detections$Signal <- extractSignals(detections$Transmitter)
detections$ExtraCol <- NA
dot <- paste(unique(spatial$Array[spatial$Type == "Hydrophone"]), collapse = "--")

test_that("Correct error is triggered if datapack is not valid", {
	expect_error(results <- explore(datapack = "a"),
		"The datapack's token is invalid or missing. Please the function preload() to compile the input data.
Additionally, data must to be compiled during the current R session.", fixed = TRUE)

	expect_error(results <- migration(datapack = "a"),
		"The datapack's token is invalid or missing. Please the function preload() to compile the input data.
Additionally, data must to be compiled during the current R session.", fixed = TRUE)

	expect_error(results <- residency(datapack = "a"),
		"The datapack's token is invalid or missing. Please the function preload() to compile the input data.
Additionally, data must to be compiled during the current R session.", fixed = TRUE)
})

test_that("explore with preload yields the same results as with traditional loading", {
	d <- preload(biometrics = bio, deployments = deployments, spatial = spatial, detections = detections, tz = "Europe/Copenhagen")

	results <- explore(datapack = d)

	results2 <- explore(tz = "Europe/Copenhagen")

	for (i in 1:length(results$valid.movements)) {
		expect_equal(results$movements[[i]], results2$movements[[i]])
		expect_equal(results$valid.movements[[i]], results2$valid.movements[[i]])
	}

	expect_equal(extractSignals(names(results$valid.movements)), extractSignals(names(results2$valid.movements)))

	expect_equal(results$arrays, results2$arrays)

	expect_equal(results$spatial, results2$spatial)
})

test_that("migration and residency don't start if datapack is incompatible", {
	d <- preload(biometrics = bio, deployments = deployments, spatial = spatial, detections = detections, tz = "Europe/Copenhagen")

	expect_error(results <- migration(datapack = d),
		"The preloaded data contains no sections, but these are mandatory for the migration analysis. Recompile the data using the argument 'sections' during preload.", fixed = TRUE)

	expect_error(results <- residency(datapack = d),
		"The preloaded data contains no sections, but these are mandatory for the migration analysis. Recompile the data using the argument 'sections' during preload.", fixed = TRUE)
})

test_that("migration and residency with preload yield the same results as with traditional loading", {
	d2 <- preload(biometrics = bio, deployments = deployments, spatial = spatial, detections = detections, 
		tz = "Europe/Copenhagen", sections = c("River", "Fjord", "Sea"))

	results <- migration(datapack = d2)
	results2 <- migration(tz = "Europe/Copenhagen", sections = c("River", "Fjord", "Sea"))

	expect_equal(extractSignals(names(results$valid.movements)), extractSignals(names(results2$valid.movements)))
	expect_equal(results$status.df[, -1], results2$status.df[, -1])
	for (i in 1:length(results$valid.movements)) {
		expect_equal(results$movements[[i]], results2$movements[[i]])
		expect_equal(results$valid.movements[[i]], results2$valid.movements[[i]])
	}


	results <- residency(datapack = d2)
	results2 <- residency(tz = "Europe/Copenhagen", sections = c("River", "Fjord", "Sea"))

	for (i in 1:length(results$valid.movements)) {
		expect_equal(results$movements[[i]], results2$movements[[i]])
		expect_equal(results$valid.movements[[i]], results2$valid.movements[[i]])
	}
	expect_equal(extractSignals(names(results$valid.movements)), extractSignals(names(results2$valid.movements)))
	expect_equal(results$arrays, results2$arrays)
	expect_equal(results$status.df[, -1], results2$status.df[, -1])
})


test_that("start.time and stop.time are working fine in preload", {
	expect_message(d <- preload(biometrics = bio, deployments = deployments, spatial = spatial, 
		detections = detections, tz = "Europe/Copenhagen", start.time = "2018-05-01 00:00"),
	"M: Discarding detection data previous to 2018-05-01 00:00 per user command (9866 detections discarded).", fixed = TRUE)

	expect_message(d <- preload(biometrics = bio, deployments = deployments, spatial = spatial, 
		detections = detections, tz = "Europe/Copenhagen", stop.time = "2018-05-08 00:00"),
		"M: Discarding detection data posterior to 2018-05-08 00:00 per user command (544 detections discarded).", fixed = TRUE)

	expect_message(d <- preload(biometrics = bio, deployments = deployments, spatial = spatial, 
		detections = detections, tz = "Europe/Copenhagen", start.time = "2018-05-01 00:00",
		stop.time = "2018-05-08 00:00"),
		"M: Data time range: 2018-05-01 00:04:30 to 2018-05-07 23:59:47 (Europe/Copenhagen).", fixed = TRUE)
})

setwd("..")
unlink("exampleWorkspace", recursive = TRUE)
setwd(tests.home)
rm(list = ls())
