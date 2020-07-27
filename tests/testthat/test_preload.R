skip_on_cran()
oldtz <- Sys.getenv('TZ', unset = NA)
Sys.setenv(TZ = 'UTC')

tests.home <- getwd()
setwd(tempdir())

exampleWorkspace("exampleWorkspace")
setwd("exampleWorkspace")

bio <- read.csv("biometrics.csv")
deployments <- read.csv("deployments.csv")
spatial <- read.csv("spatial.csv")
detections <- example.detections
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
	d <- preload(biometrics = bio, deployments = deployments, spatial = spatial, detections = detections,
		dot = dot, tz = "Europe/Copenhagen")

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
# n
# n
# n
# n

test_that("migration and residency don't start if datapack is incompatible", {
	xspatial <- spatial[, -match("Section", colnames(spatial))]
	expect_warning(d <- preload(biometrics = bio, deployments = deployments, spatial = xspatial, detections = detections, tz = "Europe/Copenhagen"),
		"The spatial input does not contain a 'Section' column. This input is only valid for explore() analyses.", fixed = TRUE)

	expect_error(results <- migration(datapack = d),
		"To run migration(), please assign the arrays to their sections using a 'Section' column in the spatial input.", fixed = TRUE)

	expect_error(results <- residency(datapack = d),
		"To run residency(), please assign the arrays to their sections using a 'Section' column in the spatial input.", fixed = TRUE)
})
# n
# n

test_that("migration and residency with preload yield the same results as with traditional loading", {
	d2 <- preload(biometrics = bio, deployments = deployments, spatial = spatial, detections = detections,
		tz = "Europe/Copenhagen")

	results <- migration(datapack = d2)
	results2 <- migration(tz = "Europe/Copenhagen")

	expect_equal(extractSignals(names(results$valid.movements)), extractSignals(names(results2$valid.movements)))
	expect_equal(results$status.df[, -1], results2$status.df[, -1])
	for (i in 1:length(results$valid.movements)) {
		expect_equal(results$movements[[i]], results2$movements[[i]])
		expect_equal(results$valid.movements[[i]], results2$valid.movements[[i]])
	}


	results <- residency(datapack = d2)
	results2 <- residency(tz = "Europe/Copenhagen")

	for (i in 1:length(results$valid.movements)) {
		expect_equal(results$movements[[i]], results2$movements[[i]])
		expect_equal(results$valid.movements[[i]], results2$valid.movements[[i]])
	}
	expect_equal(extractSignals(names(results$valid.movements)), extractSignals(names(results2$valid.movements)))
	expect_equal(results$arrays, results2$arrays)
	expect_equal(results$status.df[, -1], results2$status.df[, -1])
})
# n # migration1
# n
# n
# n
# n # migration2
# n
# n
# n
# n # residency1
# n
# n
# n # residency2
# n
# n

test_that("tz and exclude.tags stops are working", {
	expect_error(preload(tz = "test"), "'tz' could not be recognized as a timezone. Check available timezones with OlsonNames()", fixed = TRUE)

	expect_error(preload(tz = "Europe/Copenhagen", exclude.tags = "test"),
		"Not all contents in 'exclude.tags' could be recognized as tags (i.e. 'codespace-signal'). Valid examples: 'R64K-1234', A69-1303-1234'", fixed = TRUE)
})

test_that("dot stop is working", {
	expect_error(preload(biometrics = bio, deployments = deployments, spatial = spatial,
		detections = detections, tz = "Europe/Copenhagen", dot = 1),
	"'dot' was set but could not recognised as a string. Please prepare a dot string and include it in the dot argument.
You can use readDot to check the quality of your dot string.", fixed = TRUE)
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

write.csv(example.distances, "distances.csv")

test_that("distances are correctly handled with preload", {
	d <- preload(biometrics = bio, deployments = deployments, spatial = spatial, detections = detections,
		dot = dot, distances = example.distances, tz = "Europe/Copenhagen")

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
# n
# n
# n
# n

test_that("preload stops if mandatory columns are missing or have NAs", {
	x <- detections[, -1]
	expect_error(preload(biometrics = bio, deployments = deployments, spatial = spatial, detections = x,
			dot = dot, distances = example.distances, tz = "Europe/Copenhagen"),
	"The following mandatory columns are missing in the detections: Timestamp", fixed = TRUE)

	x <- detections[, -match("Signal", colnames(detections))]
	expect_error(preload(biometrics = bio, deployments = deployments, spatial = spatial, detections = x,
			dot = dot, distances = example.distances, tz = "Europe/Copenhagen"),
	"The following mandatory columns are missing in the detections: Signal
The functions extractSignals and extractCodeSpaces can be used to break transmitter codes apart.", fixed = TRUE)

	x <- detections
	x[1, 1] <- NA
	expect_error(preload(biometrics = bio, deployments = deployments, spatial = spatial, detections = x,
			dot = dot, distances = example.distances, tz = "Europe/Copenhagen"),
	"There is missing data in the following mandatory columns of the detections: Timestamp", fixed = TRUE)
})

test_that("Data conversion warnings and errors kick in", {
	expect_warning(x <- preload(biometrics = bio, deployments = deployments, spatial = spatial, detections = detections,
			dot = dot, distances = example.distances, tz = "Europe/Copenhagen"),
	"The 'Signal' column in the detections is not of type integer. Attempting to convert.", fixed = TRUE)


	expect_warning(x <- preload(biometrics = bio, deployments = deployments, spatial = spatial, detections = detections,
			dot = dot, distances = example.distances, tz = "Europe/Copenhagen"),
	"The 'Receiver' column in the detections is not of type integer. Attempting to convert.", fixed = TRUE)

	d <- detections
	d$Receiver <- paste0("a-", d$Receiver)
	expect_warning(x <- preload(biometrics = bio, deployments = deployments, spatial = spatial, detections = d,
			dot = dot, distances = example.distances, tz = "Europe/Copenhagen"),
	"Attempting to convert the 'Receiver' to integer failed. Attempting to extract only the serial numbers.", fixed = TRUE)

	d <- detections
	d$Receiver <- "a"
	expect_error(x <- preload(biometrics = bio, deployments = deployments, spatial = spatial, detections = d,
			dot = dot, distances = example.distances, tz = "Europe/Copenhagen"),
	"Extracting the serial numbers failed. Aborting.", fixed = TRUE)

	d <- detections
	d$Sensor.Value <- "1"
	expect_warning(x <- preload(biometrics = bio, deployments = deployments, spatial = spatial, detections = d,
			dot = dot, distances = example.distances, tz = "Europe/Copenhagen"),
	"The 'Sensor.Value' column in the detections is not of type numeric. Attempting to convert.", fixed = TRUE)

	d <- detections
	d$Sensor.value <- "b"
	expect_error(x <- preload(biometrics = bio, deployments = deployments, spatial = spatial, detections = d,
			dot = dot, distances = example.distances, tz = "Europe/Copenhagen"),
	"Attempting to convert the 'Sensor.Value' to numeric failed. Aborting.", fixed = TRUE)

	d <- detections
	d$Timestamp <- as.character(d$Timestamp)
	expect_message(x <- suppressWarnings(preload(biometrics = bio, deployments = deployments, spatial = spatial, detections = d,
			dot = dot, distances = example.distances, tz = "Europe/Copenhagen")),
	"Converting detection timestamps to POSIX objects", fixed = TRUE)

	d <- detections
	d$Timestamp <- "b"
	expect_error(x <- preload(biometrics = bio, deployments = deployments, spatial = spatial, detections = d,
			dot = dot, distances = example.distances, tz = "Europe/Copenhagen"),
	"Converting the timestamps failed. Aborting.", fixed = TRUE)

	d <- detections
	d$Valid <- "b"
	expect_warning(x <- preload(biometrics = bio, deployments = deployments, spatial = spatial, detections = d,
			dot = dot, distances = example.distances, tz = "Europe/Copenhagen"),
	"The detections have a column named 'Valid' but its content is not logical. Resetting to Valid = TRUE.", fixed = TRUE)
})

setwd("..")
unlink("exampleWorkspace", recursive = TRUE)
setwd(tests.home)

if (is.na(oldtz)) Sys.unsetenv("TZ") else Sys.setenv(TZ = oldtz)

rm(list = ls())
