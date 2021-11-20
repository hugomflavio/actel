skip_on_cran()
oldtz <- Sys.getenv('TZ', unset = NA)
Sys.setenv(TZ = 'UTC')
	
tests.home <- getwd()
setwd(tempdir())

if (dir.exists("detections"))
	unlink("detections", recursive = TRUE)

dir.create("detections")
aux <- split(example.detections, example.detections$Receiver)
for (i in names(aux)[1:3]) {
  write.csv(aux[[i]], paste0("detections/", i, ".csv"), row.names = FALSE)
}

detections <- loadDetections(start.time = NULL, stop.time = NULL, tz = "Europe/Copenhagen", force = FALSE)
unlink("detections", recursive = TRUE)

write.csv(example.biometrics, "biometrics.csv", row.names = FALSE)
bio <- loadBio(input = "biometrics.csv", tz = "Europe/Copenhagen")
file.remove("biometrics.csv")

test_that("splitDetections' output is as expected", {
	output <- splitDetections(detections = detections, bio = bio)
	expect_equal(output$detections[[1]], detections[detections$Transmitter == names(output$detections)[1], ])
	expect_true(any(grepl("Transmitter", colnames(output$bio))))
	expect_equal(length(output$detections), 54)
})

test_that("splitDetections excludes tags as requested.", {
	expect_message(output <- splitDetections(detections = detections, bio = bio, exclude.tags = "R64K-4451"),
		"M: Excluding tag(s) R64K-4451 from the analysis per used command (detections removed: 18)", fixed = TRUE)
	expect_true(all(!grepl("R64K-4451", names(output$detections))))
	expect_true(output$bio$Transmitter[bio$Signal == 4451] == 'Unknown-4451')
})
# n

test_that("splitDetections stops the analysis if no detections match the target tags", {
	write.csv(example.biometrics[1, ], "biometrics.csv", row.names = FALSE)
	bio <- loadBio(input = "biometrics.csv", tz = "Europe/Copenhagen")
	file.remove("biometrics.csv")
	expect_error(splitDetections(detections = detections, bio = bio),
		"No detections were found in the input data which matched the target signals.", fixed = TRUE)
})

test_that("splitDetections stops analysis if duplicated signals are present", {
	xdet <- detections
	levels(xdet$Transmitter) <- c("A69-1303-4529", levels(xdet$Transmitter), "A69-1303-4519")
	xdet$Transmitter[1:5] <- "A69-1303-4529"
	xdet$Transmitter[1364:1369] <- "A69-1303-4519"
	expect_error(splitDetections(detections = xdet, bio = bio),
		"One or more signals match more than one tag in the detections! Showing relevant signals/tags.\n   Signal 4519 was found on tags A69-1303-4519, R64K-4519.\n   Signal 4529 was found on tags A69-1303-4529, R64K-4529.", fixed = TRUE)
})

test_that("splitDetections can handle multi-sensor tags", {
	xbio <- bio[-(1:4), ]
	xbio$Signal <- as.character(xbio$Signal)
	xbio$Signal[1] <- "4453|4454"
	tryCatch(output <- splitDetections(detections = detections, bio = xbio), warning = function(w) stop("A warning was produced where it should not have been.", call = FALSE))
	expect_equal(output$bio$Transmitter[1], "R64K-4453")
	expect_equal(unique(output$detections.list[[1]]$Signal), c(4454, 4453))
	expect_equal(names(output$detections.list)[1], "R64K-4453")

	xbio$Sensor.unit <- NA
	xbio$Sensor.unit[1] <- "A|B"
	xbio$Sensor.unit[3] <- "A"
	tryCatch(output <- splitDetections(detections = detections, bio = xbio), warning = function(w) stop("A warning was produced where it should not have been.", call = FALSE))
	expect_equal(as.vector(with(output$detections.list[[1]], table(Signal, Sensor.Unit))), c(14, 0, 0, 24))
	expect_equal(unique(output$detections.list[[1]]$Sensor.Unit), c("B", "A"))
	expect_true(all(output$detections.list[[2]]$Sensor.Unit == "Temp"))
	expect_equal(unique(output$detections.list[[3]]$Sensor.Unit), "A")
})
# n
# n

setwd(tests.home)

if (is.na(oldtz)) Sys.unsetenv("TZ") else Sys.setenv(TZ = oldtz)

rm(list = ls())
