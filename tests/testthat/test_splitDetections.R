dir.create("detections")
aux <- split(example.detections, example.detections$Receiver)
for (i in names(aux)[1:3]) {
  write.csv(aux[[i]], paste0("detections/", i, ".csv"), row.names = FALSE)
}

detections <- loadDetections(start.time = NULL, stop.time = NULL, tz = "Europe/Copenhagen", force = FALSE)
unlink("detections", recursive = TRUE)

write.csv(example.biometrics, "biometrics.csv", row.names = FALSE)
bio <- loadBio(file = "biometrics.csv", tz = "Europe/Copenhagen")
file.remove("biometrics.csv")

test_that("splitDetections' output is as expected", {
	output <- splitDetections(detections = detections, bio = bio)
	expect_equal(output$detections[[1]], detections[detections$Transmitter == names(output$detections)[1], ])
	expect_true(any(grepl("Transmitter", colnames(output$bio))))
	expect_equal(length(output$detections), sum(!is.na(output$bio$Transmitter)))
})

test_that("splitDetections excludes tags as requested.", {
	expect_message(output <- splitDetections(detections = detections, bio = bio, exclude.tags = "R64K-4451"),
		"Excluding tag(s) R64K-4451 from the analysis per used command (detections removed: 18, respectively)", fixed = TRUE)
	expect_true(all(!grepl("R64K-4451", names(output$detections))))
	expect_true(is.na(output$bio$Transmitter[bio$Signal == 4451]))
})

test_that("splitDetections stores strays in file.", {
	strays <- read.csv("stray_tags.csv", stringsAsFactors = FALSE)
	expect_equal(strays$Transmitter, "R64K-4451")
	expect_equal(strays$N.detections, 18)
	file.remove("stray_tags.csv")
})

test_that("splitDetections stops the analysis if no detections match the target tags", {
	write.csv(example.biometrics[1, ], "biometrics.csv", row.names = FALSE)
	bio <- loadBio(file = "biometrics.csv", tz = "Europe/Copenhagen")
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

file.remove(list.files(pattern = "*txt$"))
