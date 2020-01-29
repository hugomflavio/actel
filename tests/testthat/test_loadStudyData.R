exampleWorkspace()
setwd("exampleWorkspace")

test_that("loadStudyData contains all the required elements", {
	expect_warning(output <- loadStudyData(tz = "Europe/Copenhagen", override = NULL, start.time = NULL, stop.time = NULL, 
	  sections = NULL, exclude.tags = NULL, disregard.parallels = TRUE),
		"No detections were found for receiver(s) 132907.", fixed = TRUE)
	file.remove("detections/actel.detections.RData")
	expect_equal(names(output), c('bio', 'deployments', 'spatial', 'dot', 'arrays', 'dotmat', 'detections', 'dist.mat', 'invalid.dist', 'detections.list', 'paths'))
})

test_that("loadStudyData stops if override contains unknown data.", {
	expect_error(loadStudyData(tz = "Europe/Copenhagen", override = "test", start.time = NULL, stop.time = NULL, 
	  sections = NULL, exclude.tags = NULL, disregard.parallels = TRUE),
		"Some tag signals listed in 'override' ('test') are not listed in the biometrics file.", fixed = TRUE)
	file.remove("detections/actel.detections.RData")
})

test_that("loadStudyData recognizes both 'study.dot' and 'study.txt' files.", {
	sink("spatial.dot")
		cat("River0--River1--River2--River3--River4--River5--River6--Fjord1--Fjord2--Sea1\n")
	sink()
	expect_message(suppressWarnings(loadStudyData(tz = "Europe/Copenhagen", override = NULL, start.time = NULL, stop.time = NULL, 
	  sections = NULL, exclude.tags = NULL, disregard.parallels = TRUE)),
		"M: A 'spatial.dot' file was detected, activating multi-branch analysis.", fixed = TRUE)
	file.remove("detections/actel.detections.RData")
	file.remove("spatial.dot")
	
	sink("spatial.txt")
		cat("River0--River1--River2--River3--River4--River5--River6--Fjord1--Fjord2--Sea1\n")
	sink()
	expect_message(suppressWarnings(loadStudyData(tz = "Europe/Copenhagen", override = NULL, start.time = NULL, stop.time = NULL, 
	  sections = NULL, exclude.tags = NULL, disregard.parallels = TRUE)),
		"M: A 'spatial.txt' file was detected, activating multi-branch analysis.", fixed = TRUE)
	file.remove("detections/actel.detections.RData")
})

setwd("..")
unlink("exampleWorkspace", recursive = TRUE)
