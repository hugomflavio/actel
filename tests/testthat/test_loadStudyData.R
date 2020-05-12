skip_on_cran()

tests.home <- getwd()
setwd(tempdir())

exampleWorkspace()
setwd("exampleWorkspace")

test_that("loadStudyData contains all the required elements", {
	expect_warning(output <- loadStudyData(tz = "Europe/Copenhagen", override = NULL, start.time = NULL, stop.time = NULL, 
	  sections = NULL, exclude.tags = NULL, disregard.parallels = TRUE),
		"No detections were found for receiver(s) 132907.", fixed = TRUE)
	file.remove("detections/actel.detections.RData")
	expect_equal(names(output), c('bio', 'sections', 'deployments', 'spatial', 'dot', 'arrays', 'dotmat', 'dist.mat', 'invalid.dist', 'detections.list', 'paths'))
})

test_that("loadStudyData stops if override contains unknown data.", {
	expect_error(loadStudyData(tz = "Europe/Copenhagen", override = "test", start.time = NULL, stop.time = NULL, 
	  sections = NULL, exclude.tags = NULL, disregard.parallels = TRUE),
		"Some tag signals listed in 'override' ('test') are not listed in the biometrics file.", fixed = TRUE)
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

test_that("loadStudyData can handle detections in unknown receivers", {
	aux <- read.csv("spatial.csv")
	write.csv(aux[-3, ], "spatial.csv", row.names = FALSE)
	expect_warning(loadStudyData(tz = "Europe/Copenhagen", override = NULL, start.time = NULL, stop.time = NULL, 
			  sections = NULL, exclude.tags = NULL, disregard.parallels = TRUE),
		"Detections from receivers 132918 are present in the data, but these receivers are not part of the study's stations. Double-check potential errors.", fixed = TRUE)
	file.remove("detections/actel.detections.RData")
	expect_warning(output <- loadStudyData(tz = "Europe/Copenhagen", override = NULL, start.time = NULL, stop.time = NULL, 
			  sections = NULL, exclude.tags = NULL, disregard.parallels = TRUE),
		"Fish R64K-4451 was detected in one or more receivers that are not listed in the study area (receiver(s): 132918)!", fixed = TRUE)
	file.remove("detections/actel.detections.RData")
	expect_equal(tail(levels(output$detections.list[[1]]$Array), 1), "Unknown")
	expect_equal(as.character(output$detections.list[[1]]$Array[13]), "Unknown")
	expect_equal(tail(levels(output$detections.list[[1]]$Standard.name), 1), "Ukn.")
	expect_equal(as.character(output$detections.list[[1]]$Standard.name[13]), "Ukn.")
	expect_equal(as.character(output$spatial$stations$Array[17]), "Unknown")
	expect_equal(as.character(output$spatial$stations$Station.name[17]), "Unknown")
	expect_equal(as.character(output$spatial$stations$Standard.name[17]), "Ukn.")
	expect_equal(tail(names(output$deployments), 1), "132918")
	expect_equal(as.character(output$deployments$`132918`$Station.name), "Unknown")
})
# b
# b

setwd("..")
unlink("exampleWorkspace", recursive = TRUE)
setwd(tests.home)
