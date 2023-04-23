skip_on_cran()
oldtz <- Sys.getenv('TZ', unset = NA)
Sys.setenv(TZ = 'UTC')

tests.home <- getwd()
setwd(tempdir())

exampleWorkspace("exampleWorkspace", force = TRUE)
setwd("exampleWorkspace")

test_that("loadStudyData contains all the required elements", {
	expect_warning(output <- loadStudyData(tz = "Europe/Copenhagen", override = NULL, start.time = NULL, stop.time = NULL,
	  exclude.tags = NULL, disregard.parallels = TRUE),
		"No detections were found for receiver(s) 132907.", fixed = TRUE)
	expect_equal(names(output), c('bio', 'deployments', 'spatial', 'dot', 'arrays', 'dotmat', 'dist.mat', 'detections.list', 'paths'))
})

test_that("loadStudyData stops if override contains unknown data.", {
	expect_error(loadStudyData(tz = "Europe/Copenhagen", override = "test", start.time = NULL, stop.time = NULL,
	  exclude.tags = NULL, disregard.parallels = TRUE),
		"Some tags listed in 'override' (test) are not listed in the biometrics file.", fixed = TRUE)
})

test_that("loadStudyData recognizes both 'study.dot' and 'study.txt' files.", {
	sink("spatial.dot")
		cat("A0--A1--A2--A3--A4--A5--A6--A7--A8--A9\n")
	sink()
	expect_message(suppressWarnings(loadStudyData(tz = "Europe/Copenhagen", override = NULL, start.time = NULL, stop.time = NULL,
	  exclude.tags = NULL, disregard.parallels = TRUE)),
		"M: A 'spatial.dot' file was detected, activating multi-branch analysis.", fixed = TRUE)
	file.remove("spatial.dot")
	
	sink("spatial.txt")
		cat("A0--A1--A2--A3--A4--A5--A6--A7--A8--A9\n")
	sink()
	expect_message(suppressWarnings(loadStudyData(tz = "Europe/Copenhagen", override = NULL, start.time = NULL, stop.time = NULL,
	  exclude.tags = NULL, disregard.parallels = TRUE)),
		"M: A 'spatial.txt' file was detected, activating multi-branch analysis.", fixed = TRUE)
	file.remove("spatial.txt")
})

test_that("loadStudyData can handle detections in unknown receivers", {
	aux <- example.spatial
	write.csv(aux[-3, ], "spatial.csv", row.names = FALSE)
	expect_warning(loadStudyData(tz = "Europe/Copenhagen", override = NULL, start.time = NULL, stop.time = NULL,
			  exclude.tags = NULL, disregard.parallels = TRUE),
		"Detections from receiver 132918 are present in the data, but this receiver is not part of the study's stations. Double-check potential errors.", fixed = TRUE)
	
	expect_warning(output <- loadStudyData(tz = "Europe/Copenhagen", override = NULL, start.time = NULL, stop.time = NULL,
			  exclude.tags = NULL, disregard.parallels = TRUE),
		"Tag R64K-4451 was detected in one or more receivers that are not listed in the study area (receiver(s): 132918)!", fixed = TRUE)
	
	expect_equal(tail(levels(output$detections.list[[1]]$Array), 1), "Unknown")
	expect_equal(as.character(output$detections.list[[1]]$Array[13]), "Unknown")
	expect_equal(tail(levels(output$detections.list[[1]]$Section), 1), "Unknown")
	expect_equal(as.character(output$detections.list[[1]]$Section[13]), "Unknown")
	expect_equal(tail(levels(output$detections.list[[1]]$Standard.name), 1), "Ukn.")
	expect_equal(as.character(output$detections.list[[1]]$Standard.name[13]), "Ukn.")
	expect_equal(as.character(output$spatial$stations$Array[17]), "Unknown")
	expect_equal(as.character(output$spatial$stations$Station.name[17]), "Unknown")
	expect_equal(as.character(output$spatial$stations$Standard.name[17]), "Ukn.")
	expect_equal(tail(names(output$deployments), 1), "132918")
	expect_equal(as.character(output$deployments$`132918`$Station.name), "Unknown")
	expect_equal(as.character(output$deployments$`132918`$Array), "Unknown")
	expect_equal(as.character(output$deployments$`132918`$Standard.name), "Ukn.")
})
# b
# b

setwd("..")
unlink("exampleWorkspace", recursive = TRUE)
setwd(tests.home)

if (is.na(oldtz)) Sys.unsetenv("TZ") else Sys.setenv(TZ = oldtz)

rm(list = ls())
