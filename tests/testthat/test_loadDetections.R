test_that("loadDetections fails with expected message if no detections are present.", {
	expect_error(loadDetections(tz = "Europe/Copenhagen"),
		"Could not find a 'detections' folder nor a 'detections.csv' file.", fixed = TRUE)
})

dir.create("detections")

test_that("loadDetections stops if detections folder is empty", {
	expect_error(loadDetections(tz = "Europe/Copenhagen"),
		"A 'detections' folder is present but appears to be empty.", fixed = TRUE)
})

aux <- split(example.detections, example.detections$Receiver)
for (i in names(aux)[1:3]) {
  write.csv(aux[[i]], paste0("detections/", i, ".csv"), row.names = FALSE)
}

test_that("loadDetections output is as expected", {
	output <- loadDetections(start.time = NULL, stop.time = NULL, tz = "Europe/Copenhagen", force = FALSE)
	expect_equal(attributes(output$Timestamp)$tz, "Europe/Copenhagen")
	expect_equal(colnames(output), c("Timestamp", "Receiver", "CodeSpace", "Signal", "Transmitter"))
	expect_equal(factor(paste(output$CodeSpace, output$Signal, sep = "-")), output$Transmitter)
	expect_equal(nrow(output), 1369)

	unlink("detections", recursive = TRUE)
	write.csv(aux[[1]], "detections.csv", row.names = FALSE)
	output <- loadDetections(start.time = NULL, stop.time = NULL, tz = "Europe/Copenhagen", force = FALSE)
	expect_equal(attributes(output$Timestamp)$tz, "Europe/Copenhagen")
	expect_equal(colnames(output), c("Timestamp", "Receiver", "CodeSpace", "Signal", "Transmitter"))
	expect_equal(factor(paste(output$CodeSpace, output$Signal, sep = "-")), output$Transmitter)
	expect_equal(nrow(output), 708)
	file.remove("detections.csv")
	file.remove("actel.detections.RData")
})

dir.create("detections")
aux <- split(example.detections, example.detections$Receiver)
for (i in names(aux)[1:3]) {
  write.csv(aux[[i]], paste0("detections/", i, ".csv"), row.names = FALSE)
}

test_that("loadDetections can handle the presence of a detections folder and detections file", {
	output <- loadDetections(start.time = NULL, stop.time = NULL, tz = "Europe/Copenhagen", force = FALSE)

	file.copy("detections/actel.detections.RData", "actel.detections.RData")
	expect_warning(loadDetections(start.time = NULL, stop.time = NULL, tz = "Europe/Copenhagen", force = TRUE),
		"Previously compiled detections were found both in the current directory and in a 'detections' folder.\n   Loading ONLY the compiled detections present in the 'detections' folder.", fixed = TRUE)
	file.remove("detections/actel.detections.RData")

	expect_message(loadDetections(start.time = NULL, stop.time = NULL, tz = "Europe/Copenhagen", force = TRUE),
		"M: Using detections previously compiled on [\\.]*...")
	file.remove("actel.detections.RData")

	file.copy("detections/VR2W-132908.csv", "detections.csv")
	expect_warning(output <- loadDetections(start.time = NULL, stop.time = NULL, tz = "Europe/Copenhagen", force = FALSE),
		"Both a 'detections' folder and a 'detections.csv' file are present in the current directory.\n   Loading ONLY the files present in the 'detections' folder.", fixed = TRUE)
	expect_equal(nrow(output), 1369)
	file.remove("detections/actel.detections.RData")
	file.remove("detections.csv")
})

test_that("loadDetectons can handle random/empty files", {
	write.csv("abc", "detections/bad_file.csv")
	expect_warning(loadDetections(start.time = NULL, stop.time = NULL, tz = "Europe/Copenhagen", force = FALSE),
		"File 'detections/bad_file.csv' does not match to any of the supported hydrophone file formats!\n   If your file corresponds to a hydrophone log and actel did not recognize it, please get in contact through www.github.com/hugomflavio/actel/issues/new", fixed = TRUE)
	file.remove("detections/actel.detections.RData")

	sink("detections/bad_file.csv")
	cat("Date and Time (UTC),Receiver,Transmitter,Transmitter Name,Transmitter Serial,Sensor Value,Sensor Unit,Station Name,Latitude,Longitude\n")
	sink()
	expect_message(loadDetections(start.time = NULL, stop.time = NULL, tz = "Europe/Copenhagen", force = FALSE),
		"File 'detections/bad_file.csv' is empty, skipping processing.", fixed = TRUE)
	file.remove("detections/actel.detections.RData")

	sink("detections/bad_file2.csv")
	cat("Date and Time (UTC),Receiver,Transmitter,Transmitter Name,Transmitter Serial,Sensor Value,Sensor Unit,Station Name,Latitude,Longitude\n")
	sink()
	expect_message(loadDetections(start.time = NULL, stop.time = NULL, tz = "Europe/Copenhagen", force = FALSE),
		"File 'detections/bad_file2.csv' is empty, skipping processing.", fixed = TRUE)
	file.remove("detections/actel.detections.RData")
	expect_message(loadDetections(start.time = NULL, stop.time = NULL, tz = "Europe/Copenhagen", force = FALSE),
		"M: 2 files were excluded from further analyses.", fixed = TRUE)
	file.remove("detections/actel.detections.RData")
	
	unlink("detections", recursive = TRUE)
	dir.create("detections")
	sink("detections/bad_file2.csv")
	cat("Date and Time (UTC),Receiver,Transmitter,Transmitter Name,Transmitter Serial,Sensor Value,Sensor Unit,Station Name,Latitude,Longitude\n")
	sink()
	expect_error(loadDetections(start.time = NULL, stop.time = NULL, tz = "Europe/Copenhagen", force = FALSE),
		"No valid detection files were found.", fixed = TRUE)
	unlink("detections", recursive = TRUE)
})

dir.create("detections")
aux <- split(example.detections, example.detections$Receiver)
for (i in names(aux)[1:3]) {
  write.csv(aux[[i]], paste0("detections/", i, ".csv"), row.names = FALSE)
}

test_that("loadDetections' start.time and stop.time arguments are working", {
	expect_message(loadDetections(start.time = "2018-04-15 00:00:00", tz = "Europe/Copenhagen", force = FALSE),
		"Discarding detection data previous to 2018-04-15 00:00:00 per user command (243 detections discarded).", fixed = TRUE)
	file.remove("detections/actel.detections.RData")
	expect_message(loadDetections(stop.time = "2018-05-01 00:00:00", tz = "Europe/Copenhagen", force = FALSE),
		"M: Discarding detection data posterior to 2018-05-01 00:00:00 per user command (267 detections discarded).", fixed = TRUE)
	file.remove("detections/actel.detections.RData")
	output <- loadDetections(start.time = "2018-04-15 00:00:00", stop.time = "2018-05-01 00:00:00", tz = "Europe/Copenhagen", force = FALSE)
	expect_equal(nrow(output), 859)
	expect_true(output$Timestamp[1] > "2018-04-15 00:00:00")
	expect_true(output$Timestamp[859] < "2018-05-01 00:00:00")
	unlink("detections", recursive = TRUE)
})


file.remove(list.files(pattern = "*txt$"))
