skip_on_cran()
oldtz <- Sys.getenv('TZ', unset = NA)
Sys.setenv(TZ = 'UTC')

tests.home <- getwd()
setwd(tempdir())

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

# Force Thelma Old structure
receiver_aux <- strsplit(as.character(aux[[2]]$Receiver), "-", fixed = TRUE)
aux[[2]] <- data.frame(
	`Date and Time UTC` = aux[[2]]$Timestamp,
	`TBR Serial Number` = aux[[2]]$Receiver,
	`Unix Timestamp UTC` = rep(NA_real_, nrow(aux[[2]])),
	Millisecond = rep(NA_real_, nrow(aux[[2]])),
	CodeType = rep("R64K", nrow(aux[[2]])),
	Id = aux[[2]]$Signal,
	Data = rep(NA_real_, nrow(aux[[2]])),
	`Signal to Noise Ratio` = rep(NA_real_, nrow(aux[[2]])))
colnames(aux[[2]])[2] <- "TBR Serial Number"

# Force Thelma New structure
aux[[3]] <- data.frame(
Date.and.Time..UTC. = aux[[3]]$Timestamp,
Unix.Timestamp..UTC. = rep(NA_real_, nrow(aux[[3]])),
ID = aux[[3]]$Signal,
Data = rep(NA_real_, nrow(aux[[3]])),
Protocol = rep("R64K-69kHz", nrow(aux[[3]])),
SNR = rep(NA_real_, nrow(aux[[3]])),
Receiver = aux[[3]]$Receiver)

# Force Vemco Structure
aux[[4]] <- data.frame(
Date.and.Time..UTC. = aux[[4]]$Timestamp,
Receiver = paste0("VR2W-", aux[[4]]$Receiver),
Transmitter = paste0("A69-1303-", aux[[4]]$Signal),
Sensor.Value = rep(NA_real_, nrow(aux[[4]])),
Sensor.Unit = rep(NA_character_, nrow(aux[[4]])))

# Force Vemco Structure without sensors
aux[[5]] <- data.frame(
Date.and.Time..UTC. = aux[[5]]$Timestamp,
Receiver = paste0("VR2W-", aux[[5]]$Receiver),
Transmitter = paste0("A69-1303-", aux[[5]]$Signal))

for (i in names(aux)[1:5]) {
  write.csv(aux[[i]], paste0("detections/", i, ".csv"), row.names = FALSE)
}

test_that("loadDetections output is as expected", {
	output <- loadDetections(start.time = NULL, stop.time = NULL, tz = "Europe/Copenhagen", force = FALSE)
	expect_equal(attributes(output$Timestamp)$tz, "Europe/Copenhagen")
	expect_equal(colnames(output), c("Timestamp", "Receiver", "CodeSpace", "Signal", 'Sensor.Value', 'Sensor.Unit', "Transmitter", "Valid"))
	expect_equal(factor(paste(output$CodeSpace, output$Signal, sep = "-")), output$Transmitter)
	expect_equal(nrow(output), 2639)

	unlink("detections", recursive = TRUE)
	write.csv(aux[[1]], "detections.csv", row.names = FALSE)
	output <- loadDetections(start.time = NULL, stop.time = NULL, tz = "Europe/Copenhagen", force = FALSE)
	expect_equal(attributes(output$Timestamp)$tz, "Europe/Copenhagen")
	expect_equal(colnames(output), c("Timestamp", "Receiver", "CodeSpace", "Signal", 'Sensor.Value', 'Sensor.Unit', "Transmitter", "Valid"))
	expect_equal(factor(paste(output$CodeSpace, output$Signal, sep = "-")), output$Transmitter)
	expect_equal(nrow(output), 708)
	file.remove("detections.csv")
})

dir.create("detections")
aux <- split(example.detections, example.detections$Receiver)
for (i in names(aux)[1:3]) {
  write.csv(aux[[i]], paste0("detections/", i, ".csv"), row.names = FALSE)
}

test_that("loadDetections can handle the presence of a detections folder and detections file", {
	output <- loadDetections(start.time = NULL, stop.time = NULL, tz = "Europe/Copenhagen", force = FALSE)

	file.copy("detections/132908.csv", "detections.csv")
	expect_warning(output <- loadDetections(start.time = NULL, stop.time = NULL, tz = "Europe/Copenhagen", force = FALSE),
		"Both a 'detections' folder and a 'detections.csv' file are present in the current directory.\n   Loading ONLY the files present in the 'detections' folder.", fixed = TRUE)
	expect_equal(nrow(output), 1369)
	file.remove("detections.csv")
})

test_that("loadDetectons can handle random/empty files", {
	write.csv("abc", "detections/bad_file.csv", row.names = FALSE)
	expect_warning(loadDetections(start.time = NULL, stop.time = NULL, tz = "Europe/Copenhagen", force = FALSE),
		"File 'detections/bad_file.csv' could not be recognized as a valid detections table (ncol < 3), skipping processing. Are you sure it is a comma separated file?", fixed = TRUE)

	sink("detections/bad_file.csv")
	cat("'abc','def','ghi'\n1,2,3\n")
	sink()
	expect_warning(loadDetections(start.time = NULL, stop.time = NULL, tz = "Europe/Copenhagen", force = FALSE),
		"File 'detections/bad_file.csv' does not match to any of the supported hydrophone file formats!\n         If your file corresponds to a hydrophone log and actel did not recognize it, please get in contact through www.github.com/hugomflavio/actel/issues/new", fixed = TRUE)

	sink("detections/bad_file.csv")
	cat("Date and Time (UTC),Receiver,Transmitter,Transmitter Name,Transmitter Serial,Sensor Value,Sensor Unit,Station Name,Latitude,Longitude\n")
	sink()
	expect_message(loadDetections(start.time = NULL, stop.time = NULL, tz = "Europe/Copenhagen", force = FALSE),
		"File 'detections/bad_file.csv' is empty, skipping processing.", fixed = TRUE)

	sink("detections/bad_file2.csv")
	cat("Date and Time (UTC),Receiver,Transmitter,Transmitter Name,Transmitter Serial,Sensor Value,Sensor Unit,Station Name,Latitude,Longitude\n")
	sink()
	expect_message(loadDetections(start.time = NULL, stop.time = NULL, tz = "Europe/Copenhagen", force = FALSE),
		"File 'detections/bad_file2.csv' is empty, skipping processing.", fixed = TRUE)
	expect_message(loadDetections(start.time = NULL, stop.time = NULL, tz = "Europe/Copenhagen", force = FALSE),
		"M: 2 files were excluded from further analyses.", fixed = TRUE)
	
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
	expect_message(loadDetections(stop.time = "2018-05-01 00:00:00", tz = "Europe/Copenhagen", force = FALSE),
		"M: Discarding detection data posterior to 2018-05-01 00:00:00 per user command (267 detections discarded).", fixed = TRUE)
	output <- loadDetections(start.time = "2018-04-15 00:00:00", stop.time = "2018-05-01 00:00:00", tz = "Europe/Copenhagen", force = FALSE)
	expect_equal(nrow(output), 859)
	expect_true(output$Timestamp[1] > "2018-04-15 00:00:00")
	expect_true(output$Timestamp[859] < "2018-05-01 00:00:00")
	unlink("detections", recursive = TRUE)
})

dir.create("detections")
aux <- split(example.detections, example.detections$Receiver)
for (i in names(aux)[1:3]) {
  write.csv(aux[[i]], paste0("detections/", i, ".csv"), row.names = FALSE)
}
  write.csv(aux[[3]], paste0("detections/test.csv"), row.names = FALSE)

test_that("checkDupDetections kicks in if needed.", {
	detections <- loadDetections(start.time = "2018-04-15 00:00:00", tz = "Europe/Copenhagen", force = FALSE)
  expect_warning(output <- checkDupDetections(input = detections),
  	"412 duplicated detections were found. Could an input file be duplicated?", fixed = TRUE)
	unlink("detections", recursive = TRUE)
})
# b

dir.create("detections")
aux <- split(example.detections, example.detections$Receiver)
for (i in names(aux)[1:3]) {
  write.csv(aux[[i]], paste0("detections/", i, ".csv"), row.names = FALSE)
}

test_that("checkDetectionsBeforeRelease kicks in if needed.", {
	detections <- loadDetections(start.time = "2018-04-15 00:00:00", tz = "Europe/Copenhagen", force = FALSE)
	bio <- example.biometrics	
  recipient <- splitDetections(detections = detections, bio = bio, exclude.tags = NULL)
  detections.list <- recipient[[1]]
  bio <- recipient[[2]]

  bio$Release.date[2] <- bio$Release.date[2] + (3600 * 24 * 13)
  expect_message(
  	expect_warning(checkDetectionsBeforeRelease(input = detections.list, bio = bio),
  		"Tag R64K-4451 was detected before being released!", fixed = TRUE),
  "12 detection(s) from tag R64K-4451 removed per user command", fixed = TRUE)

  bio$Release.date[2] <- bio$Release.date[2] + (3600 * 24 * 3)
  expect_message(
  	expect_warning(output <- checkDetectionsBeforeRelease(input = detections.list, bio = bio),
  		"Tag R64K-4451 was detected before being released!", fixed = TRUE),
  "ALL detections from tag R64K-4451 removed per user command.", fixed = TRUE)

  expect_equal(length(output), length(detections.list) - 1)
})
# b
# b

test_that("loadDetections can handle saved detections.", {
	output <- loadDetections(tz = "Europe/Copenhagen", save.detections = TRUE)

	file.copy("detections/actel.detections.RData", "actel.detections.RData")

	expect_warning(output <- loadDetections(tz = "Europe/Copenhagen"),
		"Previously compiled detections were found both in the current directory and in a 'detections' folder.
   Loading ONLY the compiled detections present in the 'detections' folder.", fixed = TRUE)

	file.remove("detections/actel.detections.RData")

	expect_message(output <- loadDetections(tz = "Europe/Copenhagen"),
		"The detections have been processed on", fixed = TRUE)

	file.remove("actel.detections.RData")
})
# n
# n

setwd(tests.home)

if (is.na(oldtz)) Sys.unsetenv("TZ") else Sys.setenv(TZ = oldtz)

rm(list = ls())
