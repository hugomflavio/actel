skip_on_cran()

tests.home <- getwd()
setwd(tempdir())

exampleWorkspace()
setwd("exampleWorkspace")
write.csv(example.distances, "distances.csv")

test_that("explore stops when any argument does not make sense", {
	
	expect_error(explore(tz = 1), 
		"'tz' could not be recognized as a timezone. Check available timezones with OlsonNames()", fixed = TRUE)
	
	expect_error(explore(tz = "abc"), 
		"'tz' could not be recognized as a timezone. Check available timezones with OlsonNames()", fixed = TRUE)
	
	expect_error(explore(tz = "Europe/Copenhagen", max.interval = "a"), 
		"'max.interval' must be numeric.", fixed = TRUE)
	
	expect_error(explore(tz = "Europe/Copenhagen", max.interval = "1"), 
		"'max.interval' must be numeric.", fixed = TRUE)
	
	expect_error(explore(tz = "Europe/Copenhagen", max.interval = -1), 
		"'max.interval' must be positive.", fixed = TRUE)
	
	expect_error(explore(tz = "Europe/Copenhagen", minimum.detections = "a"), 
		"'minimum.detections' must be numeric.", fixed = TRUE)
	
	expect_error(explore(tz = "Europe/Copenhagen", minimum.detections = "1"), 
		"'minimum.detections' must be numeric.", fixed = TRUE)
	
	expect_error(explore(tz = "Europe/Copenhagen", minimum.detections = -1), 
		"'minimum.detections' must be positive.", fixed = TRUE)
	
	expect_error(explore(tz = "Europe/Copenhagen", start.time = 1234), 
		"'start.time' must be in 'yyyy-mm-dd hh:mm:ss' format.", fixed = TRUE)
	
	expect_error(explore(tz = "Europe/Copenhagen", start.time = 'abcde'), 
		"'start.time' must be in 'yyyy-mm-dd hh:mm:ss' format.", fixed = TRUE)
	
	expect_error(explore(tz = "Europe/Copenhagen", stop.time = 1234), 
		"'stop.time' must be in 'yyyy-mm-dd hh:mm:ss' format.", fixed = TRUE)
	
	expect_error(explore(tz = "Europe/Copenhagen", stop.time = 'abcde'), 
		"'stop.time' must be in 'yyyy-mm-dd hh:mm:ss' format.", fixed = TRUE)
	
	expect_error(explore(tz = "Europe/Copenhagen", speed.method = 1), 
		"'speed.method' should be one of 'last to first' or 'last to last'.", fixed = TRUE)
	
	expect_error(explore(tz = "Europe/Copenhagen", speed.method = "abc"), 
		"'arg' should be one of ", fixed = TRUE)
	
	expect_error(explore(tz = "Europe/Copenhagen", speed.warning = "a"), 
		"'speed.warning' must be numeric.", fixed = TRUE)
	
	expect_error(explore(tz = "Europe/Copenhagen", speed.warning = -1), 
		"'speed.warning' must be positive.", fixed = TRUE)
	
	expect_error(explore(tz = "Europe/Copenhagen", speed.error = "a"), 
		"'speed.error' must be numeric.", fixed = TRUE)
	
	expect_error(explore(tz = "Europe/Copenhagen", speed.error = -1), 
		"'speed.error' must be positive.", fixed = TRUE)
	
	expect_error(explore(tz = "Europe/Copenhagen", speed.error = 1, speed.warning = 10), 
		"'speed.error' must not be lower than 'speed.warning'", fixed = TRUE)
	
	expect_error(explore(tz = "Europe/Copenhagen", jump.warning = "a"), 
		"'jump.warning' must be numeric.", fixed = TRUE)
	
	expect_error(explore(tz = "Europe/Copenhagen", jump.warning = -1), 
		"'jump.warning' must not be lower than 1.", fixed = TRUE)
	
	expect_error(explore(tz = "Europe/Copenhagen", jump.error = "a"), 
		"'jump.error' must be numeric.", fixed = TRUE)
	
	expect_error(explore(tz = "Europe/Copenhagen", jump.error = -1), 
		"'jump.error' must not be lower than 1.", fixed = TRUE)
	
	expect_error(explore(tz = "Europe/Copenhagen", jump.error = 1, jump.warning = 10), 
		"'jump.error' must not be lower than 'jump.warning'", fixed = TRUE)
	
	expect_error(explore(tz = "Europe/Copenhagen", inactive.warning = "a"), 
		"'inactive.warning' must be numeric.", fixed = TRUE)
	
	expect_error(explore(tz = "Europe/Copenhagen", inactive.warning = -1), 
		"'inactive.warning' must be positive.", fixed = TRUE)
	
	expect_error(explore(tz = "Europe/Copenhagen", inactive.error = "a"), 
		"'inactive.error' must be numeric.", fixed = TRUE)
	
	expect_error(explore(tz = "Europe/Copenhagen", inactive.error = -1), 
		"'inactive.error' must be positive.", fixed = TRUE)
	
	expect_error(explore(tz = "Europe/Copenhagen", inactive.error = 1, inactive.warning = 10), 
		"'inactive.error' must not be lower than 'inactive.warning'", fixed = TRUE)
	
	expect_error(explore(tz = "Europe/Copenhagen", exclude.tags = 1), 
		"Not all contents in 'exclude.tags' could be recognized as tags (i.e. 'codespace-signal'). Valid examples: 'R64K-1234', A69-1303-1234'", fixed = TRUE)
	
	expect_warning(explore(tz = "Europe/Copenhagen", exclude.tags = "ABC-DEF", report = FALSE, GUI = "never"), 
		"The user asked for tag 'ABC-DEF' to be excluded from the analysis, but this tag is not present in the detections.", fixed = TRUE)

	expect_error(explore(tz = "Europe/Copenhagen", override = 1), 
		"Not all contents in 'override' could be recognized as tags (i.e. 'codespace-signal'). Valid examples: 'R64K-1234', A69-1303-1234'", fixed = TRUE)
	
	expect_error(explore(tz = "Europe/Copenhagen", override = "ABC-DEF", report = FALSE, GUI = "never"), 
		"Some tag signals listed in 'override' ('ABC-DEF') are not listed in the biometrics file.", fixed = TRUE)
	
	expect_warning(explore(tz = "Europe/Copenhagen", override = "R64K-4450", report = FALSE, GUI = "never"), 
		"Override has been triggered for fish R64K-4450 but this fish was not detected.", fixed = TRUE)
	
	expect_error(explore(tz = "Europe/Copenhagen", GUI = 1), 
		"'GUI' should be one of 'needed', 'always' or 'never'.", fixed = TRUE)
	
	expect_error(explore(tz = "Europe/Copenhagen", GUI = "abc"), 
		"'arg' should be one of ", fixed = TRUE)

  aux <- c(
    length(suppressWarnings(packageDescription("gWidgets2"))),
    length(suppressWarnings(packageDescription("gWidgets2RGtk2"))),
    length(suppressWarnings(packageDescription("RGtk2"))))
  missing.packages <- sapply(aux, function(x) x == 1)
  if (any(missing.packages)) {
		expect_warning(explore(tz = "Europe/Copenhagen", report = FALSE), 
      paste0("GUI is set to 'needed' but ", 
        ifelse(sum(missing.packages) == 1, "package '", "packages '"),
        paste(c("gWidgets2", "gWidgets2RGtk2", "RGtk2")[missing.packages], collapse = "', '"),
        ifelse(sum(missing.packages) == 1, "' is", "' are"),
        " not available. Please install ",
        ifelse(sum(missing.packages) == 1, "it", "them"),
        " if you intend to run GUI.\n         Disabling GUI (i.e. GUI = 'never') for the current run."), fixed = TRUE)
  }

	expect_error(explore(tz = "Europe/Copenhagen", report = "a"),
		"'report' must be logical.", fixed = TRUE)

	expect_error(explore(tz = "Europe/Copenhagen", GUI = "never", auto.open = "a"),
		"'auto.open' must be logical.", fixed = TRUE)

	expect_error(explore(tz = "Europe/Copenhagen", GUI = "never", save.detections = "a"),
		"'save.detections' must be logical.", fixed = TRUE)

	expect_error(explore(tz = "Europe/Copenhagen", GUI = "never", print.releases = "a"),
		"'print.releases' must be logical.", fixed = TRUE)
})

test_that("explore results contains all the expected elements.", {
	output <- suppressWarnings(explore(tz = 'Europe/Copenhagen', report = FALSE, GUI = "never"))
	expect_equal(names(output), c('detections', 'valid.detections', 'spatial', 'deployments', 'arrays',
    'movements', 'valid.movements', 'times', 'rsp.info', 'dist.mat'))
	# all the contents of each object have been tested in their respective function tests, appart from rsp.info
	expect_equal(names(output$rsp.info), c('analysis.type', 'analysis.time', 'bio', 'tz', 'actel.version'))
	expect_equal(output$rsp.info$analysis.type, "explore")
	expect_equal(output$rsp.info$bio[,1:ncol(example.biometrics)], example.biometrics)
})

test_that("explore is able to run speed and inactiveness checks.", {
	output <- suppressWarnings(explore(tz = 'Europe/Copenhagen', report = FALSE, GUI = "never", speed.error = 1000000, inactive.error = 1000000))
	expect_false(any(is.na(match(names(output), c('detections', 'valid.detections', 'spatial', 'deployments', 'arrays',
    'movements', 'valid.movements', 'times', 'rsp.info', 'dist.mat')))))
	file.remove("distances.csv")
	output <- suppressWarnings(explore(tz = 'Europe/Copenhagen', report = TRUE, GUI = "never", speed.warning = 1000000, inactive.warning = 1000000))
	expect_false(any(is.na(match(names(output), c('detections', 'valid.detections', 'spatial', 'deployments', 'arrays',
    'movements', 'valid.movements', 'times', 'rsp.info')))))
})

test_that("explore can handle multi-sensor data", {
	xdet <- example.detections
	xdet$Sensor.Value <- 1
	xdet$Sensor.Unit <- "A"
	xdet$Sensor.Unit[xdet$Transmitter == "A69-1303-4454"] <- "B"
	my.list <- split(xdet, xdet$Receiver)
  for (i in names(my.list)) {
    write.csv(my.list[[i]], paste0("detections/", i, ".csv"), row.names = FALSE)
  }
	xbio <- example.biometrics[-(1:4), ]
	xbio$Signal <- as.character(xbio$Signal)
	xbio$Signal[1] <- "4453|4454"
	write.csv(xbio, "biometrics.csv", row.names = FALSE)
	output <- suppressWarnings(explore(tz = 'Europe/Copenhagen', GUI = "never"))
})

setwd("..")
unlink("exampleWorkspace", recursive = TRUE)
setwd(tests.home)
rm(list = ls())
