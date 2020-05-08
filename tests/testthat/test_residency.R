exampleWorkspace()
setwd("exampleWorkspace")
write.csv(example.distances, "distances.csv")

test_that("residency stops when any argument does not make sense", {
	skip_on_cran()
	expect_error(residency(tz = 1), 
		"'tz' could not be recognized as a timezone. Check available timezones with OlsonNames()", fixed = TRUE)
	
	expect_error(residency(tz = "abc"), 
		"'tz' could not be recognized as a timezone. Check available timezones with OlsonNames()", fixed = TRUE)
	
	expect_error(residency(sections = c("River", "Fjord", "Sea"), path = 1, tz = "Europe/Copenhagen", GUI = "never"), 
		"The selected path does not exist.", fixed = TRUE)
	
	expect_error(residency(sections = c("River", "Fjord", "Sea"), path = "abc", tz = "Europe/Copenhagen", GUI = "never"), 
		"The selected path does not exist.", fixed = TRUE)
	
	expect_error(residency(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", max.interval = "a"), 
		"'max.interval' must be numeric.", fixed = TRUE)
	
	expect_error(residency(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", max.interval = "1"), 
		"'max.interval' must be numeric.", fixed = TRUE)
	
	expect_error(residency(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", max.interval = -1),
		"'max.interval' must be positive.", fixed = TRUE)
	
	expect_error(residency(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", minimum.detections = "a"), 
		"'minimum.detections' must be numeric.", fixed = TRUE)
	
	expect_error(residency(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", minimum.detections = "1"), 
		"'minimum.detections' must be numeric.", fixed = TRUE)
	
	expect_error(residency(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", minimum.detections = -1), 
		"'minimum.detections' must be positive.", fixed = TRUE)
	
	expect_error(residency(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", start.time = 1234), 
		"'start.time' must be in 'yyyy-mm-dd hh:mm:ss' format.", fixed = TRUE)
	
	expect_error(residency(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", start.time = 'abcde'), 
		"'start.time' must be in 'yyyy-mm-dd hh:mm:ss' format.", fixed = TRUE)
	
	expect_error(residency(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", stop.time = 1234), 
		"'stop.time' must be in 'yyyy-mm-dd hh:mm:ss' format.", fixed = TRUE)
	
	expect_error(residency(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", stop.time = 'abcde'), 
		"'stop.time' must be in 'yyyy-mm-dd hh:mm:ss' format.", fixed = TRUE)
	
	expect_error(residency(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", speed.method = 1), 
		"'speed.method' should be one of 'first to first' or 'last to first'.", fixed = TRUE)
	
	expect_error(residency(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", speed.method = "abc"), 
		"'arg' should be one of ", fixed = TRUE)
	
	expect_error(residency(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", speed.warning = "a"), 
		"'speed.warning' must be numeric.", fixed = TRUE)
	
	expect_error(residency(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", speed.warning = -1), 
		"'speed.warning' must be positive.", fixed = TRUE)
	
	expect_error(residency(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", speed.error = "a"), 
		"'speed.error' must be numeric.", fixed = TRUE)
	
	expect_error(residency(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", speed.error = -1), 
		"'speed.error' must be positive.", fixed = TRUE)
	
	expect_error(residency(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", speed.error = 1, speed.warning = 10), 
		"'speed.error' must not be lower than 'speed.warning'", fixed = TRUE)
	
	expect_error(residency(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", jump.warning = "a"), 
		"'jump.warning' must be numeric.", fixed = TRUE)
	
	expect_error(residency(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", jump.warning = -1), 
		"'jump.warning' must not be lower than 1.", fixed = TRUE)
	
	expect_error(residency(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", jump.error = "a"), 
		"'jump.error' must be numeric.", fixed = TRUE)
	
	expect_error(residency(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", jump.error = -1), 
		"'jump.error' must not be lower than 1.", fixed = TRUE)
	
	expect_error(residency(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", jump.error = 1, jump.warning = 10), 
		"'jump.error' must not be lower than 'jump.warning'", fixed = TRUE)
	
	expect_error(residency(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", inactive.warning = "a"), 
		"'inactive.warning' must be numeric.", fixed = TRUE)
	
	expect_error(residency(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", inactive.warning = -1), 
		"'inactive.warning' must be positive.", fixed = TRUE)
	
	expect_error(residency(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", inactive.error = "a"), 
		"'inactive.error' must be numeric.", fixed = TRUE)
	
	expect_error(residency(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", inactive.error = -1), 
		"'inactive.error' must be positive.", fixed = TRUE)
	
	expect_error(residency(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", inactive.error = 1, inactive.warning = 10), 
		"'inactive.error' must not be lower than 'inactive.warning'", fixed = TRUE)
	
	expect_error(residency(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", exclude.tags = 1), 
		"Not all contents in 'exclude.tags' could be recognized as tags (i.e. 'codespace-signal'). Valid examples: 'R64K-1234', A69-1303-1234'", fixed = TRUE)
	
	expect_warning(residency(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", exclude.tags = "ABC-DEF", report = FALSE, GUI = "never"), 
		"The user asked for tag 'ABC-DEF' to be excluded from the analysis, but this tag is not present in the detections.", fixed = TRUE)
	
	file.remove("detections/actel.detections.RData")
	
	expect_error(residency(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", override = 1), 
		"Not all contents in 'override' could be recognized as tags (i.e. 'codespace-signal'). Valid examples: 'R64K-1234', A69-1303-1234'", fixed = TRUE)
	
	expect_error(residency(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", override = "ABC-DEF", report = FALSE, GUI = "never"), 
		"Some tag signals listed in 'override' ('ABC-DEF') are not listed in the biometrics file.", fixed = TRUE)

	expect_warning(residency(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", override = "R64K-4450", report = FALSE, GUI = "never"), 
		"Override has been triggered for fish R64K-4450 but this fish was not detected.", fixed = TRUE)
	
	file.remove("detections/actel.detections.RData")
	
	expect_error(residency(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", GUI = 1), 
		"'GUI' should be one of 'needed', 'always' or 'never'.", fixed = TRUE)
	
	expect_error(residency(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", GUI = "abc"), 
		"'arg' should be one of ", fixed = TRUE)
	
	expect_warning(
		expect_error(residency(sections = c("River", "Fjord"), tz = "Europe/Copenhagen", GUI = "never"),
			"Array 'Sea1' was not assigned to any section. Stopping to prevent function failure.\nPlease either...\n   1) Rename these arrays to match a section,\n   2) Rename a section to match these arrays, or\n   3) Include a new section in the analysis.\n... and restart the analysis.", fixed = TRUE),
	"No detections were found for receiver(s) 132907.", fixed = TRUE)

	file.remove("detections/actel.detections.RData")

	expect_warning(residency(sections = c("River", "Fjord", "Sea", "test"), tz = "Europe/Copenhagen", GUI = "never", report = FALSE), 
		"No arrays were found that match section(s) test. There could be a typing mistake! Section(s) test will be removed.", fixed = TRUE)

	file.remove("detections/actel.detections.RData")

	if (!"gWidgets2RGtk2" %in% installed.packages()) {
		expect_warning(residency(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", report = FALSE), 
			"GUI is set to 'needed' but packages 'gWidgets2RGtk2', 'RGtk2' are not available. Please install them if you intend to run GUI.\n         Disabling GUI (i.e. GUI = 'never') for the current run.", fixed = TRUE)
		file.remove("detections/actel.detections.RData")
	}

	expect_error(residency(tz = 'Europe/Copenhagen', sections = c("River", "Fjord", "Sea"), report = TRUE, GUI = "never", section.minimum = "a"),
		"'section.minimum' must be numeric", fixed = TRUE)

	expect_error(residency(tz = 'Europe/Copenhagen', sections = c("River", "Fjord", "Sea"), report = "a", GUI = "never"),
		"'report' must be logical.", fixed = TRUE)

	expect_error(residency(tz = 'Europe/Copenhagen', sections = c("River", "Fjord", "Sea"), debug = "a", GUI = "never"),
		"'debug' must be logical.", fixed = TRUE)

	expect_error(residency(tz = 'Europe/Copenhagen', sections = c("River", "Fjord", "Sea"), report = TRUE, GUI = "never", replicates = "a"),
		"'replicates' must be a list.", fixed = TRUE)

	expect_error(residency(tz = 'Europe/Copenhagen', sections = c("River", "Fjord", "Sea"), report = TRUE, GUI = "never", replicates = list("a")),
		"All list elements within 'replicates' must be named (i.e. list(Array = 'St.1') rather than list('St.1')).", fixed = TRUE)

	expect_error(suppressWarnings(residency(tz = 'Europe/Copenhagen', sections = c("River", "Fjord", "Sea"), report = TRUE, GUI = "never", replicates = list(test = "a"))),
		"Some of the array names listed in the 'replicates' argument do not match the study's arrays.", fixed = TRUE)
	file.remove("detections/actel.detections.RData")

	expect_error(residency(tz = 'Europe/Copenhagen', sections = c("River", "River", "Fjord", "Sea"), GUI = "never"),
		"Some section names are duplicated. Please include each section only once in the 'sections' argument.", fixed = TRUE)

	expect_error(residency(tz = 'Europe/Copenhagen', sections = c("River", "Ri", "Fjord", "Sea"), GUI = "never"),
		"Section 'Ri' is contained within other section names. Sections must be unique and independent.\n       Please rename your sections and arrays so that section names are not contained within each other.", fixed = TRUE)

	expect_error(residency(tz = 'Europe/Copenhagen', sections = c("River", "Ri", "Fjord", "ord", "Sea"), GUI = "never"),
		"Sections 'Ri', 'ord' are contained within other section names. Sections must be unique and independent.\n       Please rename your sections and arrays so that section names are not contained within each other.", fixed = TRUE)

	expect_error(residency(tz = 'Europe/Copenhagen', sections = c("River", "Fjord", "Sea"), print.releases = "a", GUI = "never"),
		"'print.releases' must be logical.", fixed = TRUE)
})


test_that("residency results contains all the expected elements.", {
	output <- suppressWarnings(residency(tz = 'Europe/Copenhagen', sections = c("River", "Fjord", "Sea"), report = FALSE, GUI = "never"))
	file.remove("detections/actel.detections.RData")

	expect_false(any(is.na(match(names(output), c('array.times', 'arrays', 'daily.positions', 'daily.ratios', 'deployments', 
		'detections', 'dist.mat', 'efficiency', 'global.ratios', 'intra.array.CJS', 'intra.array.matrices',
		'last.seen', 'movements', 'residency.list', 'rsp.info', 'section.movements', 'section.times', 
		'spatial', 'status.df', 'valid.detections', 'valid.movements')))))

	# all the contents of each object have been tested in their respective function tests, appart from rsp.info and last.seen
	expect_equal(names(output$rsp.info), c('analysis.type', 'analysis.time', 'bio', 'tz', 'actel.version'))
	expect_equal(output$rsp.info$analysis.type, "residency")
	expect_equal(output$rsp.info$bio[,1:ncol(example.biometrics)], example.biometrics)

	check <- read.csv(text = '"","Disap. in River","Disap. in Fjord","Disap. in Sea","Disap. at Release"
"A",0,6,20,4
"B",4,9,15,2
', row.names = 1)
	colnames(check) <- c("Disap. in River", "Disap. in Fjord", "Disap. in Sea", "Disap. at Release")
	expect_equal(output$last.seen, check)
})

test_that("residency results are stored in target directory", {
	expect_true(file.exists("actel_residency_results.RData"))
})

test_that("residency temp files are removed at the end of the analysis", {
	skip_on_cran()
	expect_false(file.exists("temp_log.txt"))
	expect_false(file.exists("temp_warnings.txt"))
	expect_false(file.exists("temp_UD.txt"))
	expect_false(file.exists("temp_debug.txt"))
})

test_that("residency is able to run speed and inactiveness checks.", {
	skip_on_cran()
	output <- suppressWarnings(residency(sections = c("River", "Fjord", "Sea"), tz = 'Europe/Copenhagen', 
		report = FALSE, GUI = "never", speed.warning = 1000000, inactive.warning = 1000000, replicates = list(Sea1 = c("St.16", "St.17"))))
	file.remove("detections/actel.detections.RData")
	expect_false(any(is.na(match(names(output), c('array.times', 'arrays', 'daily.positions', 'daily.ratios', 
		'deployments', 'detections', 'dist.mat', 'efficiency', 'global.ratios', 'intra.array.CJS', 
		'intra.array.matrices','last.seen', 'movements', 'residency.list', 'rsp.info', 'section.movements', 
		'section.times', 'spatial', 'status.df', 'valid.detections', 'valid.movements')))))
	file.remove("distances.csv")
	expect_warning(output <- residency(sections = c("River", "Fjord", "Sea"), tz = 'Europe/Copenhagen', report = TRUE, 
			GUI = "never", speed.error = 1000000, inactive.error = 1000000),
		"Running inactiveness checks without a distance matrix. Performance may be limited.", fixed = TRUE)
	file.remove("detections/actel.detections.RData")
	expect_false(any(is.na(match(names(output), c('array.times', 'arrays', 'daily.positions', 'daily.ratios', 
		'deployments', 'detections', 'efficiency', 'global.ratios', 'intra.array.CJS', 'intra.array.matrices',
		'last.seen', 'movements', 'residency.list', 'rsp.info', 'section.movements', 'section.times', 
		'spatial', 'status.df', 'valid.detections', 'valid.movements')))))
})

test_that("residency can handle multiple release sites.", {
	skip_on_cran()
	xbio <- example.biometrics
	xbio$Release.site <- as.character(xbio$Release.site)
	xbio$Release.site[c(1:15)] <- "RS2"
	xbio$Release.site[c(31:45)] <- "RS3"
	write.csv(xbio, "biometrics.csv", row.names = FALSE)

	xspatial <- example.spatial
	xspatial[19, ] <- xspatial[18, ]
	xspatial$Station.name[19] <- "RS2"
	xspatial$Array[19] <- "River2"
	xspatial[20, ] <- xspatial[18, ]
	xspatial$Station.name[20] <- "RS3"
	xspatial$Array[20] <- "River3"
	write.csv(xspatial, "spatial.csv", row.names = FALSE)

	output <- suppressWarnings(residency(sections = c("River", "Fjord", "Sea"), tz = 'Europe/Copenhagen', jump.warning = 1,
		report = TRUE, GUI = "never", speed.warning = 1000000, inactive.warning = 1000000, replicates = list(Sea1 = c("St.16", "St.17"))))
	file.remove("detections/actel.detections.RData")
	write.csv(example.biometrics, "biometrics.csv", row.names = FALSE)
	write.csv(example.spatial, "spatial.csv", row.names = FALSE)
})

test_that("residency can handle multiple expected first arrays", {
	skip_on_cran()
	xspatial <- example.spatial
	xspatial$Array[18] <- "River1|River2"
	write.csv(xspatial, "spatial.csv", row.names = FALSE)
	expect_message(suppressWarnings(output <- residency(sections = c("River", "Fjord", "Sea"), 
		tz = 'Europe/Copenhagen', report = TRUE, GUI = "never")),
		"Multiple possible first arrays detected for release site 'RS1'.", fixed = TRUE)
	file.remove("detections/actel.detections.RData")
	write.csv(example.spatial, "spatial.csv", row.names = FALSE)
})

test_that("the debug option works as expected", {
	skip_on_cran()
	output <- suppressWarnings(residency(sections = c("River", "Fjord", "Sea"), tz = 'Europe/Copenhagen', report = FALSE, 
		GUI = "never", debug = TRUE))
	file.remove("detections/actel.detections.RData")
	expect_true(file.exists("residency_debug.RData"))
	aux <- dataToList("residency_debug.RData")
	expect_false(any(is.na(match(names(aux), c('array.times', 'arrays', 'bio', 'daily.positions', 'daily.ratios', 
		'debug', 'deployments', 'detections', 'detections.list', 'dist.mat', 'do.checkInactiveness', 'do.checkSpeeds', 
		'dot', 'dotmat', 'efficiency', 'exclude.tags', 'global.ratios', 'GUI', 'inactive.error', 'inactive.warning',
		'auto.open', 'intra.array.CJS', 'intra.array.matrices', 'invalid.dist', 'jobname', 'jump.error', 
		'jump.warning', 'last.seen', 'link', 'max.interval', 'minimum.detections', 'movements', 'my.home', 
		'override', 'override.fragment', 'path', 'paths', 'print.releases', 'replicates', 'report', 'res.df', 
		'residency.list', 'resultsname', 'rsp.info', 'section.minimum', 'section.movements', 'section.times', 
		'sections', 'spatial', 'speed.error', 'speed.method', 'speed.warning', 'start.time', 'status.df', 
		'stop.time', 'study.data', 'success.arrays', 'the.function.call', 'the.time', 'tz', 'valid.detections', 
		'valid.movements')))))
	expect_true(file.exists("temp_warnings.txt"))
	expect_true(file.exists("temp_debug.txt"))
})

test_that("residency can handle multi-sensor data", {
	skip_on_cran()
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
	output <- suppressWarnings(residency(sections = c("River", "Fjord", "Sea"), tz = 'Europe/Copenhagen', GUI = "never"))
	file.remove("detections/actel.detections.RData")
})

setwd("..")
unlink("exampleWorkspace", recursive = TRUE)
rm(list = ls())
