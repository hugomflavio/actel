exampleWorkspace()
setwd("exampleWorkspace")
write.csv(example.distances, "distances.csv")

test_that("migration stops when any argument does not make sense", {
	expect_error(migration(tz = 1), 
		"'tz' could not be recognized as a timezone. Check available timezones with OlsonNames()", fixed = TRUE)
	
	expect_error(migration(tz = "abc"), 
		"'tz' could not be recognized as a timezone. Check available timezones with OlsonNames()", fixed = TRUE)
	
	expect_error(migration(sections = c("River", "Fjord", "Sea"), path = 1, tz = "Europe/Copenhagen", GUI = "never"), 
		"The selected path does not exist.", fixed = TRUE)
	
	expect_error(migration(sections = c("River", "Fjord", "Sea"), path = "abc", tz = "Europe/Copenhagen", GUI = "never"), 
		"The selected path does not exist.", fixed = TRUE)
	
	expect_error(migration(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", max.interval = "a"), 
		"'max.interval' must be numeric.", fixed = TRUE)
	
	expect_error(migration(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", max.interval = "1"), 
		"'max.interval' must be numeric.", fixed = TRUE)
	
	expect_error(migration(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", max.interval = -1), 
		"'max.interval' must be positive.", fixed = TRUE)
	
	expect_error(migration(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", minimum.detections = "a"), 
		"'minimum.detections' must be numeric.", fixed = TRUE)
	
	expect_error(migration(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", minimum.detections = "1"), 
		"'minimum.detections' must be numeric.", fixed = TRUE)
	
	expect_error(migration(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", minimum.detections = -1), 
		"'minimum.detections' must be positive.", fixed = TRUE)
	
	expect_error(migration(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", start.time = 1234), 
		"'start.time' must be in 'yyyy-mm-dd hh:mm:ss' format.", fixed = TRUE)
	
	expect_error(migration(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", start.time = 'abcde'),
	 "'start.time' must be in 'yyyy-mm-dd hh:mm:ss' format.", fixed = TRUE)
	
	expect_error(migration(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", stop.time = 1234), 
		"'stop.time' must be in 'yyyy-mm-dd hh:mm:ss' format.", fixed = TRUE)
	
	expect_error(migration(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", stop.time = 'abcde'), 
		"'stop.time' must be in 'yyyy-mm-dd hh:mm:ss' format.", fixed = TRUE)
	
	expect_error(migration(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", speed.method = 1), 
		"'speed.method' should be one of 'first to first' or 'last to first'.", fixed = TRUE)
	
	expect_error(migration(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", speed.method = "abc"), 
		"'arg' should be one of ", fixed = TRUE)
	
	expect_error(migration(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", speed.warning = "a"), 
		"'speed.warning' must be numeric.", fixed = TRUE)
	
	expect_error(migration(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", speed.warning = -1), 
		"'speed.warning' must be positive.", fixed = TRUE)
	
	expect_error(migration(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", speed.error = "a"), 
		"'speed.error' must be numeric.", fixed = TRUE)
	
	expect_error(migration(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", speed.error = -1), 
		"'speed.error' must be positive.", fixed = TRUE)
	
	expect_error(migration(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", speed.error = 1, speed.warning = 10), 
		"'speed.error' must not be lower than 'speed.warning'", fixed = TRUE)
	
	expect_error(migration(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", jump.warning = "a"), 
		"'jump.warning' must be numeric.", fixed = TRUE)
	
	expect_error(migration(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", jump.warning = -1), 
		"'jump.warning' must not be lower than 1.", fixed = TRUE)
	
	expect_error(migration(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", jump.error = "a"), 
		"'jump.error' must be numeric.", fixed = TRUE)
	
	expect_error(migration(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", jump.error = -1), 
		"'jump.error' must not be lower than 1.", fixed = TRUE)
	
	expect_error(migration(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", jump.error = 1, jump.warning = 10), 
		"'jump.error' must not be lower than 'jump.warning'", fixed = TRUE)
	
	expect_error(migration(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", inactive.warning = "a"), 
		"'inactive.warning' must be numeric.", fixed = TRUE)
	
	expect_error(migration(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", inactive.warning = -1), 
		"'inactive.warning' must be positive.", fixed = TRUE)
	
	expect_error(migration(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", inactive.error = "a"), 
		"'inactive.error' must be numeric.", fixed = TRUE)
	
	expect_error(migration(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", inactive.error = -1), 
		"'inactive.error' must be positive.", fixed = TRUE)
	
	expect_error(migration(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", inactive.error = 1, inactive.warning = 10), 
		"'inactive.error' must not be lower than 'inactive.warning'", fixed = TRUE)
	
	expect_error(migration(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", exclude.tags = 1), 
		"Not all contents in 'exclude.tags' could be recognized as tags (i.e. 'codespace-signal'). Valid examples: 'R64K-1234', A69-1303-1234'", fixed = TRUE)
	
	expect_warning(migration(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", exclude.tags = "ABC-DEF", report = FALSE, GUI = "never"), 
		"The user asked for tag 'ABC-DEF' to be excluded from the analysis, but this tag is not present in the detections.", fixed = TRUE)
	
	file.remove("detections/actel.detections.RData")
	
	expect_error(migration(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", override = 1), 
		"Not all contents in 'override' could be recognized as tags (i.e. 'codespace-signal'). Valid examples: 'R64K-1234', A69-1303-1234'", fixed = TRUE)
	
	expect_error(migration(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", override = "ABC-DEF", report = FALSE, GUI = "never"), 
		"Some tag signals listed in 'override' ('ABC-DEF') are not listed in the biometrics file.", fixed = TRUE)
		
	expect_warning(migration(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", override = "R64K-4450", report = FALSE), 
		"Override has been triggered for fish R64K-4450 but this fish was not detected.", fixed = TRUE)
	
	file.remove("detections/actel.detections.RData")
	
	expect_error(migration(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", GUI = 1), 
		"'GUI' should be one of 'needed', 'always' or 'never'.", fixed = TRUE)
	
	expect_error(migration(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", GUI = "abc"), 
		"'arg' should be one of ", fixed = TRUE)
	
	expect_warning(
		expect_error(migration(sections = c("River", "Fjord"), tz = "Europe/Copenhagen", GUI = "never"),
			"Array 'Sea1' was not assigned to any section. Stopping to prevent function failure.\nPlease either...\n   1) Rename these arrays to match a section,\n   2) Rename a section to match these arrays, or\n   3) Include a new section in the analysis.\n... and restart the analysis.", fixed = TRUE),
	"No detections were found for receiver(s) 132907.", fixed = TRUE)
	
	file.remove("detections/actel.detections.RData")

	expect_warning(migration(sections = c("River", "Fjord", "Sea", "test"), tz = "Europe/Copenhagen", GUI = "never", report = FALSE), 
		"No arrays were found that match section(s) test. There could be a typing mistake! Section(s) test will be removed.", fixed = TRUE)
	
	file.remove("detections/actel.detections.RData")

	expect_warning(
		expect_error(migration(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", GUI = "never", report = FALSE, success.arrays = 1),
			"Array '1' is listed in the 'success.arrays' argument, but this array is not part of the study arrays.", fixed = TRUE),
	"No detections were found for receiver(s) 132907.", fixed = TRUE)
	
	file.remove("detections/actel.detections.RData")

	expect_warning(
		expect_error(migration(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", GUI = "never", report = FALSE, success.arrays = c("Sea1", 1)),
			"Array '1' is listed in the 'success.arrays' argument, but this array is not part of the study arrays.", fixed = TRUE),
	"No detections were found for receiver(s) 132907.", fixed = TRUE)
	
	file.remove("detections/actel.detections.RData")

	expect_warning(
		expect_error(migration(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", GUI = "never", report = FALSE, success.arrays = c("Sea1", 1, "a")),
			"Arrays '1', 'a' are listed in the 'success.arrays' argument, but these arrays are not part of the study arrays.", fixed = TRUE),
	"No detections were found for receiver(s) 132907.", fixed = TRUE)

	file.remove("detections/actel.detections.RData")

	if (!"gWidgetsRGtk2" %in% installed.packages()) {
		expect_warning(migration(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", report = FALSE), 
			"GUI is set to 'needed' but packages 'gWidgetsRGtk2', 'RGtk2' are not available. Please install them if you intend to run GUI.\n         Disabling GUI (i.e. GUI = 'never') for the current run.", fixed = TRUE)
		file.remove("detections/actel.detections.RData")
	}

	expect_error(migration(tz = 'Europe/Copenhagen', sections = c("River", "Fjord", "Sea"), report = TRUE, GUI = "never", if.last.skip.section = "a"),
		"'if.last.skip.section' must be logical.", fixed = TRUE)

	expect_error(migration(tz = 'Europe/Copenhagen', sections = c("River", "Fjord", "Sea"), report = "a", GUI = "never"),
		"'report' must be logical.", fixed = TRUE)

	expect_error(migration(tz = 'Europe/Copenhagen', sections = c("River", "Fjord", "Sea"), debug = "a", GUI = "never"),
		"'debug' must be logical.", fixed = TRUE)

	expect_error(migration(tz = 'Europe/Copenhagen', sections = c("River", "Fjord", "Sea"), report = TRUE, GUI = "never", replicates = "a"),
		"'replicates' must be a list.", fixed = TRUE)

	expect_error(migration(tz = 'Europe/Copenhagen', sections = c("River", "Fjord", "Sea"), report = TRUE, GUI = "never", replicates = list("a")),
		"All list elements within 'replicates' must be named (i.e. list(Array = 'St.1') rather than list('St.1')).", fixed = TRUE)

	expect_error(suppressWarnings(migration(tz = 'Europe/Copenhagen', sections = c("River", "Fjord", "Sea"), report = TRUE, GUI = "never", replicates = list(test = "a"))),
		"Some of the array names listed in the 'replicates' argument do not match the study's arrays.", fixed = TRUE)
	file.remove("detections/actel.detections.RData")

	expect_error(migration(tz = 'Europe/Copenhagen', sections = c("River", "River", "Fjord", "Sea"), GUI = "never"),
		"Some section names are duplicated. Please include each section only once in the 'sections' argument.", fixed = TRUE)

	expect_error(migration(tz = 'Europe/Copenhagen', sections = c("River", "Ri", "Fjord", "Sea"), GUI = "never"),
		"Section 'Ri' is contained within other section names. Sections must be unique and independent.\n       Please rename your sections and arrays so that section names are not contained within each other.", fixed = TRUE)

	expect_error(migration(tz = 'Europe/Copenhagen', sections = c("River", "Ri", "Fjord", "ord", "Sea"), GUI = "never"),
		"Sections 'Ri', 'ord' are contained within other section names. Sections must be unique and independent.\n       Please rename your sections and arrays so that section names are not contained within each other.", fixed = TRUE)

	expect_error(migration(tz = 'Europe/Copenhagen', sections = c("River", "Fjord", "Sea"), print.releases = "a", GUI = "never"),
		"'print.releases' must be logical.", fixed = TRUE)
})

test_that("migration results contains all the expected elements.", {
	output <- suppressWarnings(migration(tz = 'Europe/Copenhagen', sections = c("River", "Fjord", "Sea"), report = TRUE, GUI = "never"))
	
	file.remove("detections/actel.detections.RData")
	
	expect_equal(names(output), c('detections', 'valid.detections', 'spatial', 'deployments', 'arrays', 'movements', 
		'valid.movements', 'section.movements', 'status.df', 'section.overview', 'group.overview', 'release.overview', 
		'matrices', 'overall.CJS', 'intra.array.CJS', 'times', 'rsp.info', 'dist.mat'))

	# all the contents of each object have been tested in their respective function tests, appart from rsp.info, release.overview and group.overview
	expect_equal(names(output$rsp.info), c('analysis.type', 'analysis.time', 'bio', 'tz', 'actel.version'))
	expect_equal(output$rsp.info$analysis.type, "migration")
	expect_equal(output$rsp.info$bio[,1:ncol(example.biometrics)], example.biometrics)

	expect_equal(names(output$release.overview), c("A.RS1", "B.RS1"))
  check <- read.csv(text = '"","Release","River0","River1","River2","River3","River4","River5","River6","Fjord1","Fjord2","Sea1"
"Known",30,0,26,26,26,26,26,26,26,25,20
"Estimated",NA,NA,26,26,26,26,26,26,26,25,NA
"Difference",NA,NA,0,0,0,0,0,0,0,0,NA
', row.names = 1)
  expect_equal(output$release.overview[[1]], check)

  check <- read.csv(text = '"","Release","River0","River1","River2","River3","River4","River5","River6","Fjord1","Fjord2","Sea1"
"Known",30,0,28,28,28,26,26,26,24,19,15
"Estimated",NA,NA,28,28,28,26,26,26,24,19,NA
"Difference",NA,NA,0,0,0,0,0,0,0,0,NA
', row.names = 1)
  expect_equal(output$release.overview[[2]], check) 

	expect_equal(names(output$group.overview), c("A", "B"))
  check <- read.csv(text = '"","Release","River0","River1","River2","River3","River4","River5","River6","Fjord1","Fjord2","Sea1"
"Known",30,0,26,26,26,26,26,26,26,25,20
"Estimated",NA,NA,26,26,26,26,26,26,26,25,NA
"Difference",NA,NA,0,0,0,0,0,0,0,0,NA
', row.names = 1)
  expect_equal(output$group.overview[[1]], check)

  check <- read.csv(text = '"","Release","River0","River1","River2","River3","River4","River5","River6","Fjord1","Fjord2","Sea1"
"Known",30,0,28,28,28,26,26,26,24,19,15
"Estimated",NA,NA,28,28,28,26,26,26,24,19,NA
"Difference",NA,NA,0,0,0,0,0,0,0,0,NA
', row.names = 1)
  expect_equal(output$group.overview[[2]], check) 
})

test_that("migration results are stored in target directory", {
	expect_true(file.exists("actel_migration_results.RData"))
	expect_true(file.exists("actel_migration_report.html"))
	expect_true(dir.exists("Report"))
})

test_that("migration temp files are removed at the end of the analysis", {
	expect_false(file.exists("temp_log.txt"))
	expect_false(file.exists("temp_warnings.txt"))
	expect_false(file.exists("temp_UD.txt"))
	expect_false(file.exists("temp_debug.txt"))
})

test_that("migration is able to run speed and inactiveness checks.", {
	output <- suppressWarnings(migration(sections = c("River", "Fjord", "Sea"), tz = 'Europe/Copenhagen', report = TRUE, 
		GUI = "never", speed.warning = 1000000, inactive.warning = 1000000, replicates = list(Sea1 = c("St.16", "St.17"))))
	
	file.remove("detections/actel.detections.RData")
	
	expect_equal(names(output), c('detections', 'valid.detections', 'spatial', 'deployments', 'arrays', 'movements', 
		'valid.movements', 'section.movements', 'status.df', 'section.overview', 'group.overview', 'release.overview', 
		'matrices', 'overall.CJS', 'intra.array.CJS', 'times', 'rsp.info', 'dist.mat'))
	
	file.remove("distances.csv")
	
	expect_warning(output <- migration(sections = c("River", "Fjord", "Sea"), tz = 'Europe/Copenhagen', report = TRUE, 
			GUI = "never", speed.error = 1000000, inactive.error = 1000000),
		"Running inactiveness checks without a distance matrix. Performance may be limited.", fixed = TRUE)
	
	file.remove("detections/actel.detections.RData")
	
	expect_equal(names(output), c('detections', 'valid.detections', 'spatial', 'deployments', 'arrays', 'movements', 
		'valid.movements', 'section.movements', 'status.df', 'section.overview', 'group.overview', 'release.overview', 
		'matrices', 'overall.CJS', 'intra.array.CJS', 'times', 'rsp.info'))
})

test_that("migration can handle multiple expected first arrays", {
	xspatial <- example.spatial
	xspatial$Array[18] <- "River1|River2"
	write.csv(xspatial, "spatial.csv", row.names = FALSE)
	expect_message(suppressWarnings(output <- migration(sections = c("River", "Fjord", "Sea"), 
		tz = 'Europe/Copenhagen', report = TRUE, success.arrays = "Sea1", GUI = "never")),
		"Multiple possible first arrays detected for release site 'RS1'.", fixed = TRUE)
	file.remove("detections/actel.detections.RData")
})

test_that("the debug option works as expected", {
	output <- suppressWarnings(migration(sections = c("River", "Fjord", "Sea"), , tz = 'Europe/Copenhagen', report = FALSE, 
		GUI = "never", debug = TRUE))
	file.remove("detections/actel.detections.RData")

	expect_true(file.exists("migration_debug.RData"))
	aux <- dataToList("migration_debug.RData")
	expect_equal(names(aux), c('study.data', 'jump.error', 'speed.error', 'valid.movements', 'valid.detections', 
		'times', 'success.arrays', 'split.CJS', 'overall.CJS', 'movements', 'print.releases', 'replicates', 'invalid.dist', 'detections.list', 
		'dotmat', 'dist.mat', 'resultsname', 'my.home', 'status.df', 'group.overview', 'section.overview', 'tz', 
		'the.function.call', 'max.interval', 'detections', 'minimum.detections', 'paths', 'deployments', 'arrays', 
		'group.CJS', 'override.fragment', 'report', 'CJS.list', 'inst.ver.short', 'timetable', 'the.time', 'override', 
		'jobname', 'speed.warning', 'inactive.warning', 'release.overview', 'path', 'calculate.efficiency', 'GUI', 
		'spatial', 'inactive.error', 'the.matrices', 'do.checkInactiveness', 'section.movements', 'sections', 
		'exclude.tags', 'matrices', 'intra.array.CJS', 'release_nodes', 'disregard.parallels', 'do.checkSpeeds', 
		'speed.method', 'dot', 'stop.time', 'start.time', 'debug', 'jump.warning', 'm.by.array', 'link', 
		'if.last.skip.section', 'bio', 'rsp.info'))
	expect_true(file.exists("temp_warnings.txt"))
	expect_true(file.exists("temp_debug.txt"))
})

setwd("..")
unlink("exampleWorkspace", recursive = TRUE)
rm(list = ls())
