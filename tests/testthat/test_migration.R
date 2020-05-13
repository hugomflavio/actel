tests.home <- getwd()
setwd(tempdir())

exampleWorkspace()
setwd("exampleWorkspace")
write.csv(example.distances, "distances.csv")

test_that("migration stops when any argument does not make sense", {
	skip_on_cran()
	expect_error(migration(tz = 1), 
		"'tz' could not be recognized as a timezone. Check available timezones with OlsonNames()", fixed = TRUE)
	
	expect_error(migration(tz = "abc"), 
		"'tz' could not be recognized as a timezone. Check available timezones with OlsonNames()", fixed = TRUE)
	
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
	
	expect_error(migration(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", override = 1), 
		"Not all contents in 'override' could be recognized as tags (i.e. 'codespace-signal'). Valid examples: 'R64K-1234', A69-1303-1234'", fixed = TRUE)
	
	expect_error(migration(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", override = "ABC-DEF", report = FALSE, GUI = "never"), 
		"Some tag signals listed in 'override' ('ABC-DEF') are not listed in the biometrics file.", fixed = TRUE)
		
	expect_warning(migration(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", override = "R64K-4450", report = FALSE), 
		"Override has been triggered for fish R64K-4450 but this fish was not detected.", fixed = TRUE)
	
	expect_error(migration(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", GUI = 1), 
		"'GUI' should be one of 'needed', 'always' or 'never'.", fixed = TRUE)
	
	expect_error(migration(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", GUI = "abc"), 
		"'arg' should be one of ", fixed = TRUE)
	
	expect_warning(
		expect_error(migration(sections = c("River", "Fjord"), tz = "Europe/Copenhagen", GUI = "never"),
			"Array 'Sea1' was not assigned to any section. Stopping to prevent function failure.\nPlease either...\n   1) Rename these arrays to match a section,\n   2) Rename a section to match these arrays, or\n   3) Include a new section in the analysis.\n... and restart the analysis.", fixed = TRUE),
	"No detections were found for receiver(s) 132907.", fixed = TRUE)
	
	expect_warning(migration(sections = c("River", "Fjord", "Sea", "test"), tz = "Europe/Copenhagen", GUI = "never", report = FALSE), 
		"No arrays were found that match section(s) test. There could be a typing mistake! Section(s) test will be removed.", fixed = TRUE)
	
	expect_warning(
		expect_error(migration(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", GUI = "never", report = FALSE, success.arrays = 1),
			"Array '1' is listed in the 'success.arrays' argument, but this array is not part of the study arrays.", fixed = TRUE),
	"No detections were found for receiver(s) 132907.", fixed = TRUE)
	
	expect_warning(
		expect_error(migration(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", GUI = "never", report = FALSE, success.arrays = c("Sea1", 1)),
			"Array '1' is listed in the 'success.arrays' argument, but this array is not part of the study arrays.", fixed = TRUE),
	"No detections were found for receiver(s) 132907.", fixed = TRUE)
	
	expect_warning(
		expect_error(migration(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", GUI = "never", report = FALSE, success.arrays = c("Sea1", 1, "a")),
			"Arrays '1', 'a' are listed in the 'success.arrays' argument, but these arrays are not part of the study arrays.", fixed = TRUE),
	"No detections were found for receiver(s) 132907.", fixed = TRUE)

  aux <- c(
    length(suppressWarnings(packageDescription("gWidgets2"))),
    length(suppressWarnings(packageDescription("gWidgets2RGtk2"))),
    length(suppressWarnings(packageDescription("RGtk2"))))
  missing.packages <- sapply(aux, function(x) x == 1)
  if (any(missing.packages)) {
		expect_warning(migration(sections = c("River", "Fjord", "Sea"), tz = "Europe/Copenhagen", report = FALSE), 
      paste0("GUI is set to 'needed' but ", 
        ifelse(sum(missing.packages) == 1, "package '", "packages '"),
        paste(c("gWidgets2", "gWidgets2RGtk2", "RGtk2")[missing.packages], collapse = "', '"),
        ifelse(sum(missing.packages) == 1, "' is", "' are"),
        " not available. Please install ",
        ifelse(sum(missing.packages) == 1, "it", "them"),
        " if you intend to run GUI.\n         Disabling GUI (i.e. GUI = 'never') for the current run."), fixed = TRUE)
  }

	expect_error(migration(tz = 'Europe/Copenhagen', sections = c("River", "Fjord", "Sea"), report = TRUE, GUI = "never", if.last.skip.section = "a"),
		"'if.last.skip.section' must be logical.", fixed = TRUE)

	expect_error(migration(tz = 'Europe/Copenhagen', sections = c("River", "Fjord", "Sea"), report = "a", GUI = "never"),
		"'report' must be logical.", fixed = TRUE)

	expect_error(migration(tz = 'Europe/Copenhagen', sections = c("River", "Fjord", "Sea"), auto.open = "a", GUI = "never"),
		"'auto.open' must be logical.", fixed = TRUE)

	expect_error(migration(tz = 'Europe/Copenhagen', sections = c("River", "Fjord", "Sea"), save.detections = "a", GUI = "never"),
		"'save.detections' must be logical.", fixed = TRUE)

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
	output <<- suppressWarnings(migration(tz = 'Europe/Copenhagen', sections = c("River", "Fjord", "Sea"), 
		report = FALSE, GUI = "never", print.releases = FALSE))
		
	expect_false(any(is.na(match(names(output), c('arrays', 'deployments', 'detections', 'dist.mat',
		'group.overview', 'intra.array.CJS', 'intra.array.matrices','matrices', 'movements', 'overall.CJS', 
		'release.overview', 'rsp.info', 'section.movements', 'section.overview', 'spatial', 'status.df', 
		'times', 'valid.detections', 'valid.movements')))))

	# all the contents of each object have been tested in their respective function tests, appart from rsp.info, release.overview and group.overview
	expect_equal(names(output$rsp.info), c('analysis.type', 'analysis.time', 'bio', 'tz', 'actel.version'))
	expect_equal(output$rsp.info$analysis.type, "migration")
	expect_equal(output$rsp.info$bio[,1:ncol(example.biometrics)], example.biometrics)

	expect_equal(names(output$release.overview), c("A.RS1", "B.RS1"))
  check <- read.csv(text = '"","Release","River0","River1","River2","River3","River4","River5","River6","Fjord1","Fjord2","Sea1"
"detected",30,0,26,26,25,26,26,26,26,25,20
"here plus on peers",NA,NA,26,26,25,26,26,26,25,20,NA
"not here but on peers",NA,NA,0,0,1,0,0,0,0,0,NA
"known",30,0,26,26,26,26,26,26,26,25,20
"estimated",NA,NA,26,26,26,26,26,26,26,25,NA
"difference",NA,NA,0,0,0,0,0,0,0,0,NA
', row.names = 1)
  expect_equal(output$release.overview[[1]], check)

  check <- read.csv(text = '"","Release","River0","River1","River2","River3","River4","River5","River6","Fjord1","Fjord2","Sea1"
"detected",30,0,28,28,27,26,26,26,23,19,15
"here plus on peers",NA,NA,28,28,25,26,26,24,18,15,NA
"not here but on peers",NA,NA,0,0,1,0,0,0,1,0,NA
"known",30,0,28,28,28,26,26,26,24,19,15
"estimated",NA,NA,28,28,28,26,26,26,24,19,NA
"difference",NA,NA,0,0,0,0,0,0,0,0,NA
', row.names = 1)
  expect_equal(output$release.overview[[2]], check) 

	expect_equal(names(output$group.overview), c("A", "B"))
  check <- read.csv(text = '"","Release","River0","River1","River2","River3","River4","River5","River6","Fjord1","Fjord2","Sea1"
"detected",30,0,26,26,25,26,26,26,26,25,20
"here plus on peers",NA,NA,26,26,25,26,26,26,25,20,NA
"not here but on peers",NA,NA,0,0,1,0,0,0,0,0,NA
"known",30,0,26,26,26,26,26,26,26,25,20
"estimated",NA,NA,26,26,26,26,26,26,26,25,NA
"difference",NA,NA,0,0,0,0,0,0,0,0,NA
', row.names = 1)
  expect_equal(output$group.overview[[1]], check)

  check <- read.csv(text = '"","Release","River0","River1","River2","River3","River4","River5","River6","Fjord1","Fjord2","Sea1"
"detected",30,0,28,28,27,26,26,26,23,19,15
"here plus on peers",NA,NA,28,28,25,26,26,24,18,15,NA
"not here but on peers",NA,NA,0,0,1,0,0,0,1,0,NA
"known",30,0,28,28,28,26,26,26,24,19,15
"estimated",NA,NA,28,28,28,26,26,26,24,19,NA
"difference",NA,NA,0,0,0,0,0,0,0,0,NA
', row.names = 1)
  expect_equal(output$group.overview[[2]], check) 
})

test_that("advEfficiency can calculate efficiency from release and group overviews", {
	skip_on_cran()
	expect_message(output <- advEfficiency(output$group.overview$A[, -5], q = c(0.5, 0.9)),
		"M: All arrays were estimated to have either 0% or 100% efficiency, skipping plotting for all arrays.", fixed = TRUE)
		check <- read.csv(text = '"","50%","90%"
"River1",1,1
"River2",1,1
"River4",1,1
"River5",1,1
"River6",1,1
"Fjord1",1,1
"Fjord2",1,1
', row.names = 1)
	colnames(check) <- c("50%","90%")
	expect_equal(output, check)
})

rm(output)

test_that("migration is able to run speed and inactiveness checks.", {
	skip_on_cran()
	output <- suppressWarnings(migration(sections = c("River", "Fjord", "Sea"), tz = 'Europe/Copenhagen', report = TRUE, 
		GUI = "never", speed.warning = 1000000, inactive.warning = 1000000, replicates = list(Sea1 = c("St.16", "St.17"))))
		
	expect_false(any(is.na(match(names(output), c('arrays', 'deployments', 'detections', 'dist.mat', 'group.overview', 
		'intra.array.CJS', 'intra.array.matrices','matrices', 'movements', 'overall.CJS', 'release.overview', 
		'rsp.info', 'section.movements', 'section.overview', 'spatial', 'status.df', 'times', 'valid.detections', 
		'valid.movements')))))
	
	file.remove("distances.csv")
	
	expect_warning(output <- migration(sections = c("River", "Fjord", "Sea"), tz = 'Europe/Copenhagen', report = TRUE, 
			GUI = "never", speed.error = 1000000, inactive.error = 1000000),
		"Running inactiveness checks without a distance matrix. Performance may be limited.", fixed = TRUE)
	
	expect_false(any(is.na(match(names(output), c('arrays', 'deployments', 'detections', 'group.overview', 'intra.array.CJS',
	 'intra.array.matrices','matrices', 'movements', 'overall.CJS', 'release.overview', 'rsp.info', 'section.movements',
	  'section.overview', 'spatial', 'status.df', 'times', 'valid.detections', 'valid.movements')))))
})

test_that("migration can handle multiple expected first arrays", {
	skip_on_cran()
	xspatial <- example.spatial
	xspatial$Array[18] <- "River1|River2"
	write.csv(xspatial, "spatial.csv", row.names = FALSE)
	expect_message(suppressWarnings(output <- migration(sections = c("River", "Fjord", "Sea"), 
		tz = 'Europe/Copenhagen', report = TRUE, success.arrays = "Sea1", GUI = "never")),
		"Multiple possible first arrays detected for release site 'RS1'.", fixed = TRUE)
})

test_that("migration can handle multi-sensor data", {
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
	output <- suppressWarnings(migration(sections = c("River", "Fjord", "Sea"), tz = 'Europe/Copenhagen', GUI = "never"))
})

setwd("..")
unlink("exampleWorkspace", recursive = TRUE)
setwd(tests.home)
rm(list = ls())

