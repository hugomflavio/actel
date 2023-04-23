oldtz <- Sys.getenv('TZ', unset = NA)
Sys.setenv(TZ = 'UTC')

tests.home <- getwd()
setwd(tempdir())

exampleWorkspace("exampleWorkspace", force = TRUE)
setwd("exampleWorkspace")
write.csv(example.distances, "distances.csv")

test_that("residency results contains all the expected elements.", {
	output <- suppressWarnings(residency(tz = 'Europe/Copenhagen', report = FALSE, GUI = "never"))

	expect_false(any(is.na(match(names(output), c('array.times', 'arrays', 'time.positions', 'time.ratios', 'deployments',
		'detections', 'dist.mat', 'efficiency', 'global.ratios', 'group.ratios', 'intra.array.CJS', 'intra.array.matrices',
		'last.seen', 'movements', 'residency.list', 'rsp.info', 'section.movements', 'section.times',
		'spatial', 'status.df', 'valid.detections', 'valid.movements')))))

	# all the contents of each object have been tested in their respective function tests, apart from rsp.info and last.seen
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
# n
# n
# n

test_that("residency is able to run speed and inactiveness checks.", {
	skip_on_cran()
	output <- suppressWarnings(residency(tz = 'Europe/Copenhagen',
		report = FALSE, GUI = "never", speed.warning = 1000000, inactive.warning = 1000000, replicates = list(A9 = c("St.16", "St.17"))))
	expect_false(any(is.na(match(names(output), c('array.times', 'arrays', 'time.positions', 'time.ratios',
		'deployments', 'detections', 'dist.mat', 'efficiency', 'global.ratios', 'group.ratios', 'intra.array.CJS',
		'intra.array.matrices','last.seen', 'movements', 'residency.list', 'rsp.info', 'section.movements',
		'section.times', 'spatial', 'status.df', 'valid.detections', 'valid.movements')))))
	file.remove("distances.csv")
	expect_warning(output <- residency(tz = 'Europe/Copenhagen', report = TRUE,
			GUI = "never", speed.error = 1000000, inactive.error = 1000000),
		"Running inactiveness checks without a distance matrix. Performance may be limited.", fixed = TRUE)
	expect_false(any(is.na(match(names(output), c('array.times', 'arrays', 'time.positions', 'time.ratios',
		'deployments', 'detections', 'efficiency', 'global.ratios', 'group.ratios', 'intra.array.CJS', 'intra.array.matrices',
		'last.seen', 'movements', 'residency.list', 'rsp.info', 'section.movements', 'section.times',
		'spatial', 'status.df', 'valid.detections', 'valid.movements')))))
})
# n
# n
# n
# n
# n

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
	xspatial$Array[19] <- "A2"
	xspatial[20, ] <- xspatial[18, ]
	xspatial$Station.name[20] <- "RS3"
	xspatial$Array[20] <- "A3"
	write.csv(xspatial, "spatial.csv", row.names = FALSE)

	output <- suppressWarnings(residency(tz = 'Europe/Copenhagen', jump.warning = 1,
		report = TRUE, GUI = "never", speed.warning = 1000000, inactive.warning = 1000000, replicates = list(A9 = c("St.16", "St.17"))))

	expect_true(TRUE) # dummy test so it doesn't count as skipped.
})
# n
# n

# reset bio and spatial
write.csv(example.biometrics, "biometrics.csv", row.names = FALSE)
write.csv(example.spatial, "spatial.csv", row.names = FALSE)

# Throw in a fake report just to test the number appending code
write(1, file = "actel_residency_report.html")

test_that("residency can handle multiple expected first arrays", {
	skip_on_cran()
	xspatial <- example.spatial
	xspatial$Array[18] <- "A1|A2"
	write.csv(xspatial, "spatial.csv", row.names = FALSE)
	expect_message(suppressWarnings(output <- residency(tz = 'Europe/Copenhagen', report = TRUE, GUI = "never")),
		"Multiple possible first arrays detected for release site 'RS1'.", fixed = TRUE)
})
# n
# n

# reset spatial
write.csv(example.spatial, "spatial.csv", row.names = FALSE)

test_that("residency can handle multi-sensor data", {
	skip_on_cran()
	xdet <- example.detections
	xdet$Sensor.Value <- 1
	xdet$Sensor.Unit <- "A"
	xdet$Sensor.Unit[xdet$Signal == 4454] <- "B"
	my.list <- split(xdet, xdet$Receiver)
  for (i in names(my.list)) {
    write.csv(my.list[[i]], paste0("detections/", i, ".csv"), row.names = FALSE)
  }
	xbio <- example.biometrics[-(1:4), ]
	xbio$Signal <- as.character(xbio$Signal)
	xbio$Signal[1] <- "4453|4454"
	write.csv(xbio, "biometrics.csv", row.names = FALSE)
	output <- suppressWarnings(residency(tz = 'Europe/Copenhagen', GUI = "never"))

	expect_true(TRUE) # dummy test so it doesn't count as skipped.
})
# n
# n
# n
# n
# n

# reset bio
write.csv(example.biometrics, "biometrics.csv", row.names = FALSE)

# Throw in a fake results object just to test the number appending code
a = 1
save(a, file = "actel_residency_results.RData")

write.csv(example.distances, "distances.csv")

test_that("the discard.first argument is working properly", {
	expect_message(output <- suppressWarnings(residency(tz = 'Europe/Copenhagen', GUI = "never", jump.warning = Inf, jump.error = Inf, discard.first = 24 * 30)),
		"M: 13467 detection(s) were invalidated because they were recorded before the time set in 'discard.first' had passed.", fixed = TRUE)
	expect_true(is.na(output$valid.movements[[1]]$Time.travelling[1]))
	expect_true(is.na(output$valid.movements[[1]]$Average.speed.m.s[1]))
	expect_true(is.na(output$section.movements[[1]]$Time.travelling[1]))
})
# n
# n

# last test just for timestep = hours
test_that("timestep = 'hours' is working too", {
	skip_on_cran()
	output <- suppressWarnings(residency(tz = 'Europe/Copenhagen', report = TRUE, GUI = "never", timestep = "hours"))
	expect_false(any(is.na(match(names(output), c('array.times', 'arrays', 'time.positions', 'time.ratios',
		'deployments', 'detections', 'dist.mat', 'efficiency', 'global.ratios', 'group.ratios', 'intra.array.CJS',
		'intra.array.matrices','last.seen', 'movements', 'residency.list', 'rsp.info', 'section.movements',
		'section.times', 'spatial', 'status.df', 'valid.detections', 'valid.movements')))))
})
# n
# n

setwd("..")
unlink("exampleWorkspace", recursive = TRUE)
setwd(tests.home)

if (is.na(oldtz)) Sys.unsetenv("TZ") else Sys.setenv(TZ = oldtz)

rm(list = ls())
