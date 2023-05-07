skip_on_cran()

tests.home <- getwd()
setwd(tempdir())
if (dir.exists("actel_report_auxiliary_files"))
	unlink("actel_report_auxiliary_files", recursive = TRUE)

dir.create("actel_report_auxiliary_files")

exampleWorkspace("exampleWorkspace", force = TRUE)
setwd("exampleWorkspace")
write.csv(example.distances, "distances.csv")

test_that("printProgression can handle over eight sections", {
	xspatial <- example.spatial
	xspatial$Section <- as.character(xspatial$Section)
	xspatial$Section[1:(nrow(xspatial) - 1)] <- xspatial$Array[1:(nrow(xspatial) - 1)]
	xspatial$Section <- as.factor(xspatial$Section)

	dot <- readDot(string = paste(unique(example.results$spatial$stations$Array), collapse = " -- "))

	tryCatch(printProgression(dot, overall.CJS = example.results$overall.CJS,
		spatial = xspatial, status.df = example.results$status.df, FALSE),
		warning = function(w) stop ("Warning in printProgression:", w))

	expect_true(file.exists(paste0(tempdir(), "/actel_report_auxiliary_files/mb_efficiency.svg")))
})

test_that("printProgression can handle only one array", {
	xspatial <- example.spatial
	xspatial$Section <- as.character(xspatial$Section)
	xspatial$Section[1:(nrow(xspatial) - 1)] <- "River"
	xspatial$Section <- as.factor(xspatial$Section)

	dot <- readDot(string = "River1 -- River1")

	tryCatch(printProgression(dot, overall.CJS = example.results$overall.CJS,
		spatial = xspatial, status.df = example.results$status.df, FALSE),
		warning = function(w) stop ("Warning in printProgression:", w))

	expect_true(file.exists(paste0(tempdir(), "/actel_report_auxiliary_files/mb_efficiency.svg")))
})

test_that("printProgression can handle two arrays", {
	xspatial <- example.spatial
	xspatial$Section <- as.character(xspatial$Section)
	xspatial$Section[1:(nrow(xspatial) - 1)] <- "River"
	xspatial$Section <- as.factor(xspatial$Section)

	dot <- readDot(string = "River1 -- River2")

	tryCatch(printProgression(dot, overall.CJS = example.results$overall.CJS,
		spatial = xspatial, status.df = example.results$status.df, FALSE),
		warning = function(w) stop ("Warning in printProgression:", w))

	expect_true(file.exists(paste0(tempdir(), "/actel_report_auxiliary_files/mb_efficiency.svg")))
})


## printDot

test_that("printDot can handle over eight sections", {
	xspatial <- example.spatial
	xspatial$Section <- as.character(xspatial$Section)
	xspatial$Section[1:(nrow(xspatial) - 1)] <- xspatial$Array[1:(nrow(xspatial) - 1)]
	xspatial$Section <- as.factor(xspatial$Section)

	dot <- readDot(string = paste(unique(example.results$spatial$stations$Array), collapse = " -- "))

	tryCatch(printDot(dot, spatial = xspatial, FALSE),
		warning = function(w) stop ("Warning in printDot:", w))

	expect_true(file.exists(paste0(tempdir(), "/actel_report_auxiliary_files/mb_arrays.svg")))
})

test_that("printDot can handle only one array", {
	xspatial <- example.spatial
	xspatial$Section <- as.character(xspatial$Section)
	xspatial$Section[1:(nrow(xspatial) - 1)] <- "River"
	xspatial$Section <- as.factor(xspatial$Section)

	dot <- readDot(string = "River1 -- River1")

	tryCatch(printDot(dot, spatial = xspatial, FALSE),
		warning = function(w) stop ("Warning in printDot:", w))

	expect_true(file.exists(paste0(tempdir(), "/actel_report_auxiliary_files/mb_arrays.svg")))
})

test_that("printDot can handle two arrays", {
	xspatial <- example.spatial
	xspatial$Section <- as.character(xspatial$Section)
	xspatial$Section[1:(nrow(xspatial) - 1)] <- "River"
	xspatial$Section <- as.factor(xspatial$Section)

	dot <- readDot(string = "River1 -- River2")

	tryCatch(printDot(dot, spatial = xspatial, FALSE),
		warning = function(w) stop ("Warning in printDot:", w))

	expect_true(file.exists(paste0(tempdir(), "/actel_report_auxiliary_files/mb_arrays.svg")))
})

## printBiometrics

test_that("printBiometrics works for one variable", {
	bio <- example.results$rsp.info$bio
	bio <- bio[, -7]
	output <- printBiometrics(bio)
	
	expect_true(file.exists(paste0(tempdir(), "/actel_report_auxiliary_files/Total_Length_mm_boxplot.png")))
})

test_that("printBiometrics works for more than two variables", {
	bio <- example.results$rsp.info$bio
	bio$Length_two <- bio$Total.Length.mm
	output <- printBiometrics(bio)
	
	expect_true(file.exists(paste0(tempdir(), "/actel_report_auxiliary_files/Total_Length_mm_boxplot.png")))
	expect_true(file.exists(paste0(tempdir(), "/actel_report_auxiliary_files/Length_two_boxplot.png")))
	expect_true(file.exists(paste0(tempdir(), "/actel_report_auxiliary_files/Mass_g_boxplot.png")))
})


## printSurvivalGraphic

test_that("printSurvivalGraphic can handle sections with 0 survivors", {
	x <- example.results$section.overview
	x[1, 5:6] <- 0
	tryCatch(printSurvivalGraphic(x), warning = function(w) stop("Warning in printSurvivalGraphic: w"))
	expect_is("The real test is above, this is just to prevent test_that from complaining", "character")
})


## printEfficiency

test_that("printEfficiency returns right string when eff. cannot be calculated", {
	output <- printEfficiency(intra.CJS = NULL, type = "migration")
	expect_equal(output, "Inter-array efficiency could not be calculated. See full log for more details.\n")
	
	output <- printEfficiency(intra.CJS = NULL, type = "residency")
	expect_equal(output, "Inter-array efficiency could not be calculated. See full log for more details.\n")
})


## printCircular

test_that("printCircular complains if option actel.circular.scale is set but value is not good", {
	options(actel.circular.scale = "bananas")
	on.exit(options(actel.circular.scale = NULL))

	expect_warning(x <- explore(tz = "Europe/Copenhagen", report = TRUE),
		"Option actel.circular.scale was set but value is not recognized (accepted values: 'area', 'linear'). Defaulting back to 'area'.", fixed = TRUE)
})

setwd("..")
unlink("exampleWorkspace", recursive = TRUE)
unlink("actel_report_auxiliary_files", recursive = TRUE)

setwd(tests.home)
rm(list = ls())
