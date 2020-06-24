skip_on_cran()

tests.home <- getwd()
setwd(tempdir())

exampleWorkspace("exampleWorkspace")
setwd("exampleWorkspace")
write.csv(example.distances, "distances.csv")


test_that("printProgression can handle over eight sections", {
	if (file.exists(paste0(tempdir(), "/mb_efficiency.svg")))
		file.remove(paste0(tempdir(), "/mb_efficiency.svg"))

	sections <- c("River0", "River1", "River2", "River3", "River4", "River5", "River6", "Fjord1", "Fjord2", "Sea")

	dot <- readDot(string = paste(unique(example.results$spatial$stations$Array), collapse = " -- "))

	tryCatch(printProgression(dot, sections, overall.CJS = example.results$overall.CJS,
		spatial = example.results$spatial, status.df = example.results$status.df, FALSE),
		warning = function(w) stop ("Warning in printProgression:", w))

	expect_true(file.exists(paste0(tempdir(), "/mb_efficiency.svg")))
})

test_that("printProgression can handle only one array", {
	if (file.exists(paste0(tempdir(), "/mb_efficiency.svg")))
		file.remove(paste0(tempdir(), "/mb_efficiency.svg"))

	sections <- c("River")

	dot <- readDot(string = "River1 -- River1")

	tryCatch(printProgression(dot, sections, overall.CJS = example.results$overall.CJS,
		spatial = example.results$spatial, status.df = example.results$status.df, FALSE),
		warning = function(w) stop ("Warning in printProgression:", w))

	expect_true(file.exists(paste0(tempdir(), "/mb_efficiency.svg")))
})

test_that("printProgression can handle two arrays", {
	if (file.exists(paste0(tempdir(), "/mb_efficiency.svg")))
		file.remove(paste0(tempdir(), "/mb_efficiency.svg"))

	sections <- c("River")

	dot <- readDot(string = "River1 -- River2")

	tryCatch(printProgression(dot, sections, overall.CJS = example.results$overall.CJS,
		spatial = example.results$spatial, status.df = example.results$status.df, FALSE),
		warning = function(w) stop ("Warning in printProgression:", w))

	expect_true(file.exists(paste0(tempdir(), "/mb_efficiency.svg")))
})


## printDot

test_that("printDot can handle over eight sections", {
	if (file.exists(paste0(tempdir(), "/mb_arrays.svg")))
		file.remove(paste0(tempdir(), "/mb_arrays.svg"))

	sections <- c("River0", "River1", "River2", "River3", "River4", "River5", "River6", "Fjord1", "Fjord2", "Sea")

	dot <- readDot(string = paste(unique(example.results$spatial$stations$Array), collapse = " -- "))

	tryCatch(printDot(dot, sections, spatial = example.results$spatial, FALSE),
		warning = function(w) stop ("Warning in printDot:", w))

	expect_true(file.exists(paste0(tempdir(), "/mb_arrays.svg")))
})

test_that("printDot can handle only one array", {
	if (file.exists(paste0(tempdir(), "/mb_arrays.svg")))
		file.remove(paste0(tempdir(), "/mb_arrays.svg"))

	sections <- c("River")

	dot <- readDot(string = "River1 -- River1")

	tryCatch(printDot(dot, sections, spatial = example.results$spatial, FALSE),
		warning = function(w) stop ("Warning in printDot:", w))

	expect_true(file.exists(paste0(tempdir(), "/mb_arrays.svg")))
})

test_that("printDot can handle two arrays", {
	if (file.exists(paste0(tempdir(), "/mb_arrays.svg")))
		file.remove(paste0(tempdir(), "/mb_arrays.svg"))

	sections <- c("River")

	dot <- readDot(string = "River1 -- River2")

	tryCatch(printDot(dot, sections, spatial = example.results$spatial, FALSE),
		warning = function(w) stop ("Warning in printDot:", w))

	expect_true(file.exists(paste0(tempdir(), "/mb_arrays.svg")))
})



## printBiometrics

test_that("printBiometrics works for one variable", {
	if (file.exists(paste0(tempdir(), "/Total_Length_mm_boxplot.png")))
		file.remove(paste0(tempdir(), "/Total_Length_mm_boxplot.png"))

	bio <- example.results$rsp.info$bio
	bio <- bio[, -7]
	output <- printBiometrics(bio)
	
	expect_true(file.exists(paste0(tempdir(), "/Total_Length_mm_boxplot.png")))
})

test_that("printBiometrics works for more than two variables", {
	if (file.exists(paste0(tempdir(), "/Total_Length_mm_boxplot.png")))
		file.remove(paste0(tempdir(), "/Total_Length_mm_boxplot.png"))

	if (file.exists(paste0(tempdir(), "/Length_two_boxplot.png")))
		file.remove(paste0(tempdir(), "/Length_two_boxplot.png"))

	if (file.exists(paste0(tempdir(), "/Mass_g_boxplot.png")))
		file.remove(paste0(tempdir(), "/Mass_g_boxplot.png"))

	bio <- example.results$rsp.info$bio
	bio$Length_two <- bio$Total.Length.mm
	output <- printBiometrics(bio)
	
	expect_true(file.exists(paste0(tempdir(), "/Total_Length_mm_boxplot.png")))
	expect_true(file.exists(paste0(tempdir(), "/Length_two_boxplot.png")))
	expect_true(file.exists(paste0(tempdir(), "/Mass_g_boxplot.png")))
})


## printSurvivalGraphic

test_that("printSurvivalGraphic can handle sections with 0 survivors", {
	x <- example.results$section.overview
	x[1, 5:6] <- 0
	tryCatch(printSurvivalGraphic(x), warning = function(w) stop("Warning in printSurvivalGraphic: w"))
})


## printEfficiency

test_that("printEfficiency returns right string when eff. cannot be calculated", {
	output <- printEfficiency(intra.CJS = NULL, type = "migration")
	expect_equal(output, "Inter-array efficiency could not be calculated. See full log for more details.\n")
	
	output <- printEfficiency(intra.CJS = NULL, type = "residency")
	expect_equal(output, "Inter-array efficiency could not be calculated. See full log for more details.\n")
})

setwd("..")
unlink("exampleWorkspace", recursive = TRUE)
setwd(tests.home)
rm(list = ls())
