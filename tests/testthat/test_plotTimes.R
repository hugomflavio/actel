skip_on_cran()

my.home <- getwd()
setwd(tempdir())

load("aux_plotTimes.RData")
times <- timesToCircular(times)

test_that("plotTimes fail-safes kick in when needed", {
	expect_error(plotTimes(times = 1),
		"'times' must be a list.", fixed = TRUE)
	expect_error(plotTimes(times = times, night = 1:3),
		"'night' must have two values.", fixed = TRUE)
	expect_error(plotTimes(times = times, night = c("20:00", "6:00")),
		"'night' values must be either numeric (between 0 and 24) or in a HH:MM format.", fixed = TRUE)
	expect_error(plotTimes(times = times, night = c(25, 1)),
		"'night' values must be either numeric (between 0 and 24) or in a HH:MM format.", fixed = TRUE)
	expect_error(plotTimes(times = times, title = c(25, 1)),
		"Please provide only one 'title'.", fixed = TRUE)
	expect_error(plotTimes(times = times, mean.dash = 25),
		"'mean.dash' must be either TRUE or FALSE.", fixed = TRUE)
	expect_error(plotTimes(times = times, mean.range = 25),
		"'mean.range' must be either TRUE or FALSE.", fixed = TRUE)
	expect_error(plotTimes(times = times, rings = 25),
		"'rings' must be either TRUE or FALSE.", fixed = TRUE)
	expect_error(plotTimes(times = times, file = c(25, 1)),
		"Please provide only one 'file' name.", fixed = TRUE)
	expect_error(plotTimes(times = times, alpha = c(25, 1)),
		"Please provide only one 'alpha' value.", fixed = TRUE)
	expect_error(plotTimes(times = times, alpha = -1),
		"'alpha' must be numeric (between 0 and 1).", fixed = TRUE)
	expect_error(plotTimes(times = times, alpha = 101),
		"'alpha' must be numeric (between 0 and 1).", fixed = TRUE)
	expect_error(plotTimes(times = times, alpha = "a"),
		"'alpha' must be numeric (between 0 and 1).", fixed = TRUE)
	xtimes <- list(a = times[[1]], b = times[[1]], c = times[[1]], d = times[[1]], e = times[[1]], 
		f = times[[1]], g = times[[1]], h = times[[1]], i = times[[1]])
	expect_error(plotTimes(times = xtimes),
		"To plot this many time series simultaneously, colours must be specified using 'col'.")
})

test_that("plotTimes returns no errors on actual data, plus saves files", {
	tryCatch(plotTimes(times = times, night = c("20:00", "06:00"), title = "This is a test!"), 
		warning = function(w) stop("A warning was issued where it should not have been."))
	tryCatch(plotTimes(times = times, night = c(20, 6), title = "This is a test!", col = c("red", "orange")), 
		warning = function(w) stop("A warning was issued where it should not have been."))
	expect_message(plotTimes(times = times, file = "test_plotTimes_output"),
		"M: Plot saved to test_plotTimes_output.svg", fixed = TRUE)
	expect_true(file.exists("test_plotTimes_output.svg"))
	file.remove("test_plotTimes_output.svg")
})

setwd(my.home)
rm(list = ls())
