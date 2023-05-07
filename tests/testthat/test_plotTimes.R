skip_on_cran()

tests.home <- getwd()
setwd(tempdir())

load(paste0(tests.home, "/aux_plotTimes.RData"))
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

	expect_error(plotTimes(times = times, col = "blue"),
		"'col' must be of the same length as 'times' (1 != 2).", fixed = TRUE)
})

test_that("plotTimes can handle lots of datasets", {
	xtimes <- list(a = times[[1]], b = times[[1]], c = times[[1]], d = times[[1]], e = times[[1]],
		f = times[[1]], g = times[[1]], h = times[[1]], i = times[[1]], j = times[[1]])

	tryCatch(plotTimes(times = xtimes), warning = function(w) stop("plotTimes threw an unexpected warning"))	
	expect_is("The real test is above, this is just to prevent test_that from complaining", "character")
})

test_that("plotTimes returns no errors on actual data, plus saves files", {
	tryCatch(plotTimes(times = times, night = c("20:00", "06:00"), col = c("blue", "red"), title = "This is a test!"),
		warning = function(w) stop("A warning was issued where it should not have been."))

	tryCatch(plotTimes(times = times, night = c("20:00", "06:00"), col = c("blue", "red"), circular.scale = "linear", title = "This is a test!"),
		warning = function(w) stop("A warning was issued where it should not have been."))

	tryCatch(plotTimes(times = times, night = c("20:00", "06:00"), title = "This is a test!"),
		warning = function(w) stop("A warning was issued where it should not have been."))

	xtimes <- list(a = times[[1]], b = times[[2]], c = times[[1]])
	tryCatch(plotTimes(times = xtimes, night = c(20, 6), title = "This is a test!"),
		warning = function(w) stop("A warning was issued where it should not have been."))

	expect_message(plotTimes(times = times, file = "test_plotTimes_output.svg"),
		"M: Plot saved to test_plotTimes_output.svg", fixed = TRUE)

	expect_true(file.exists("test_plotTimes_output.svg"))
	file.remove("test_plotTimes_output.svg")

	expect_message(plotTimes(times = times, file = "test_plotTimes_output.svg", cex = 2),
		"M: When saving vectorial plots, it is recommended to refine the 'width' and 'height', rather than the 'cex'.", fixed = TRUE)
})

test_that("different extensions are working in plotTimes", {
	tryCatch(plotTimes(times = times, file = "test.svg"), 
		warning = function(w) stop("plotTimes threw an unexpected warning"))	
	expect_true(file.exists("test.svg"))
	file.remove("test.svg")

	tryCatch(plotTimes(times = times, file = "test.pdf"), 
		warning = function(w) stop("plotTimes threw an unexpected warning"))	
	expect_true(file.exists("test.pdf"))
	file.remove("test.pdf")

	tryCatch(plotTimes(times = times, file = "test.png"), 
		warning = function(w) stop("plotTimes threw an unexpected warning"))	
	expect_true(file.exists("test.png"))
	file.remove("test.png")

	tryCatch(plotTimes(times = times, file = "test.tiff"), 
		warning = function(w) stop("plotTimes threw an unexpected warning"))	
	expect_true(file.exists("test.tiff"))
	file.remove("test.tiff")

	expect_error(plotTimes(times = times, file = "test"), 
		"Could not recognise 'file' extension (recognised extensions: .svg, .pdf, .png, .tiff).", fixed = TRUE)	
})

test_that("plotTimes complains if xjust is set in corner legend", {
	expect_warning(plotTimes(times = times, xjust = "centre"), 
		"'xjust' was set but legend is being plotted in the corner. Ignoring 'xjust'.", fixed = TRUE)	

	tryCatch(plotTimes(times = times, legend.pos = "bottom"), 
		warning = function(w) stop("plotTimes threw an unexpected warning"))	

	tryCatch(plotTimes(times = times, legend.pos = "bottom", xjust = "right"), 
		warning = function(w) stop("plotTimes threw an unexpected warning"))	
})

setwd(tests.home)
rm(list = ls())
