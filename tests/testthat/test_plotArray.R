skip_on_cran()

test_that("plotArray' failsafes kick in when needed", {
	expect_error(plotArray("a"), "Could not recognise the input as an actel results object.", fixed = TRUE)
	expect_error(plotArray(list("a")), "Could not recognise the input as an actel results object.", fixed = TRUE)
	expect_error(plotArray(example.results, arrays = 1:2), "Could not find array(s) '1', '2' in the study area.", fixed = TRUE)
})


test_that("plotArray works properly", {
	tryCatch(plotArray(example.results, arrays = "A1"),
	warning = function(w) stop("plotArray threw an unexpected warning!\n", w))

	tryCatch(plotArray(example.results, arrays = "A1", timestep = "hours"),
	warning = function(w) stop("plotArray threw an unexpected warning!\n", w))

	tryCatch(plotArray(example.results, arrays = "A1", timestep = "mins"),
	warning = function(w) stop("plotArray threw an unexpected warning!\n", w))

	tryCatch(plotArray(example.results, arrays = c("A1", "A2"), cumulative = TRUE),
	warning = function(w) stop("plotArray threw an unexpected warning!\n", w))
})
