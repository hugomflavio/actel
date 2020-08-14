skip_on_cran()

test_that("plotArray works properly", {
	tryCatch(plotArray(example.results, arrays = "A1"),
	warning = function(w) stop("plotMoves threw an unexpected warning!\n", w))

	tryCatch(plotMoves(example.results, arrays = c("A1", "A2", cumulative = TRUE),
	warning = function(w) stop("plotMoves threw an unexpected warning!\n", w))
})
