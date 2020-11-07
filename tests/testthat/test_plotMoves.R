skip_on_cran()

test_that("plotMoves failsafes kick in", {
	expect_error(plotMoves("a"), "Could not recognise the input as an actel results object.", fixed = TRUE)
	expect_error(plotMoves(list("a")), "Could not recognise the input as an actel results object.", fixed = TRUE)
	expect_warning(plotMoves(example.results, array.alias = c("a" = "b")), "Could not find array 'a' in the study's arrays.", fixed = TRUE)
	expect_error(plotMoves(example.results, tags = "a"), "There are no valid movements for tag(s) 'a'.", fixed = TRUE)
})

test_that("plotMoves complains if many tags are drawn at the same time", {
	expect_message(
		expect_warning(plotMoves(example.results), "Plotting many tags at the same time will likely result in ugly output.", fixed = TRUE),
		"M: Argument 'tags' not set. Plotting all detected tags.", fixed = TRUE)

	expect_warning(plotMoves(example.results, tags = names(example.results$valid.movements)[1:6]),
		"Plotting many tags at the same time will likely result in ugly output.", fixed = TRUE)
})

test_that("plotMoves complains if not enough colours are provided", {
	expect_warning(plotMoves(example.results, tags = names(example.results$valid.movements)[1:2], col = "black"),
		"Not enough colours supplied in 'col' (1 supplied and 2 needed). Reusing colours.", fixed = TRUE)
})

test_that("plotMoves works properly", {
	tryCatch(plotMoves(example.results, tags = "R64K-4451"),
	warning = function(w) stop("plotMoves threw an unexpected warning!\n", w))

	tryCatch(plotMoves(example.results, tags = "R64K-4451", show.release = FALSE),
	warning = function(w) stop("plotMoves threw an unexpected warning!\n", w))

	tryCatch(plotMoves(example.results, tags = "R64K-4451", array.alias = c("A1", "test")),
	warning = function(w) stop("plotMoves threw an unexpected warning!\n", w))

	expect_is("The real tests are above, this is just to prevent test_that from complaining", "character")
})
