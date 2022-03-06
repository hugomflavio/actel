skip_on_cran()

example.residency.results <- c(example.results, additional.residency.results)
example.residency.results$rsp.info$analysis.type <- "residency"

test_that("plotRatios' failsafes kick in when needed", {
	expect_error(plotRatios("a"), "Could not recognise the input as an actel results object.", fixed = TRUE)
	expect_error(plotRatios(list("a")), "Could not recognise the input as an actel results object.", fixed = TRUE)
	expect_error(plotRatios(example.results),
		"plotRatios can only be used with residency results.", fixed = TRUE)
	expect_error(plotRatios(example.residency.results, group = 1),
		"Could not find group(s) '1' in the input.", fixed = TRUE)
	expect_error(plotRatios(example.residency.results, sections = 1),
		"Section '1' does not exist, or no tags have ever been assigned to it.", fixed = TRUE)
})

test_that("default plotRatios is working", {
	p <- plotRatios(example.residency.results)
	expect_that(p, is_a("ggplot"))
})

test_that("personalised colours are running fine", {
	tryCatch(p <- plotRatios(example.residency.results, col = rev(gg_colour_hue(5))),
		warning = function(w) stop("plotRatios threw an unexpected warning:", w))
	expect_that(p, is_a("ggplot"))
	expect_warning(plotRatios(example.residency.results, col = "red"),
		"Not enough colours supplied in 'col' (1 supplied and 5 needed). Reusing colours.", fixed = TRUE)
})

test_that("plotRatios with groups is working", {
	tryCatch(p <- plotRatios(example.residency.results, group = "A"),
		warning = function(w) stop("plotRatios threw an unexpected warning:", w))
	expect_that(p, is_a("ggplot"))
	tryCatch(p <- plotRatios(example.residency.results, group = "A", type = "percentages"),
		warning = function(w) stop("plotRatios threw an unexpected warning:", w))
	expect_that(p, is_a("ggplot"))
})

test_that("plotRatios with sections is working", {
	tryCatch(p <- plotRatios(example.residency.results, sections = c("River", "River-Fjord")),
		warning = function(w) stop("plotRatios threw an unexpected warning:", w))
	expect_that(p, is_a("ggplot"))
	tryCatch(p <- plotRatios(example.residency.results, sections = c("River", "River-Fjord"), type = "percentages"),
		warning = function(w) stop("plotRatios threw an unexpected warning:", w))
	expect_that(p, is_a("ggplot"))
})

rm(list = ls())
