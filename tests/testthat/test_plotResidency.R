skip_on_cran()

example.residency.results <- c(example.results, additional.residency.results)
example.residency.results$rsp.info$analysis.type <- "residency"

test_that("plotResidency' failsafes kick in when needed", {
	expect_error(plotResidency("a"), "Could not recognise the input as an actel results object.", fixed = TRUE)
	expect_error(plotResidency(list("a")), "Could not recognise the input as an actel results object.", fixed = TRUE)
	expect_error(plotResidency(example.residency.results, tag = 1:2), "Please list only one tag", fixed = TRUE)
	expect_error(plotResidency(example.residency.results, tag = "a"),
		"Could not find tag 'a' in the input.", fixed = TRUE)
	expect_error(plotResidency(example.results, tag = "R64K-4451"),
		"plotResidency can only be used with residency results.", fixed = TRUE)
})

test_that("plotResidency is working", {
	p <- plotResidency(example.residency.results, tag = "R64K-4451")
	expect_that(p, is_a("ggplot"))
})

test_that("personalised colours are running fine", {
	p <- plotResidency(example.residency.results, tag = "R64K-4451", col = rev(gg_colour_hue(5)))
	expect_that(p, is_a("ggplot"))
	expect_warning(plotResidency(example.residency.results, tag = "R64K-4451", col = "red"),
		"Not enough colours supplied in 'col' (1 supplied and 5 needed). Reusing colours.", fixed = TRUE)
})

rm(list = ls())
