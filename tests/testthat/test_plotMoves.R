skip_on_cran()

test_that("plotMoves' failsafes kick in when needed", {
	expect_error(plotMoves("a"), "Could not recognise the input as an actel results object.", fixed = TRUE)
	expect_error(plotMoves(list("a")), "Could not recognise the input as an actel results object.", fixed = TRUE)
	expect_error(plotMoves(example.results, tag = "a"), 
		"Could not find tag 'a' in the input.", fixed = TRUE)
})

test_that("plotMoves is working", {
	p <- plotMoves(example.results, tag = "R64K-4457")
	expect_that(p, is_a("ggplot"))
})

test_that("array aliases are running fine", {
	p <- plotMoves(example.results, tag = "R64K-4457", array.alias = c(River0 = "test"))
	expect_that(p, is_a("ggplot"))
	expect_warning(plotMoves(example.results, tag = "R64K-4457", array.alias = c(wrong_name = "test")),
		"Could not find array wrong_name in the study's arrays.", fixed = TRUE)
})

test_that("personalised colours are running fine", {
	p <- plotMoves(example.results, tag = "R64K-4457", col = rev(gg_colour_hue(10)))
	expect_that(p, is_a("ggplot"))
	expect_warning(plotMoves(example.results, tag = "R64K-4457", col = "red"),
		"Not enough colours supplied in 'col' (1 supplied and 10 needed). Reusing colours.", fixed = TRUE)
})

test_that("run code for colour frames", {
	xresults <- example.results
	attributes(xresults$movements[["R64K-4457"]])$p.type <- "Manual"
	p <- plotMoves(xresults, tag = "R64K-4457")
	expect_that(p, is_a("ggplot"))
	
	attributes(xresults$movements[["R64K-4457"]])$p.type <- "Overridden"
	p <- plotMoves(xresults, tag = "R64K-4457")
	expect_that(p, is_a("ggplot"))
})
