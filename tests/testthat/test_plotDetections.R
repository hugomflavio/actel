skip_on_cran()

test_that("plotDetections' failsafes kick in when needed", {
	expect_error(plotDetections("a"), "Could not recognise the input as an actel results object.", fixed = TRUE)
	expect_error(plotDetections(list("a")), "Could not recognise the input as an actel results object.", fixed = TRUE)
	expect_error(plotSensors(example.results, tag = 1:2), "Please list only one tag", fixed = TRUE)
	expect_error(plotDetections(example.results, tag = "a"),
		"Could not find tag 'a' in the input.", fixed = TRUE)
})

test_that("plotDetections is working", {
	p <- plotDetections(example.results, tag = "R64K-4451")
	expect_that(p, is_a("ggplot"))
})

test_that("array aliases are running fine", {
	p <- plotDetections(example.results, tag = "R64K-4451", array.alias = c(A0 = "test"))
	expect_that(p, is_a("ggplot"))
	expect_warning(plotDetections(example.results, tag = "R64K-4451", array.alias = c(wrong_name = "test")),
		"Could not find array wrong_name in the study's arrays.", fixed = TRUE)
})

test_that("personalised colours are running fine", {
	p <- plotDetections(example.results, tag = "R64K-4451", col = rev(gg_colour_hue(10)))
	expect_that(p, is_a("ggplot"))
	expect_warning(plotDetections(example.results, tag = "R64K-4451", col = "red"),
		"Not enough colours supplied in 'col' (1 supplied and 10 needed). Reusing colours.", fixed = TRUE)
})

test_that("run code for colour frames", {
	xresults <- example.results
	attributes(xresults$movements[["R64K-4451"]])$p.type <- "Manual"
	p <- plotDetections(xresults, tag = "R64K-4451")
	expect_that(p, is_a("ggplot"))
	
	attributes(xresults$movements[["R64K-4451"]])$p.type <- "Overridden"
	p <- plotDetections(xresults, tag = "R64K-4451")
	expect_that(p, is_a("ggplot"))
})
