skip_on_cran()

tests.home <- getwd()
setwd(tempdir())

xspatial <- data.frame(
	Station.name = 1:7,
	Array = c("A1", "A2", "B1", "B2", "B3", "C1", "C2"),
	Section = factor(c("A", "A", "B", "B", "B", "C", "C"), 
				levels = c("A", "B", "C")),
	Type = "Hydrophone",
	x = 1:7,
	y = c(1,1,2,2,2,3,3))
dot <- readDot(string = "A1->A2--B1--B2--B3--C1--C2")

test_that("plotDot stops if dot argument is bad", {
	expect_error(plotDot("gibberish"),
		paste0("Could not recognise the input contents as DOT ",
      		"formatted connections."),
		fixed = TRUE)
	a <- data.frame(A = "gibberish")
	expect_error(plotDot(a),
		paste0("Could not recognise the input contents as DOT ",
      		"formatted connections."),
		fixed = TRUE)
	a <- list(A = "gibberish")
	expect_error(plotDot(a),
		paste0("Could not recognize the input ",
        "as a dataframe nor a dot string."),
		fixed = TRUE)
})

test_that("plotDot stops if spatial argument is bad", {
	expect_error(plotDot(dot, spatial = xpatial),
		"spatial was provided, but coord.x and/or coord.y are missing.",
		fixed = TRUE)
	expect_error(plotDot(dot, spatial = xspatial, coord.x = "b", coord.y = "y"),
		"Could not find column 'b' in spatial.",
		fixed = TRUE)
	expect_error(plotDot(dot, spatial = xspatial, coord.x = "x", coord.y = "a"),
		"Could not find column 'a' in spatial.",
		fixed = TRUE)
	expect_error(plotDot(dot, spatial = xspatial[1:5,], 
			coord.x = "x", coord.y = "y"),
		paste0("spatial was provided, but not all arrays specified ",
        "in the dot exist in spatial."),
		fixed = TRUE)
})

test_that("plotDot warns if spatial has more arrays than dot", {
	xspatial2 <- data.frame(
		Station.name = 1:8,
		Array = c("A1", "A2", "B1", "B2", "B3", "C1", "C2", "C3"),
		Section = factor(c("A", "A", "B", "B", "B", "C", "C", "C"), 
					levels = c("A", "B", "C")),
		Type = "Hydrophone",
		x = 1:8,
		y = c(1,1,2,2,2,3,3,3))

	expect_warning(plotDot(dot, spatial = xspatial2, 
			coord.x = "x", coord.y = "y"),
		"Not all arrays present in spatial were listed in dot.",
		fixed = TRUE)
})

test_that("plotDot warns if not enough fill colours are provided", {
	expect_warning(plotDot(dot, spatial = xspatial, 
			coord.x = "x", coord.y = "y", fill = "grey"),
		"Not enough colours defined. Using colour ramp instead.",
		fixed = TRUE)
})

test_that("plotDot stops if file argument is bad", {
	expect_error(plotDot(dot, file = "gibberish"),
		paste0("Could not recognise file extension. ",
      		"Please chose a png, pdf or svg file format."),
		fixed = TRUE)
})

test_that("plotDot throws no unexpected warnings", {
	tryCatch(plotDot(dot, file = "plotDot_output1.pdf"),
		warning = function(w) stop("plotDot threw an unexpected warning!\n", w))
	expect_true(file.exists("plotDot_output1.pdf"))
	
	tryCatch(plotDot(dot, spatial = xspatial, 
			coord.x = "x", coord.y = "y", file = "plotDot_output2.svg"),
		warning = function(w) stop("plotDot threw an unexpected warning!\n", w))
	expect_true(file.exists("plotDot_output2.svg"))

	tryCatch(plotDot(dot = dot, spatial = xspatial, 
			coord.x = "x", coord.y = "y", 
			expand = 0.2, file = "plotDot_output3.png"),
		warning = function(w) stop("plotDot threw an unexpected warning!\n", w))
	expect_true(file.exists("plotDot_output3.png"))
})

setwd(tests.home)
rm(list = ls())
