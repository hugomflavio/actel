skip_on_cran()

test_that("dotList correctly assigns before, after and neighbours", {
	xspatial <- data.frame(
		Station.name = 1:7,
		Array = c("A1", "A2", "B1", "B2", "B3", "C1", "C2"),
		Section = factor(c("A", "A", "B", "B", "B", "C", "C"), levels = c("A", "B", "C")))

	dot <- readDot(string = "A1--A2--B1--B2--B3--C1--C2")
	output <- dotList(input = dot, spatial = xspatial)
	expect_equal(names(output), c('A1', 'A2', 'B1', 'B2', 'B3', 'C1', 'C2'))
	
	expect_null(output$A1$before)
	expect_equal(output$A1$after, "A2")
	expect_equal(output$A1$neighbours, "A2")

	expect_equal(output$B1$before, "A2")
	expect_equal(output$B1$after, "B2")
	expect_equal(output$B1$neighbours, c("B2", "A2"))

	expect_equal(output$C2$before, "C1")
	expect_null(output$C2$after)
	expect_equal(output$C2$neighbours, "C1")
})

test_that("dotList correctly handles parallels", {
	xspatial <- data.frame(
		Station.name = 1:8,
		Array = c("A1", "A2", "B1", "B2", "B3", "B4", "C1", "C2"),
		Section = factor(c("A", "A", "B", "B", "B", "B", "C", "C"), levels = c("A", "B", "C")))

	dot <- readDot(string = "A1--A2--B1--B2--B3--C1--C2\nB1--B4--B3\nB4--B2--B4")
	output <- dotList(input = dot, spatial = xspatial)
	expect_equal(names(output), c('A1', 'A2', 'B1', 'B2', 'B3', 'C1', 'C2', 'B4'))
	
	expect_null(output$A1$before)
	expect_equal(output$A1$after, "A2")
	expect_equal(output$A1$neighbours, "A2")
	expect_null(output$A1$parallel)

	expect_equal(output$B1$before, "A2")
	expect_equal(output$B1$after, c("B2", "B4"))
	expect_equal(output$B1$neighbours, c("B2", "B4", "A2"))
	expect_null(output$B1$parallel)

	expect_equal(output$B2$before, "B1")
	expect_equal(output$B2$after, "B3")
	expect_equal(output$B2$neighbours, c("B3", "B1", "B4"))
	expect_equal(output$B2$parallel, "B4")

	expect_equal(output$B4$before, "B1")
	expect_equal(output$B4$after, "B3")
	expect_equal(output$B4$neighbours, c("B3", "B1", "B2"))
	expect_equal(output$B4$parallel, "B2")

	expect_equal(output$B3$before, c("B2", "B4"))
	expect_equal(output$B3$after, "C1")
	expect_equal(output$B3$neighbours, c("C1", "B2", "B4"))
	expect_null(output$B3$parallel)
})


test_that("dotList includes edge information if sections are provided", {
	xspatial <- data.frame(
		Station.name = 1:7,
		Array = c("A1", "A2", "B1", "B2", "B3", "C1", "C2"),
		Section = factor(c("A", "A", "B", "B", "B", "C", "C"), levels = c("A", "B", "C")))

	# without parallels
	dot <- readDot(string = "A1--A2--B1--B2--B3--C1--C2")
	output <- dotList(input = dot, spatial = xspatial)	
	expect_true(output$A2$edge)
	expect_true(output$B3$edge)
	expect_false(output$A1$edge)
	expect_false(output$B2$edge)
	expect_false(output$C2$edge)

	xspatial <- data.frame(
		Station.name = 1:8,
		Array = c("A1", "A2", "B1", "B2", "B3", "B4", "C1", "C2"),
		Section = factor(c("A", "A", "B", "B", "B", "B", "C", "C"), levels = c("A", "B", "C")))

	dot <- readDot(string = "A1--A2--B1--B2--B3--C1--C2\nB2--B4--C1\nB4--B3--B4")
	output <- dotList(input = dot, spatial = xspatial)	
	expect_true(output$B3$edge)
	expect_true(output$B4$edge)
	expect_false(output$C1$edge)
	expect_false(output$B2$edge)

	dot <- readDot(string = "A1--A2--B1--B2--B3--C1--C2\nA2--B4--B2\nB4--B1--B4")
	output <- dotList(input = dot, spatial = xspatial)	
	expect_true(output$A2$edge)
	expect_true(output$B3$edge)
	expect_false(output$B1$edge)
	expect_false(output$B4$edge)
})
