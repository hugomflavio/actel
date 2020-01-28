test_that("dataToList is working", {
	a <- matrix(c(1,2,3,4), 2, 2)
	b <- data.frame(A = 1:2, B = 3:4)
	c <- "migration"
	save(a, b, c, file = "temp.RData")
	output <- dataToList(source = "temp.RData")
	expect_equal(names(output), c("a", "b", "c"))
	expect_is(output$a, "matrix")
	expect_is(output$b, "data.frame")
	expect_is(output$c, "character")
	file.remove("temp.RData")
})

test_that("stripCodeSpaces is working", {
	expect_equal(stripCodeSpaces(input = "A-B-123"), "123")
	expect_equal(stripCodeSpaces(input = "ABV-023"), "023")	
})
