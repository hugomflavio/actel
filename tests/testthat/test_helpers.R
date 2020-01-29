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

test_that("std.error.circular computes circular std. errors and not regular ones", {
	regular.std <- sd(1:5) / sqrt(length(1:5))
	circular.std <- std.error.circular(1:5)
	expect_false(regular.std == circular.std)

	expect_message(std.error.circular(c(1:5, NA)),
		"M: Ommited 1 missing value.", fixed = TRUE)

	expect_equal(std.error.circular(c(1:5, NA)), circular.std)

	expect_equal(std.error.circular(c(1:5, NA), na.rm = FALSE), NA_real_)
})

test_that("decimalTime works as expected", {
	expect_equal(decimalTime("02:30:00"), c("02:30:00" = 2.5))
	expect_equal(decimalTime("02:30"), c("02:30" = 2.5))
	expect_equal(decimalTime("06:15", unit = "m"), c("06:15" = 375))
	expect_equal(decimalTime("06:15:30", unit = "m"), c("06:15:30" = 375.5))
	expect_equal(decimalTime("01:15:15", unit = "s"), c("01:15:15" = 4515))
	expect_equal(decimalTime(c("01:00", "02:00", "03:00")), c("01:00" = 1, "02:00" = 2, "03:00" = 3))
	expect_error(decimalTime(), "Input appears to be empty.")
	expect_warning(decimalTime("abc"),
		"NAs introduced by coercion", fixed = TRUE)
	expect_warning(decimalTime(c("abc", "01:00")),
		"NAs introduced by coercion", fixed = TRUE)
})