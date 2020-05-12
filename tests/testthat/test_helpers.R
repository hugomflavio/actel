skip_on_cran()

my.home <- getwd()
setwd(tempdir())

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

test_that("minuteTime works as expected.", {
	expect_error(minuteTime(), "Input appears to be empty.")
	expect_error(minuteTime("1"), "Input is not numeric.")
	expect_equal(minuteTime(-5), "-05:00:00")
	expect_equal(minuteTime(-5, seconds = FALSE), "-05:00")
	expect_equal(minuteTime(-5, format = "m"), "-00:05:00")
	expect_equal(minuteTime(-5, format = "s"), "-00:00:05")
	expect_equal(minuteTime(-5, format = "s", seconds = FALSE), "-00:00")
	expect_equal(minuteTime(c(-5, 5.5)), c("-05:00:00", "05:30:00"))
})

test_that("combine works as expected.", {
	expect_error(combine("test"), "'combine' is only intended to combine a list of vectors to a single vector.")
	expect_error(combine(list(A = 1:5, B = 1:4)), "All vectors to combine should have the same length.")
	expect_error(combine(list(A = 1:5, B = 1:5)), "Trying to combine value to an already used position.")
	expect_equal(combine(list(A = c(NA, 1, NA), B = c(2, NA, 2))), c(2, 1, 2))
})

test_that("roundUp works as expected.", {
	expect_equal(roundUp(153), 160)
	expect_equal(roundUp(15.3, to = 1), 16)
	expect_equal(roundUp(15.3, to = 2), 16)
	expect_equal(roundUp(14.2, to = 1), 15)
	expect_equal(roundUp(14.1, to = 2), 16)
	expect_equal(roundUp(list(A = 1:5, B = 10.3:13.3), to = 1), list(A = 1:5, B = 11:14))
})

test_that("appendTo stores comments.", {
	appendTo("Comment", "test comment", "Test fish")
	expect_true(file.exists("temp_comments.txt"))
	expect_equal(read.table("temp_comments.txt", sep = "\t"), read.csv(text = '"V1","V2"\n"Test fish","test comment"'))
	file.remove("temp_comments.txt")
})

test_that("emergencyBreak stores UD.", {
	appendTo("Report", "test start of report")
	appendTo("UD", 1)
	emergencyBreak()
	x <- readLines(list.files(pattern = gsub("-","\\-", Sys.Date())))
	expect_equal(x[6], "User interventions:")
	expect_equal(x[8], "1")
	file.remove(list.files(pattern = "*txt$"))
})

test_that("moveHelpers transfers helper files to new directory.", {
	x <- getwd()
	appendTo(c("Report", "Warning"), "create test files")
	dir.create("temp")
	setwd("temp")
	moveHelpers(x)
	expect_equal(list.files(), c("temp_debug.txt", "temp_log.txt", "temp_warnings.txt"))
	setwd("..")
	expect_false(file.exists("temp_debug.txt"))
	expect_false(file.exists("temp_log.txt"))
	expect_false(file.exists("temp_warnings.txt"))
	unlink("temp", recursive = TRUE)
})

test_that("clearWorkspace removes target files.", {
	test <- 1
	save(test, file = "actel_migration_results.RData")
	dir.create("Report")
	sink("temp.txt")
	clearWorkspace()
	sink()
	expect_message(clearWorkspace(), "Workspace already clean.")
})

setwd(my.home)
