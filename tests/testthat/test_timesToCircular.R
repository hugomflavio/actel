skip_on_cran()

test_that("timesToCircular's failsafes kick in if needed", {
	expect_error(timesToCircular(data.frame(a = 1, b = 2, c = 3)),
		"timesToCircular only works on data frames where the second column is a grouping structure and columns three and onwards are timestamps.", fixed = TRUE)
})

test_that("by_group works as expected", {
	x <- data.frame(ID = c(1:5),
	Group = c("A", "A", "B", "B", "B"),
	A1 = as.POSIXct(
	  c("2019-01-03 11:21:12",
	    "2019-01-04 12:22:21",
	    "2019-01-05 13:31:34",
	    "2019-01-06 14:32:43",
	    "2019-01-07 15:23:52")),
	A2 = as.POSIXct(
	  c("2019-01-08 16:51:55",
	    "2019-01-09 17:42:42",
	    "2019-01-10 18:33:33",
	    "2019-01-11 19:24:32",
	    "2019-01-12 20:15:22")),
	stringsAsFactors = TRUE)
	output <- timesToCircular(x, by.group = TRUE)	
	expect_equal(names(output), c("A.A1", "A.A2", "B.A1", "B.A2"))
})