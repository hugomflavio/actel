skip_on_cran()

tests.home <- getwd()
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

test_that("extractSignals is working", {
	expect_equal(extractSignals(input = "A-B-123"), "123")
	expect_equal(extractSignals(input = "ABV-023"), "023")	
})

test_that("extractCodeSpaces is working", {
	expect_equal(extractCodeSpaces(input = "A-B-123"), "A-B")
	expect_equal(extractCodeSpaces(input = "ABV-023"), "ABV")	
})

test_that("stationName is working", {
	expect_error(stationName("a"), "Could not recognise the input as an actel results object.")
	expect_error(stationName(list("a")), "Could not recognise the input as an actel results object.")

	expect_equal(stationName(example.results, 2:3), c("Station 1", "Station 2"))
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

test_that("appendTo stores comments.", {
	appendTo("Comment", "test comment", "Test tag")
	expect_true(file.exists("temp_comments.txt"))
	expect_equal(read.table("temp_comments.txt", sep = "\t"), read.csv(text = '"V1","V2"\n"Test tag","test comment"'))
	file.remove("temp_comments.txt")
})

test_that("splitN works as expected", {
	x <- data.frame(V1 = 1:10, V2 = 1:10)
	rownames(x) <- letters[1:10]
	output <- splitN(x, 5)
	expect_equal(length(output), 2)
	expect_equal(output[[1]]$V1, 1:5)
	expect_equal(output[[2]]$V2, 6:10)
	expect_equal(rownames(output[[1]]), as.character(1:5))
	expect_equal(rownames(output[[2]]), as.character(1:5))

	output <- splitN(x, 5, row.names = TRUE)
	expect_equal(length(output), 2)
	expect_equal(output[[1]]$V1, 1:5)
	expect_equal(output[[2]]$V2, 6:10)
	expect_equal(rownames(output[[1]]), letters[1:5])
	expect_equal(rownames(output[[2]]), letters[6:10])
})

test_that("createEventRanges works as expected", {
	x <- createEventRanges(c(1,2,3,5,6,7,10))
	expect_equal(x, c("1:3", "5:7", "10"))
})

test_that("recoverLog works as expected", {
	if (file.exists("latest_actel_error_log.txt"))
		file.remove("latest_actel_error_log.txt")
	expect_error(recoverLog(), "No crash logs found")

	sink("latest_actel_error_log.txt")
cat("Actel R package report.
Version: 1.2.0.9022

Target folder: C:/Users/hdmfla/AppData/Local/Temp/Rtmp2tLEAI
Timestamp: 2020-12-23 12:30:55
Function: explore()

M: Importing data. This process may take a while.
Error: Could not find a 'biometrics.csv' file in the working directory.

A fatal exception occurred, stopping the process!
Found a bug? Report it here: https://github.com/hugomflavio/actel/issues

-------------------
Function call:
-------------------
explore(tz = 'Europe/Copenhagen', datapack = NULL, max.interval = 60, minimum.detections = 2, start.time = NULL, stop.time = NULL, speed.method = c('last to first'), speed.warning = NULL, speed.error = NULL, jump.warning = 2, jump.error = 3, inactive.warning = NULL, inactive.error = NULL, exclude.tags = NULL, override = NULL, report = FALSE, discard.orphans = FALSE, discard.first = NULL, auto.open = TRUE, save.detections = FALSE, GUI = 'needed', save.tables.locally = 'FALSE, print.releases = TRUE, plot.detections.by = 'auto')
-------------------
", fill = TRUE)	
	sink()
	expect_warning(recoverLog(), "'file' argument is missing. Attempting to save log to 'actel_job_log.txt'. To specify a different target, use the 'file' argument.")	
	expect_true(file.exists("actel_job_log.txt"))

	recoverLog("test.txt")
	expect_true(file.exists("test.txt"))

	file.remove("actel_job_log.txt")
	file.remove("test.txt")
	file.remove("latest_actel_error_log.txt")
})

setwd(tests.home)
