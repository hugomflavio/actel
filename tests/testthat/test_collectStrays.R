skip_on_cran()

tests.home <- getwd()
setwd(tempdir())

test_that("collectStrays work as expected", {
	xdet <- list(Test = example.detections[1:5, ])
	colnames(xdet[[1]])[1] <- "Timestamp"
	collectStrays(input = xdet)
	expect_true(file.exists("temp_strays.csv"))
	output <- read.csv("temp_strays.csv")
	expect_equal(nrow(output), 1)
	collectStrays(input = xdet)
	output <- read.csv("temp_strays.csv")
	expect_equal(nrow(output), 2)

	storeStrays()
	expect_true(file.exists("stray_tags.csv"))
	collectStrays(input = xdet)
	storeStrays()
	expect_true(file.exists("stray_tags.1.csv"))

	file.remove(list.files(pattern = "stray_tags"))
	file.remove("temp_strays.csv")
})
# y
# y

setwd(tests.home)
