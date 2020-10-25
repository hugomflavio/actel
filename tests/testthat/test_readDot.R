skip_on_cran()

tests.home <- getwd()
setwd(tempdir())

test_that("readDot stops if argument or file is missing", {
	expect_error(readDot(),
		"No dot file or data were specified.", fixed = TRUE)
	expect_error(readDot(input = "test"),
		"Could not find a 'test' file in the working directory.", fixed = TRUE)
	dot <- read.csv(text = c("A,to,B
A,--,B
B,--,C
C,--,D
D,<-,E
E,->,F
F,<-,D
"), stringsAsFactors = FALSE)
	expect_equal(readDot(string = "A--B--C--D<-E->F<-D"), dot)
})

test_that("readDot stops if the data inside the file/string does not meet expectations", {
	expect_error(readDot(string = "randomtestwithoutpropperformatting!A>"),
		"Could not recognise the input contents as DOT formatted connections.", fixed = TRUE)
	sink("test.txt")
	cat("anotherSillyTeststringWithnodotinfointit\n")
	sink()
	expect_error(readDot(input = "test.txt"),
		"Could not recognise the input contents as DOT formatted connections.", fixed = TRUE)
	file.remove("test.txt")
})

setwd(tests.home)
rm(list = ls())