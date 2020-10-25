skip_on_cran()

test_that("dotMatrix stops if any connector is not one of '--', '<-' or '->'", {
	dot <- readDot(string = "A--B--C--D--E--F--G")
	dot$to[1] <- "<>"
	expect_error(dotMatrix(input = dot),
		"Unrecognized connectors. Only use '--', '->' or '<-' to connect nodes.", fixed = TRUE)
	# all other combinations are not recognized by readDot (i.e <<, >>, ><, >-, -<)
})

test_that("dotMatrix output matches expected", {
	dot <- readDot(string = "A--B--C--D--E--F--G")
	output <- read.csv(text = '"","A","B","C","D","E","F","G"
"A",0,1,2,3,4,5,6
"B",1,0,1,2,3,4,5
"C",2,1,0,1,2,3,4
"D",3,2,1,0,1,2,3
"E",4,3,2,1,0,1,2
"F",5,4,3,2,1,0,1
"G",6,5,4,3,2,1,0
', stringsAsFactors = FALSE, row.names = 1)
	expect_equal(dotMatrix(input = dot), as.matrix(output))

	dot <- readDot(string = "A--B--C--D->E--F--G")
	output <- read.csv(text = '"","A","B","C","D","E","F","G"
"A",0,1,2,3,4,5,6
"B",1,0,1,2,3,4,5
"C",2,1,0,1,2,3,4
"D",3,2,1,0,1,2,3
"E",NA,NA,NA,NA,0,1,2
"F",NA,NA,NA,NA,1,0,1
"G",NA,NA,NA,NA,2,1,0
', stringsAsFactors = FALSE, row.names = 1)
	expect_equal(dotMatrix(input = dot), as.matrix(output))

	dot <- readDot(string = "A--B--C--D--E<-F--G")
	output <- read.csv(text = '"","A","B","C","D","E","F","G"
"A",0,1,2,3,4,NA,NA
"B",1,0,1,2,3,NA,NA
"C",2,1,0,1,2,NA,NA
"D",3,2,1,0,1,NA,NA
"E",4,3,2,1,0,NA,NA
"F",5,4,3,2,1,0,1
"G",6,5,4,3,2,1,0
', stringsAsFactors = FALSE, row.names = 1)
	expect_equal(dotMatrix(input = dot), as.matrix(output))

	# more complex output checks are run under the loadDot tests
})

