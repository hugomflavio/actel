skip_on_cran()

tests.home <- getwd()
setwd(tempdir())

test_that("loadDot stops if arguments or file are missing", {
	expect_error(loadDot(), "No dot file or dot string were specified.", fixed = TRUE)
})

test_that("loadDot stops if file is badly formatted or missing", {
	expect_error(loadDot(input = "test"),
		"The contents of the 'test' file could not be recognised by the readDot function.", fixed = TRUE)
	write.table("abcd\n", "spatial.dot")
	expect_error(loadDot(input = "spatial.dot"),
		"The contents of the 'spatial.dot' file could not be recognised by the readDot function.", fixed = TRUE)
	file.remove("spatial.dot")
})

test_that("loadDot stops if contents of file do not match spatial", {
	expect_error(loadDot(string = "A1--A2--A3--A4--A5--A6\n", spatial = example.spatial, disregard.parallels = TRUE, preloading = TRUE),
"Not all arrays listed in the spatial input are present in the dot input. Double-check that the array names in the dot string match the array names in the spatial input.\n       Missing arrays: A0, A7, A8, A9", fixed = TRUE)

	expect_error(loadDot(string = "A0--A1--A2--A3--A20--A4--A5--A6--A7--A8--A9\n", spatial = example.spatial, disregard.parallels = TRUE, preloading = TRUE),
"Not all arrays listed in the dot input are present in the spatial input. The dot input should only contain arrays that are listed in spatial.\n       Alien arrays: A20", fixed = TRUE)

	sink("spatial.dot")
	cat("A1--A2--A3--A4--A5--A6\n")
	sink()
	expect_error(loadDot(input = "spatial.dot", spatial = example.spatial, disregard.parallels = TRUE),
		"Not all the arrays listed in the spatial.csv file are present in the spatial.dot.\n       Missing arrays: A0, A7, A8, A9", fixed = TRUE)
	file.remove("spatial.dot")

	sink("spatial.txt")
	cat("A1--A2--A3--A4--A5--A6\n")
	sink()
	expect_error(loadDot(input = "spatial.txt", spatial = example.spatial, disregard.parallels = TRUE),
		"Not all the arrays listed in the spatial.csv file are present in the spatial.txt.\n       Missing arrays: A0, A7, A8, A9", fixed = TRUE)
	file.remove("spatial.txt")

	sink("spatial.txt")
	cat("A0--A1--A2--A3--A4--A5--A6--A7--A8--A9--A10\n")
	sink()
	expect_error(loadDot(input = "spatial.txt", spatial = example.spatial, disregard.parallels = TRUE),
		"Some arrays listed in the spatial.txt file are not present in the spatial.csv file. The dot input should only contain arrays that are listed in spatial.\n       Alien arrays: A10", fixed = TRUE)
	file.remove("spatial.txt")

	sink("spatial.dot")
	cat("A0--A1--A2--A3--A4--A5--A6--A7--A8--A9--A10\n")
	sink()
	expect_error(loadDot(input = "spatial.dot", spatial = example.spatial, disregard.parallels = TRUE),
		"Some arrays listed in the spatial.dot file are not present in the spatial.csv file. The dot input should only contain arrays that are listed in spatial.\n       Alien arrays: A10", fixed = TRUE)
	file.remove("spatial.dot")
})

test_that("loadDot output is as expected for simple single channel study areas", {
	sink("spatial.txt")
	cat("A0--A1--A2--A3--A4--A5--A6--A7--A8--A9\n")
	sink()
	output <- loadDot(input = "spatial.txt", spatial = example.spatial, disregard.parallels = TRUE)	
	file.remove("spatial.txt")

	expect_equal(names(output), c("dot", "arrays", "dotmat", "paths"))
	
	expect_equal(names(output$arrays), c('A0', 'A1', 'A2', 'A3',
		'A4', 'A5', 'A6', 'A7', 'A8', 'A9'))
	
	dot <- read.csv(text = c('"A","to","B"
"A0","--","A1"
"A1","--","A2"
"A2","--","A3"
"A3","--","A4"
"A4","--","A5"
"A5","--","A6"
"A6","--","A7"
"A7","--","A8"
"A8","--","A9"
'), stringsAsFactors = FALSE)
	expect_equal(output$dot, dot)
	
	dotmat <- read.csv(text =
c('"","A0","A1","A2","A3","A4","A5","A6","A7","A8","A9"
"A0",0,1,2,3,4,5,6,7,8,9
"A1",1,0,1,2,3,4,5,6,7,8
"A2",2,1,0,1,2,3,4,5,6,7
"A3",3,2,1,0,1,2,3,4,5,6
"A4",4,3,2,1,0,1,2,3,4,5
"A5",5,4,3,2,1,0,1,2,3,4
"A6",6,5,4,3,2,1,0,1,2,3
"A7",7,6,5,4,3,2,1,0,1,2
"A8",8,7,6,5,4,3,2,1,0,1
"A9",9,8,7,6,5,4,3,2,1,0
'), stringsAsFactors = FALSE, row.names = 1)
	expect_equal(output$dotmat, as.matrix(dotmat))

	pathnames <- c('A0_to_A2', 'A0_to_A3', 'A0_to_A4', 'A0_to_A5',
		'A0_to_A6', 'A0_to_A7', 'A0_to_A8', 'A0_to_A9', 'A1_to_A3',
		'A1_to_A4', 'A1_to_A5', 'A1_to_A6', 'A1_to_A7', 'A1_to_A8',
		'A1_to_A9', 'A2_to_A0', 'A2_to_A4', 'A2_to_A5', 'A2_to_A6',
		'A2_to_A7', 'A2_to_A8', 'A2_to_A9', 'A3_to_A1', 'A3_to_A5',
		'A3_to_A0', 'A3_to_A6', 'A3_to_A7', 'A3_to_A8', 'A3_to_A9',
		'A4_to_A2', 'A4_to_A6', 'A4_to_A1', 'A4_to_A7', 'A4_to_A0',
		'A4_to_A8', 'A4_to_A9', 'A5_to_A3', 'A5_to_A7', 'A5_to_A2',
		'A5_to_A8', 'A5_to_A1', 'A5_to_A9', 'A5_to_A0', 'A6_to_A4',
		'A6_to_A8', 'A6_to_A3', 'A6_to_A9', 'A6_to_A2', 'A6_to_A1',
		'A6_to_A0', 'A7_to_A5', 'A7_to_A9', 'A7_to_A4', 'A7_to_A3',
		'A7_to_A2', 'A7_to_A1', 'A7_to_A0', 'A8_to_A6', 'A8_to_A5',
		'A8_to_A4', 'A8_to_A3', 'A8_to_A2', 'A8_to_A1', 'A8_to_A0',
		'A9_to_A7', 'A9_to_A6', 'A9_to_A5', 'A9_to_A4', 'A9_to_A3',
		'A9_to_A2', 'A9_to_A1', 'A9_to_A0')
	expect_equal(names(output$paths), pathnames)
	# sample 2 contents
	expect_equal(output$paths[[3]], "A1 -> A2 -> A3")
	expect_equal(output$paths[[6]], "A1 -> A2 -> A3 -> A4 -> A5 -> A6")
})


test_that("loadDot output is as expected for single channel study areas with a barrier", {
	sink("spatial.txt")
	cat("A0--A1--A2--A3--A4--A5->A6--A7--A8--A9\n")
	sink()
	output <- loadDot(input = "spatial.txt", spatial = example.spatial, disregard.parallels = TRUE)	
	file.remove("spatial.txt")

	expect_equal(names(output), c("dot", "arrays", "dotmat", "paths"))
	
	expect_equal(names(output$arrays), c('A0', 'A1', 'A2', 'A3',
		'A4', 'A5', 'A6', 'A7', 'A8', 'A9'))
	
	dot <- read.csv(text = c('"A","to","B"
"A0","--","A1"
"A1","--","A2"
"A2","--","A3"
"A3","--","A4"
"A4","--","A5"
"A5","->","A6"
"A6","--","A7"
"A7","--","A8"
"A8","--","A9"
'), stringsAsFactors = FALSE)
	expect_equal(output$dot, dot)
	
	dotmat <- read.csv(text =
c('"","A0","A1","A2","A3","A4","A5","A6","A7","A8","A9"
"A0",0,1,2,3,4,5,6,7,8,9
"A1",1,0,1,2,3,4,5,6,7,8
"A2",2,1,0,1,2,3,4,5,6,7
"A3",3,2,1,0,1,2,3,4,5,6
"A4",4,3,2,1,0,1,2,3,4,5
"A5",5,4,3,2,1,0,1,2,3,4
"A6",NA,NA,NA,NA,NA,NA,0,1,2,3
"A7",NA,NA,NA,NA,NA,NA,1,0,1,2
"A8",NA,NA,NA,NA,NA,NA,2,1,0,1
"A9",NA,NA,NA,NA,NA,NA,3,2,1,0
'), stringsAsFactors = FALSE, row.names = 1)
	expect_equal(output$dotmat, as.matrix(dotmat))

	pathnames <- c('A0_to_A2', 'A0_to_A3', 'A0_to_A4', 'A0_to_A5',
		'A0_to_A6', 'A0_to_A7', 'A0_to_A8', 'A0_to_A9', 'A1_to_A3',
		'A1_to_A4', 'A1_to_A5', 'A1_to_A6', 'A1_to_A7', 'A1_to_A8',
		'A1_to_A9', 'A2_to_A0', 'A2_to_A4', 'A2_to_A5', 'A2_to_A6',
		'A2_to_A7', 'A2_to_A8', 'A2_to_A9', 'A3_to_A1', 'A3_to_A5',
		'A3_to_A0', 'A3_to_A6', 'A3_to_A7', 'A3_to_A8', 'A3_to_A9',
		'A4_to_A2', 'A4_to_A6', 'A4_to_A1', 'A4_to_A7', 'A4_to_A0',
		'A4_to_A8', 'A4_to_A9', 'A5_to_A3', 'A5_to_A7', 'A5_to_A2',
		'A5_to_A8', 'A5_to_A1', 'A5_to_A9', 'A5_to_A0', 'A6_to_A8',
		'A6_to_A9', 'A7_to_A9', 'A8_to_A6', 'A9_to_A7', 'A9_to_A6')
	expect_equal(names(output$paths), pathnames)
	# sample 2 contents
	expect_equal(output$paths[[3]], "A1 -> A2 -> A3")
	expect_equal(output$paths[[6]], "A1 -> A2 -> A3 -> A4 -> A5 -> A6")
})


test_that("loadDot output is as expected for simple multi channel study areas", {
	sink("spatial.txt")
	cat("A0--A1--A2--A4--A5--A6--A7--A8--A9\n")
	cat("A1--A3--A4\n")
	cat("A2--A3--A2\n")
	sink()
	output <- loadDot(input = "spatial.txt", spatial = example.spatial, disregard.parallels = TRUE)	
	file.remove("spatial.txt")

	expect_equal(names(output), c("dot", "arrays", "dotmat", "paths"))
	
	expect_equal(names(output$arrays), c('A0', 'A1', 'A2', 'A4',
		'A5', 'A6', 'A7', 'A8', 'A9', 'A3'))
	
	dot <- read.csv(text = c('"A","to","B"
"A0","--","A1"
"A1","--","A2"
"A2","--","A4"
"A4","--","A5"
"A5","--","A6"
"A6","--","A7"
"A7","--","A8"
"A8","--","A9"
"A1","--","A3"
"A3","--","A4"
"A2","--","A3"
"A3","--","A2"
'), stringsAsFactors = FALSE)
	expect_equal(output$dot, dot)
	
	dotmat <- read.csv(text =
c('"","A0","A1","A2","A4","A5","A6","A7","A8","A3","A9"
"A0",0,1,2,3,4,5,6,7,2,8
"A1",1,0,1,2,3,4,5,6,1,7
"A2",2,1,0,1,2,3,4,5,1,6
"A4",3,2,1,0,1,2,3,4,1,5
"A5",4,3,2,1,0,1,2,3,2,4
"A6",5,4,3,2,1,0,1,2,3,3
"A7",6,5,4,3,2,1,0,1,4,2
"A8",7,6,5,4,3,2,1,0,5,1
"A3",2,1,1,1,2,3,4,5,0,6
"A9",8,7,6,5,4,3,2,1,6,0
'), stringsAsFactors = FALSE, row.names = 1)
	expect_equal(output$dotmat, as.matrix(dotmat))

	pathnames <- c('A0_to_A2', 'A0_to_A3', 'A0_to_A4', 'A0_to_A5',
		'A0_to_A6', 'A0_to_A7', 'A0_to_A8', 'A0_to_A9', 'A1_to_A4',
		'A1_to_A5', 'A1_to_A6', 'A1_to_A7', 'A1_to_A8', 'A1_to_A9',
		'A2_to_A0', 'A2_to_A5', 'A2_to_A6', 'A2_to_A7', 'A2_to_A8',
		'A2_to_A9', 'A4_to_A1', 'A4_to_A6', 'A4_to_A0', 'A4_to_A7',
		'A4_to_A8', 'A4_to_A9', 'A5_to_A2', 'A5_to_A3', 'A5_to_A7',
		'A5_to_A1', 'A5_to_A8', 'A5_to_A0', 'A5_to_A9', 'A6_to_A4',
		'A6_to_A8', 'A6_to_A2', 'A6_to_A3', 'A6_to_A9', 'A6_to_A1',
		'A6_to_A0', 'A7_to_A5', 'A7_to_A9', 'A7_to_A4', 'A7_to_A2',
		'A7_to_A3', 'A7_to_A1', 'A7_to_A0', 'A8_to_A6', 'A8_to_A5',
		'A8_to_A4', 'A8_to_A2', 'A8_to_A3', 'A8_to_A1', 'A8_to_A0',
		'A9_to_A7', 'A9_to_A6', 'A9_to_A5', 'A9_to_A4', 'A9_to_A2',
		'A9_to_A3', 'A9_to_A1', 'A9_to_A0', 'A3_to_A0', 'A3_to_A5',
		'A3_to_A6', 'A3_to_A7', 'A3_to_A8', 'A3_to_A9')
	expect_equal(names(output$paths), pathnames)
	# sample 2 contents
	expect_equal(output$paths[[3]], c("A1 -> A2", "A1 -> A3"))
	expect_equal(output$paths[[6]], c("A1 -> A2 -> A4 -> A5 -> A6", "A1 -> A3 -> A4 -> A5 -> A6"))
	expect_equal(output$paths$A7_to_A1, c("A6 -> A5 -> A4 -> A2", "A6 -> A5 -> A4 -> A3"))	
})


test_that("loadDot output is as expected for multi channel study areas with barriers", {
	sink("spatial.txt")
	cat("A0--A1--A2--A4--A5--A6--A7--A8--A9\n")
	cat("A1--A3->A4\n")
	cat("A2--A3--A2\n")
	sink()
	output <- loadDot(input = "spatial.txt", spatial = example.spatial, disregard.parallels = TRUE)	
	file.remove("spatial.txt")

	expect_equal(names(output), c("dot", "arrays", "dotmat", "paths"))
	
	expect_equal(names(output$arrays), c('A0', 'A1', 'A2', 'A4',
		'A5', 'A6', 'A7', 'A8', 'A9', 'A3'))
	
	dot <- read.csv(text = c('"A","to","B"
"A0","--","A1"
"A1","--","A2"
"A2","--","A4"
"A4","--","A5"
"A5","--","A6"
"A6","--","A7"
"A7","--","A8"
"A8","--","A9"
"A1","--","A3"
"A3","->","A4"
"A2","--","A3"
"A3","--","A2"
'), stringsAsFactors = FALSE)
	expect_equal(output$dot, dot)
	
	dotmat <- read.csv(text =
c('"","A0","A1","A2","A4","A5","A6","A7","A8","A3","A9"
"A0",0,1,2,3,4,5,6,7,2,8
"A1",1,0,1,2,3,4,5,6,1,7
"A2",2,1,0,1,2,3,4,5,1,6
"A4",3,2,1,0,1,2,3,4,2,5
"A5",4,3,2,1,0,1,2,3,3,4
"A6",5,4,3,2,1,0,1,2,4,3
"A7",6,5,4,3,2,1,0,1,5,2
"A8",7,6,5,4,3,2,1,0,6,1
"A3",2,1,1,1,2,3,4,5,0,6
"A9",8,7,6,5,4,3,2,1,7,0
'), stringsAsFactors = FALSE, row.names = 1)
	expect_equal(output$dotmat, as.matrix(dotmat))

	pathnames <- c('A0_to_A2', 'A0_to_A3', 'A0_to_A4', 'A0_to_A5',
		'A0_to_A6', 'A0_to_A7', 'A0_to_A8', 'A0_to_A9', 'A1_to_A4',
		'A1_to_A5', 'A1_to_A6', 'A1_to_A7', 'A1_to_A8', 'A1_to_A9',
		'A2_to_A0', 'A2_to_A5', 'A2_to_A6', 'A2_to_A7', 'A2_to_A8',
		'A2_to_A9', 'A4_to_A1', 'A4_to_A3', 'A4_to_A6', 'A4_to_A0',
		'A4_to_A7', 'A4_to_A8', 'A4_to_A9', 'A5_to_A2', 'A5_to_A7',
		'A5_to_A1', 'A5_to_A3', 'A5_to_A8', 'A5_to_A0', 'A5_to_A9',
		'A6_to_A4', 'A6_to_A8', 'A6_to_A2', 'A6_to_A9', 'A6_to_A1',
		'A6_to_A3', 'A6_to_A0', 'A7_to_A5', 'A7_to_A9', 'A7_to_A4',
		'A7_to_A2', 'A7_to_A1', 'A7_to_A3', 'A7_to_A0', 'A8_to_A6',
		'A8_to_A5', 'A8_to_A4', 'A8_to_A2', 'A8_to_A1', 'A8_to_A3',
		'A8_to_A0', 'A9_to_A7', 'A9_to_A6', 'A9_to_A5', 'A9_to_A4',
		'A9_to_A2', 'A9_to_A1', 'A9_to_A3', 'A9_to_A0', 'A3_to_A0',
		'A3_to_A5', 'A3_to_A6', 'A3_to_A7', 'A3_to_A8', 'A3_to_A9')
	expect_equal(names(output$paths), pathnames)
	# sample 2 contents
	expect_equal(output$paths$A0_to_A4, c("A1 -> A2", "A1 -> A3"))
	expect_equal(output$paths$A0_to_A7, c("A1 -> A2 -> A4 -> A5 -> A6", "A1 -> A3 -> A4 -> A5 -> A6"))
	expect_equal(output$paths$A7_to_A1, "A6 -> A5 -> A4 -> A2")
})

test_that("loadDot correctly identifies edge arrays", {
	sink("spatial.txt")
	cat("A0--A1--A2--A3--A4--A5--A6--A7--A8--A9\n")
	sink()
	output <- loadDot(input = "spatial.txt", spatial = example.spatial,
		disregard.parallels = TRUE)	
	file.remove("spatial.txt")

	# check edges
	expect_equal(output$arrays$A3$edge, FALSE)
	expect_equal(output$arrays$A6$edge, TRUE)
	expect_equal(output$arrays$A7$edge, FALSE)
	expect_equal(output$arrays$A8$edge, TRUE)
})

test_that("loadDot handles parallel arrays properly", {
	sink("spatial.txt")
	cat("A0--A1--A2--A4--A5--A6--A7--A8--A9\n")
	cat("A1--A3--A4\n")
	cat("A2--A3--A2\n")
	sink()

	output <- loadDot(input = "spatial.txt", spatial = example.spatial,
		disregard.parallels = TRUE)	

	output2 <- loadDot(input = "spatial.txt", spatial = example.spatial,
		disregard.parallels = FALSE)	

	file.remove("spatial.txt")

	expect_equal(names(output), c("dot", "arrays", "dotmat", "paths"))
	
	expect_equal(names(output$arrays), c('A0', 'A1', 'A2', 'A4',
		'A5', 'A6', 'A7', 'A8', 'A9', 'A3'))

	# check parallels
	expect_equal(output$arrays$A3$parallel, "A2")
	expect_equal(output$arrays$A2$parallel, "A3")
	expect_equal(output$arrays$A1$parallel, NULL)

	# check peers with disregard.parallels = TRUE
	expect_equal(output$arrays$A0$after.peers, paste0("A", 1:9))
	expect_equal(output$arrays$A2$after.peers, paste0("A", 4:9))
	expect_equal(output$arrays$A3$after.peers, paste0("A", 4:9))
	expect_equal(output$arrays$A5$after.peers, paste0("A", 6:9))

	expect_equal(output$arrays$A0$before.peers, NULL)
	expect_equal(output$arrays$A2$before.peers, paste0("A", 1:0))
	expect_equal(output$arrays$A3$before.peers, paste0("A", 1:0))
	expect_equal(output$arrays$A5$before.peers, paste0("A", c(4, 2, 3, 1, 0)))

	# check peers with disregard.parallels = TRUE
	expect_equal(output2$arrays$A0$after.peers, paste0("A", 1:9))
	expect_equal(output2$arrays$A2$after.peers, NULL)
	expect_equal(output2$arrays$A3$after.peers, NULL)
	expect_equal(output2$arrays$A5$after.peers, paste0("A", 6:9))

	expect_equal(output2$arrays$A0$before.peers, NULL)
	expect_equal(output2$arrays$A2$before.peers, NULL)
	expect_equal(output2$arrays$A3$before.peers, NULL)
	expect_equal(output2$arrays$A5$before.peers, paste0("A", c(4, 2, 3, 1, 0)))
})


# Push dot functions to the limit with additional connections to make sure everything is working

test_that("dot mechanisms are handling multipaths and parallels properly", {
	xspatial <- data.frame(
		Station.name = c("River1", "River2", "River3", "River4", "River5", "River6", "Fjord", "Sea1", "Sea2", "Sea3", "Sea4", "Sea5"),
		Array = c("River1", "River2", "River3", "River4", "River5", "River6", "Fjord", "Sea1", "Sea2", "Sea3", "Sea4", "Sea5"),
		Section = factor(c(rep("River", 6), "Fjord", rep("Sea", 5)), levels = c("River", "Fjord", "Sea")))

	dot <- readDot(string = "River1 -- River2
River2 -- River3
River3 -- Fjord
Fjord -- Sea1
Sea1 -- Sea2
Sea2 -- Sea3
Sea3 -- Sea4
Sea2 -- Sea5
Sea3 -- Sea5 -- Sea3
River1 -- River4
River4 -- River5
River5 -- Fjord
River4 -- River6
River6 -- Fjord
River3 -- River5
River5 -- River3
River5 -- River6
River6 -- River5
River6 -- River3
River3 -- River6")
	
	mat <- dotMatrix(input = dot)
	arrays <- dotList(input = dot, spatial = xspatial)
	arrays <- dotPaths(input = arrays, disregard.parallels = TRUE)
	# ONLY RUN THIS TO RESET REFERENCE
	# aux_dotPaths_complex_text_disregard_parallels_true <- arrays
	# save(aux_dotPaths_complex_text_disregard_parallels_true, file = paste0(tests.home, "/aux_dotPaths_complex_text_disregard_parallels_true.RData"))
	load(paste0(tests.home, "/aux_dotPaths_complex_text_disregard_parallels_true.RData"))
	expect_equal(arrays, aux_dotPaths_complex_text_disregard_parallels_true)

	arrays <- dotList(input = dot, spatial = xspatial)
	arrays <- dotPaths(input = arrays, disregard.parallels = FALSE)
	# ONLY RUN THIS TO RESET REFERENCE
	# aux_dotPaths_complex_text_disregard_parallels_false <- arrays
	# save(aux_dotPaths_complex_text_disregard_parallels_false, file = paste0(tests.home, "/aux_dotPaths_complex_text_disregard_parallels_false.RData"))
	load(paste0(tests.home, "/aux_dotPaths_complex_text_disregard_parallels_false.RData"))
	expect_equal(arrays, aux_dotPaths_complex_text_disregard_parallels_false)
})

setwd(tests.home)
rm(list = ls())
