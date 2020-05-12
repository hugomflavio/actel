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
	expect_error(loadDot(string = "River1--River2--River3--River4--River5--River6\n", spatial = example.spatial, disregard.parallels = TRUE),
		"Something went wrong when compiling the dot file. Try restarting R and trying again. If the problem persists, contact the developer.", fixed = TRUE)

	sink("spatial.dot")
	cat("River1--River2--River3--River4--River5--River6\n")
	sink()
	expect_error(loadDot(input = "spatial.dot", spatial = example.spatial, disregard.parallels = TRUE),
		"Not all the arrays listed in the spatial.csv file are present in the spatial.dot.\nMissing arrays: River0, Fjord1, Fjord2, Sea", fixed = TRUE)
	file.remove("spatial.dot")

	sink("spatial.txt")
	cat("River1--River2--River3--River4--River5--River6\n")
	sink()
	expect_error(loadDot(input = "spatial.txt", spatial = example.spatial, disregard.parallels = TRUE),
		"Not all the arrays listed in the spatial.csv file are present in the spatial.txt.\nMissing arrays: River0, Fjord1, Fjord2, Sea", fixed = TRUE)
	file.remove("spatial.txt")
})

test_that("loadDot output is as expected for simple single channel study areas", {
	sink("spatial.txt")
	cat("River0--River1--River2--River3--River4--River5--River6--Fjord1--Fjord2--Sea1\n")
	sink()
	output <- loadDot(input = "spatial.txt", spatial = example.spatial, disregard.parallels = TRUE)	
	file.remove("spatial.txt")

	expect_equal(names(output), c("dot", "arrays", "dotmat", "paths"))
	
	expect_equal(names(output$arrays), c('River0', 'River1', 'River2', 'River3', 
		'River4', 'River5', 'River6', 'Fjord1', 'Fjord2', 'Sea1'))
	
	dot <- read.csv(text = c('"A","to","B"
"River0","--","River1"
"River1","--","River2"
"River2","--","River3"
"River3","--","River4"
"River4","--","River5"
"River5","--","River6"
"River6","--","Fjord1"
"Fjord1","--","Fjord2"
"Fjord2","--","Sea1"
'), stringsAsFactors = FALSE)
	expect_equal(output$dot, dot)
	
	dotmat <- read.csv(text = 
c('"","River0","River1","River2","River3","River4","River5","River6","Fjord1","Fjord2","Sea1"
"River0",0,1,2,3,4,5,6,7,8,9
"River1",1,0,1,2,3,4,5,6,7,8
"River2",2,1,0,1,2,3,4,5,6,7
"River3",3,2,1,0,1,2,3,4,5,6
"River4",4,3,2,1,0,1,2,3,4,5
"River5",5,4,3,2,1,0,1,2,3,4
"River6",6,5,4,3,2,1,0,1,2,3
"Fjord1",7,6,5,4,3,2,1,0,1,2
"Fjord2",8,7,6,5,4,3,2,1,0,1
"Sea1",9,8,7,6,5,4,3,2,1,0
'), stringsAsFactors = FALSE, row.names = 1)
	expect_equal(output$dotmat, as.matrix(dotmat))

	pathnames <- c('River0_to_River2', 'River0_to_River3', 'River0_to_River4', 'River0_to_River5', 
		'River0_to_River6', 'River0_to_Fjord1', 'River0_to_Fjord2', 'River0_to_Sea1', 'River1_to_River3', 
		'River1_to_River4', 'River1_to_River5', 'River1_to_River6', 'River1_to_Fjord1', 'River1_to_Fjord2', 
		'River1_to_Sea1', 'River2_to_River0', 'River2_to_River4', 'River2_to_River5', 'River2_to_River6', 
		'River2_to_Fjord1', 'River2_to_Fjord2', 'River2_to_Sea1', 'River3_to_River1', 'River3_to_River5', 
		'River3_to_River0', 'River3_to_River6', 'River3_to_Fjord1', 'River3_to_Fjord2', 'River3_to_Sea1', 
		'River4_to_River2', 'River4_to_River6', 'River4_to_River1', 'River4_to_Fjord1', 'River4_to_River0', 
		'River4_to_Fjord2', 'River4_to_Sea1', 'River5_to_River3', 'River5_to_Fjord1', 'River5_to_River2', 
		'River5_to_Fjord2', 'River5_to_River1', 'River5_to_Sea1', 'River5_to_River0', 'River6_to_River4', 
		'River6_to_Fjord2', 'River6_to_River3', 'River6_to_Sea1', 'River6_to_River2', 'River6_to_River1', 
		'River6_to_River0', 'Fjord1_to_River5', 'Fjord1_to_Sea1', 'Fjord1_to_River4', 'Fjord1_to_River3', 
		'Fjord1_to_River2', 'Fjord1_to_River1', 'Fjord1_to_River0', 'Fjord2_to_River6', 'Fjord2_to_River5', 
		'Fjord2_to_River4', 'Fjord2_to_River3', 'Fjord2_to_River2', 'Fjord2_to_River1', 'Fjord2_to_River0', 
		'Sea1_to_Fjord1', 'Sea1_to_River6', 'Sea1_to_River5', 'Sea1_to_River4', 'Sea1_to_River3', 
		'Sea1_to_River2', 'Sea1_to_River1', 'Sea1_to_River0')
	expect_equal(names(output$paths), pathnames)
	# sample 2 contents
	expect_equal(output$paths[[3]], "River1 -> River2 -> River3")
	expect_equal(output$paths[[6]], "River1 -> River2 -> River3 -> River4 -> River5 -> River6")
})


test_that("loadDot output is as expected for single channel study areas with a barrier", {
	sink("spatial.txt")
	cat("River0--River1--River2--River3--River4--River5->River6--Fjord1--Fjord2--Sea1\n")
	sink()
	output <- loadDot(input = "spatial.txt", spatial = example.spatial, disregard.parallels = TRUE)	
	file.remove("spatial.txt")

	expect_equal(names(output), c("dot", "arrays", "dotmat", "paths"))
	
	expect_equal(names(output$arrays), c('River0', 'River1', 'River2', 'River3', 
		'River4', 'River5', 'River6', 'Fjord1', 'Fjord2', 'Sea1'))
	
	dot <- read.csv(text = c('"A","to","B"
"River0","--","River1"
"River1","--","River2"
"River2","--","River3"
"River3","--","River4"
"River4","--","River5"
"River5","->","River6"
"River6","--","Fjord1"
"Fjord1","--","Fjord2"
"Fjord2","--","Sea1"
'), stringsAsFactors = FALSE)
	expect_equal(output$dot, dot)
	
	dotmat <- read.csv(text = 
c('"","River0","River1","River2","River3","River4","River5","River6","Fjord1","Fjord2","Sea1"
"River0",0,1,2,3,4,5,6,7,8,9
"River1",1,0,1,2,3,4,5,6,7,8
"River2",2,1,0,1,2,3,4,5,6,7
"River3",3,2,1,0,1,2,3,4,5,6
"River4",4,3,2,1,0,1,2,3,4,5
"River5",5,4,3,2,1,0,1,2,3,4
"River6",NA,NA,NA,NA,NA,NA,0,1,2,3
"Fjord1",NA,NA,NA,NA,NA,NA,1,0,1,2
"Fjord2",NA,NA,NA,NA,NA,NA,2,1,0,1
"Sea1",NA,NA,NA,NA,NA,NA,3,2,1,0
'), stringsAsFactors = FALSE, row.names = 1)
	expect_equal(output$dotmat, as.matrix(dotmat))

	pathnames <- c('River0_to_River2', 'River0_to_River3', 'River0_to_River4', 'River0_to_River5', 
		'River0_to_River6', 'River0_to_Fjord1', 'River0_to_Fjord2', 'River0_to_Sea1', 'River1_to_River3', 
		'River1_to_River4', 'River1_to_River5', 'River1_to_River6', 'River1_to_Fjord1', 'River1_to_Fjord2', 
		'River1_to_Sea1', 'River2_to_River0', 'River2_to_River4', 'River2_to_River5', 'River2_to_River6', 
		'River2_to_Fjord1', 'River2_to_Fjord2', 'River2_to_Sea1', 'River3_to_River1', 'River3_to_River5', 
		'River3_to_River0', 'River3_to_River6', 'River3_to_Fjord1', 'River3_to_Fjord2', 'River3_to_Sea1', 
		'River4_to_River2', 'River4_to_River6', 'River4_to_River1', 'River4_to_Fjord1', 'River4_to_River0', 
		'River4_to_Fjord2', 'River4_to_Sea1', 'River5_to_River3', 'River5_to_Fjord1', 'River5_to_River2', 
		'River5_to_Fjord2', 'River5_to_River1', 'River5_to_Sea1', 'River5_to_River0', 'River6_to_Fjord2', 
		'River6_to_Sea1', 'Fjord1_to_Sea1', 'Fjord2_to_River6', 'Sea1_to_Fjord1', 'Sea1_to_River6')
	expect_equal(names(output$paths), pathnames)
	# sample 2 contents
	expect_equal(output$paths[[3]], "River1 -> River2 -> River3")
	expect_equal(output$paths[[6]], "River1 -> River2 -> River3 -> River4 -> River5 -> River6")
})


test_that("loadDot output is as expected for simple multi channel study areas", {
	sink("spatial.txt")
	cat("River0--River1--River2--River4--River5--River6--Fjord1--Fjord2--Sea1\n")
	cat("River1--River3--River4\n")
	cat("River2--River3--River2\n")
	sink()
	output <- loadDot(input = "spatial.txt", spatial = example.spatial, disregard.parallels = TRUE)	
	file.remove("spatial.txt")

	expect_equal(names(output), c("dot", "arrays", "dotmat", "paths"))
	
	expect_equal(names(output$arrays), c('River0', 'River1', 'River2', 'River4', 
		'River5', 'River6', 'Fjord1', 'Fjord2', 'Sea1', 'River3'))
	
	dot <- read.csv(text = c('"A","to","B"
"River0","--","River1"
"River1","--","River2"
"River2","--","River4"
"River4","--","River5"
"River5","--","River6"
"River6","--","Fjord1"
"Fjord1","--","Fjord2"
"Fjord2","--","Sea1"
"River1","--","River3"
"River3","--","River4"
"River2","--","River3"
"River3","--","River2"
'), stringsAsFactors = FALSE)
	expect_equal(output$dot, dot)
	
	dotmat <- read.csv(text = 
c('"","River0","River1","River2","River4","River5","River6","Fjord1","Fjord2","River3","Sea1"
"River0",0,1,2,3,4,5,6,7,2,8
"River1",1,0,1,2,3,4,5,6,1,7
"River2",2,1,0,1,2,3,4,5,1,6
"River4",3,2,1,0,1,2,3,4,1,5
"River5",4,3,2,1,0,1,2,3,2,4
"River6",5,4,3,2,1,0,1,2,3,3
"Fjord1",6,5,4,3,2,1,0,1,4,2
"Fjord2",7,6,5,4,3,2,1,0,5,1
"River3",2,1,1,1,2,3,4,5,0,6
"Sea1",8,7,6,5,4,3,2,1,6,0
'), stringsAsFactors = FALSE, row.names = 1)
	expect_equal(output$dotmat, as.matrix(dotmat))

	pathnames <- c('River0_to_River2', 'River0_to_River3', 'River0_to_River4', 'River0_to_River5', 
		'River0_to_River6', 'River0_to_Fjord1', 'River0_to_Fjord2', 'River0_to_Sea1', 'River1_to_River4', 
		'River1_to_River5', 'River1_to_River6', 'River1_to_Fjord1', 'River1_to_Fjord2', 'River1_to_Sea1', 
		'River2_to_River0', 'River2_to_River5', 'River2_to_River6', 'River2_to_Fjord1', 'River2_to_Fjord2', 
		'River2_to_Sea1', 'River4_to_River1', 'River4_to_River6', 'River4_to_River0', 'River4_to_Fjord1', 
		'River4_to_Fjord2', 'River4_to_Sea1', 'River5_to_River2', 'River5_to_River3', 'River5_to_Fjord1', 
		'River5_to_River1', 'River5_to_Fjord2', 'River5_to_River0', 'River5_to_Sea1', 'River6_to_River4', 
		'River6_to_Fjord2', 'River6_to_River2', 'River6_to_River3', 'River6_to_Sea1', 'River6_to_River1', 
		'River6_to_River0', 'Fjord1_to_River5', 'Fjord1_to_Sea1', 'Fjord1_to_River4', 'Fjord1_to_River2', 
		'Fjord1_to_River3', 'Fjord1_to_River1', 'Fjord1_to_River0', 'Fjord2_to_River6', 'Fjord2_to_River5', 
		'Fjord2_to_River4', 'Fjord2_to_River2', 'Fjord2_to_River3', 'Fjord2_to_River1', 'Fjord2_to_River0', 
		'Sea1_to_Fjord1', 'Sea1_to_River6', 'Sea1_to_River5', 'Sea1_to_River4', 'Sea1_to_River2', 
		'Sea1_to_River3', 'Sea1_to_River1', 'Sea1_to_River0', 'River3_to_River0', 'River3_to_River5', 
		'River3_to_River6', 'River3_to_Fjord1', 'River3_to_Fjord2', 'River3_to_Sea1')
	expect_equal(names(output$paths), pathnames)
	# sample 2 contents
	expect_equal(output$paths[[3]], c("River1 -> River2", "River1 -> River3"))
	expect_equal(output$paths[[6]], c("River1 -> River2 -> River4 -> River5 -> River6", "River1 -> River3 -> River4 -> River5 -> River6"))
	expect_equal(output$paths$Fjord1_to_River1, c("River6 -> River5 -> River4 -> River2", "River6 -> River5 -> River4 -> River3"))	
})


test_that("loadDot output is as expected for multi channel study areas with barriers", {
	sink("spatial.txt")
	cat("River0--River1--River2--River4--River5--River6--Fjord1--Fjord2--Sea1\n")
	cat("River1--River3->River4\n")
	cat("River2--River3--River2\n")
	sink()
	output <- loadDot(input = "spatial.txt", spatial = example.spatial, disregard.parallels = TRUE)	
	file.remove("spatial.txt")

	expect_equal(names(output), c("dot", "arrays", "dotmat", "paths"))
	
	expect_equal(names(output$arrays), c('River0', 'River1', 'River2', 'River4', 
		'River5', 'River6', 'Fjord1', 'Fjord2', 'Sea1', 'River3'))
	
	dot <- read.csv(text = c('"A","to","B"
"River0","--","River1"
"River1","--","River2"
"River2","--","River4"
"River4","--","River5"
"River5","--","River6"
"River6","--","Fjord1"
"Fjord1","--","Fjord2"
"Fjord2","--","Sea1"
"River1","--","River3"
"River3","->","River4"
"River2","--","River3"
"River3","--","River2"
'), stringsAsFactors = FALSE)
	expect_equal(output$dot, dot)
	
	dotmat <- read.csv(text = 
c('"","River0","River1","River2","River4","River5","River6","Fjord1","Fjord2","River3","Sea1"
"River0",0,1,2,3,4,5,6,7,2,8
"River1",1,0,1,2,3,4,5,6,1,7
"River2",2,1,0,1,2,3,4,5,1,6
"River4",3,2,1,0,1,2,3,4,2,5
"River5",4,3,2,1,0,1,2,3,3,4
"River6",5,4,3,2,1,0,1,2,4,3
"Fjord1",6,5,4,3,2,1,0,1,5,2
"Fjord2",7,6,5,4,3,2,1,0,6,1
"River3",2,1,1,1,2,3,4,5,0,6
"Sea1",8,7,6,5,4,3,2,1,7,0
'), stringsAsFactors = FALSE, row.names = 1)
	expect_equal(output$dotmat, as.matrix(dotmat))

	pathnames <- c('River0_to_River2', 'River0_to_River3', 'River0_to_River4', 'River0_to_River5', 
		'River0_to_River6', 'River0_to_Fjord1', 'River0_to_Fjord2', 'River0_to_Sea1', 'River1_to_River4', 
		'River1_to_River5', 'River1_to_River6', 'River1_to_Fjord1', 'River1_to_Fjord2', 'River1_to_Sea1', 
		'River2_to_River0', 'River2_to_River5', 'River2_to_River6', 'River2_to_Fjord1', 'River2_to_Fjord2', 
		'River2_to_Sea1', 'River4_to_River1', 'River4_to_River3', 'River4_to_River6', 'River4_to_River0', 
		'River4_to_Fjord1', 'River4_to_Fjord2', 'River4_to_Sea1', 'River5_to_River2', 'River5_to_Fjord1', 
		'River5_to_River1', 'River5_to_River3', 'River5_to_Fjord2', 'River5_to_River0', 'River5_to_Sea1', 
		'River6_to_River4', 'River6_to_Fjord2', 'River6_to_River2', 'River6_to_Sea1', 'River6_to_River1', 
		'River6_to_River3', 'River6_to_River0', 'Fjord1_to_River5', 'Fjord1_to_Sea1', 'Fjord1_to_River4', 
		'Fjord1_to_River2', 'Fjord1_to_River1', 'Fjord1_to_River3', 'Fjord1_to_River0', 'Fjord2_to_River6', 
		'Fjord2_to_River5', 'Fjord2_to_River4', 'Fjord2_to_River2', 'Fjord2_to_River1', 'Fjord2_to_River3', 
		'Fjord2_to_River0', 'Sea1_to_Fjord1', 'Sea1_to_River6', 'Sea1_to_River5', 'Sea1_to_River4', 
		'Sea1_to_River2', 'Sea1_to_River1', 'Sea1_to_River3', 'Sea1_to_River0', 'River3_to_River0', 
		'River3_to_River5', 'River3_to_River6', 'River3_to_Fjord1', 'River3_to_Fjord2', 'River3_to_Sea1')
	expect_equal(names(output$paths), pathnames)
	# sample 2 contents
	expect_equal(output$paths$River0_to_River4, c("River1 -> River2", "River1 -> River3"))
	expect_equal(output$paths$River0_to_Fjord1, c("River1 -> River2 -> River4 -> River5 -> River6", "River1 -> River3 -> River4 -> River5 -> River6"))
	expect_equal(output$paths$Fjord1_to_River1, "River6 -> River5 -> River4 -> River2")
})

test_that("loadDot correctly identifies edge arrays", {
	sink("spatial.txt")
	cat("River0--River1--River2--River3--River4--River5--River6--Fjord1--Fjord2--Sea1\n")
	sink()
	output <- loadDot(input = "spatial.txt", spatial = example.spatial, 
		sections = c("River", "Fjord", "Sea"), disregard.parallels = TRUE)	
	file.remove("spatial.txt")

	# check edges
	expect_equal(output$arrays$River3$edge, FALSE)
	expect_equal(output$arrays$River6$edge, TRUE)
	expect_equal(output$arrays$Fjord1$edge, FALSE)
	expect_equal(output$arrays$Fjord2$edge, TRUE)
})

test_that("loadDot handles parallel arrays properly", {
	sink("spatial.txt")
	cat("River0--River1--River2--River4--River5--River6--Fjord1--Fjord2--Sea1\n")
	cat("River1--River3--River4\n")
	cat("River2--River3--River2\n")
	sink()
	output <- loadDot(input = "spatial.txt", spatial = example.spatial, 
		sections = c("River", "Fjord", "Sea"), disregard.parallels = TRUE)	
	file.remove("spatial.txt")

	expect_equal(names(output), c("dot", "arrays", "dotmat", "paths"))
	
	expect_equal(names(output$arrays), c('River0', 'River1', 'River2', 'River4', 
		'River5', 'River6', 'Fjord1', 'Fjord2', 'Sea1', 'River3'))

	# check parallels
	expect_equal(output$arrays$River3$parallel, "River2")
	expect_equal(output$arrays$River2$parallel, "River3")
	expect_equal(output$arrays$River1$parallel, NULL)

	# check peers
	expect_equal(output$arrays$River2$after.peers, NULL)
	expect_equal(output$arrays$River3$after.peers, NULL)		
	expect_equal(output$arrays$River1$after.peers, c('River2', 'River3', 'River4', 'River5', 'River6', 'Fjord1', 'Fjord2', 'Sea1'))
})


# Push dot functions to the limit with additional connections to make sure everything is working

test_that("dot mechanisms are handling multipaths and parallels properly", {
	# cannot be run directly through loadDot because it would require a spatial object
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
	arrays <- dotList(input = dot, sections = c("River", "Fjord", "Sea"))
	arrays <- dotPaths(input = arrays, dotmat = mat, disregard.parallels = TRUE)
	# ONLY RUN THIS TO RESET REFERENCE
	# aux_dotPaths_complex_text_disregard_parallels_true <- arrays
	# save(aux_dotPaths_complex_text_disregard_parallels_true, file = paste0(tests.home, "/aux_dotPaths_complex_text_disregard_parallels_true.RData"))
	load(paste0(tests.home, "/aux_dotPaths_complex_text_disregard_parallels_true.RData"))
	expect_equal(arrays, aux_dotPaths_complex_text_disregard_parallels_true)

	arrays <- dotList(input = dot, sections = c("River", "Fjord", "Sea"))
	arrays <- dotPaths(input = arrays, dotmat = mat, disregard.parallels = FALSE)
	# ONLY RUN THIS TO RESET REFERENCE
	# aux_dotPaths_complex_text_disregard_parallels_false <- arrays
	# save(aux_dotPaths_complex_text_disregard_parallels_false, file = paste0(tests.home, "/aux_dotPaths_complex_text_disregard_parallels_false.RData"))
	load(paste0(tests.home, "/aux_dotPaths_complex_text_disregard_parallels_false.RData"))
	expect_equal(arrays, aux_dotPaths_complex_text_disregard_parallels_false)
})

setwd(tests.home)

