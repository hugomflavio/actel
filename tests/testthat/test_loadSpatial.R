skip_on_cran()

tests.home <- getwd()
setwd(tempdir())

test_that("loadSpatial stops if file is missing", {
	expect_error(loadSpatial(file = "test"), 
		"Could not find a 'test' file in the working directory.", fixed = TRUE)
})

test_that("loadSpatial stops if columns are missing or duplicated", {
	spatial <- example.spatial
	colnames(spatial)[1:2] <- "test"
	write.csv(spatial, "spatial.csv", row.names = FALSE)
	expect_error(loadSpatial(),
		"The following columns are duplicated in the file 'spatial.csv': 'test'.", fixed = TRUE)

	spatial <- example.spatial
	colnames(spatial)[1] <- "test"
	write.csv(spatial, "spatial.csv", row.names = FALSE)
	expect_error(loadSpatial(),
		"The spatial.csv file must contain a 'Station.name' column.", fixed = TRUE)

	spatial <- example.spatial
	colnames(spatial)[1] <- "Station.Name"
	write.csv(spatial, "spatial.csv", row.names = FALSE)
	expect_equal(colnames(loadSpatial())[1],"Station.name", fixed = TRUE)

	spatial <- example.spatial
	colnames(spatial)[4] <- "test"
	write.csv(spatial, "spatial.csv", row.names = FALSE)
	expect_error(loadSpatial(),
		"The spatial.csv file must contain an 'Array' column.", fixed = TRUE)
	})

test_that("loadSpatial responds correctly if data is missing or badly formatted", {
	spatial <- example.spatial
	spatial$Station.name[1:2] <- "test"
	write.csv(spatial, "spatial.csv", row.names = FALSE)
	expect_error(loadSpatial(),
		"The 'Station.name' column in the spatial.csv file must not have duplicated values.\nStations appearing more than once: test", fixed = TRUE)

	spatial <- example.spatial
	spatial$Array[1] <- NA
	write.csv(spatial, "spatial.csv", row.names = FALSE)
	expect_error(loadSpatial(),
		"Some rows do not contain 'Array' information in the spatial.csv file. Please double-check the input files.", fixed = TRUE)

	spatial <- example.spatial
	spatial$Array[1] <- "River 0 a"
	write.csv(spatial, "spatial.csv", row.names = FALSE)
	expect_message(loadSpatial(),
		"M: Replacing spaces in array names to prevent function failure.", fixed = TRUE)
	
	write.csv(spatial, "spatial.csv", row.names = FALSE)
	expect_message(output <- loadSpatial(),
		"M: Replacing spaces in array names to prevent function failure.", fixed = TRUE)
	expect_equal(output$Array[1], "River_0_a")

	write.csv(example.spatial[,-5], "spatial.csv", row.names = FALSE)
	expect_message(loadSpatial(),
		"M: No 'Type' column found in the spatial.csv file. Assigning all rows as hydrophones.", fixed = TRUE)
	write.csv(example.spatial[,-5], "spatial.csv", row.names = FALSE)
	expect_message(loadSpatial(),
		"M: No 'Type' column found in the spatial.csv file. Assigning all rows as hydrophones.", fixed = TRUE)

	spatial <- example.spatial
	spatial$Type[1] <- "test"
	write.csv(spatial, "spatial.csv", row.names = FALSE)
	expect_error(loadSpatial(),
		"Could not recognise the data in the 'Type' column as only one of 'Hydrophone' or 'Release'. Please double-check the spatial.csv file.", fixed = TRUE)
	file.remove("spatial.csv")

	spatial <- example.spatial
	spatial$Array[18] <- "test"
	write.csv(spatial, "spatial.csv", row.names = FALSE)
	expect_error(loadSpatial(),
		"Not all the expected first arrays of the release sites exist.
Unknown expected first arrays: 'test'.
In the spatial.csv file, the expected first arrays of the release sites should match the arrays where hydrophone stations where deployed.", fixed = TRUE)
})

test_that("loadSpatial output is exactly as expected", {
	write.csv(example.spatial, "spatial.csv", row.names = FALSE)
	output <- loadSpatial()
	spatial <- example.spatial
	spatial$Standard.name <- spatial$Station.name
	spatial$Standard.name[1:17] <- paste0("St.", 1:17)
	expect_equal(output, spatial)
	file.remove("spatial.csv")
})

setwd(tests.home)
