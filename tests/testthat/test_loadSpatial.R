skip_on_cran()

tests.home <- getwd()
setwd(tempdir())

test_that("loadSpatial stops if file is missing", {
	expect_error(loadSpatial(input = "test"),
		"Could not find a 'test' file in the working directory.", fixed = TRUE)
})

test_that("loadSpatial stops if columns are missing or duplicated", {
	spatial <- example.spatial
	colnames(spatial)[1:2] <- "test"
	write.csv(spatial, "spatial.csv", row.names = FALSE)
	expect_error(loadSpatial(),
		"The following columns are duplicated in the spatial input: 'test'.", fixed = TRUE)

	spatial <- example.spatial
	colnames(spatial)[1] <- "test"
	write.csv(spatial, "spatial.csv", row.names = FALSE)
	expect_error(loadSpatial(),
		"The spatial input must contain a 'Station.name' column.", fixed = TRUE)

	spatial <- example.spatial
	colnames(spatial)[1] <- "Station.Name"
	write.csv(spatial, "spatial.csv", row.names = FALSE)
	expect_equal(colnames(loadSpatial())[1],"Station.name", fixed = TRUE)

	spatial <- example.spatial
	colnames(spatial)[6] <- "test"
	write.csv(spatial, "spatial.csv", row.names = FALSE)
	expect_error(loadSpatial(),
		"The spatial input must contain an 'Array' column.", fixed = TRUE)
})

test_that("loadSpatial responds correctly if data is missing or badly formatted", {
	spatial <- example.spatial
	spatial$Station.name[1:2] <- "test"
	write.csv(spatial, "spatial.csv", row.names = FALSE)
	expect_error(loadSpatial(),
		"The 'Station.name' column in the spatial input must not have duplicated values.\nStations appearing more than once: test", fixed = TRUE)

	spatial <- example.spatial
	spatial$Array[1] <- NA
	write.csv(spatial, "spatial.csv", row.names = FALSE)
	expect_error(loadSpatial(),
		"Some rows do not contain 'Array' information in the spatial input. Please double-check the input files.", fixed = TRUE)

	spatial <- example.spatial
	spatial$Array[1] <- "River 0 a"
	write.csv(spatial, "spatial.csv", row.names = FALSE)
	expect_warning(
		expect_message(loadSpatial(),
			"M: Replacing spaces with '_' in array names to prevent function failure.", fixed = TRUE),
		"Long array names detected. To improve graphic rendering, consider keeping array names under six characters.", fixed = TRUE)

	write.csv(spatial, "spatial.csv", row.names = FALSE)
	expect_warning(
		expect_message(output <- loadSpatial(),
			"M: Replacing spaces with '_' in array names to prevent function failure.", fixed = TRUE),
		"Long array names detected. To improve graphic rendering, consider keeping array names under six characters.", fixed = TRUE)
	expect_equal(output$Array[1], "River_0_a")

	write.csv(example.spatial[, -match("Type", colnames(example.spatial))], "spatial.csv", row.names = FALSE)
	expect_error(
		expect_message(loadSpatial(),
			"M: No 'Type' column found in the spatial input. Assigning all rows as hydrophones.", fixed = TRUE),
		"Some rows do not contain 'Section' information in the spatial input. Please double-check the input files.", fixed = TRUE)

	write.csv(example.spatial[, -match("Section", colnames(example.spatial))], "spatial.csv", row.names = FALSE)
	expect_warning(loadSpatial(),
		"The spatial input does not contain a 'Section' column. This input is only valid for explore() analyses.", fixed = TRUE)

	spatial <- example.spatial
	spatial$Type[1] <- "test"
	write.csv(spatial, "spatial.csv", row.names = FALSE)
	expect_error(loadSpatial(),
		"Could not recognise the data in the 'Type' column as only one of 'Hydrophone' or 'Release'. Please double-check the spatial input.", fixed = TRUE)
	file.remove("spatial.csv")

	spatial <- example.spatial
	spatial$Array[18] <- "test"
	write.csv(spatial, "spatial.csv", row.names = FALSE)
	expect_error(loadSpatial(),
		"Not all the expected first arrays of the release sites exist.
Unknown expected first arrays: 'test'.
In the spatial input, the expected first arrays of the release sites should match the arrays where hydrophone stations where deployed.", fixed = TRUE)

	spatial <- example.spatial
	spatial$Array[18] <- "Release"
	write.csv(spatial, "spatial.csv", row.names = FALSE)
	expect_error(loadSpatial(),
		"The term 'Release' is reserved for internal calculations. Do not name any sections or arrays as 'Release'.", fixed = TRUE)

	spatial <- example.spatial
	spatial$Array[18] <- "Invalid"
	write.csv(spatial, "spatial.csv", row.names = FALSE)
	expect_error(loadSpatial(),
		"The term 'Invalid' is reserved for internal calculations. Do not name any sections or arrays as 'Invalid'.", fixed = TRUE)

	spatial <- example.spatial
	spatial$Array[18] <- "Total"
	write.csv(spatial, "spatial.csv", row.names = FALSE)
	expect_error(loadSpatial(),
		"The term 'Total' is reserved for internal calculations. Do not name any sections or arrays as 'Total'.", fixed = TRUE)

	spatial <- example.spatial
	spatial$Section <- as.character(spatial$Section)
	spatial$Section[17] <- "Release"
	write.csv(spatial, "spatial.csv", row.names = FALSE)
	expect_error(loadSpatial(),
		"The term 'Release' is reserved for internal calculations. Do not name any sections or arrays as 'Release'.", fixed = TRUE)

	spatial <- example.spatial
	spatial$Section <- as.character(spatial$Section)
	spatial$Section[17] <- "Total"
	write.csv(spatial, "spatial.csv", row.names = FALSE)
	expect_error(loadSpatial(),
		"The term 'Total' is reserved for internal calculations. Do not name any sections or arrays as 'Total'.", fixed = TRUE)

	spatial <- example.spatial
	spatial$Section <- as.character(spatial$Section)
	spatial$Section[3] <- "Ri ver"
	write.csv(spatial, "spatial.csv", row.names = FALSE)
	expect_message(loadSpatial(),
		"Replacing spaces with '_' in section names to prevent function failure.", fixed = TRUE)

	spatial <- example.spatial
	spatial$Section <- as.character(spatial$Section)
	spatial$Section[3] <- "Ri"
	write.csv(spatial, "spatial.csv", row.names = FALSE)
	expect_error(loadSpatial(),
		"Section 'Ri' is contained within other section names. Sections must be unique and independent.
       Please rename your sections so that section names are not contained within each other.", fixed = TRUE)

	write.csv(example.spatial, "spatial.csv", row.names = FALSE)
	expect_error(loadSpatial(section.order = c("Fjord" ,"Sea")),
		"Not all sections are listed in 'section.order'. Sections missing: River", fixed = TRUE)

	expect_warning(loadSpatial(section.order = c("Fjord" ,"Sea", "River", "Ri")),
		"Not all values listed in 'section.order' correspond to sections. Discarding the following values: Ri", fixed = TRUE)

	spatial <- example.spatial
	spatial$Section <- as.character(spatial$Section)
	spatial$Section[3] <- "Rivlongname"
	write.csv(spatial, "spatial.csv", row.names = FALSE)
	expect_warning(loadSpatial(),
		"Long section names detected. To improve graphic rendering, consider keeping section names under six characters.", fixed = TRUE)

	write.csv(example.spatial[, -match("Section", colnames(example.spatial))], "spatial.csv", row.names = FALSE)
	expect_warning(loadSpatial(section.order = c("Fjord" ,"Sea")),
		"'section.order' was set but input has no 'Section' column. Ignoring argument.", fixed = TRUE)
})

test_that("loadSpatial converts factors to character", {
	xspatial <- example.spatial
	xspatial$Station.name <- as.factor(xspatial$Station.name)
	xspatial$Type <- as.factor(xspatial$Type)
	output <- loadSpatial(input = xspatial)
	expect_equal(typeof(output$Station.name), "character")	
	expect_equal(typeof(output$Type), "character")	
	# note: The Section column is converted into a factor within loadSpatial
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

test_that("loadSpatial stops if arrays were assigned to multiple sections", {
	xspatial <- example.spatial
	xspatial$Array[17] <- "A1"
	expect_error(loadSpatial(xspatial),
		"Array 'A1' has been assigned to more than one section! Each array can only belong to one section. Please correct the spatial input before continuing.", fixed = TRUE)
})
setwd(tests.home)
rm(list = ls())
