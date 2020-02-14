
write.csv(example.spatial, "spatial.csv", row.names = FALSE)
spatial <- loadSpatial()
file.remove("spatial.csv")

write.csv(example.biometrics, "biometrics.csv", row.names = FALSE)
bio <- loadBio(file = "biometrics.csv", tz = "Europe/Copenhagen")
file.remove("biometrics.csv")

dot <- loadDot(string = paste(unique(spatial$Array), collapse = "--"), spatial = spatial, disregard.parallels = TRUE)

test_that("transformSpatial handles release site mismatches properly and stops when needed", {
	xbio <- bio
	xbio$Release.site <- "unspecified"

	expect_error(transformSpatial(spatial = spatial, bio = xbio, arrays = dot$arrays),
		"There is more than one top level array in the study area. Please specify release site(s) in the 'spatial.csv' file and in the 'biometrics.csv' file.", fixed = TRUE)

	xbio <- bio
	levels(xbio$Release.site) <- c("RS1", "test")
	xbio$Release.site[2] <- "test"
	
	expect_error(transformSpatial(spatial = spatial, bio = xbio, arrays = dot$arrays, first.array = NULL),
		"There is a mismatch between the release sites reported in the spatial.csv file and the release locations for the fish in the biometrics.csv file.\n       Release sites listed in the spatial.csv file: RS1\n       Sites listed in the biometrics.csv file 'Release.site' column: RS1, test", fixed = TRUE)

	xspatial <- spatial
	xspatial$Array[18] <- "test"
	expect_error(transformSpatial(spatial = xspatial, bio = bio, arrays = dot$arrays, first.array = NULL),
		"There is a mismatch between the expected first array of a release site and the list of arrays.\n       Arrays listed in the spatial.csv file: River0, River1, River2, River3, River4, River5, River6, Fjord1, Fjord2, Sea1\n       Expected first arrays of the release sites: test\nThe expected first arrays should match the arrays where stations where deployed in the spatial.csv file.", fixed = TRUE)

	xspatial <- spatial[-18, ]
	expect_error(transformSpatial(spatial = xspatial, bio = bio, arrays = dot$arrays, first.array = NULL),
		"There is more than one top level array in the study area. Please specify release site(s) in the spatial.csv file and in the biometrics.csv file.", fixed = TRUE)
})

test_that("transformSpatial handles release site mismatches properly and delivers correct output when possible", {
	xbio <- bio
	xbio$Release.site <- "unspecified"

	expect_warning(output <- transformSpatial(spatial = spatial, bio = xbio, arrays = dot$arrays, first.array = "River1"),
		"At least one release site has been indicated in the spatial.csv file, but no release sites were specified in the biometrics file.\n   Discarding release site information and assuming all fish were released at the top level array to avoid function failure.\n   Please double-check your data.", fixed = TRUE)

	expect_equal(output$spatial$release.sites$Station.name, factor("unspecified"))

	expect_equal(output$spatial$release.sites$Array, factor("River1"))

	xspatial <- spatial[-18, -5]
	xspatial$Array <- factor(xspatial$Array, levels = levels(output$spatial$stations$Array))
	expect_equal(output$spatial$stations, xspatial)

	
	xspatial <- spatial[-18, ]
	expect_warning(output <- transformSpatial(spatial = xspatial, bio = bio, arrays = dot$arrays, first.array = "River1"),
	"Release sites were not specified in the spatial.csv file. Attempting to assume all released fish start at the top level array.", fixed = TRUE)
	expect_equal(output$spatial$release.sites$Station.name, factor("RS1"))
	expect_equal(output$spatial$release.sites$Array, factor("River1"))
	
	# check counting of fish released per site
	expect_equal(output$spatial$release.sites$n.A, 30)
	expect_equal(output$spatial$release.sites$n.B, 30)
})

test_that("transformSpatial handles sections properly", {
	output <- transformSpatial(spatial = spatial, bio = bio, arrays = dot$arrays, sections = NULL)
	expect_equal(names(output$spatial$array.order), "all")
	expect_equal(output$spatial$array.order$all, c('River0', 'River1', 'River2', 'River3', 'River4', 'River5', 'River6', 'Fjord1', 'Fjord2', 'Sea1'))
	expect_equal(output$sections, NULL)

	output <- transformSpatial(spatial = spatial, bio = bio, arrays = dot$arrays, sections = c("River", "Fjord", "Sea"))
	expect_equal(names(output$spatial$array.order), c("River", "Fjord", "Sea"))
	expect_equal(output$spatial$array.order$River, c('River0', 'River1', 'River2', 'River3', 'River4', 'River5', 'River6'))
	expect_equal(output$spatial$array.order$Fjord, c('Fjord1', 'Fjord2'))
	expect_equal(output$spatial$array.order$Sea, c('Sea1'))
	expect_equal(output$sections, c("River", "Fjord", "Sea"))

	expect_warning(transformSpatial(spatial = spatial, bio = bio, arrays = dot$arrays, sections = c("River", "Fjord", "Sea", "test")),
		"No arrays were found that match section(s) test. There could be a typing mistake! Section(s) test will be removed.", fixed = TRUE)

	expect_error(suppressWarnings(transformSpatial(spatial = spatial, bio = bio, arrays = dot$arrays, sections = c("River", "Fjord"))),
		"Array 'Sea1' was not assigned to any section. Stopping to prevent function failure.\nPlease either...\n   1) Rename these arrays to match a section,\n   2) Rename a section to match these arrays, or\n   3) Include a new section in the analysis.\n... and restart the analysis.", fixed = TRUE)
})

file.remove(list.files(pattern = "*txt$"))
