skip_on_cran()

tests.home <- getwd()
setwd(tempdir())

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

	expect_warning(
		expect_error(transformSpatial(spatial = spatial, bio = xbio, arrays = dot$arrays),
			"There is more than one top level array in the study area. Please specify release site(s) in the 'spatial.csv' file and in the 'biometrics.csv' file.", fixed = TRUE),
	"At least one release site has been indicated in the spatial.csv file, but no release sites were specified in the biometrics file.\n         Discarding release site information and assuming all fish were released at the top level array to avoid function failure.\n         Please double-check your data.", fixed = TRUE)

	xbio <- bio
	levels(xbio$Release.site) <- c("RS1", "test")
	xbio$Release.site[2] <- "test"
	
	expect_error(transformSpatial(spatial = spatial, bio = xbio, arrays = dot$arrays, first.array = NULL),
		"The following release sites were listed in the biometrics.csv file but are not part of the release sites listed in the spatial.csv file: test\nPlease include the missing release sites in the spatial.csv file.", fixed = TRUE)

	xspatial <- spatial[-18, ]
	expect_warning(
		expect_error(transformSpatial(spatial = xspatial, bio = bio, arrays = dot$arrays, first.array = NULL),
			"There is more than one top level array in the study area. Please specify release site(s) in the spatial.csv file and in the biometrics.csv file.", fixed = TRUE),
		"Release sites were not specified in the spatial.csv file. Attempting to assume all released fish start at the top level array.", fixed = TRUE)
})

test_that("transformSpatial handles release site mismatches properly and delivers correct output when possible", {
	xbio <- bio
	xbio$Release.site <- "unspecified"

	expect_warning(output <- transformSpatial(spatial = spatial, bio = xbio, arrays = dot$arrays, first.array = "River1"),
		"At least one release site has been indicated in the spatial.csv file, but no release sites were specified in the biometrics file.\n         Discarding release site information and assuming all fish were released at the top level array to avoid function failure.\n         Please double-check your data.", fixed = TRUE)

	expect_equal(as.character(output$spatial$release.sites$Station.name), "unspecified")

	expect_equal(as.character(output$spatial$release.sites$Array), "River1")

	xspatial <- spatial[-18, -7]
	xspatial$Array <- factor(xspatial$Array, levels = levels(output$spatial$stations$Array))
	expect_equal(output$spatial$stations, xspatial)

	
	xspatial <- spatial[-18, ]
	expect_warning(output <- transformSpatial(spatial = xspatial, bio = bio, arrays = dot$arrays, first.array = "River1"),
	"Release sites were not specified in the spatial.csv file. Attempting to assume all released fish start at the top level array.", fixed = TRUE)
	expect_equal(as.character(output$spatial$release.sites$Station.name), "RS1")
	expect_equal(as.character(output$spatial$release.sites$Array), "River1")
	
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
# y

test_that("transformSpatial handles multiple expected first arrays correctly", {
	xspatial <- spatial
	xspatial$Array[18] <- "River1|River2"
	expect_message(output <- transformSpatial(spatial = xspatial, bio = bio, dotmat = dot$dotmat, arrays = dot$arrays, sections = NULL),
		"M: Multiple possible first arrays detected for release site 'RS1'.", fixed = TRUE)

	xspatial$Array[18] <- "River1|Sea1"
	expect_warning(output <- transformSpatial(spatial = xspatial, bio = bio, dotmat = dot$dotmat, arrays = dot$arrays, sections = NULL),
		"Release site RS1 has multiple possible first arrays (River1, Sea1), but not all of these arrays appear to be directly connected with each other. Could there be a mistake in the input?", fixed = TRUE)

	xspatial$Array[18] <- "River1|River2"
	xspatial[19:25, ] <- xspatial[18, ]
	
	xspatial$Array[25] <- "River1|Sea1"

	xspatial$Standard.name[19:25] <- paste0("RS", 2:8)
	xspatial$Station.name[19:25] <- paste0("RS", 2:8)

	expect_message(
		expect_warning(output <- transformSpatial(spatial = xspatial, bio = bio, dotmat = dot$dotmat, arrays = dot$arrays, sections = NULL),
			"Release site RS8 has multiple possible first arrays (River1, Sea1), but not all of these arrays appear to be directly connected with each other. Could there be a mistake in the input?", fixed = TRUE),
		"Multiple possible first arrays detected for more than five release sites.", fixed = TRUE)
})

setwd(tests.home)
rm(list = ls())
