skip_on_cran()

tests.home <- getwd()
setwd(tempdir())

write.csv(example.spatial, "spatial.csv", row.names = FALSE)
spatial <- loadSpatial()
file.remove("spatial.csv")

write.csv(example.biometrics, "biometrics.csv", row.names = FALSE)
bio <- loadBio(input = "biometrics.csv", tz = "Europe/Copenhagen")
file.remove("biometrics.csv")

dot <- loadDot(string = paste(unique(spatial$Array), collapse = "--"), spatial = spatial, disregard.parallels = TRUE)

test_that("transformSpatial handles release site mismatches properly and stops when needed", {
	xbio <- bio
	xbio$Release.site <- "unspecified"

	expect_warning(
		expect_error(transformSpatial(spatial = spatial, bio = xbio, arrays = dot$arrays),
			"There is more than one top level array in the study area. Please specify release site(s) in the 'spatial.csv' file and in the 'biometrics.csv' file.", fixed = TRUE),
	"At least one release site has been indicated in the spatial.csv file, but no release sites were specified in the biometrics file.\n         Discarding release site information and assuming all animals were released at the top level array to avoid function failure.\n         Please double-check your data.", fixed = TRUE)

	xbio <- bio
	levels(xbio$Release.site) <- c("RS1", "test")
	xbio$Release.site[2] <- "test"
	
	expect_error(transformSpatial(spatial = spatial, bio = xbio, arrays = dot$arrays, first.array = NULL),
		"The following release sites were listed in the biometrics.csv file but are not part of the release sites listed in the spatial.csv file: test\nPlease include the missing release sites in the spatial.csv file.", fixed = TRUE)

	xspatial <- spatial[-18, ]
	expect_warning(
		expect_error(transformSpatial(spatial = xspatial, bio = bio, arrays = dot$arrays, first.array = NULL),
			"There is more than one top level array in the study area. Please specify release site(s) in the spatial.csv file and in the biometrics.csv file.", fixed = TRUE),
		"Release sites were not specified in the spatial.csv file. Attempting to assume all released animals start at the top level array.", fixed = TRUE)
})

test_that("transformSpatial handles release site mismatches properly and delivers correct output when possible", {
	xbio <- bio
	xbio$Release.site <- "unspecified"

	expect_warning(output <- transformSpatial(spatial = spatial, bio = xbio, arrays = dot$arrays, first.array = "A1"),
		"At least one release site has been indicated in the spatial.csv file, but no release sites were specified in the biometrics file.\n         Discarding release site information and assuming all animals were released at the top level array to avoid function failure.\n         Please double-check your data.", fixed = TRUE)

	expect_equal(as.character(output$release.sites$Station.name), "unspecified")

	expect_equal(as.character(output$release.sites$Array), "A1")

	xspatial <- spatial[-18, -match("Type", colnames(spatial))]
	xspatial$Array <- factor(xspatial$Array, levels = levels(output$stations$Array))
	expect_equal(output$stations, xspatial)

	
	xspatial <- spatial[-18, ]
	expect_warning(output <- transformSpatial(spatial = xspatial, bio = bio, arrays = dot$arrays, first.array = "A1"),
	"Release sites were not specified in the spatial.csv file. Attempting to assume all released animals start at the top level array.", fixed = TRUE)
	expect_equal(as.character(output$release.sites$Station.name), "RS1")
	expect_equal(as.character(output$release.sites$Array), "A1")
	
	# check counting of animals released per site
	expect_equal(output$release.sites$n.A, 30)
	expect_equal(output$release.sites$n.B, 30)
})

test_that("transformSpatial handles sections properly", {
	xspatial <- spatial
	xspatial$Section <- as.character(xspatial$Section)
	xspatial$Section[1:(nrow(xspatial) - 1)] <- "all"
	xspatial$Section <- as.factor(xspatial$Section)
	output <- transformSpatial(spatial = xspatial, bio = bio, arrays = dot$arrays)
	expect_equal(names(output$array.order), "all")
	expect_equal(output$array.order$all, c('A0', 'A1', 'A2', 'A3', 'A4', 'A5', 'A6', 'A7', 'A8', 'A9'))
	expect_equal(output$sections, NULL)

	output <- transformSpatial(spatial = spatial, bio = bio, arrays = dot$arrays)
	expect_equal(names(output$array.order), c("River", "Fjord", "Sea"))
	expect_equal(output$array.order$River, c('A0', 'A1', 'A2', 'A3', 'A4', 'A5', 'A6'))
	expect_equal(output$array.order$Fjord, c('A7', 'A8'))
	expect_equal(output$array.order$Sea, c('A9'))
})
# y

test_that("transformSpatial handles multiple expected first arrays correctly", {
	xspatial <- spatial
	xspatial$Array[18] <- "A1|A2"
	expect_message(output <- transformSpatial(spatial = xspatial, bio = bio, dotmat = dot$dotmat, arrays = dot$arrays),
		"M: Multiple possible first arrays detected for release site 'RS1'.", fixed = TRUE)

	xspatial$Array[18] <- "A1|A9"
	expect_warning(output <- transformSpatial(spatial = xspatial, bio = bio, dotmat = dot$dotmat, arrays = dot$arrays),
		"Release site RS1 has multiple possible first arrays (A1, A9), but not all of these arrays appear to be directly connected with each other. Could there be a mistake in the input?", fixed = TRUE)

	xspatial$Array[18] <- "A1|A2"
	xspatial[19:25, ] <- xspatial[18, ]
	
	xspatial$Array[25] <- "A1|A9"

	xspatial$Standard.name[19:25] <- paste0("RS", 2:8)
	xspatial$Station.name[19:25] <- paste0("RS", 2:8)

	expect_message(
		expect_warning(output <- transformSpatial(spatial = xspatial, bio = bio, dotmat = dot$dotmat, arrays = dot$arrays),
			"Release site RS8 has multiple possible first arrays (A1, A9), but not all of these arrays appear to be directly connected with each other. Could there be a mistake in the input?", fixed = TRUE),
		"Multiple possible first arrays detected for more than five release sites.", fixed = TRUE)
})

setwd(tests.home)
rm(list = ls())
