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

spatial <- transformSpatial(spatial = spatial, bio = bio, arrays = dot$arrays)

test_that("loadDistances returns correct output if distances.csv is not present", {
	expect_true(is.na(loadDistances(spatial = spatial)))
})

test_that("loadDistances output imports data correctly", {
	write.csv(example.distances[, 1], "distances.csv")
	expect_warning(loadDistances(spatial = spatial),
		"Only one column was identified in 'distances.csv'. If this seems wrong, please make sure that the values are separated using commas.", fixed = TRUE)
	expect_warning(loadDistances(spatial = spatial),
		"The distances matrix appears to be missing data (ncol != nrow). Deactivating speed calculations to avoid function failure.", fixed = TRUE)
	expect_false(suppressWarnings(attributes(loadDistances(spatial = spatial))$valid))

	xdist <- example.distances
	rownames(xdist)[1] <- "test"
	write.csv(xdist, "distances.csv")
	expect_warning(loadDistances(spatial = spatial),
		"The column and row names in the distances matrix do not match each other. Deactivating speed calculations to avoid function failure.", fixed = TRUE)
	expect_false(suppressWarnings(attributes(loadDistances(spatial = spatial))$valid))
	
	write.csv(example.distances[-1, -1], "distances.csv")
	expect_warning(loadDistances(spatial = spatial),
		"The number of spatial points does not match the number of rows in the distances matrix. Deactivating speed calculations to avoid function failure.", fixed = TRUE)
	expect_false(suppressWarnings(attributes(loadDistances(spatial = spatial))$valid))

	xdist <- example.distances
	rownames(xdist)[17:18] <- c("A", "B")
	colnames(xdist)[17:18] <- c("A", "B")
	write.csv(xdist, "distances.csv")
	expect_warning(loadDistances(spatial = spatial),
		"Some stations and/or release sites are not present in the distances matrix. Deactivating speed calculations to avoid function failure.", fixed = TRUE)
	expect_false(suppressWarnings(attributes(loadDistances(spatial = spatial))$valid))
	file.remove("distances.csv")

	spatial$release.sites$Station.name[1] <- "unspecified"
	spatial$release.sites$Standard.name[1] <- "unspecified"

	write.csv(example.distances[-18, -18], "distances.csv")

	output <- loadDistances(spatial = spatial)
	expect_equal(nrow(output), 18)
	expect_equal(colnames(output)[18], "unspecified")
	expect_equal(rownames(output)[18], "unspecified")
	expect_true(all(is.na(output[18, ])))
	expect_true(all(is.na(output[, 18])))
})

setwd(tests.home)
rm(list = ls())
