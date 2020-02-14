write.csv(example.spatial, "spatial.csv", row.names = FALSE)
spatial <- loadSpatial()
file.remove("spatial.csv")

write.csv(example.biometrics, "biometrics.csv", row.names = FALSE)
bio <- loadBio(file = "biometrics.csv", tz = "Europe/Copenhagen")
file.remove("biometrics.csv")

dot <- loadDot(string = paste(unique(spatial$Array), collapse = "--"), spatial = spatial, disregard.parallels = TRUE)

spatial <- transformSpatial(spatial = spatial, bio = bio, arrays = dot$arrays, sections = NULL)[[1]]

test_that("loadDistances returns correct output if distances.csv is not present", {
	expect_equal(loadDistances(spatial = spatial), list(dist.mat = NA, invalid.dist = TRUE))
})

test_that("loadDistances output imports data correctly", {
	write.csv(example.distances[,1], "distances.csv")
	expect_warning(loadDistances(spatial = spatial),
		"Only one column was identified in 'distances.csv'. If this seems wrong, please make sure that the values are separated using commas.", fixed = TRUE)
	expect_warning(loadDistances(spatial = spatial),
		"The distance matrix appears to be missing data (ncol != nrow). Deactivating speed calculation to avoid function failure.", fixed = TRUE)
	expect_true(suppressWarnings(loadDistances(spatial = spatial)$invalid.dist))

	xdist <- example.distances
	rownames(xdist)[1] <- "test"
	write.csv(xdist, "distances.csv")
	expect_warning(loadDistances(spatial = spatial),
		"The column and row names in the distance matrix do not match each other. Deactivating speed calculation to avoid function failure.", fixed = TRUE)
	expect_true(suppressWarnings(loadDistances(spatial = spatial)$invalid.dist))
	
	write.csv(example.distances[-1, -1], "distances.csv")
	expect_warning(loadDistances(spatial = spatial),
		"The number of spatial points does not match the number of rows in the distance matrix. Deactivating speed calculation to avoid function failure.", fixed = TRUE)
	expect_true(suppressWarnings(loadDistances(spatial = spatial)$invalid.dist))

	xdist <- example.distances
	rownames(xdist)[17:18] <- c("A", "B")
	colnames(xdist)[17:18] <- c("A", "B")
	write.csv(xdist, "distances.csv")
	expect_warning(loadDistances(spatial = spatial),
		"Some stations and/or release sites are not present in the distance matrix. Deactivating speed calculation to avoid function failure.", fixed = TRUE)
	expect_true(suppressWarnings(loadDistances(spatial = spatial)$invalid.dist))
	file.remove("distances.csv")
})

file.remove(list.files(pattern = "*txt$"))
rm(list = ls())
