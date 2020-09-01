skip_on_cran()

aux <- c(
  length(suppressWarnings(packageDescription("raster"))),
  length(suppressWarnings(packageDescription("gdistance"))),
  length(suppressWarnings(packageDescription("sp"))),
  length(suppressWarnings(packageDescription("tools"))),
  length(suppressWarnings(packageDescription("rgdal"))))
missing.packages <- sapply(aux, function(x) x == 1)

if (any(missing.packages)) {
  test_that("loadShape, transitionLayer and distancesMatrix stop due to missing dependencies", {
  	expect_error(loadShape(),
  		paste0("This function requires packages '", paste(c("raster", "gdistance", "sp", "tools", "rgdal")[missing.packages], collapse = "', '"),
      	"' to operate. Please install ", ifelse(sum(missing.packages) > 1, "them", "it"), " before proceeding.\n"), fixed = TRUE)
  	expect_error(transitionLayer(),
  		paste0("This function requires packages '", paste(c("raster", "gdistance", "sp", "tools", "rgdal")[missing.packages], collapse = "', '"),
      	"' to operate. Please install ", ifelse(sum(missing.packages) > 1, "them", "it"), " before proceeding.\n"), fixed = TRUE)
  	expect_error(distancesMatrix(),
  		paste0("This function requires packages '", paste(c("raster", "gdistance", "sp", "tools", "rgdal")[missing.packages], collapse = "', '"),
      	"' to operate. Please install ", ifelse(sum(missing.packages) > 1, "them", "it"), " before proceeding.\n"), fixed = TRUE)
  })
} else {
	if (suppressWarnings(require("rgdal")) & suppressWarnings(require("gdistance"))) {
		tests.home <- getwd()
		setwd(tempdir())

		test_that("loadShape complains if coord.y or coord.x are given and spatial.csv is not present", {
			expect_warning(loadShape(path = tests.home, shape = "aux_transitionLayer.shp",
					size = 10, coord.x = "x.32632", coord.y = "y.32632"),
			"Could not find a spatial.csv file in the current working directory. Skipping range check.", fixed = TRUE)

			expect_warning(loadShape(path = tests.home, shape = "aux_transitionLayer.shp",
					size = 10, coord.x = "x.32632"),
			"'coord.x' was set but 'coord.y' was not. Skipping range check.", fixed = TRUE)

			expect_warning(loadShape(path = tests.home, shape = "aux_transitionLayer.shp",
					size = 10, coord.y = "x.32632"),
			"'coord.y' was set but 'coord.x' was not. Skipping range check.", fixed = TRUE)
		})

		xspatial <- example.spatial[8:11, ]
		xspatial$x.32632 <- c(453568, 453400, 453047, 452975)
		xspatial$y.32632 <- c(6242912, 6242630, 6242387, 6242169)
		write.csv(xspatial, "spatial.csv", row.names = FALSE)

		test_that("loadShape complains if coord.y or coord.x are not valid column names", {
			expect_warning(loadShape(path = tests.home, shape = "aux_transitionLayer.shp", size = 10,
					coord.x = "test", coord.y = "y.32632", type = "water"),
			"Could not find column 'test' in the spatial data frame. Skipping range check.", fixed = TRUE)	

			expect_warning(loadShape(path = tests.home, shape = "aux_transitionLayer.shp", size = 10,
					coord.x = "x.32632", coord.y = "test"),
			"Could not find column 'test' in the spatial data frame. Skipping range check.", fixed = TRUE)

			expect_warning(loadShape(path = tests.home, shape = "aux_transitionLayer.shp", size = 10,
					coord.x = "test2", coord.y = "test"),
			"Could not find columns 'test2' and 'test' in the spatial data frame. Skipping range check.", fixed = TRUE)
		})

		test_that("loadShape only allows valid buffers", {
			expect_error(loadShape(path = tests.home, shape = "aux_transitionLayer.shp", size = 10, buffer = "a"),
			"'buffer' must be numeric (in metres or degrees, depending on the shape coordinate system).", fixed = TRUE)

			expect_error(loadShape(path = tests.home, shape = "aux_transitionLayer.shp", size = 10, buffer = 1:2),
			"'buffer' must either contain one value (applied to all four corners), or four values (applied to xmin, xmax, ymin and ymax, respectively)", fixed = TRUE)
			
			expect_error(loadShape(path = tests.home, shape = "aux_transitionLayer.shp", size = 10, buffer = -1),
			"'buffer' values cannot be negative.", fixed = TRUE)
			
			tryCatch(loadShape(path = tests.home, shape = "aux_transitionLayer.shp", size = 10, buffer = 100),
				warning = function(w) stop("A warning was issued where it should not have been."))
			
			tryCatch(base.raster <<- loadShape(path = tests.home, shape = "aux_transitionLayer.shp", size = 10, buffer = c(50, 100, 200, 250)),
				warning = function(w) stop("A warning was issued where it should not have been."))
		})

		test_that("loadShape stops if shape is not present or is not .shp", {
			expect_error(loadShape(path = "test", shape = "test.shp", size = 10, buffer = 100),
			"Could not find file 'test/test.shp'.", fixed = TRUE)

			expect_error(loadShape(shape = "test.shp", size = 10, buffer = 100),
			"Could not find file 'test.shp'.", fixed = TRUE)

			write.table(1, "test.txt")

			expect_error(loadShape(shape = "test.txt", size = 10, buffer = 100),
			"'shape' must be a .shp file.", fixed = TRUE)
		})

		test_that("transitionLayer is working properly", {
			tryCatch(t.layer <- transitionLayer(base.raster),
				warning = function(w) stop("A warning was issued where it should not have been."))
			expect_equal(as.character(class(t.layer)), "TransitionLayer")
		})

		base.raster <- loadShape(path = tests.home, shape = "aux_transitionLayer.shp", size = 5,
			coord.x = "x.32632", coord.y = "y.32632")
		t.layer <- transitionLayer(base.raster)

		test_that("distancesMatrix produces a warning when there are no viable passages between stations", {
			expect_warning(dist.mat <- distancesMatrix(t.layer = t.layer,
		  		coord.x = "x.32632", coord.y = "y.32632", actel = TRUE),
			"At least one station is completely blocked off from the remaining stations by land. Filling
the respective fields with NA. If your fish was expected to travel around the areas present
in the shape file, consider applying a 'buffer' when calculating the transition layer. This
will artificially add water space around the shape file.", fixed = TRUE)
		})
		# n

		test_that("distancesMatrix handles bad data correctly pt1", {
			expect_error(distancesMatrix(t.layer = t.layer, id.col = 1:2),
				"Please provide only one column name in 'id.col'", fixed = TRUE)
			expect_error(distancesMatrix(t.layer = t.layer, id.col = 2),
				"Please refer to the column name in 'id.col', rather than the column index.", fixed = TRUE)
			
			expect_error(distancesMatrix(t.layer = "test"),
				"Could not recognise 't.layer' as a TransitionLayer object. Make sure to compile it using the function transitionLayer.\n", fixed = TRUE)

			expect_error(distancesMatrix(t.layer =  t.layer,
		  		coord.x = "x.32632", coord.y = "y.32632", starters = "test.txt", actel = FALSE),
				"'starters' must be a data frame.", fixed = TRUE)

			expect_warning(distancesMatrix(t.layer =  t.layer,
		  		coord.x = "x.32632", coord.y = "y.32632", starters = "test", id.col = "test", actel = TRUE),
			"starters' or 'targets' were set but will be ignored because 'actel' is set to TRUE. Set 'actel' to FALSE to use the 'starters' and 'targets' arguments.", fixed = TRUE)

			expect_warning(distancesMatrix(t.layer =  t.layer,
		  		coord.x = "x.32632", coord.y = "y.32632", starters = "test", id.col = "test", actel = TRUE),
			"id.col' was set but will be ignored because 'actel' is set to TRUE. Set 'actel' to FALSE to use the 'id.col' argument.", fixed = TRUE)
		})
		# n
		# n

		base.raster <- loadShape(path = tests.home, shape = "aux_transitionLayer.shp", size = 5,
			coord.x = "x.32632", coord.y = "y.32632", buffer = 100)
		t.layer <- transitionLayer(base.raster)

		test_that("distancesMatrix handles bad data correctly pt2", {
			expect_warning(distancesMatrix(t.layer =  t.layer,
		  		coord.x = "x.32632", coord.y = "y.32632", starters = loadSpatial(), id.col = "Array", actel = FALSE),
			"The 'Array' column in 'starters' contains duplicated values; skipping row naming.", fixed = TRUE)

			expect_warning(distancesMatrix(t.layer =  t.layer,
		  		coord.x = "x.32632", coord.y = "y.32632", starters = loadSpatial(), id.col = "Array", actel = FALSE),
			"The 'Array' column in 'targets' contains duplicated values; skipping column naming.", fixed = TRUE)

			colnames(xspatial)[ncol(xspatial)] <- "test"
			write.csv(xspatial, "spatial2.csv", row.names = FALSE)

			expect_error(distancesMatrix(t.layer =  t.layer,
		  		coord.x = "test", coord.y = "y.32632", starters = loadSpatial(), targets = loadSpatial("spatial2.csv"), actel = FALSE),
			"Could not find a column 'test' in 'starters'.", fixed = TRUE)

			expect_error(distancesMatrix(t.layer =  t.layer,
		  		coord.x = "x.32632", coord.y = "test", starters = loadSpatial("spatial2.csv"), targets = loadSpatial(), actel = FALSE),
			"Could not find a column 'test' in 'targets'.", fixed = TRUE)
		})

		test_that("distancesMatrix output is as expected", {
		 output <- distancesMatrix(t.layer = t.layer, coord.x = "x.32632", coord.y = "y.32632")
		 expect_equal(colnames(output), paste("St", 1:4, sep = "."))
		 expect_equal(rownames(output), paste("St", 1:4, sep = "."))
		 expect_equal(output[, 1], c(   0, 586, 934, 1154))
		 expect_equal(output[, 2], c( 586,   0, 490,  656))
		 expect_equal(output[, 3], c( 934, 490,   0,  237))
		 expect_equal(output[, 4], c(1154, 656, 237,    0))
		})
		# n

		xspatial <- example.spatial[8:11, ]
		xspatial$x.32632 <- c(454568, 453400, 452047, 452975)
		xspatial$y.32632 <- c(6243912, 6242630, 6242387, 6241169)

		test_that("loadShape expands the grid range if the spatial objects are outside the shape range.", {
			expect_message(loadShape(path = tests.home, shape = "aux_transitionLayer.shp", size = 10,
					coord.x = "x.32632", coord.y = "y.32632", spatial = xspatial),
			"Extending the shape ranges with open water to ensure the stations fit inside it.", fixed = TRUE)
		})

		xspatial <- example.spatial[8:11, ]
		xspatial$x.32632 <- c(453500, 453400, 452047, 452975)
		xspatial$y.32632 <- c(6242800, 6242630, 6242387, 6241169)
		test_that("loadShape expands the grid range if the spatial objects are outside the shape range.", {
			expect_warning(loadShape(path = tests.home, shape = "aux_transitionLayer.shp", size = 10,
					coord.x = "x.32632", coord.y = "y.32632", spatial = xspatial),
			"Station 'Station 7' is not placed in water! This can cause several problems.", fixed = TRUE)
		})

		file.remove("spatial.csv")
		setwd(tests.home)
		rm(list = ls())
	}
}
# n
# n
# n
# n

# ------------------------
# Manual functions

tests.home <- getwd()
setwd(tempdir())

test_that("emptyMatrix stops if the required file are not present.", {
	expect_error(emptyMatrix(), "Could not find file 'spatial.csv'.", fixed = TRUE)
})

test_that("emptyMatrix creates a distances matrix with the correct structure", {
	dist.mat <<- emptyMatrix(paste0(system.file(package = "actel")[1], "/example_spatial.csv"))
	expect_equal(ncol(dist.mat), nrow(dist.mat))
	expect_true(sum(is.na(dist.mat)) == (ncol(dist.mat) ^ 2) - ncol(dist.mat))
})

for(i in 2:ncol(dist.mat)) {
	for(j in 1:(i - 1)) {
		dist.mat[j, i] <- i + j
	}
}

test_that("completeMatrix works as expected", {
	expect_error(completeMatrix("a"), "The input must be a matrix", fixed = TRUE)

	expect_error(completeMatrix(matrix(ncol = 2, nrow = 3)), "The matrix does not contain the same number of columns and rows. Aborting.", fixed = TRUE)

	output <- completeMatrix(dist.mat)
	for(i in 2:ncol(output)) {
		for(j in 1:(i - 1)) {
			expect_equal(output[i, j], i + j)
		}
	}
})

setwd(tests.home)
rm(list = ls())
