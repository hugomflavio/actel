skip_on_cran()

aux <- c(
  length(suppressWarnings(packageDescription("raster"))),
  length(suppressWarnings(packageDescription("gdistance"))),
  length(suppressWarnings(packageDescription("sp"))),
  length(suppressWarnings(packageDescription("tools"))),
  length(suppressWarnings(packageDescription("rgdal"))))
missing.packages <- sapply(aux, function(x) x == 1 && is.na(x))

if (any(missing.packages)) {
  test_that("transitionLayer and distancesMatrix stop due to missing dependencies", {
  	expect_error(transitionLayer(),
  		paste0("This function requires packages '", paste(c("raster", "gdistance", "sp", "tools", "rgdal")[missing.packages], collapse = "', '"), 
      	"' to operate. Please install ", ifelse(sum(missing.packages) > 1, "them", "it"), " before proceeding.\n"), fixed = TRUE)
  	expect_error(distancesMatrix(),
  		paste0("This function requires packages '", paste(c("raster", "gdistance", "sp", "tools", "rgdal")[missing.packages], collapse = "', '"), 
      	"' to operate. Please install ", ifelse(sum(missing.packages) > 1, "them", "it"), " before proceeding.\n"), fixed = TRUE)
  })
} else {
tests.home <- getwd()
setwd(tempdir())

test_that("transitionLayer complains if coord.y or coord.x are given and spatial.csv is not present", {
	expect_warning(transitionLayer(path = tests.home, shape = "aux_transitionLayer.shp", size = 10, EPSGcode = 32632, 
			coord.x = "x.32632", coord.y = "y.32632", directions = 4, force = FALSE),
	"'coord.x' and 'coord.y' were set but could not find a spatial.csv file in the current working directory. Skipping spatial.csv check.", fixed = TRUE)

	expect_warning(transitionLayer(path = tests.home, shape = "aux_transitionLayer.shp", size = 10, EPSGcode = 32632, 
			coord.x = "x.32632", directions = 4, force = FALSE),
	"'coord.x' was set but 'coord.y' was not. Skipping spatial.csv check.", fixed = TRUE)

	expect_warning(transitionLayer(path = tests.home, shape = "aux_transitionLayer.shp", size = 10, EPSGcode = 32632, 
			coord.y = "x.32632", directions = 4, force = FALSE),
	"'coord.y' was set but 'coord.x' was not. Skipping spatial.csv check.", fixed = TRUE)
})

xspatial <- example.spatial[8:11, ]
xspatial$x.32632 <- c(453568, 453400, 453047, 452975)
xspatial$y.32632 <- c(6242912, 6242630, 6242387, 6242169)
write.csv(xspatial, "spatial.csv", row.names = FALSE)

test_that("transitionLayer complains if coord.y or coord.x are not valid column names", {
	expect_warning(transitionLayer(path = tests.home, shape = "aux_transitionLayer.shp", size = 10, EPSGcode = 32632, 
			coord.x = "test", coord.y = "y.32632", directions = 4, force = FALSE),
	"Could not find column 'test' in the spatial.csv file. Skipping spatial.csv check.", fixed = TRUE)	

	expect_warning(transitionLayer(path = tests.home, shape = "aux_transitionLayer.shp", size = 10, EPSGcode = 32632, 
			coord.x = "x.32632", coord.y = "test", directions = 4, force = FALSE),
	"Could not find column 'test' in the spatial.csv file. Skipping spatial.csv check.", fixed = TRUE)

	expect_warning(transitionLayer(path = tests.home, shape = "aux_transitionLayer.shp", size = 10, EPSGcode = 32632, 
			coord.x = "test2", coord.y = "test", directions = 4, force = FALSE),
	"Could not find columns 'test2' and 'test' in the spatial.csv file. Skipping spatial.csv check.", fixed = TRUE)
})

test_that("transitionLayer only allows valid buffers", {
	expect_error(transitionLayer(path = tests.home, shape = "aux_transitionLayer.shp", size = 10, EPSGcode = 32632, 
			buffer = "a", directions = 4, force = FALSE),
	"'buffer' must be numeric (in metres).", fixed = TRUE)

	expect_error(transitionLayer(path = tests.home, shape = "aux_transitionLayer.shp", size = 10, EPSGcode = 32632, 
			buffer = 1:2, directions = 4, force = FALSE),
	"'buffer' must either contain one value (applied to all four corners), or four values (applied to xmin, xmax, ymin and ymax, respectively)", fixed = TRUE)
	
	expect_error(transitionLayer(path = tests.home, shape = "aux_transitionLayer.shp", size = 10, EPSGcode = 32632, 
			buffer = -1, directions = 4, force = FALSE),
	"'buffer' values cannot be negative.", fixed = TRUE)
	
	tryCatch(transitionLayer(path = tests.home, shape = "aux_transitionLayer.shp", size = 10, EPSGcode = 32632, 
			buffer = 100, directions = 4, force = FALSE), 
		warning = function(w) stop("A warning was issued where it should not have been."))
	
	tryCatch(t.layer <<- transitionLayer(path = tests.home, shape = "aux_transitionLayer.shp", size = 10, EPSGcode = 32632, 
			buffer = c(50, 100, 200, 250), directions = 4, force = FALSE),
		warning = function(w) stop("A warning was issued where it should not have been."))
})

test_that("transitionLayer stops if shape is not present or is not .shp", {
	expect_error(transitionLayer(shape = "test.shp", size = 10, EPSGcode = 32632, 
			buffer = 100, directions = 4, force = FALSE),
	"Could not find file 'test.shp'.", fixed = TRUE)

	write.table(1, "test.txt")

	expect_error(transitionLayer(shape = "test.txt", size = 10, EPSGcode = 32632, 
			buffer = 100, directions = 4, force = FALSE),
	"'shape' must be a .shp file.", fixed = TRUE)
})

test_that("transitionLayer stops if requested resolution is very high.", {
	expect_error(transitionLayer(path = tests.home, shape = "aux_transitionLayer.shp", size = 0.1, EPSGcode = 32632, 
		directions = 4, force = FALSE),
	"The chosen pixel size creates a transition layer with one or two axes greater than
2000 pixels. This can lead to very long computing times and ultimately the function  may
fail due to lack of free RAM to allocate the results. If you really want to use this pixel
size, rerun the function with force = TRUE.", fixed = TRUE)
})

test_that("transitionLayer and distancesMatrix stop if more than one value included in 'EPSGcode'", {
	expect_error(transitionLayer(path = tests.home, shape = "aux_transitionLayer.shp", size = 5, EPSGcode = 1:2, 
			coord.x = "x.32632", coord.y = "y.32632", directions = 16, force = FALSE),
	"Please provide only one EPSG code.", fixed = TRUE)

	expect_error(transitionLayer(path = tests.home, shape = "aux_transitionLayer.shp", size = 5, EPSGcode = "a", 
			coord.x = "x.32632", coord.y = "y.32632", directions = 16, force = FALSE),
	"'EPSGcode' must be numeric.", fixed = TRUE)

	expect_error(transitionLayer(path = tests.home, shape = "aux_transitionLayer.shp", size = 5, EPSGcode = 11234122, 
			coord.x = "x.32632", coord.y = "y.32632", directions = 16, force = FALSE),
	"Could not recognize the selected EPSG code. You can find a list of available EPSG codes by running rgdal::make_EPSG()", fixed = TRUE)

	expect_error(distancesMatrix(t.layer = t.layer, EPSGcode = 1:2, 
  		coord.x = "x.32632", coord.y = "y.32632", actel = TRUE),
	"Please provide only one EPSG code.", fixed = TRUE)

	expect_error(distancesMatrix(t.layer = t.layer, EPSGcode = "a", 
  		coord.x = "x.32632", coord.y = "y.32632", actel = TRUE),
	"'EPSGcode' must be numeric.", fixed = TRUE)

	expect_error(distancesMatrix(t.layer = t.layer, EPSGcode = 32612332, 
  		coord.x = "x.32632", coord.y = "y.32632", actel = TRUE),
	"Could not recognise the selected EPSG code. You can find a list of available EPSG codes by running rgdal::make_EPSG()", fixed = TRUE)
})

test_that("distancesMatrix produces a warning when there are viable passages between stations", {
	t.layer <- transitionLayer(path = tests.home, shape = "aux_transitionLayer.shp", size = 5, EPSGcode = 32632, 
		coord.x = "x.32632", coord.y = "y.32632", directions = 16, force = FALSE)
	expect_warning(dist.mat <- distancesMatrix(t.layer = t.layer, EPSGcode = 32632, 
  		coord.x = "x.32632", coord.y = "y.32632", actel = TRUE),
	"At least one station is completely blocked off from the remaining stations by land. Filling 
the respective fields with NA. If your fish was expected to travel around the areas present 
in the shape file, consider applying a 'buffer' when calculating the transition layer. This
will artificially add water space around the shape file.", fixed = TRUE)
})
# n

test_that("distancesMatrix handles bad data correctly", {
	expect_error(distancesMatrix(t.layer = t.layer, EPSGcode = 32632, id.col = 1:2),
		"Please provide only one column name in 'id.col'", fixed = TRUE)
	expect_error(distancesMatrix(t.layer = t.layer, EPSGcode = 32632, id.col = 2),
		"Please refer to the column name in 'id.col', rather than the column index.", fixed = TRUE)
	
	expect_error(distancesMatrix(t.layer = "test", EPSGcode = 32632),
		"Could not recognise 't.layer' as a TransitionLayer object. Make sure to compile it using the function transitionLayer.\n", fixed = TRUE)

	expect_error(distancesMatrix(t.layer =  t.layer, EPSGcode = 32632, 
  		coord.x = "x.32632", coord.y = "y.32632", starters = "test.txt", actel = FALSE),
		"'starters' must be a data frame.", fixed = TRUE)

	expect_warning(distancesMatrix(t.layer =  t.layer, EPSGcode = 32632, 
  		coord.x = "x.32632", coord.y = "y.32632", starters = "test", id.col = "test", actel = TRUE),
	"starters' or 'targets' were set but will be ignored because 'actel' is set to TRUE. Set 'actel' to FALSE to use the 'starters' and 'targets' arguments.", fixed = TRUE)

	expect_warning(distancesMatrix(t.layer =  t.layer, EPSGcode = 32632, 
  		coord.x = "x.32632", coord.y = "y.32632", starters = "test", id.col = "test", actel = TRUE),
	"id.col' was set but will be ignored because 'actel' is set to TRUE. Set 'actel' to FALSE to use the 'id.col' argument.", fixed = TRUE)

	t.layer <<- transitionLayer(path = tests.home, shape = "aux_transitionLayer.shp", size = 5, EPSGcode = 32632, 
		coord.x = "x.32632", coord.y = "y.32632", directions = 16, force = FALSE, buffer = 100)

	expect_warning(distancesMatrix(t.layer =  t.layer, EPSGcode = 32632, 
  		coord.x = "x.32632", coord.y = "y.32632", starters = loadSpatial(), id.col = "Array", actel = FALSE),
	"The 'Array' column in 'starters' contains duplicated values; skipping row naming.", fixed = TRUE)

	expect_warning(distancesMatrix(t.layer =  t.layer, EPSGcode = 32632, 
  		coord.x = "x.32632", coord.y = "y.32632", starters = loadSpatial(), id.col = "Array", actel = FALSE),
	"The 'Array' column in 'targets' contains duplicated values; skipping column naming.", fixed = TRUE)

	colnames(xspatial)[7] <- "test"
	write.csv(xspatial, "spatial2.csv", row.names = FALSE)

	expect_error(distancesMatrix(t.layer =  t.layer, EPSGcode = 32632, 
  		coord.x = "test", coord.y = "y.32632", starters = loadSpatial(), targets = loadSpatial("spatial2.csv"), actel = FALSE),
	"Could not find a column 'test' in 'starters'.", fixed = TRUE)

	expect_error(distancesMatrix(t.layer =  t.layer, EPSGcode = 32632, 
  		coord.x = "x.32632", coord.y = "test", starters = loadSpatial("spatial2.csv"), targets = loadSpatial(), actel = FALSE),
	"Could not find a column 'test' in 'targets'.", fixed = TRUE)
})
# n
# n

test_that("distancesMatrix output is as expected", {
 output <- distancesMatrix(t.layer = t.layer, EPSGcode = 32632, coord.x = "x.32632", coord.y = "y.32632")
 expect_equal(colnames(output), paste("St", 1:4, sep = "."))
 expect_equal(rownames(output), paste("St", 1:4, sep = "."))
 trunc <- round(output, 0)
 expect_equal(trunc[, 1], c(   0, 586, 934, 1154))
 expect_equal(trunc[, 2], c( 586,   0, 490,  656))
 expect_equal(trunc[, 3], c( 934, 490,   0,  237))
 expect_equal(trunc[, 4], c(1154, 656, 237,    0))
})
# n

xspatial <- example.spatial[8:11, ]
xspatial$x.32632 <- c(454568, 453400, 452047, 452975)
xspatial$y.32632 <- c(6243912, 6242630, 6242387, 6241169)
write.csv(xspatial, "spatial.csv", row.names = FALSE)
test_that("transitionLayer expands the grid range if the spatial objects are outside the shape range.", {
	expect_message(transitionLayer(path = tests.home, shape = "aux_transitionLayer.shp", size = 10, EPSGcode = 32632, 
			coord.x = "x.32632", coord.y = "y.32632", directions = 4, force = FALSE),
	"Extending shape's minimum X range to ensure the stations fit in the range.", fixed = TRUE)

	expect_message(transitionLayer(path = tests.home, shape = "aux_transitionLayer.shp", size = 10, EPSGcode = 32632, 
			coord.x = "x.32632", coord.y = "y.32632", directions = 4, force = FALSE),
	"Extending shape's maximum X range to ensure the stations fit in the range.", fixed = TRUE)

	expect_message(transitionLayer(path = tests.home, shape = "aux_transitionLayer.shp", size = 10, EPSGcode = 32632, 
			coord.x = "x.32632", coord.y = "y.32632", directions = 4, force = FALSE),
	"Extending shape's minimum Y range to ensure the stations fit in the range.", fixed = TRUE)

	expect_message(transitionLayer(path = tests.home, shape = "aux_transitionLayer.shp", size = 10, EPSGcode = 32632, 
			coord.x = "x.32632", coord.y = "y.32632", directions = 4, force = FALSE),
	"Extending shape's maximum Y range to ensure the stations fit in the range.", fixed = TRUE)
})

file.remove("spatial.csv")
setwd(tests.home)
rm(list = ls())
}

# ------------------------
# Manual functions

tests.home <- getwd()
setwd(tempdir())

test_that("emptyMatrix stops if the required file are not present.", {
	expect_error(emptyMatrix(), "Could not find a 'spatial.csv' file in the current working directory.", fixed = TRUE)
})

test_that("emptyMatrix creates a distances.csv file with the correct structure", {
	write.csv(example.spatial, "spatial.csv", row.names = FALSE)
	dist.mat <<- emptyMatrix()
	expect_equal(ncol(dist.mat), nrow(dist.mat))
	expect_true(sum(is.na(dist.mat)) == (ncol(dist.mat) ^ 2) - ncol(dist.mat))
})

for(i in 2:ncol(dist.mat)) {
	for(j in 1:(i - 1)) {
		dist.mat[j, i] <- i + j
	}
}

test_that("completeMatrix works as expected", {
	output <- completeMatrix(dist.mat)
	for(i in 2:ncol(output)) {
		for(j in 1:(i - 1)) {
			expect_equal(output[i, j], i + j)
		}
	}
})

file.remove("spatial.csv")
setwd(tests.home)
rm(list = ls())
