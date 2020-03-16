test_that("transitionLayer complains if coord.y or coord.x are given and spatial.csv is not present", {
	sink("temp.txt")

	expect_warning(transitionLayer(shape = "aux_transitionLayer.shp", size = 10, EPSGcode = 32632, 
			coord.x = "x.32632", coord.y = "y.32632", directions = 4, force = FALSE),
	"'coord.x' and 'coord.y' were set but could not find a spatial.csv file in the current working directory. Skipping spatial.csv check.", fixed = TRUE)

	expect_warning(transitionLayer(shape = "aux_transitionLayer.shp", size = 10, EPSGcode = 32632, 
			coord.x = "x.32632", directions = 4, force = FALSE),
	"'coord.x' was set but 'coord.y' was not. Skipping spatial.csv check.", fixed = TRUE)

	expect_warning(transitionLayer(shape = "aux_transitionLayer.shp", size = 10, EPSGcode = 32632, 
			coord.y = "x.32632", directions = 4, force = FALSE),
	"'coord.y' was set but 'coord.x' was not. Skipping spatial.csv check.", fixed = TRUE)

	sink()
})

xspatial <- example.spatial[8:11, ]
xspatial$x.32632 <- c(453568, 453400, 453047, 452975)
xspatial$y.32632 <- c(6242912, 6242630, 6242387, 6242169)
write.csv(xspatial, "spatial.csv", row.names = FALSE)

test_that("transitionLayer complains if coord.y or coord.x are not valid column names", {
	sink("temp.txt")

	expect_warning(transitionLayer(shape = "aux_transitionLayer.shp", size = 10, EPSGcode = 32632, 
			coord.x = "test", coord.y = "y.32632", directions = 4, force = FALSE),
	"Could not find column 'test' in the spatial.csv file. Skipping spatial.csv check.", fixed = TRUE)	

	expect_warning(transitionLayer(shape = "aux_transitionLayer.shp", size = 10, EPSGcode = 32632, 
			coord.x = "x.32632", coord.y = "test", directions = 4, force = FALSE),
	"Could not find column 'test' in the spatial.csv file. Skipping spatial.csv check.", fixed = TRUE)

	expect_warning(transitionLayer(shape = "aux_transitionLayer.shp", size = 10, EPSGcode = 32632, 
			coord.x = "test2", coord.y = "test", directions = 4, force = FALSE),
	"Could not find columns 'test2' and 'test' in the spatial.csv file. Skipping spatial.csv check.", fixed = TRUE)

	sink()
})

test_that("transitionLayer only allows valid buffers", {
	sink("temp.txt")

	expect_error(transitionLayer(shape = "aux_transitionLayer.shp", size = 10, EPSGcode = 32632, 
			buffer = "a", directions = 4, force = FALSE),
	"'buffer' must be numeric (in metres).", fixed = TRUE)

	expect_error(transitionLayer(shape = "aux_transitionLayer.shp", size = 10, EPSGcode = 32632, 
			buffer = 1:2, directions = 4, force = FALSE),
	"'buffer' must either contain one value (applied to all four corners), or four values (applied to xmin, xmax, ymin and ymax, respectively)", fixed = TRUE)
	
	expect_error(transitionLayer(shape = "aux_transitionLayer.shp", size = 10, EPSGcode = 32632, 
			buffer = -1, directions = 4, force = FALSE),
	"'buffer' values cannot be negative.", fixed = TRUE)
	
	tryCatch(transitionLayer(shape = "aux_transitionLayer.shp", size = 10, EPSGcode = 32632, 
			buffer = 100, directions = 4, force = FALSE), 
		warning = function(w) stop("A warning was issued where it should not have been."))
	
	tryCatch(transitionLayer(shape = "aux_transitionLayer.shp", size = 10, EPSGcode = 32632, 
			buffer = c(50, 100, 200, 250), directions = 4, force = FALSE),
		warning = function(w) stop("A warning was issued where it should not have been."))

	sink()
})

test_that("transitionLayer stops if shape is not present or is not .shp", {
	sink("temp.txt")

	expect_error(transitionLayer(shape = "test.shp", size = 10, EPSGcode = 32632, 
			buffer = 100, directions = 4, force = FALSE),
	"Could not find file 'test.shp' in the working directory.", fixed = TRUE)

	write.table(1, "test.txt")

	expect_error(transitionLayer(shape = "test.txt", size = 10, EPSGcode = 32632, 
			buffer = 100, directions = 4, force = FALSE),
	"'shape' must be a .shp file.", fixed = TRUE)

	file.remove("test.txt")

	sink()
})

test_that("transitionLayer stops if requested resolution is very high.", {
	sink("temp.txt")

	expect_error(transitionLayer(shape = "aux_transitionLayer.shp", size = 0.1, EPSGcode = 32632, 
		directions = 4, force = FALSE),
	"The chosen pixel size creates a transition layer with one or two axes greater than
2000 pixels. This can lead to very long computing times and ultimately the function  may
fail due to lack of free RAM to allocate the results. If you really want to use this pixel
size, rerun the function with force = TRUE.", fixed = TRUE)

	sink()
})

test_that("transitionLayer and distancesMatrix stop if more than one value included in 'EPSGcode'", {
	sink("temp.txt")

	expect_error(transitionLayer(shape = "aux_transitionLayer.shp", size = 5, EPSGcode = 1:2, 
			coord.x = "x.32632", coord.y = "y.32632", directions = 16, force = FALSE),
	"Please provide only one EPSG code.", fixed = TRUE)

	expect_error(transitionLayer(shape = "aux_transitionLayer.shp", size = 5, EPSGcode = "a", 
			coord.x = "x.32632", coord.y = "y.32632", directions = 16, force = FALSE),
	"'EPSGcode' must be numeric.", fixed = TRUE)

	expect_error(transitionLayer(shape = "aux_transitionLayer.shp", size = 5, EPSGcode = 11234122, 
			coord.x = "x.32632", coord.y = "y.32632", directions = 16, force = FALSE),
	"Could not recognize the selected EPSG code. You can find a list of available EPSG codes by running rgdal::make_EPSG()", fixed = TRUE)

	expect_error(distancesMatrix(t.layer = "transition.layer.RData", EPSGcode = 1:2, 
  		coord.x = "x.32632", coord.y = "y.32632", actel = TRUE),
	"Please provide only one EPSG code.", fixed = TRUE)

	expect_error(distancesMatrix(t.layer = "transition.layer.RData", EPSGcode = "a", 
  		coord.x = "x.32632", coord.y = "y.32632", actel = TRUE),
	"'EPSGcode' must be numeric.", fixed = TRUE)

	expect_error(distancesMatrix(t.layer = "transition.layer.RData", EPSGcode = 32612332, 
  		coord.x = "x.32632", coord.y = "y.32632", actel = TRUE),
	"Could not recognize the selected EPSG code. You can find a list of available EPSG codes by running rgdal::make_EPSG()", fixed = TRUE)

	sink()
})

test_that("distancesMatrix produces a warning when there are viable passages between stations", {
	sink("temp.txt")

	transitionLayer(shape = "aux_transitionLayer.shp", size = 5, EPSGcode = 32632, 
		coord.x = "x.32632", coord.y = "y.32632", directions = 16, force = FALSE)

	sink()

	expect_warning(dist.mat <- distancesMatrix(t.layer = "transition.layer.RData", EPSGcode = 32632, 
  		coord.x = "x.32632", coord.y = "y.32632", actel = TRUE),
	"At least one station is completely blocked off from the remaining stations by land. Filling 
the respective fields with NA. If your fish was expected to travel around the areas present 
in the shape file, consider applying a 'buffer' when calculating the transition layer. This
will artificially add water space around the shape file.", fixed = TRUE)
})

test_that("distancesMatrix handles bad data correctly", {
	expect_error(distancesMatrix(EPSGcode = 32632, id.col = 1:2),
		"Please provide only one column name in 'id.col'", fixed = TRUE)
	expect_error(distancesMatrix(EPSGcode = 32632, id.col = 2),
		"Please refer to the column name in 'id.col', rather than the column index.", fixed = TRUE)
	
	expect_error(distancesMatrix(t.layer = "test", EPSGcode = 32632),
		"Could not find file 'test' in the working directory.", fixed = TRUE)
	test <- 1
	save(test, file = "test.RData")
	expect_error(distancesMatrix(t.layer = "test.RData", EPSGcode = 32632),
		"Could not find a transition layer in 'test.RData'.", fixed = TRUE)
	file.remove("test.RData")

	write.table(1, "test.txt")
	expect_error(distancesMatrix(t.layer = "test.txt", EPSGcode = 32632),
		"'test.txt' could not be recognised as .RData file, please make sure the include the file extension in 't.layer'.", fixed = TRUE)

	expect_error(distancesMatrix(t.layer = "transition.layer.RData", EPSGcode = 32632, 
  		coord.x = "x.32632", coord.y = "y.32632", starters = "test.txt", actel = FALSE),
	"One of the point files (starters or targets) does not appear to be written in csv format. Please make sure to include the '.csv' extension in the file name.", fixed = TRUE)

	expect_warning(distancesMatrix(t.layer = "transition.layer.RData", EPSGcode = 32632, 
  		coord.x = "x.32632", coord.y = "y.32632", starters = "spatial.csv", id.col = "test", actel = FALSE),
	"Could not find a 'test' column in the 'spatial.csv' file; skipping row naming.", fixed = TRUE)

	expect_warning(distancesMatrix(t.layer = "transition.layer.RData", EPSGcode = 32632, 
  		coord.x = "x.32632", coord.y = "y.32632", starters = "spatial.csv", id.col = "test", actel = FALSE),
	"Could not find a 'test' column in the 'spatial.csv' file; skipping column naming.", fixed = TRUE)

	expect_error(distancesMatrix(t.layer = "transition.layer.RData", EPSGcode = 32632, 
  		coord.x = "x.32632", coord.y = "y.32632", starters = "test", id.col = "test", actel = FALSE),
	"Could not find a 'test' file in the working directory.", fixed = TRUE)

	expect_error(distancesMatrix(t.layer = "transition.layer.RData", EPSGcode = 32632, 
  		coord.x = "x.32632", coord.y = "y.32632", starters = "spatial.csv", targets = "test", id.col = "test", actel = FALSE),
	"Could not find a 'test' file in the working directory.", fixed = TRUE)

	sink("temp.txt")
	transitionLayer(shape = "aux_transitionLayer.shp", size = 5, EPSGcode = 32632, 
		coord.x = "x.32632", coord.y = "y.32632", directions = 16, force = FALSE, buffer = 100)
	sink()

	expect_warning(distancesMatrix(t.layer = "transition.layer.RData", EPSGcode = 32632, 
  		coord.x = "x.32632", coord.y = "y.32632", starters = "spatial.csv", id.col = "Array", actel = FALSE),
	"The 'Array' column in the 'spatial.csv' file contains duplicated values; skipping row naming.", fixed = TRUE)

	expect_warning(distancesMatrix(t.layer = "transition.layer.RData", EPSGcode = 32632, 
  		coord.x = "x.32632", coord.y = "y.32632", starters = "spatial.csv", id.col = "Array", actel = FALSE),
	"The 'Array' column in the 'spatial.csv' file contains duplicated values; skipping column naming.", fixed = TRUE)

	colnames(xspatial)[7] <- "test"
	write.csv(xspatial, "spatial2.csv", row.names = FALSE)

	expect_error(distancesMatrix(t.layer = "transition.layer.RData", EPSGcode = 32632, 
  		coord.x = "test", coord.y = "y.32632", starters = "spatial.csv", targets = "spatial2.csv", actel = FALSE),
	"Could not find a column 'test' in the file 'spatial.csv'.", fixed = TRUE)

	expect_error(distancesMatrix(t.layer = "transition.layer.RData", EPSGcode = 32632, 
  		coord.x = "x.32632", coord.y = "test", starters = "spatial.csv", targets = "spatial2.csv", actel = FALSE),
	"Could not find a column 'test' in the file 'spatial.csv'.", fixed = TRUE)

	expect_error(distancesMatrix(t.layer = "transition.layer.RData", EPSGcode = 32632, 
  		coord.x = "y.32632", coord.y = "x.32632", starters = "spatial.csv", targets = "spatial2.csv", actel = FALSE),
	"Could not find a column 'y.32632' in the file 'spatial2.csv'.", fixed = TRUE)

	expect_error(distancesMatrix(t.layer = "transition.layer.RData", EPSGcode = 32632, 
  		coord.x = "x.32632", coord.y = "y.32632", starters = "spatial.csv", targets = "spatial2.csv", actel = FALSE),
	"Could not find a column 'y.32632' in the file 'spatial2.csv'.", fixed = TRUE)
})


test_that("distancesMatrix output is as expected", {
 output <- distancesMatrix(EPSGcode = 32632, coord.x = "x.32632", coord.y = "y.32632")
 expect_true(file.exists("distances.csv"))
 expect_equal(colnames(output), paste("St", 1:4, sep = "."))
 expect_equal(rownames(output), paste("St", 1:4, sep = "."))
 trunc <- round(output, 0)
 expect_equal(trunc[, 1], c(   0, 586, 934, 1154))
 expect_equal(trunc[, 2], c( 586,   0, 490,  656))
 expect_equal(trunc[, 3], c( 934, 490,   0,  237))
 expect_equal(trunc[, 4], c(1154, 656, 237,    0))
})

xspatial <- example.spatial[8:11, ]
xspatial$x.32632 <- c(454568, 453400, 452047, 452975)
xspatial$y.32632 <- c(6243912, 6242630, 6242387, 6241169)
write.csv(xspatial, "spatial.csv", row.names = FALSE)
test_that("transitionLayer expands the grid range if the spatial objects are outside the shape range.", {
	sink("temp.txt")

	expect_message(transitionLayer(shape = "aux_transitionLayer.shp", size = 10, EPSGcode = 32632, 
			coord.x = "x.32632", coord.y = "y.32632", directions = 4, force = FALSE),
	"Extending shape's minimum X range to ensure the stations fit in the range.", fixed = TRUE)

	expect_message(transitionLayer(shape = "aux_transitionLayer.shp", size = 10, EPSGcode = 32632, 
			coord.x = "x.32632", coord.y = "y.32632", directions = 4, force = FALSE),
	"Extending shape's maximum X range to ensure the stations fit in the range.", fixed = TRUE)

	expect_message(transitionLayer(shape = "aux_transitionLayer.shp", size = 10, EPSGcode = 32632, 
			coord.x = "x.32632", coord.y = "y.32632", directions = 4, force = FALSE),
	"Extending shape's minimum Y range to ensure the stations fit in the range.", fixed = TRUE)

	expect_message(transitionLayer(shape = "aux_transitionLayer.shp", size = 10, EPSGcode = 32632, 
			coord.x = "x.32632", coord.y = "y.32632", directions = 4, force = FALSE),
	"Extending shape's maximum Y range to ensure the stations fit in the range.", fixed = TRUE)

	sink()
})

file.remove(list.files(pattern = "*txt$"))
file.remove("transition.layer.RData")
file.remove("distances.csv")
file.remove("spatial.csv")
rm(list = ls())

# ------------------------
# Manual functions

test_that("emptyMatrix and completeMatrix stop their required files are not present.", {
	expect_error(emptyMatrix(), "Could not find a 'spatial.csv' file in the current working directory.", fixed = TRUE)
	expect_error(completeMatrix(), "Could not find a 'distances.csv' file in the current working directory.", fixed = TRUE)
})

test_that("emptyMatrix creates a distances.csv file with the correct structure", {
	write.csv(example.spatial, "spatial.csv", row.names = FALSE)
	emptyMatrix()
	expect_true(file.exists("distances.csv"))
	dist.mat <<- read.csv("distances.csv", row.names = 1)	
})

for(i in 2:ncol(dist.mat)) {
	for(j in 1:(i - 1)) {
		dist.mat[j, i] <- i + j
	}
}

write.csv(dist.mat, "distances.csv", row.names = TRUE)

test_that("completeMatrix works as expected", {
	completeMatrix()
	output <- read.csv("distances.csv", row.names = 1)
	for(i in 2:ncol(output)) {
		for(j in 1:(i - 1)) {
			expect_equal(output[i, j], i + j)
		}
	}
})

file.remove(list.files(pattern = "*txt$"))
file.remove("distances.csv")
file.remove("spatial.csv")
file.remove("spatial2.csv")
rm(list = ls())
