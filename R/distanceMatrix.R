#' Calculate distance matrix
#' 
#' Adapted from Grant Adams' script "distance to closest mpa". (grant.adams@eagles.usm.edu)
#' 
#' https://cran.r-project.org/web/packages/gdistance/vignettes/gdistance1.pdf
#'
#' @param shape A shape file from which to create the transition layer.
#' @param size The pixel size, in metres.
#' @param EPSGcode The EPSG code corresponding to the coordinate system of the input data. All inputs must be in the same metric system. DO NOT use degree-based coordinates.
#' @param directions The number of directions considered for every movement situation during cost calculation.
#' @param force logical: if TRUE, allows producing transition layers with more than 2000 pixels on one or both axes.
#' 
#' @export
#' 
transitionLayer <- function(shape, size, EPSGcode, directions = c(16,8,4), force = FALSE){
	directions <- as.character(directions)
	directions <- match.arg(directions)
	if (!file.exists(shape))
		stop(paste0("Could not find file '", shape, "' in the working directory.\n"))
	if (tools::file_ext(shape) == "shp") {
		shape <- sub(".shp", "", shape)
		shape <- rgdal::readOGR(dsn = ".", layer = shape, verbose = FALSE) #study area shapefile
	} else {
		stop("'shape' must be a .shp file.\n")
	}
	data.crs <- raster::crs(paste("+init=epsg:", EPSGcode, sep = ""))
	raster::crs(shape)<-raster::crs(data.crs) # Set CRS 
	pixel.res <- (shape@bbox[,2] - shape@bbox[,1]) / size
	if (any(pixel.res %% 1 != 0)) {
		cat("The chosen pixel size does not allow for an integer number of pixels\n\nShapefile resolution:\n")
		print(shape@bbox)
		cat(paste("\nChosen pixel size:", size, "\n\n"))
		cat("Number of resulting pixels:\n")
		print(pixel.res)
		cat("\n")
		stop("The extent of the shapefile divided by the pixel size must result in an integer.\n")
	}
	if (!force && any(pixel.res > 2000)) {
		warning("The chosen pixel size creates a transition layer with one or two axes greater 
  than 2000 pixels. This can lead to very long computing times and ultimately the function 
  may fail due to lack of free RAM to allocate the results. If you really want to use this 
  pixel size, rerun the function with force = TRUE.")
	} else {
		ras <- raster::raster(nrow = pixel.res["y"], ncol = pixel.res["x"], crs = raster::crs(shape)) # create a recipient raster
		raster::extent(ras) <- raster::extent(shape) #Make the raster have the same extent as the shapefile
		#### "Burn" the shapefile into the raster
		message("M: Constructing the transition layer. This process may take several minutes depending on the study area size and chosen pixel size."); flush.console()
		shape.mask <- raster::rasterize(shape, ras)
		project.raster <- is.na(shape.mask)
		project.raster[project.raster == 0] <- 10000 # make land extremely hard to cross
		#### The transition layer will be used as the shape for calculating least-cost distance
		transition.layer <- gdistance::transition(1 / project.raster, transitionFunction = mean, directions = as.numeric(directions))
		transition.layer <- gdistance::geoCorrection(transition.layer, type = "c") # correct for shape distortion, as well as for diagonal connections between grid cells
		save(transition.layer, file = "transition.layer.RData")
		message("M: The transition layer was saved to 'transition.layer.RData' in the current work directory.")
	}
}

#' Calculate distance matrix
#' 
#' Adapted from Grant Adams' script "distance to closest mpa". (grant.adams@eagles.usm.edu)
#' https://cran.r-project.org/web/packages/gdistance/vignettes/gdistance1.pdf
#'
#' @param t.layer A .RData file containing a transition layer.
#' @param starters The points from which to start measuring the distance.
#' @param targets The points to which a way must be found.
#' @param EPSGcode The EPSG code corresponding to the coordinate system of the input data. All inputs must be in the same metric system. DO NOT use degree-based coordinates.
#' @param coord.x,coord.y The names of the columns containing the x and y information. Must be identical in the starters and targets.
#' @param PointIDCol The name of the column containing the IDs of the points to be used as starters and targets. Must be identical in both files.
#' @param actel logical: if TRUE, a distance matrix optimized for actel will be saved in the working directory.
#'
#' @return The distance matrix
#' 
#' @export
#' 
distancesMatrix <- function(t.layer = "transition.layer.RData", starters = NULL, targets = starters, EPSGcode, 
	coord.x = "x", coord.y = "y", PointIDCol = NA, actel = TRUE){
	list.of.packages <- c("raster", "gdistance", "sp", "tools", "rgdal")
	new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
	if (length(new.packages)>0) {
		stop(paste("This function requires packages '", paste(new.packages,collapse="', '"), 
			"' to operate. Please install them before proceeding.\n", sep = ""))
	}
	data.crs <- raster::crs(paste("+init=epsg:", EPSGcode, sep = ""))

	if (tools::file_ext(t.layer) == "RData") {
		load(t.layer)
		if (!exists("transition.layer")) stop(paste("Could not find a transition layer in '", t.layer, "'.\n", sep = ""))
	} else {
		stop(paste("'", t.layer, "' could not be recognised as .RData file, please make sure the file name is correct.\n", sep = ""))
	}

	if (actel)
		starters <- targets <- "spatial.csv"

	if (tools::file_ext(starters) != "csv" | tools::file_ext(targets) != "csv"){
		stop("One of the point files (starters or targets) does not appear to be writen in csv format.\n")
	}
	starters <- read.csv(starters) 
	
	if (actel) {
		message("M: Creating actel-compatible distances matrix."); flush.console()
		PointIDCol <- "Standard.Name"
	  starters$Standard.Name <- as.character(starters$Station.Name)
	  link <- starters$Type == "Hydrophone"
	  starters$Standard.Name[link] <- paste("St.", seq_len(sum(starters$Type == "Hydrophone")), sep = "")
	  targets = starters
	} else {
		targets <- read.csv(targets)
	}
	colnames(starters)[colnames(starters) == coord.x] <- "longitude"
	colnames(starters)[colnames(starters) == coord.y] <- "latitude"
	colnames(targets)[colnames(targets) == coord.x] <- "longitude"
	colnames(targets)[colnames(targets) == coord.y] <- "latitude"
	if (!is.na("PointIDCol")) {
		rename <- TRUE
		if (!is.na(match(PointIDCol, colnames(starters)))) outputRows <- starters[, PointIDCol] else rename <- FALSE
		if (!is.na(match(PointIDCol, colnames(targets )))) outputCols <- targets[, PointIDCol] else rename <- FALSE
	} else {
		rename <- FALSE
	}
	#### Process the "from" coordinates (this would be the starters ".csv" file)
	sp::coordinates(starters) <- ~ longitude + latitude # converts the file to a spatialPoints object
	raster::crs(starters) <- raster::crs(data.crs) # sets the crs to metres
	#### Process the "to" coordinates (this would be the targets ".csv" file)
	sp::coordinates(targets) <- ~ longitude + latitude # converts the file to a spatialPoints object
	raster::crs(targets) <- raster::crs(data.crs)
	#### Calculate a matrix of distances to each object
	dist.mat <- data.frame(gdistance::costDistance(transition.layer, starters, targets))
	if (rename) {
		rownames(dist.mat) <- outputRows
		colnames(dist.mat) <- outputCols
	}
	if (actel) {
		cat("M: Saving actel-compatible distances matrix as 'distances.csv'.\n"); flush.console()
		write.csv(dist.mat, "distances.csv", row.names = TRUE)
	}
	return(dist.mat)
}

#' Create an empty distances matrix
#' 
#' Creates a matrix based on the local 'spatial.csv' file and saves it to 'distances.csv' so the
#' user can manually fill it.
#' 
#' @export
#' 
emptyMatrix <- function(){
	if(!file.exists("spatial.csv"))
		stop("Could not find a 'spatial.csv' file in the current working directory.\n")

	input <- read.csv("spatial.csv")

	if (!any(grepl("Type", colnames(input))))
    stop("No 'Type' column found in 'spatial.csv'. Please identify receivers as 'Hydrophone' and release sites as 'Release' in a 'Type' column.\n")

	if (any(is.na(match(unique(input$Type), c("Hydrophone","Release")))))
    stop("Could not recognise the data in the 'Type' column as only one of 'Hydrophone' or 'Release'. Please doublecheck the spatial file.\n")

	if (sum(input$Type == "Release") == 0)
    stop("No release sites are present in 'spatial.csv'. At least one release site is necessary for speed calculations.\n")

  input <- setSpatialStandards(input)

  output <- matrix(nrow = nrow(input), ncol = nrow(input))
  colnames(output) <- rownames(output) <- input$Standard.Name

  for(i in 1:nrow(output))
  	output[i,i] = 0

  if (file.exists("distances.csv"))
  	decision <- readline("A file named 'distances.csv' is already present in the working directory. Do you want to overwrite it?(y/N) ")
  else 
  	decision <- "Y"

  if(decision == "Y" | decision == "y")
  	write.csv(output, file = "distances.csv", na = "", row.names = TRUE)
  else
  	cat("Aborting.\n")
}

#' Complete an half-filled distances matrix
#' 
#' Completes a matrix that has the upper diagonal half filled.
#' 
#' @export
#' 
completeMatrix <- function(){
	if (!file.exists("distances.csv"))
		stop("Could not find a 'distances.csv' file in the current working directory.\n")

	input <- read.csv("distances.csv", row.names = 1)

	for (i in 1:ncol(input)) {
		input[i:ncol(input), i] <- t(input[i, i:ncol(input)])
	}

	if (any(is.na(input)))
		stop("There are NA's in the upper diagonal of the matrix. Please fill in all values above the diagonal 0 line.\n")

	write.csv(input, file = "distances.csv", row.names = TRUE)
	cat("M: Distances matrix successfully completed and stored in 'distances.csv'.\n")
	print(input)
}
