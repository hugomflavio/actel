#' Calculate distance matrix
#' 
#' Adapted from Grant Adams' script "distance to closest mpa". (grant.adams@eagles.usm.edu)
#' https://cran.r-project.org/web/packages/gdistance/vignettes/gdistance1.pdf
#'
#' @param shape A shape file from which to create the transition layer.
#' @param x.res,y.res If map is a shapefile, sets the resolution for the creation of the transition layer. Ignored if map is a pre-rendered transition layer.
#' @param directions If map is a shapefile,the number of directions considered for every single movement situation during cost calculation. Ignored if map is a pre-rendered transition layer.
#' 
#' @export
#' 
transitionLayer <- function(shape, x.res, y.res, EPSGcode, directions = c(16,8,4)){
	directions <- as.character(directions)
	directions <- match.arg(directions)
	if (tools::file_ext(shape)=="shp") {
		shape <- sub(".shp", "", shape)
		shape <- rgdal::readOGR(dsn = ".", layer = shape, verbose = FALSE) #study area shapefile
	} else {
		stop("'shape' must be the name of a shape file present in the current directory.\n")
	}
	data.crs <- raster::crs(paste("+init=epsg:", EPSGcode, sep = ""))
	raster::crs(shape)<-raster::crs(data.crs) # Set CRS 
	ras <- raster::raster(nrow = y.res, ncol = x.res, crs = raster::crs(shape)) # create a recipient raster
	raster::extent(ras) <- raster::extent(shape) #Make the raster have the same extent as the shapefile
	#### "Burn" the shapefile into the raster
	cat("M: Constructing the transition layer. This process may take several minutes depending on the study area size and chosen resolution.\n"); flush.console()
	shape.mask <- raster::rasterize(shape, ras)
	project.raster <- is.na(shape.mask)
	project.raster[project.raster == 0] <- 10000 # make land extremely hard to cross
	#### The transition layer will be used as the shape for calculating least-cost distance
	transition.layer <- gdistance::transition(1 / project.raster, mean, directions = as.numeric(directions))
	transition.layer <- gdistance::geoCorrection(transition.layer, type = "c") # correct for shape distortion, as well as for diagonal connections between grid cells
	save(transition.layer,file="transition.layer.RData")
	cat("M: Saving the transition layer to 'transition.layer.RData' in the current work directory.\n"); flush.console()
}

#' Calculate distance matrix
#' 
#' Adapted from Grant Adams' script "distance to closest mpa". (grant.adams@eagles.usm.edu)
#' https://cran.r-project.org/web/packages/gdistance/vignettes/gdistance1.pdf
#'
#' @param t.layer A .RData file containing a transition layer.
#' @param starters The points from which to start measuring the distance.
#' @param targets The points to which a way must be found.
#' @param EPSGcode The EPSG code corresponding to the coordinate system of the imput data. All inputs must be in the same metric system. DO NOT use degree-based coordinates.
#' @param coord.x,coord.y The names of the columns containing the x and y information. Must be identical in the starters and targets.
#' @param PointIDCol The name of the column containing the IDs of the points to be used as starters and targets. Must be identical in both files.
#' @param actel logical: if TRUE, a distance matrix optimized for actel will be saved in the working directory.
#'
#' @return The distance matrix
#' 
#' @export
#' 
distanceMatrix <- function(t.layer, starters, targets = starters, EPSGcode, 
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
	if (tools::file_ext(starters) != "csv" | tools::file_ext(targets) != "csv"){
		stop("One of the point files (starters or targets) does not appear to be writen in csv format.\n")
	}
	starters <- read.csv(starters) 
	
	if (actel) {
		cat("M: Creating actel-compatible distance matrix.\n"); flush.console()
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
		cat("M: Saving actel-compatible distance matrix as 'distances.csv'.\n"); flush.console()
		write.csv(dist.mat, "distances.csv", row.names = TRUE)
	}
	return(dist.mat)
}


