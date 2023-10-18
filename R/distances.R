#' Load shapefile and convert to a raster object.
#'
#' shapeToRaster can also perform early quality checks on the shape file, to ensure it is compatible
#' with the remaining study data. To activate these, set the names of the columns in the spatial.csv
#' file that contain the x and y coordinates of the stations using coord.x and coord.y. By default,
#' shapeToRaster looks for a spatial.csv file in the current working directory, but this can be
#' personalized using the spatial argument.
#'
#' It is highly recommended to read the manual page regarding distances matrices before running this function.
#' You can find it here: \href{https://hugomflavio.github.io/actel-website/manual-distances.html}{https://hugomflavio.github.io/actel-website/manual-distances.html}
#'
#' @param shape The path to a shapefile containing land polygons of the study area.
#' @param size The pixel size, in metres.
#' @param spatial Either a character string specifying the path to a spatial.csv file or a spatial data frame.
#'  This argument is not mandatory, but can be used to perform an early check of the shape file's compatibility
#'  with the study stations and release sites.
#' @param coord.x,coord.y The names of the columns containing the x and y positions of the stations
#'  in the spatial.csv file. these coordinates MUST BE in the same coordinate system as the shape file.
#' @param buffer Artificially expand the map edges. Can be a single value (applied to all edges)
#'  or four values (xmin, xmax, ymin, ymax). The unit of the buffer depends on the shape file's
#'  coordinate system.
#' @param type The type of shapefile being loaded. One of "land", if the shapefile's polygons represent landmasses, or "water", if the shapefile's polygons represent water bodies.
#'
#' @examples
#' \donttest{
#' # check if R can run the distance functions
#' aux <- c(
#'   length(suppressWarnings(packageDescription("raster"))),
#'   length(suppressWarnings(packageDescription("gdistance"))),
#'   length(suppressWarnings(packageDescription("sp"))),
#'   length(suppressWarnings(packageDescription("terra"))))
#' 
#' missing.packages <- sapply(aux, function(x) x == 1)
#'
#' if (any(missing.packages)) {
#'   message("Sorry, this function requires packages '",
#'     paste(c("raster", "gdistance", "sp", "terra")[missing.packages], collapse = "', '"),
#'     "' to operate. Please install ", ifelse(sum(missing.packages) > 1, "them", "it"),
#'     " before proceeding.")
#' } else {
#'   # Fetch actel's example shapefile
#'   example.shape <- paste0(system.file(package = "actel")[1], "/example_shapefile.shp")
#'
#'   # import the shape file
#'   x <- shapeToRaster(shape = example.shape, size = 20)
#'
#'   # have a look at the resulting raster,
#'   # where the blank spaces are the land areas
#'   terra::plot(x)
#' }
#' rm(aux, missing.packages)
#' }
#' @return A raster object.
#'
#' @export
#'
shapeToRaster <- function(shape, size, spatial = "spatial.csv",
  coord.x = NULL, coord.y = NULL, buffer = NULL, type = c("land", "water")){
  # initial check on package presence
  aux <- c(
    length(suppressWarnings(packageDescription("raster"))),
    length(suppressWarnings(packageDescription("gdistance"))),
    length(suppressWarnings(packageDescription("sp"))),
    length(suppressWarnings(packageDescription("terra"))))
  missing.packages <- sapply(aux, function(x) x == 1)
  if (any(missing.packages)) {
    stop(paste0("This function requires packages '", paste(c("raster", "gdistance", "sp", "terra")[missing.packages], collapse = "', '"),
      "' to operate. Please install ", ifelse(sum(missing.packages) > 1, "them", "it"), " before proceeding.\n"), call. = FALSE)
  }

  type <- match.arg(type)

  if (!is.null(buffer) & length(buffer) != 4 & length(buffer) != 1)
    stop("'buffer' must either contain one value (applied to all four corners), or four values (applied to xmin, xmax, ymin and ymax, respectively).\n", call. = FALSE)
  if (!is.null(buffer) & !is.numeric(buffer))
    stop("'buffer' must be numeric (in metres or degrees, depending on the shape coordinate system).\n", call. = FALSE)
  if (any(buffer < 0))
    stop("'buffer' values cannot be negative.\n", call. = FALSE)

  if (is.null(coord.x) & !is.null(coord.y))
    warning("'coord.y' was set but 'coord.x' was not. Skipping range check.", call. = FALSE, immediate. = TRUE)
  if (!is.null(coord.x) & is.null(coord.y))
    warning("'coord.x' was set but 'coord.y' was not. Skipping range check.", call. = FALSE, immediate. = TRUE)

  # check if spatial information is present
  if (!is.null(coord.x) & !is.null(coord.y)) {
    check.spatial <- TRUE
    if (is.character(spatial)) {
      if (file.exists(spatial)) {
        spatial <- loadSpatial(spatial)
      } else {
        warning("Could not find a ", spatial, " file in the current working directory. Skipping range check.", call. = FALSE, immediate. = TRUE)
        check.spatial <- FALSE
        spatial <- NULL
      }
    }
    if (check.spatial) {
      if (any(is.na(xy <- match(c(coord.x, coord.y), colnames(spatial))))) {
        if (all(is.na(xy))) {
          warning("Could not find columns '", coord.x, "' and '", coord.y, "' in the spatial data frame. Skipping range check.", call. = FALSE, immediate. = TRUE)
        } else {
          if (is.na(xy[1]))
            warning("Could not find column '", coord.x, "' in the spatial data frame. Skipping range check.", call. = FALSE, immediate. = TRUE)
          else
            warning("Could not find column '", coord.y, "' in the spatial data frame. Skipping range check.", call. = FALSE, immediate. = TRUE)
        }
        spatial <- NULL
      }
    }
  } else {
    spatial <- NULL
  }

  # remaining input quality checks
  if (!file.exists(shape))
    stop(paste0("Could not find file '", shape, "'.\n"), call. = FALSE)

  # load shape file
  if (tools::file_ext(shape) == "shp") {
    shape <- terra::vect(x = shape)
  } else {
    stop("'shape' must be a .shp file.\n", call. = FALSE)
  }
  
  # extend ranges with the buffer
  if (!is.null(buffer)) {
    OriEx <- terra::ext(shape)
    # terra does xmin, xmax, ymin, ymax
    
    if (length(buffer) == 1){
      NewEx <- terra::ext(OriEx[1] - buffer, #xmin
                          OriEx[2] + buffer, #xmax
                          OriEx[3] - buffer, #ymin
                          OriEx[4] + buffer) #ymax
    } else {
      NewEx <- terra::ext(OriEx[1] - buffer[1], #xmin
                          OriEx[2] + buffer[2], #xmax
                          OriEx[3] - buffer[3], #ymin
                          OriEx[4] + buffer[4]) #ymax
    }
  } else {
    NewEx <- terra::ext(shape)
  }

  # Compare shape range with station positioning
  issue.message.1 <- FALSE
  if (!is.null(spatial)) {
    xmax <- max(spatial[, xy[1]]) + size
    xmin <- min(spatial[, xy[1]]) - size
    ymax <- max(spatial[, xy[2]]) + size
    ymin <- min(spatial[, xy[2]]) - size

    if (NewEx[1] > xmin) {
      issue.message.1 <- TRUE
      NewEx[1] <- xmin
    }
    if (NewEx[2] < xmax) {
      issue.message.1 <- TRUE
      NewEx[2] <- xmax
    }
    if (NewEx[3] > ymin) {
      issue.message.1 <- TRUE
      NewEx[3] <- ymin
    }
    if (NewEx[4] < ymax) {
      issue.message.1 <- TRUE
      NewEx[4] <- ymax
    }
    if (issue.message.1)
      message("M: Extending the shape ranges with open water to ensure the stations fit inside it.")
  }

  # ensure range allows for integer pixels
  issue.message.2 <- FALSE

  fix.x <- ((NewEx[2] - NewEx[1]) %% size) / 2
  if (fix.x != 0) {
    issue.message.2 <- TRUE
    NewEx[1] <- NewEx[1] + fix.x #xmin
    NewEx[2] <- NewEx[2] - fix.x #xmax
  }

  fix.y <- ((NewEx[4] - NewEx[3]) %% size) / 2
  if (fix.y != 0) {
    issue.message.2 <- TRUE
    NewEx[3] <- NewEx[3] + fix.y #ymin
    NewEx[4] <- NewEx[4] - fix.y #ymax
  }

  if (issue.message.2)
    message("M: Applying a small correction to the shape extent to ensure an integer number of pixels.")

  if (issue.message.1 | issue.message.2) {
    message("M: New shape extent:")
    message(paste0(capture.output(print(NewEx)), collapse = "\n"), "\n")
  }

  pixel.res <- (NewEx[c(2, 4)] - NewEx[c(1, 3)]) / size
  message(paste("M: Chosen pixel size:", size, "\nM: Resulting pixel dimensions:"))
  message(paste0(capture.output(print(pixel.res)), collapse = "\n"), "\n")

  ras <- terra::rast(nrows = pixel.res[2], 
                    ncols = pixel.res[1],
                    xmin = NewEx[1],
                    xmax = NewEx[2],
                    ymin = NewEx[3],
                    ymax = NewEx[4],
                    crs = terra::crs(shape)) # create a recipient raster


  #### "Burn" the shapefile into the raster
  message("M: Burning the shape into a raster. This process may take several minutes depending on the shape size and chosen pixel size."); flush.console()
  shape.mask <- terra::rasterize(x = shape, y = ras)

  project.raster <- is.na(shape.mask)
  project.raster[project.raster == 0] <- NA # make land impossible to cross

  if (type == "water") {
    # invert raster
    project.raster[is.na(project.raster)] <- 2
    project.raster[project.raster == 1] <- NA
    project.raster[project.raster == 2] <- 1
  }

  # check if stations are in water
  if (!is.null(spatial)) {
    sp_points <- terra::vect(spatial, geom = c(coord.x, coord.y), crs = terra::crs(project.raster))

    check <- terra::extract(project.raster, sp_points)

    if (any(is.na(check))) {
      one <- sum(is.na(check)) == 1
      warning(ifelse(one, "Station '", "Stations '"), paste(spatial$Station.name[is.na(check$layer)], collapse = "', '"),
        ifelse(one, "' is", "' are"), " not placed in water! This can cause several problems.", call. = FALSE, immediate. = TRUE)
    }
  }

  return(project.raster)
}


#' DEPRECATED
#' 
#' Please use shapeToRaster instead.
#' 
#' @inheritParams shapeToRaster
#'
#' @examples
#' \donttest{
#'  message("This function is deprecated, please use shapeToRaster instead.")
#' }
#' 
#' @return A raster object.
#'
#' @export
#'
loadShape <- function(shape, size, spatial = "spatial.csv",
  coord.x = NULL, coord.y = NULL, buffer = NULL, type = c("land", "water")) {

  .Deprecated(shapeToRaster, package=NULL, "loadShape is deprecated. Please use shapeToRaster instead. This function will stop working once the next version of actel is released.",
              old = as.character(sys.call(sys.parent()))[1L])

  output <- shapeToRaster(shape = shape, size = size, spatial = spatial,
                          coord.x = coord.x, coord.y = coord.y, buffer = buffer, type = type)

  return(output)
}


#' Calculate Transition Layer
#'
#' Using a previously imported shape file that has been converted to a raster (see \code{\link{shapeToRaster}}),
#' Prepares a TransitionLayer object to be used in distance
#' estimations (see \code{\link{distancesMatrix}}). Adapted from Grant Adams' script "distance to closest mpa".
#'
#' It is highly recommended to read the manual page regarding distances matrices before running this function.
#' You can find it here: \href{https://hugomflavio.github.io/actel-website/manual-distances.html}{https://hugomflavio.github.io/actel-website/manual-distances.html}
#'
#' @param x A water raster; for example the output of \code{\link{shapeToRaster}}
#' @param directions The number of directions considered for every movement situation during cost
#'  calculation. See the manual page linked above for more details.
#'
#' @examples
#' \donttest{
#' # check if R can run the distance functions
#' aux <- c(
#'   length(suppressWarnings(packageDescription("raster"))),
#'   length(suppressWarnings(packageDescription("gdistance"))),
#'   length(suppressWarnings(packageDescription("sp"))),
#'   length(suppressWarnings(packageDescription("terra"))))
#' 
#' missing.packages <- sapply(aux, function(x) x == 1)
#'
#' if (any(missing.packages)) {
#'   message("Sorry, this function requires packages '",
#'     paste(c("raster", "gdistance", "sp", "terra")[missing.packages], collapse = "', '"),
#'     "' to operate. Please install ", ifelse(sum(missing.packages) > 1, "them", "it"),
#'     " before proceeding.")
#' } else {
#'   # Fetch actel's example shapefile
#'   example.shape <- paste0(system.file(package = "actel")[1], "/example_shapefile.shp")
#'
#'   # import the shape file
#'   x <- shapeToRaster(shape = example.shape, size = 20)
#'
#'   # Build the transition layer
#'   t.layer <- transitionLayer(x)
#'
#'   # inspect the output
#'   t.layer
#' }
#' rm(aux, missing.packages)
#' }
#' @return A TransitionLayer object.
#'
#' @export
#'
transitionLayer <- function(x, directions = c(16, 8, 4)){
  # initial checks on package presence
  aux <- c(
    length(suppressWarnings(packageDescription("raster"))),
    length(suppressWarnings(packageDescription("gdistance"))),
    length(suppressWarnings(packageDescription("sp"))),
    length(suppressWarnings(packageDescription("terra"))))
  missing.packages <- sapply(aux, function(x) x == 1)
  if (any(missing.packages)) {
    stop(paste0("This function requires packages '", paste(c("raster", "gdistance", "sp", "terra")[missing.packages], collapse = "', '"),
      "' to operate. Please install ", ifelse(sum(missing.packages) > 1, "them", "it"), " before proceeding.\n"), call. = FALSE)
  }

  # argument quality
  directions <- as.character(directions[1])
  directions <- match.arg(directions)

  #### The transition layer will be used as the shape for calculating least-cost distance
  message("M: Constructing the transition layer. This process may take several minutes depending on the study area size and chosen pixel size."); flush.console()

  transition.layer <- gdistance::transition(raster::raster(x), transitionFunction = mean, directions = as.numeric(directions))
  transition.layer <- gdistance::geoCorrection(transition.layer, type = "c") # correct for shape distortion, as well as for diagonal connections between grid cells
  return(transition.layer)
}

#' Calculate Distances Matrix
#'
#' Using a previously created transition layer (see \code{\link{transitionLayer}}), calculates the distances
#' between spatial points. Adapted from Grant Adams' script "distance to closest mpa". if the argument 'actel'
#' is set to TRUE (default), an actel-compatible matrix is generated, and the user will be asked if they would
#' like to save the matrix as 'distances.csv' in the current directory.
#'
#' It is highly recommended to read the manual page regarding distances matrices before running this function.
#' You can find it here: \href{https://hugomflavio.github.io/actel-website/manual-distances.html}{https://hugomflavio.github.io/actel-website/manual-distances.html}
#'
#' @param t.layer A TransitionLayer object, generated by \code{\link{transitionLayer}}.
#' @param starters A data frame with the points from which to start measuring the distance. Ignored if actel = TRUE (default), as the 'spatial.csv' is loaded as starters.
#' @param targets A data frame with the points to which a way must be found. Ignored if actel = TRUE (default), as the 'spatial.csv' is loaded as targets.
#' @param coord.x,coord.y The names of the columns containing the x and y coordinates in the starters and targets. Must be identical in the starters and targets.
#' @param id.col The name of the column containing the IDs of the points to be used as starters and targets. Must be identical in both files. Ignored if actel = TRUE (default), as the stations' standard names are used.
#' @param actel Logical: Should the distance matrix be optimized for actel? Defaults to TRUE.
#'
#' @examples
#' \donttest{
#' # check if R can run the distance functions
#' aux <- c(
#'   length(suppressWarnings(packageDescription("raster"))),
#'   length(suppressWarnings(packageDescription("gdistance"))),
#'   length(suppressWarnings(packageDescription("sp"))),
#'   length(suppressWarnings(packageDescription("terra"))))
#' 
#' missing.packages <- sapply(aux, function(x) x == 1)
#'
#' if (any(missing.packages)) {
#'   message("Sorry, this function requires packages '",
#'     paste(c("raster", "gdistance", "sp", "terra")[missing.packages], collapse = "', '"),
#'     "' to operate. Please install ", ifelse(sum(missing.packages) > 1, "them", "it"),
#'     " before proceeding.")
#' } else {
#'   # move to a temporary directory
#'   old.wd <- getwd()
#'   setwd(tempdir())
#'
#'   # Fetch location of actel's example files
#'   aux <- system.file(package = "actel")[1]
#'
#'   # create a temporary spatial.csv file
#'   file.copy(paste0(aux, "/example_spatial.csv"), "spatial.csv")
#'
#'   # import the shape file and use the spatial.csv
#'   # to check the extents.
#'   x <- shapeToRaster(shape = paste0(aux, "/example_shapefile.shp"),
#'     coord.x = "x", coord.y = "y", size = 20)
#'
#'   raster::plot(x)
#'
#'   # Build the transition layer
#'   t.layer <- transitionLayer(x)
#'
#'   # compile the distances matrix. Columns x and y in the spatial dataframe
#'   # contain the coordinates of the stations and release sites.
#'   distancesMatrix(t.layer, coord.x = 'x', coord.y = 'y')
#'
#'   # return to original directory
#'   setwd(old.wd)
#'   rm(old.wd)
#' }
#' rm(aux, missing.packages)
#' }
#'
#' @return A matrix with the distances between each pair of points.
#'
#' @export
#'
distancesMatrix <- function(t.layer, starters = NULL, targets = starters,
  coord.x = "x", coord.y = "y", id.col = NULL, actel = TRUE){
  # initial checks on package presence
  aux <- c(
    length(suppressWarnings(packageDescription("raster"))),
    length(suppressWarnings(packageDescription("gdistance"))),
    length(suppressWarnings(packageDescription("sp"))),
    length(suppressWarnings(packageDescription("terra"))))
  missing.packages <- sapply(aux, function(x) x == 1)
  if (any(missing.packages)) {
    stop(paste0("This function requires packages '", paste(c("raster", "gdistance", "sp", "terra")[missing.packages], collapse = "', '"),
      "' to operate. Please install ", ifelse(sum(missing.packages) > 1, "them", "it"), " before proceeding.\n"), call. = FALSE)
  }

  if (!inherits(t.layer, "TransitionLayer"))
    stop("Could not recognise 't.layer' as a TransitionLayer object. Make sure to compile it using the function transitionLayer.\n", call. = FALSE)

  if (!is.null(id.col) && length(id.col) > 1)
    stop("Please provide only one column name in 'id.col'", call. = FALSE)
  if (!is.null(id.col) && is.numeric(id.col))
    stop("Please refer to the column name in 'id.col', rather than the column index.\n", call. = FALSE)

  if (actel) {
    message("M: Creating actel-compatible distances matrix."); flush.console()
    if (!is.null(starters) | !is.null(targets))
      warning("starters' or 'targets' were set but will be ignored because 'actel' is set to TRUE. Set 'actel' to FALSE to use the 'starters' and 'targets' arguments.", call. = FALSE, immediate. = TRUE)
    starters <- targets <- loadSpatial()
    if (!is.null(id.col))
      warning("id.col' was set but will be ignored because 'actel' is set to TRUE. Set 'actel' to FALSE to use the 'id.col' argument.", call. = FALSE, immediate. = TRUE)
    id.col <- "Standard.name"
  }

  if (!inherits(starters, "data.frame"))
    stop("'starters' must be a data frame.\n", call. = FALSE)
  if (!inherits(targets, "data.frame"))
    stop("'targets' must be a data frame.\n", call. = FALSE)

  if (is.na(match(coord.x, colnames(starters))))
    stop(paste0("Could not find a column '", coord.x, "' in 'starters'."), call. = FALSE)
  if (is.na(match(coord.y, colnames(starters))))
    stop(paste0("Could not find a column '", coord.y, "' in 'starters'."), call. = FALSE)
  if (is.na(match(coord.x, colnames(targets))))
    stop(paste0("Could not find a column '", coord.x, "' in 'targets'."), call. = FALSE)
  if (is.na(match(coord.y, colnames(targets))))
    stop(paste0("Could not find a column '", coord.y, "' in 'targets'."), call. = FALSE)

    starters <- starters[, c(id.col, coord.x, coord.y)]
    colnames(starters) <- c(id.col, "longitude", "latitude")

    targets <- targets[, c(id.col, coord.x, coord.y)]
    colnames(targets) <- c(id.col, "longitude", "latitude")
    
  if (!is.null(id.col)) {
    if (!is.na(match(id.col, colnames(starters)))) {
      outputRows <- starters[, id.col]
      if (any(duplicated(outputRows))) {
        warning("The '", id.col, "' column in 'starters' contains duplicated values; skipping row naming.", immediate. = TRUE, call. = FALSE)
        row.rename <- FALSE
      } else {
        row.rename <- TRUE
      }
    } else {
      warning("Could not find a '", id.col, "' column in 'starters'; skipping row naming.", immediate. = TRUE, call. = FALSE)
      row.rename <- FALSE
    }
    if (!is.na(match(id.col, colnames(targets)))) {
      outputCols <- targets[, id.col]
      if (any(duplicated(outputCols))) {
        warning("The '", id.col, "' column in 'targets' contains duplicated values; skipping column naming.", immediate. = TRUE, call. = FALSE)
        col.rename <- FALSE
      } else {
        col.rename <- TRUE
      }
    } else {
      warning("Could not find a '", id.col, "' column in 'targets'; skipping column naming.", immediate. = TRUE, call. = FALSE)
      col.rename <- FALSE
    }
  } else {
    row.rename <- FALSE
    col.rename <- FALSE
  }


  #### Create starters and targets spatial dataframes
  sp::coordinates(starters) <- ~ longitude + latitude # converts the file to a spatialPoints object
  raster::crs(starters) <- raster::crs(t.layer) # sets the crs
  
  sp::coordinates(targets) <- ~ longitude + latitude # converts the file to a spatialPoints object
  raster::crs(targets) <- raster::crs(t.layer)

  # NOTE: THE LINES ABOVE COULD BE CHANGED ONCE gdistance'S 
  # FUNCTIONS START LIKING SF OBJECTS LAYER
  # starters <- sf::st_as_sf(starters, coords = c("longitude","latitude"), crs = ...)
  # targets <- sf::st_as_sf(targets, coords = c("longitude","latitude"), crs = ...)
  # NOTE: currently, transition layer objects are not 
  # responding correctly to crs requests (e.g. sf::st_crs)

  #### Calculate a matrix of distances to each object
  dist.mat <- data.frame(gdistance::costDistance(t.layer, starters, targets))
  if (any(dist.mat == Inf)) {
    warning("At least one station is completely blocked off from the remaining stations by land. Filling
the respective fields with NA. If your animals were expected to travel around the areas present
in the shape file, consider applying a 'buffer' when calculating the transition layer. This
will artificially add water space around the shape file.", call. = FALSE)
    dist.mat[dist.mat == Inf] <- NA
  }
  
  if (row.rename)
    rownames(dist.mat) <- outputRows
  
  if (col.rename)
    colnames(dist.mat) <- outputCols
  
  if (interactive() & actel) { # nocov start
    decision <- userInput("Would you like to save an actel-compatible distances matrix as 'distances.csv' in the current work directory?(y/n) ",
                          choices = c("y", "n"))
    if (decision == "y") {
      if (file.exists('distances.csv')) {
        warning("A file 'distances.csv' is already present in the current directory.", call. = FALSE, immediate. = TRUE)
        decision <- userInput("Continuing will overwrite this file. Would you like to continue?(y/n) ", choices = c("y", "n"))
      }
      if (decision == "y")
        write.csv(round(dist.mat, 0), "distances.csv", row.names = TRUE)
    }
  } # nocov end
  return(round(dist.mat, 0))
}

#' Create a Template Distances Matrix
#'
#' Creates an empty matrix based on the local 'spatial.csv' file and saves it to 'distances.csv' so the
#' user can manually fill it.
#'
#' It is highly recommended to read the manual page regarding distances matrices before running this function.
#' You can find it here: \href{https://hugomflavio.github.io/actel-website/manual-distances.html}{https://hugomflavio.github.io/actel-website/manual-distances.html}
#'
#' @param input Either a data frame with spatial data or the path to the file containing the spatial information.
#'
#' @examples
#' # This function requires a file with spatial information
#'
#' # Fetch location of actel's example files
#' aux <- system.file(package = "actel")[1]
#'
#' # run emptyMatrix on the temporary spatial.csv file
#' emptyMatrix(paste0(aux, "/example_spatial.csv"))
#'
#' @return An empty matrix with the rows and columns required to
#'  operate with the target spatial file.
#'
#' @export
#'
emptyMatrix <- function(input = "spatial.csv"){
  if (is.character(input)) {
    if(!file.exists(input))
      stop("Could not find file '", input, "'.\n", call. = FALSE)
  }

  input <- loadSpatial(input = input)

  output <- matrix(nrow = nrow(input), ncol = nrow(input))
  colnames(output) <- rownames(output) <- input$Standard.name

  for(i in 1:nrow(output))
    output[i, i] = 0

  return(output)
}

#' Complete a Distances Matrix
#'
#' Completes the bottom diagonal of a matrix with the same number of rows and columns.
#'
#' It is highly recommended to read the manual page regarding distances matrices before running this function.
#' You can find it here: \href{https://hugomflavio.github.io/actel-website/manual-distances.html}{https://hugomflavio.github.io/actel-website/manual-distances.html}
#'
#' @param x A distances matrix to be completed.
#'
#' @examples
#' # Create dummy matrix with upper diagonal filled.
#' x <- matrix(
#'  c( 0,  1,  2,  3,  4, 5,
#'    NA,  0,  1,  2,  3, 4,
#'    NA, NA,  0,  1,  2, 3,
#'    NA, NA, NA,  0,  1, 2,
#'    NA, NA, NA, NA,  0, 1,
#'    NA, NA, NA, NA, NA, 0),
#'  ncol = 6, byrow = TRUE)
#'
#' # inspect x
#' x
#'
#' # run completeMatrix
#' completeMatrix(x)
#'
#' @return A matrix of distances between pairs of points.
#'
#' @export
#'
completeMatrix <- function(x){
  if (!inherits(x, "matrix"))
    stop("The input must be a matrix", call. = FALSE)
  if (nrow(x) != ncol(x))
    stop("The matrix does not contain the same number of columns and rows. Aborting.", call. = FALSE)

  for (i in 1:ncol(x)) {
    x[i:ncol(x), i] <- t(x[i, i:ncol(x)])
  }
  return(x)
}

 
