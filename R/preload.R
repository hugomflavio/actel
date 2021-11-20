#' Load a dataset before running an analysis
#'
#' This function allows the user to prepare a set of R objects to be run through
#' an \code{\link{explore}}, \code{\link{migration}} or \code{\link{residency}}
#' analysis.
#'
#' @inheritParams explore
#' @inheritParams migration
#' @param biometrics A data frame containing biometric information.
#' @param spatial A data frame containing spatial information.
#' @param deployments A data frame containing deployment information.
#' @param detections A data frame containing the detections.
#' @param dot A DOT string of the array configuration.
#' @param distances A distances matrix between arrays. See \code{\link{distancesMatrix}}.
#'
#' @export
#'
#' @examples
#' # This function requires a series of pre-created R objects.
#' # We can create them by loading the example files from actel:
#' aux <- system.file(package = "actel")[1]
#'
#' bio <- read.csv(paste0(aux, "/example_biometrics.csv"))
#' deployments <- read.csv(paste0(aux, "/example_deployments.csv"))
#' spatial <- read.csv(paste0(aux, "/example_spatial.csv"))
#' detections <- read.csv(paste0(aux, "/example_detections.csv"))
#'
#' dot <- "A0--A1--A2--A3--A4--A5--A6--A7--A8--A9"
#'
#' # Now that we have the R objects created, we can run preload:
#'
#' x <- preload(biometrics = bio, deployments = deployments, spatial = spatial,
#'  detections = detections, dot = dot, tz = "Europe/Copenhagen")
#'
#'
#' @return A dataset that can be used as an input for actel's main analyses.
#' This dataset contains:
#' \itemize{
#'  \item \code{bio}: The biometric data
#'  \item \code{sections}: The sections of the study area, if set using the argument sections (required to run residency and migration analyses)
#'  \item \code{deployments}: The deployment data
#'  \item \code{spatial}: The spatial data, split in stations and release sites.
#'  \item \code{dot}: A table of array connections.
#'  \item \code{arrays}: A list with the details of each array
#'  \item \code{dotmat}: A matrix of distances between arrays (in number of arrays).
#'  \item \code{dist.mat}: The distances matrix.
#'  \item \code{detections.list}: A processed list of detections for each tag.
#'  \item \code{paths}: A list of the possible paths between each pair of arrays.
#'  \item \code{disregard.parallels}: Logical: Should parallel arrays invalidate efficiency peers? (required to run residency and migration analyses)
#'  \item \code{tz}: The time zone of the study area
#' }
#'
preload <- function(biometrics, spatial, deployments, detections, dot = NULL, distances = NULL, tz,
  start.time = NULL, stop.time = NULL, section.order = NULL, exclude.tags = NULL,
  disregard.parallels = FALSE, discard.orphans = FALSE) {

  deleteHelpers()

# debug lines
  if (getOption("actel.debug", default = FALSE)) { # nocov start
    on.exit(message("Debug: Progress log available at ", gsub("\\\\", "/", paste0(tempdir(), "/actel_debug_file.txt"))))
    on.exit(message("Debug: Saving carbon copy to ", gsub("\\\\", "/", paste0(tempdir(), "/actel.preload.debug.RData"))), add = TRUE)
    on.exit(save(list = ls(), file = paste0(tempdir(), "/actel.preload.debug.RData")), add = TRUE)
    message("!!!--- Debug mode has been activated ---!!!")
  } # nocov end
# ------------------------

# Store function call
  the.function.call <- paste0("preload(biometrics = ", deparse(substitute(biometrics)),
    ", spatial = ", deparse(substitute(spatial)),
		", deployments = ", deparse(substitute(deployments)),
		", detections = ", deparse(substitute(detections)),
		", dot = ", ifelse(is.null(dot), "NULL", deparse(substitute(dot))),
		", distances = ", ifelse(is.null(distances), "NULL", deparse(substitute(distances))),
		", tz = '", tz, "'",
    ", start.time = ", ifelse(is.null(start.time), "NULL", paste0("'", start.time, "'")),
    ", stop.time = ", ifelse(is.null(stop.time), "NULL", paste0("'", stop.time, "'")),
		", section.order = ", ifelse(is.null(section.order), "NULL", paste0("c('", paste(section.order, collapse = "', '"), "')")),
    ", exclude.tags = ", ifelse(is.null(exclude.tags), "NULL", paste0("c('", paste(exclude.tags, collapse = "', '"), "')")),
    ", disregard.parallels = ", ifelse(disregard.parallels, "TRUE", "FALSE"),
    ", discard.orphans = ", ifelse(discard.orphans, "TRUE", "FALSE"),
    ")")

  appendTo("debug", the.function.call)
# ------------------------

  if (is.na(match(tz, OlsonNames())))
    stop("'tz' could not be recognized as a timezone. Check available timezones with OlsonNames()\n", call. = FALSE)

  if (!is.null(exclude.tags) && any(!grepl("-", exclude.tags, fixed = TRUE)))
    stop("Not all contents in 'exclude.tags' could be recognized as tags (i.e. 'codespace-signal'). Valid examples: 'R64K-1234', A69-1303-1234'\n", call. = FALSE)

  if (!is.null(section.order) && any(table(section.order) > 1))
    stop("Some section names are duplicated in the 'section.order' argument. Please include each section only once.\n", call. = FALSE)

  bio <- loadBio(input = biometrics, tz = tz)

  appendTo(c("Screen", "Report"), paste0("M: Number of target tags: ", nrow(bio), "."))

  deployments <- loadDeployments(input = deployments, tz = tz)
  checkDeploymentTimes(input = deployments) # check that receivers are not deployed before being retrieved

  spatial <- loadSpatial(input = spatial, section.order = section.order)

  deployments <- checkDeploymentStations(input = deployments, spatial = spatial) # match Station.name in the deployments to Station.name in spatial, and vice-versa
  deployments <- createUniqueSerials(input = deployments) # Prepare serial numbers to overwrite the serials in detections


  detections <- preloadDetections(input = detections, tz = tz, start.time = start.time, stop.time = stop.time)
  detections <- checkDupDetections(input = detections)

  if (is.null(dot)) {
    n <- length(unique(spatial$Array[spatial$Type == "Hydrophone"]))
    if (n > 1) {
      fakedot <- paste(unique(spatial$Array[spatial$Type == "Hydrophone"]), collapse = "--")
    } else {
      aux <- unique(spatial$Array[spatial$Type == "Hydrophone"])
      fakedot <- paste(aux, "--", aux)
    }
    recipient <- loadDot(string = fakedot, spatial = spatial, disregard.parallels = disregard.parallels, preloading = TRUE)
  } else {
  	if (!is.character(dot))
  		stop("'dot' was set but could not recognised as a string. Please prepare a dot string and include it in the dot argument.\nYou can use readDot to check the quality of your dot string.", call. = FALSE)
  	else
  		recipient <- loadDot(string = dot, spatial = spatial, disregard.parallels = disregard.parallels, preloading = TRUE)
  }
  dot <- recipient$dot
  arrays <- recipient$arrays
  dotmat <- recipient$dotmat
  paths <- recipient$paths
  if (is.null(dot) | is.null(arrays) | is.null(dotmat) | is.null(paths))
    stop("Something went wrong when assigning recipient objects (loadDot). If this error persists, contact the developer.") # nocov

  rm(recipient)

  # Check if there is a logical first array in the study area, should a replacement release site need to be created.
  if (sum(unlist(lapply(arrays, function(a) is.null(a$before)))) == 1)
    first.array <- names(arrays)[unlist(lapply(arrays, function(a) is.null(a$before)))]
  else
    first.array <- NULL
  spatial <- transformSpatial(spatial = spatial, bio = bio, arrays = arrays, dotmat = dotmat, first.array = first.array) # Finish structuring the spatial file

  detections <- createStandards(detections = detections, spatial = spatial, deployments = deployments, discard.orphans = discard.orphans) # get standardized station and receiver names, check for receivers with no detections
  appendTo(c("Screen", "Report"), paste0("M: Data time range: ", as.character(head(detections$Timestamp, 1)), " to ", as.character(tail(detections$Timestamp, 1)), " (", tz, ")."))
  checkUnknownReceivers(input = detections) # Check if there are detections from unknown receivers

  arrays <- liveArrayTimes(arrays = arrays, deployments = deployments, spatial = spatial)

  # Reorder arrays by spatial order
  link <- match(unlist(spatial$array.order), names(arrays))
  arrays <- arrays[link]
  rm(link)

  if (is.null(distances)) {
  	dist.mat <- NA
    attributes(dist.mat)$valid <- FALSE
  } else {
	  dist.mat <- loadDistances(input = distances, spatial = spatial) # Load distances and check if they are valid
  }

  recipient <- splitDetections(detections = detections, bio = bio, exclude.tags = exclude.tags) # Split the detections by tag, store full transmitter names in bio
  detections.list <- recipient$detections.list
  bio <- recipient$bio
  if (is.null(detections.list) | is.null(bio))
    stop("Something went wrong when assigning recipient objects (splitDetections). If this error persists, contact the developer.\n", call. = FALSE) # nocov
  rm(recipient)

  recipient <- checkTagsInUnknownReceivers(detections.list = detections.list, deployments = deployments, spatial = spatial) # Check if there is any data loss due to unknown receivers
  spatial <- recipient$spatial
  deployments <- recipient$deployments
  detections.list <- recipient$detections.list
  if (is.null(spatial) | is.null(deployments) | is.null(detections.list))
    stop("Something went wrong when assigning recipient objects (unknownReceivers). If this error persists, contact the developer.\n", call. = FALSE)# nocov
  rm(recipient)

  detections.list <- checkDetectionsBeforeRelease(input = detections.list, bio = bio, discard.orphans = discard.orphans)
  appendTo(c("Screen", "Report"), "M: Data successfully imported!")

  output <- list(bio = bio, deployments = deployments, spatial = spatial, dot = dot,
   arrays = arrays, dotmat = dotmat, dist.mat = dist.mat, detections.list = detections.list, 
   paths = paths, disregard.parallels = disregard.parallels, tz = tz)

	# create actel token
	actel.token <- stringi::stri_rand_strings(1, 10)
	timestamp <- as.character(Sys.time())

	key <- data.frame(Token = actel.token, Timestamp = timestamp)

	# save token
	write.table(key, paste0(tempdir(), "/actel_token_list.csv"), sep = ",",
		append = file.exists(paste0(tempdir(), "/actel_token_list.csv")),
		col.names = !file.exists(paste0(tempdir(), "/actel_token_list.csv")),
		row.names = FALSE)

	# stamp the output
	attributes(output)$actel.token <- actel.token
	attributes(output)$timestamp <- timestamp

  # carbon copy report messages
  attributes(output)$loading_messages <- readLines(paste0(tempdir(), "/temp_log.txt"))
  attributes(output)$function_call <- the.function.call
	return(output)
}


#' Run quality checks on preloaded detections
#'
#' @param input The detection data frame
#' @inheritParams explore
#'
#' @return A data frame with all the detections
#'
#' @keywords internal
#'
preloadDetections <- function(input, tz, start.time = NULL, stop.time = NULL) {
	mandatory.cols <- c("Timestamp", "Receiver", "CodeSpace", "Signal")

	if (any(link <- is.na(match(mandatory.cols, colnames(input))))) {
		if (any(is.na(match(mandatory.cols[3:4], colnames(input)))))
			also.say <- "\nThe functions extractSignals and extractCodeSpaces can be used to break transmitter codes apart."
		else
			also.say <- ""

		stop("The following mandatory columns are missing in the detections: ", paste(mandatory.cols[link], collapse = ", "), also.say, call. = FALSE)
	}

  if (any(link <- apply(input[, mandatory.cols], 2, function(i) any(is.na(i)))))
    stop("There is missing data in the following mandatory columns of the detections: ", paste0(mandatory.cols[link], collapse = ", "), call. = FALSE)

	if (!is.integer(input$Signal)) {
		appendTo(c("Screen", "Warning", "Report"), "The 'Signal' column in the detections is not of type integer. Attempting to convert.")
		input$Signal <- tryCatch(as.integer(input$Signal),
			warning = function(w) stop("Attempting to convert the 'Signal' to integer failed. Aborting.", call. = FALSE))
	}

	if (!is.integer(input$Receiver)) {
		appendTo(c("Screen", "Warning", "Report"), "The 'Receiver' column in the detections is not of type integer. Attempting to convert.")
		aux <- tryCatch(as.integer(input$Receiver),
			warning = function(w) {
				appendTo(c("Screen", "Warning", "Report"), "Attempting to convert the 'Receiver' to integer failed. Attempting to extract only the serial numbers.")
				return(NULL) })
		if (is.null(aux)) {
			input$Receiver <- extractSignals(input$Receiver) # this works for extracting only the receiver numbers too
			aux <- tryCatch(as.integer(input$Receiver),
				warning = function(w) stop("Extracting the serial numbers failed. Aborting.", call. = FALSE))
		}
		input$Receiver <- aux
	}

  if (!is.character(input$Timestamp))
    input$Timestamp <- as.character(input$Timestamp)

  if (!is.character(input$CodeSpace))
    input$CodeSpace <- as.character(input$CodeSpace)

	if (length(the.col <- grep("^[S|s]ensor.[U|u]nit$", colnames(input))) > 0)
		colnames(input)[the.col] <- "Sensor.Unit"

	if (length(the.col <- grep("^[S|s]ensor.[V|v]alue$", colnames(input))) > 0)
		colnames(input)[the.col] <- "Sensor.Value"
	
	if (!any(grepl("^Sensor.Value$", colnames(input)))) {
		appendTo(c("Screen", "Warning", "Report"), "Could not find a 'Sensor.Value' column in the detections. Filling one with NA.")
		input$Sensor.Value <- NA_real_
	}

	if (!any(grepl("^Sensor.Unit$", colnames(input)))) {
		appendTo(c("Screen", "Warning", "Report"), "Could not find a 'Sensor.Unit' column in the detections. Filling one with NA.")
		input$Sensor.Unit <- NA_character_
	}

	if (!is.numeric(input$Sensor.Value)) {
		appendTo(c("Screen", "Warning", "Report"), "The 'Sensor.Value' column in the detections is not of type numeric. Attempting to convert.")
		input$Sensor.Value <- tryCatch(as.numeric(input$Sensor.Value),
			warning = function(w) stop("Attempting to convert the 'Sensor.Value' to numeric failed. Aborting.", call. = FALSE))
	}

	if (!inherits(input$Timestamp, "POSIXct")) {
		appendTo(c("Screen", "Report"), "M: Converting detection timestamps to POSIX objects.")
		input$Timestamp <- fasttime::fastPOSIXct(input$Timestamp, tz = "UTC")
    if (any(is.na(input$Timestamp)))
      stop("Converting the timestamps failed. Aborting.", call. = FALSE)
	}

	attributes(input$Timestamp)$tzone <- tz

  input <- input[order(input$Timestamp), ]

  if (!is.null(start.time)){
    onr <- nrow(input)
    input <- input[input$Timestamp >= as.POSIXct(start.time, tz = tz), ]
    appendTo(c("Screen", "Report"), paste0("M: Discarding detection data previous to ",start.time," per user command (", onr - nrow(input), " detections discarded)."))
  }
  if (!is.null(stop.time)){
    onr <- nrow(input)
    input <- input[input$Timestamp <= as.POSIXct(stop.time, tz = tz), ]
    appendTo(c("Screen", "Report"), paste0("M: Discarding detection data posterior to ",stop.time," per user command (", onr - nrow(input), " detections discarded)."))
  }

	if (any(grepl("^Valid$", colnames(input)))) {
	  if (!is.logical(input$Valid)) {
			appendTo(c("Screen", "Warning", "Report"), "The detections have a column named 'Valid' but its content is not logical. Resetting to Valid = TRUE.")
  		input$Valid <- TRUE
  	}
  } else {
		input$Valid <- TRUE  	
  }

  input$Transmitter <- as.factor(paste(input$CodeSpace, input$Signal, sep = "-"))

  # Convert object to data.table for compatibility with downtream functions.
  input <- data.table::as.data.table(input)

  return(input)
}
