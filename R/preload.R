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
#' @return ...
#' 
preload <- function(biometrics, spatial, deployments, detections, dot, distances, tz, 
	sections = NULL, exclude.tags = NULL, disregard.parallels = FALSE, discard.orphans = FALSE) {

  if (is.na(match(tz, OlsonNames())))
    stop("'tz' could not be recognized as a timezone. Check available timezones with OlsonNames()\n", call. = FALSE)

  if (!is.null(exclude.tags) && any(!grepl("-", exclude.tags, fixed = TRUE)))
    stop("Not all contents in 'exclude.tags' could be recognized as tags (i.e. 'codespace-signal'). Valid examples: 'R64K-1234', A69-1303-1234'\n", call. = FALSE)

  bio <- loadBio(input = bio, tz = tz)

  message("M: Number of target tags: ", nrow(bio), ".")
  
  deployments <- loadDeployments(input = deployments, tz = tz)
  checkDeploymentTimes(input = deployments) # check that receivers are not deployed before being retrieved

  spatial <- loadSpatial(input = spatial)
  deployments <- checkDeploymentStations(input = deployments, spatial = spatial) # match Station.name in the deployments to Station.name in spatial, and vice-versa
  deployments <- createUniqueSerials(input = deployments) # Prepare serial numbers to overwrite the serials in detections


  detections <- preloadDetections(input = detections, tz = tz)
  detections <- checkDupDetections(input = detections)
  detections <- createStandards(detections = detections, spatial = spatial, deployments = deployments, discard.orphans = discard.orphans) # get standardized station and receiver names, check for receivers with no detections
  message("M: Data time range: ", as.character(head(detections$Timestamp, 1)), " to ", as.character(tail(detections$Timestamp, 1)), " (", tz, ").")
  checkUnknownReceivers(input = detections) # Check if there are detections from unknown detections

  if (missing(dot)) {
    n <- length(unique(spatial$Array[spatial$Type == "Hydrophone"]))
    if (n > 1) {
      fakedot <- paste(unique(spatial$Array[spatial$Type == "Hydrophone"]), collapse = "--")
      recipient <- loadDot(string = fakedot, spatial = spatial, sections = sections, disregard.parallels = disregard.parallels)
    } else {
      aux <- unique(spatial$Array[spatial$Type == "Hydrophone"])
      fakedot <- paste(aux, "--", aux)
      recipient <- loadDot(string = fakedot, spatial = spatial, sections = sections, disregard.parallels = disregard.parallels)
    }
  } else {
  	if (!is.character(dot))
  		stop("'dot' was set but could not recognised as a string. Please prepare a dot string and include it in the dot argument.\nYou can use readDot to check the quality of your dot string.", call. = FALSE)
  	else
  		recipient <- loadDot(string = dot, spatial = spatial, sections = sections, disregard.parallels = disregard.parallels)
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
  recipient <- transformSpatial(spatial = spatial, bio = bio, sections = sections, arrays = arrays, dotmat = dotmat, first.array = first.array) # Finish structuring the spatial file
  spatial <- recipient$spatial
  sections <- recipient$sections
  detections$Array <- factor(detections$Array, levels = unlist(spatial$array.order)) # Fix array levels
  link <- match(unlist(spatial$array.order), names(arrays))
  arrays <- arrays[link] # Reorder arrays by spatial order
  rm(link)
  
  if (missing(distances)) {
  	dist.mat <- NA
  	invalid.dist <- TRUE
  } else {
	  recipient <- loadDistances(input = distances, spatial = spatial) # Load distances and check if they are valid
	  dist.mat <- recipient$dist.mat
	  invalid.dist <- recipient$invalid.dist
	  if (is.null(dist.mat) | is.null(invalid.dist))
	    stop("Something went wrong when assigning recipient objects (loadDistances). If this error persists, contact the developer.\n", call. = FALSE) # nocov
	  rm(recipient)
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

  output <- list(bio = bio, sections = sections, deployments = deployments, spatial = spatial, dot = dot,
   arrays = arrays, dotmat = dotmat, dist.mat = dist.mat, invalid.dist = invalid.dist, 
   detections.list = detections.list, paths = paths, disregard.parallels = disregard.parallels, tz = tz)

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
preloadDetections <- function(input, tz) {
	mandatory.cols <- c("Timestamp", "Receiver", "CodeSpace", "Signal")

	if (any(link <- is.na(match(mandatory.cols, colnames(input))))) {
		if (any(is.na(match(mandatory.cols[3:4], colnames(input)))))
			also.say <- "\nThe functions extractSignals and extractCodeSpaces can be used to break transmitter codes apart."
		else
			also.say <- ""

		stop("The following mandatory columns are missing in the detections:", paste(mandatory.cols[link], collapse = ", "), also.say, call. = FALSE)
	}

	if (!is.integer(input$Signal)) {
		warning("The 'Signal' column in the detections is not of type integer. Attempting to convert.", immediate. = TRUE, call. = FALSE)
		input$Signal <- tryCatch(as.integer(input$Signal), 
			warning = function(w) stop("Attempting to convert the 'Signal' to integer failed. Aborting.", call. = FALSE))
	}

	if (!is.integer(input$Receiver)) {
		warning("The 'Receiver' column in the detections is not of type integer. Attempting to convert.", immediate. = TRUE, call. = FALSE)
		aux <- tryCatch(as.integer(input$Receiver), 
			warning = function(w) {
				warning("Attempting to convert the 'Receiver' to integer failed. Attempting to extract only the serial numbers.", immediate. = TRUE, call. = FALSE)
				return(NULL) })
		if (is.null(aux)) {
			input$Receiver <- extractSignals(input$Receiver) # this works for extracting only the receiver numbers too
			aux <- tryCatch(as.integer(input$Receiver), 
				warning = function(w) stop("Extracting the serial numbers failed. Aborting.", call. = FALSE))
		}
		input$Receiver <- aux
	}

	if (length(the.col <- grep("^[S|s]ensor.[U|u]nit$", colnames(input))) > 0)
		colnames(input)[the.col] <- "Sensor.Unit"

	if (length(the.col <- grep("^[S|s]ensor.[V|v]alue$", colnames(input))) > 0)
		colnames(input)[the.col] <- "Sensor.Value"
	
	if (!any(grepl("^Sensor.Value$", colnames(input)))) {
		warning("Could not find a 'Sensor.Value' column in the detections. Filling one with NA.", immediate. = TRUE, call. = FALSE)
		input$Sensor.Value <- NA_real_
	}

	if (!any(grepl("^Sensor.Unit$", colnames(input)))) {
		warning("Could not find a 'Sensor.Unit' column in the detections. Filling one with NA.", immediate. = TRUE, call. = FALSE)
		input$Sensor.Unit <- NA_character_
	}

	if (!is.numeric(input$Sensor.Value)) {
		warning("The 'Sensor.Value' column in the detections is not of type numeric. Attempting to convert.", immediate. = TRUE, call. = FALSE)
		input$Signal <- tryCatch(as.numeric(input$Signal), 
			warning = function(w) stop("Attempting to convert the 'Sensor.Value' to numeric failed. Aborting.", call. = FALSE))
	}

	if (!inherits(input$Timestamp, "POSIXct")) {
		message("M: Converting detection timestamps to POSIX objects"); flush.console()
		input$Timestamp <- fasttime::fastPOSIXct(input$Timestamp, tz = UTC)
	}

	attributes(input$Timestamp)$tzone <- tz

	if (any(grepl("^Valid$", colnames(input)))) {
	  if (!is.logical(input$Valid)) {
			warning("The detections have a column named 'Valid' but its content is not logical. Resetting to Valid = TRUE.", immediate. = TRUE, call. = FALSE)
  		input$Valid <- TRUE
  	}
  } else {
		input$Valid <- TRUE  	
  }

  input$Transmitter <- as.factor(paste(input$CodeSpace, input$Signal, sep = "-"))

  return(input)
}