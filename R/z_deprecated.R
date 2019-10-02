#' Check for unknown receivers in the raw detections
#' 
#' @inheritParams splitDetections
#' @inheritParams loadDetections
#' 
#' @keywords internal

checkEmptyReceivers <- function(spatial, detections){
  appendTo("debug", "Starting emptyReceiversCheck.")
  empty.receivers <- sum(is.na(match(spatial$receivers.serial, unique(detections$Receiver))))
  appendTo(c("Screen", "Report"), paste("M: Number of ALS: ", spatial$number.of.receivers, " (of which ", empty.receivers, " had no detections)", sep = ""))
  if (empty.receivers > 0) 
    appendTo(c("Screen", "Report", "Warning"), paste("W: No detections were found for receiver(s) ", paste(sort(spatial$receivers.serial[is.na(match(spatial$receivers.serial, unique(detections$Receiver)))]), 
      collapse = ", "), ".", sep = ""))
  rm(empty.receivers)
  appendTo("debug", "Terminating emptyReceiversCheck.")
}

#' Check for unknown receivers in the raw detections
#' 
#' @inheritParams splitDetections
#' @inheritParams loadDetections
#' 
#' @keywords internal
#' 
unknownReceiversCheckA <- function(detections, spatial){
  appendTo("debug", "Starting unknownReceiversCheckA.")  
  if (any(trigger <- is.na(match(unique(detections$Receiver), spatial$receivers.serial)))) {
    appendTo(c("Screen", "Report", "Warning"), paste0("W: Detections from receivers ", paste(unique(detections$Receiver)[trigger], collapse = ", "), " are present in the data, but these receivers are not part of the study's stations. Doublecheck potential errors."))
  }
  appendTo("debug", "Terminating unknownReceiversCheckA.")  
}

#' Check for target data in the unknown receivers
#' 
#' @inheritParams groupMovements
#' @inheritParams loadDetections
#' 
#' @return A list containing an updated spatial list and a TRUE/FALSE object indicating whether or not Standard station names must be reprocessed.
#' 
#' @keywords internal
#' 
unknownReceiversCheckB <- function(detections.list, spatial) {
  appendTo("debug", "Starting unknownReceiversCheckB.")
  reset.names <- FALSE
  for (i in names(detections.list)) {
    if (any(is.na(match(detections.list[[i]][, Receiver], spatial$receivers.serial)))) {
      A <- detections.list[[i]][, Receiver]
      B <- spatial$receivers.serial
      unknown.receivers <- unique(detections.list[[i]][is.na(match(A, B)), Receiver])
      appendTo(c("Screen", "Report", "Warning"), paste("W: Fish ", i, " was detected in one or more receivers that are not listed in the study area (receiver(s): ", paste(unknown.receivers, collapse = ", "), ")!", sep = ""))
      cat("Possible options:\n   a) Stop and doublecheck the data (recommended)\n   b) Temporary include the hydrophone(s) to the stations list\n")
      check <- TRUE
      while (check) {
        decision <- commentCheck(line = "Which option should be followed?(a/b/comment) ", tag = i)
        if (decision == "a" | decision == "A" | decision == "b" | decision == "B") 
          check <- FALSE else cat("Option not recognized, please try again.\n")
        appendTo("UD", decision)
      }
      if (decision == "a" | decision == "A") {
        emergencyBreak()
        stop("Stopping analysis per user command.\n")
      }
      if (decision == "b" | decision == "B") {
        reset.names <- TRUE
        spatial <- deprecated_includeUnknownReceiver(spatial = spatial, unknown.receivers = unknown.receivers)
      }
    }
  }
  appendTo("debug", "Terminating unknownReceiversCheckB.")
  return(list(spatial = spatial, reset.names = reset.names))
}


#' Temporarily include missing receivers in the spatial object
#' 
#' @param unknown.receivers serial number of the receivers to be included
#' @inheritParams loadDetections
#' 
#' @keywords internal
#' 
deprecated_includeUnknownReceiver <- function(spatial, unknown.receivers){
  appendTo("debug", "Starting includeUnknownReceiver.")
  appendTo(c("Screen", "Report"), "M: Including missing receiver(s) in the stations list. Assigning to array 'Unknown'.")
    levels(spatial$stations$Receiver) <- c(levels(spatial$stations$Receiver), unknown.receivers)
  if (all(!grepl("Unknown", levels(spatial$stations$Station.Name)))) 
    levels(spatial$stations$Station.Name) <- c(levels(spatial$stations$Station.Name), "Unknown")
  if (all(!grepl("Unknown", levels(spatial$stations$Array)))) 
    levels(spatial$stations$Array) <- c(levels(spatial$stations$Array), "Unknown")
  for (j in seq_len(length(unknown.receivers))) {
    spatial$stations[nrow(spatial$stations) + 1, "Station.Name"] <- "Unknown"
    spatial$stations[nrow(spatial$stations), "Receiver"] <- unknown.receivers[j]
    spatial$stations[nrow(spatial$stations), "Array"] <- "Unknown"
    spatial$stations[nrow(spatial$stations), "Standard.Name"] <- paste("Ukn.", sum(spatial$stations$Station.Name == "Unknown"), sep = "")
  }
  spatial$receivers.serial <- as.matrix(spatial$stations[, spatial$receiver.columns])[!is.na(as.matrix(spatial$stations[, spatial$receiver.columns]))]
  appendTo("debug", "Terminating includeUnknownReceiver.")
  return(spatial)
}

#' Split detections by tag
#'
#' Splits the detections' table by tags and selects only detections from target tags
#' 
#' @inheritParams actel
#' @inheritParams loadDetections
#' @param bio A table with the tags and biometrics of the studied fish.
#' @param detections A dataframe with all the detections. Supplied by loadDetections.
#' @param silent logical: if TRUE, suppress messages.
#' 
#' @return A list of detections for each tag.
#' 
#' @keywords internal
#' 
deprecated_splitDetections <- function(detections, bio, spatial, exclude.tags = NULL, silent = FALSE) {
  appendTo("debug", "Starting splitDetections.")
  my.list <- split(detections, detections$Transmitter)
  my.list <- excludeTags(input = my.list, exclude.tags = exclude.tags, silent = silent)
  
  tags <- checkNoDetections(input = my.list, bio = bio)
  checkDupSignals(input = my.list, bio = bio, tag.list = tags$list)
  
  appendTo("debug", "Debug: Creating 'trimmed.list'.")
  bio$Transmitter <- names(my.list)[tags[["link"]]]
  trimmed.list <- my.list[tags[["link"]]]
  non.empty <- unlist(lapply(trimmed.list, function(x) length(x) != 0))
  trimmed.list <- trimmed.list[non.empty]
  if (!silent){
    collectStrays(input = my.list[-na.exclude(tags[["link"]])])
    storeStrays()
  }

  for (i in names(trimmed.list)) {
    trimmed.list[[i]] <- assignArrays(input = trimmed.list[[i]], spatial = spatial)
  }

  appendTo("debug", "Terminating splitDetections.")
  return(list(trimmed.list = trimmed.list, bio = bio))
}

#' Load Spatial file and Check the structure
#' 
#' @inheritParams assembleSpatial
#' 
#' @return The spatial dataframe
#' 
#' @keywords internal
#' 
deprecated_loadSpatial <- function(file){
  appendTo("debug","Starting loadSpatial.")
  if (file.exists(file))
    input <- read.csv(file)
  else {
    emergencyBreak()
    stop("Could not find a '", file, "' file in the working directory.\n")
  }
  if (!any(grepl("Array", colnames(input)))) {
    if (any(grepl("Group", colnames(input)))) {
      decision <- readline("Error: No 'Array' column found in the spatial file, but a 'Group' column is present. Use the 'Group' column to define the arrays? (Y/n) ")
      appendTo("UD", decision)
      if (decision == "N" & decision == "n") {
        emergencyBreak()
        stop("The spatial file must contain an 'Array' column.\n")
      } else {
        appendTo("Report","Error: No 'Array' column found in the spatial file, but a 'Group' column is present. Using the 'Group' column to define the arrays.")  
        colnames(input)[grepl("Group", colnames(input))] <- "Array"
      }
    }
  }
  if (!any(grepl("Receiver", colnames(input)))) {
    appendTo("Report", "Error: No 'Receiver' columns found in the spatial file.")
    emergencyBreak()
    stop("The spatial file must contain at least one 'Receiver' column.\n")
  }
  if (!any(grepl("Type", colnames(input)))) {
    appendTo("Screen", "M: No 'Type' column found in 'spatial.csv'. Assigning all rows as hydrophones.")
    input$Type <- "Hydrophone"
  } else {
    if (any(is.na(match(unique(input$Type), c("Hydrophone","Release"))))){
      emergencyBreak()
      stop("Could not recognise the data in the 'Type' column as only one of 'Hydrophone' or 'Release'. Please doublecheck the spatial file.\n")
    }
  }
  if (!any(grepl("Station.Name", colnames(input)))) {
    appendTo(c("Screen", "Warning", "Report"), "W: No 'Station.Name' column found in the spatial file.\n   Creating a 'Station.Name' column to avoid function failure.")
    input$Station.Name <- paste0("St.", 1:nrow(input))
    input[input$Type == "Release", "Station.Name"] <- paste0("RS.", 1:sum(input$Type == "Release"))
  }
  appendTo("debug","Terminating loadSpatial.")
  return(input)
}

#' Process spatial elements
#' 
#' Creates a list containing multiple spatial elements required throughout the analyses
#' 
#' @param file an input file with spatial data.
#' @inheritParams splitDetections
#' @inheritParams actel
#' 
#' @return A list of 1) stations, 2) release sites, 3) ALS columns in the spatial file, 4) the Number of ASL, 5) The ALS serial numbers and 6) the array order.
#' 
#' @keywords internal
#' 
assembleSpatial <- function(file, bio, sections = NULL) {
  appendTo("debug", "Starting assembleSpatial.")
  input <- deprecated_loadSpatial(file = file)
  # Create standard names
  input <- setSpatialStandards(input = input)
  # Break the stations away
  appendTo("debug", "Creating 'stations'.")
  stations <- input[input$Type == "Hydrophone", -match("Type", colnames(input))]
  stations$Array <- factor(stations$Array, levels = unique(stations$Array))
  appendTo("debug", "Creating 'release.sites'.")
  # Extract serial numbers only
  receiver.columns <- grep("Receiver", colnames(stations))
  for (i in receiver.columns) {
    if (any(grepl("VR", stations[, i]))) {
      stations[, i] <- as.character(stations[, i])
      for (j in 1:nrow(stations)) stations[j, i] <- tail(unlist(strsplit(stations[j, i], "-")), 1)
      rm(j)
      stations[, i] <- as.factor(stations[, i])
    }
  }
  receivers.serial <- as.matrix(stations[, receiver.columns])[!is.na(as.matrix(stations[, receiver.columns]))]
  if (any(link <- table(receivers.serial) > 1)) {
    emergencyBreak()
    stop(ifelse(sum(link) > 1,"Receivers ","Receiver "), paste(names(table(receivers.serial))[link], collapse = ", "), ifelse(sum(link) > 1," are ", " is "), "duplicated in the spatial file.")
  }
  # If there is any release site in the spatial file
  if (sum(input$Type == "Release") > 0) {
    if (length(unique(bio$Release.site)) == 1 && unique(bio$Release.site) == "unspecified") {
      appendTo(c("Screen", "Report", "Warning"), "W: At least one release site has been indicated in the spatial file, but no release sites were specified in the biometrics file.\n   Discarding release site information to avoid script failure. Please doublecheck your data.")
      release.sites <- data.frame(Station.Name = "unspecified", 
                                  Longitude = NA, 
                                  Latitude = NA, 
                                  Array = stations$Array[1])
    } else {
      A <- input$Station.Name[input$Type == "Release"]
      B <- unique(bio$Release.site)
      if (any(is.na(match(B, A)))) {
        appendTo(c("Screen", "Report", "Warning"), "Error: There is a mismatch between the release sites reported and the release locations for the smolts.")
        cat("   Release sites listed in the spatial file:", paste(A, collapse = ", "), "\n")
        cat("   Sites listed in the biometrics file 'Release.site' column:", paste(B, collapse = ", ", "\n"))
        emergencyBreak()
        stop("The release names should be identical in the spatial objects file and in the biometrics file.\n")
      } else {
        from.row <- input$Type == "Release"
        from.col <- colnames(input)[!grepl("Receiver", colnames(input))]
        release.sites <- input[from.row, from.col]
        row.names(release.sites) <- 1:nrow(release.sites)
      }
      A <- unique(stations$Array)
      B <- unique(release.sites$Array)
      if (any(is.na(match(B, A)))) {
        appendTo(c("Screen", "Report", "Warning"), "Error: There is a mismatch between the expected first array of a release site and the list of arrays.")
        cat("   Arrays listed in the spatial file:", paste(A, collapse = ", "), "\n")
        cat("   Expected first arrays of the release sites:", paste(B, collapse = ", ", "\n"))
        emergencyBreak()
        stop("The expected first arrays should match the arrays where stations where deployed in the spatial file.\n")
      }
    }
  } else {
    if (length(unique(bio$Release.site)) > 1){
      appendTo("Screen", "M: Release sites were not specified in the spatial file but more than one release site is reported in the biometric data.\n   Assuming all released fish go through all receiver arrays.")
    }
    release.sites <- data.frame(Station.Name = unique(bio$Release.site), 
                                Longitude = NA,
                                Latitude = NA, 
                                Array = rep(stations$Array[1], length(unique(bio$Release.site))))
  }
  # Wrap up
  number.of.receivers <- sum(!is.na(stations[, receiver.columns]))
  if (!is.null(sections)) {
    array.order <- list()  # Used to determine if the fish's last detection was in the last array of a given section
    for (j in sections) {
      array.order[[j]] <- levels(stations$Array)[grepl(j, levels(stations$Array))]
    }
    if (any(trigger <- unlist(lapply(array.order,length)) == 0)) {
      appendTo(c("Screen", "Warning"), decision <- readline(paste("W: No arrays were found that match section(s) ",paste(names(array.order)[trigger], collapse = ", "), ". There could be a typing mistake!\n   Continue the analysis?(y/N) ", sep = "")))
      if (decision != "y" | decision != "Y" ){
        emergencyBreak()
        stop("Stopping analysis per user command.\n")
      }
    }
  } else {
    array.order <- list(all = levels(stations$Array))
  }
  # Order release sites by entry point.
  if (!is.ordered(match(release.sites$Array, unlist(array.order))))
    release.sites <- release.sites[order(match(release.sites$Array, unlist(array.order))),]
  # join everything
  output <- list(stations = stations, 
                 release.sites = release.sites, 
                 receiver.columns = receiver.columns, 
                 number.of.receivers = number.of.receivers, 
                 receivers.serial = receivers.serial, 
                 array.order = array.order)
  appendTo("debug", "Terminating assembleSpatial.")
  return(output)
}


#' Standardize Stations
#' 
#' Matches the ALS serial number to the stations in the study area, standardizing the ALS station names.
#' 
#' @inheritParams convertCodes
#' @param spatial A list of spatial objects in the study area
#'
#' @return A dataframe with standardized station names.
#'
#' @keywords internal
#' 
standardizeStations <- function(input, spatial) {
  appendTo("debug", "Starting standardizeStations.")
  input$Standard.Name <- NA
  for (i in spatial$receiver.columns) {
    link <- match(input$Receiver, spatial$stations[, i])
    new.names <- spatial$stations$Standard.Name[link]
    places <- seq_len(nrow(input))[!is.na(link)]
    input$Standard.Name[places] <- new.names[!is.na(link)]
  }
  input$Standard.Name <- factor(input$Standard.Name, spatial$stations$Standard.Name)
  appendTo("debug", "Terminating standardizeStations.")
  return(input)
}

