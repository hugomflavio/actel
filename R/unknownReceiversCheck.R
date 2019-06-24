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
    appendTo(c("Screen", "Report", "Warning"), paste("W: Detections from receivers ", paste(unique(detections$Receiver)[trigger], collapse = ", "), " are present in the data, but these receivers are not part of the study's stations. Doublecheck potential errors.", 
      sep = ""))
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
    if (any(is.na(match(detections.list[[i]][, "Receiver"], spatial$receivers.serial)))) {
      A <- detections.list[[i]][, "Receiver"]
      B <- spatial$receivers.serial
      unknown.receivers <- as.character(unique(detections.list[[i]][is.na(match(A, B)), "Receiver"]))
      appendTo(c("Screen", "Report", "Warning"), paste("W: Fish ", i, " was detected in one or more receivers that are not listed in the study area (receiver(s): ", paste(unknown.receivers, collapse = ", "), ")!", sep = ""))
      cat("Possible options:\n   a) Stop and doublecheck the data (recommended)\n   b) Temporary include the hydrophone(s) to the stations list\n")
      check <- TRUE
      while (check) {
        decision <- commentCheck(line = "Which option should be followed?(a/b) ", tag = i)
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
        spatial <- includeUnknownReceiver(spatial = spatial, unknown.receivers = unknown.receivers)
      }
    }
  }
  appendTo("debug", "Terminating unknownReceiversCheckB.")
  return(list(spatial = spatial, reset.names = reset.names))
}

#' Check for unknown receivers in the raw detections
#' 
#' @inheritParams splitDetections
#' @inheritParams loadDetections
#' 
#' @keywords internal

emptyReceiversCheck <- function(spatial, detections){
  appendTo("debug", "Starting emptyReceiversCheck.")
  empty.receivers <- sum(is.na(match(spatial$receivers.serial, unique(detections$Receiver))))
  appendTo("Report", paste("Number of ALS: ", spatial$number.of.receivers, " (of which ", empty.receivers, " had no detections)", sep = ""))
  if (empty.receivers > 0) 
    appendTo(c("Screen", "Report", "Warning"), paste("W: No detections were found for receiver(s) ", paste(sort(spatial$receivers.serial[is.na(match(spatial$receivers.serial, unique(detections$Receiver)))]), 
      collapse = ", "), ".", sep = ""))
  rm(empty.receivers)
  appendTo("debug", "Terminating emptyReceiversCheck.")
}

#' Temporarily include missing receivers in the spatial object
#' 
#' @param unknown.receivers serial number of the receivers to be inlucded
#' @inheritParams loadDetections
#' 
#' @keywords internal
#' 
includeUnknownReceiver <- function(spatial, unknown.receivers){
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