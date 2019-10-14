#' Confirm that receivers were not re-deployed before being retrieved
#' 
#' @param input the table of deployments
#' 
#' @keywords internal
#' 
checkDeploymentTimes <- function(input) {
  appendTo("debug","Terminating checkDeploymentTimes")
  aux <- split(input, input$Receiver)
  for (i in 1:length(aux)) {
    if (nrow(aux[[i]]) > 1) {
      A <- aux[[i]]$Start[-1]
      B <- aux[[i]]$Stop[-nrow(aux[[i]])]
      AtoB <- as.numeric(difftime(A, B))
      if (any(AtoB < 0)) {
        appendTo(c("Screen", "Report"), paste0("Error: Receiver ", names(aux)[i], " was re-deployed before being retrieved:\n"))
        aux[[i]][, "Bleh"] <- ""
        aux[[i]][c(FALSE, AtoB < 0), "Bleh"] <- " <- !!!"
        names(aux[[i]])[ncol(aux[[i]])] <- ""
        print(aux[[i]], row.names = FALSE)
        cat("\n")
        emergencyBreak()
        stop("Fatal exception found. Read lines above for more details.\n")      
      }
    }
  }
  appendTo("debug","Terminating checkDeploymentTimes")
}

#' Confirm that the station names in the deployments table match those listed in the spatial file
#' 
#' @inheritParams checkDeploymentTimes
#' @param spatial The spatial data frame, as loaded by loadSpatial
#' 
#' @keywords internal
#' 
checkDeploymentStations <- function(input, spatial) {
  appendTo("debug","Terminating checkDeploymentStations")
  aux <- spatial[spatial$Type == "Hydrophone", ]
  link <- match(unique(input$Station.Name), aux$Station.Name)
  if (any(is.na(link))) {
    appendTo(c("Screen", "Report", "Warning"), paste0("W: ", ifelse(sum(is.na(link)) > 1, "Stations", "Station"), " '", paste(unique(input$Station.Name)[is.na(link)], collapse = "', '"), "' ", ifelse(sum(is.na(link)) > 1, "are", "is"), " listed in the deployments but are not part of the study's stations. Discarding deployments at unknown stations."))
    to.remove <- match(input$Station.Name, unique(input$Station.Name)[is.na(link)])
    input <- input[is.na(to.remove), ]
  }
  link <- match(aux$Station.Name, unique(input$Station.Name))
  if (any(is.na(link))) {
    appendTo(c("Screen", "Report"), paste0("Error: Stations '", paste(aux$Station.Name[is.na(link)], collapse = "', '"), "' are listed in the spatial file but no receivers were ever deployed there."))
    emergencyBreak()
    stop("Fatal exception found. Read lines above for more details.\n")      
  }
  appendTo("debug","Terminating checkDeploymentStations")
  return(input)
}

#' Find detections from unknown receivers
#' 
#' @param input The detections data frame
#' 
#' @keywords internal
#' 
checkUnknownReceivers <- function(input) {
  appendTo("debug","Terminating unknownReceivers")
  unknown <- is.na(input$Standard.Name)
  if (any(unknown)) {
    appendTo(c("Screen", "Report", "Warning"), paste0("W: Detections from receivers ", paste(unique(input$Receiver[unknown]), collapse = ", "), " are present in the data, but these receivers are not part of the study's stations. Doublecheck potential errors."))
  }
  appendTo("debug","Terminating unknownReceivers")
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
checkTagsInUnknownReceivers <- function(detections.list, deployments, spatial) {
  # NOTE: The NULL variables below are actually column names used by data.table.
  # This definition is just to prevent the package check from issuing a note due unknown variables.
  Receiver <- NULL

  appendTo("debug", "Starting tagsInUnknownReceivers")
  for (i in names(detections.list)) {
    if (any(is.na(detections.list[[i]]$Standard.Name))) {
      A <- detections.list[[i]]$Receiver
      B <- names(deployments)
      unknown.receivers <- unique(detections.list[[i]][is.na(match(A, B)), Receiver])
      appendTo(c("Screen", "Report", "Warning"), paste("W: Fish ", i, " was detected in one or more receivers that are not listed in the study area (receiver(s): ", paste(unknown.receivers, collapse = ", "), ")!", sep = ""))
      cat("Possible options:\n   a) Stop and doublecheck the data (recommended)\n   b) Temporary include the hydrophone(s) to the stations list\n")
      check <- TRUE
      while (check) {
        decision <- commentCheck(line = "Which option should be followed?(a/b/comment) ", tag = i)
        if (decision == "a" | decision == "A" | decision == "b" | decision == "B") 
          check <- FALSE 
        else 
          cat("Option not recognized, please try again.\n")
        appendTo("UD", decision)
      }
      if (decision == "a" | decision == "A") {
        emergencyBreak()
        stop("Stopping analysis per user command.\n")
      }
      if (decision == "b" | decision == "B") {
        recipient <- includeUnknownReceiver(spatial = spatial, deployments = deployments, unknown.receivers = unknown.receivers)
        spatial <- recipient[[1]]
        deployments <- recipient[[2]]
      }
    }
  }
  appendTo("debug", "Terminating unknownReceiversCheckB.")
  return(list(spatial = spatial, deployments = deployments))
}

#' Check if there are detections for the target tags before release.
#'
#' @param input list of detections
#' @inheritParams splitDetections
#' 
#' @keywords internal
#' 
#' @return The detections list without invalid detections.
#' 
checkDetectionsBeforeRelease <- function(input, bio){
  appendTo("debug", "Starting detectionBeforeReleaseCheck.")  
  remove.tag <- NULL
  tag.list <- stripCodeSpaces(names(input))
  link <- match(bio$Signal, tag.list)
  for(i in seq_len(length(link))) {
    if (!is.na(link[i])) {
      if (any(to.remove <- !(input[[link[i]]]$Timestamp > bio$Release.date[i]))) {
        appendTo(c("Screen", "Warning", "Report"), paste0("Error: Fish ", names(input)[link[i]], " was detected before being released!"))
        appendTo("Screen", paste0("  Release time: ", bio$Release.date[i]))
        appendTo("Screen", paste0("  First detection time: ", input[[link[i]]]$Timestamp[1]))
        appendTo("Screen", paste0("  Number of detections before release: ", sum(to.remove)))
        cat("\n")
        appendTo("Screen", "You may either:\n  a) Stop the analysis and check the data;\n  b) Discard the before-release detections and continue.")
        cat("\n")
        unknown.input = TRUE
        while (unknown.input) {
          decision <- commentCheck(line = "Decision:(a/b/comment) ", tag = bio$Transmitter[i])
          if (decision == "a" | decision == "A") {
            unknown.input = FALSE
            emergencyBreak()
            stop("Script stopped by user command.")
          }
          if (decision == "b" | decision == "B")
            unknown.input = FALSE
          if (unknown.input)
            cat("Option not recognised, please input either 'a' or 'b'.\n")
        }
        appendTo("UD", decision)
        if (decision == "b" | decision == "B") {
          if (all(to.remove)) {
            appendTo(c("Screen", "Warning", "Report"), paste0("W: ALL detections from Fish ", names(input)[link[i]], " were removed per user command."))
            remove.tag <- c(remove.tag, link[i])
          } else {
            input[[link[i]]] <- input[[link[i]]][!to.remove, ]
            appendTo(c("Screen", "Warning", "Report"), paste0("M: ", sum(to.remove), " detections from Fish ", names(input)[link[i]], " were removed per user command."))
          }
        }
      }
    }
  }
  if (!is.null(remove.tag)) {
    input <- input[-remove.tag]
  }
  appendTo("debug", "Terminating detectionBeforeReleaseCheck.")  
  return(input)
}

#' Check if there are detections matching the target tags.
#'
#' @param input list of detections
#' @inheritParams splitDetections
#' 
#' @keywords internal
#' 
checkNoDetections <- function(input, bio){
  appendTo("debug", "Starting noDetectionsCheck.")  
  tag.list <- stripCodeSpaces(names(input))
  link <- match(bio$Signal, tag.list)
  if (all(is.na(link))) {
    appendTo(c("Screen", "Report"), "M: No detections were found in the input data which matched the target signals.")
    emergencyBreak()
    stop("Stopping analysis due to absence of valid detections.\n")
  }
  appendTo("debug", "Terminating noDetectionsCheck.")  
  return(list(list = tag.list,link = link))
}

#' Check if there are duplicated signals in the detected tags.
#'
#' @param input list of detections
#' @inheritParams splitDetections
#' @param tag.list list of the target signals
#' 
#' @keywords internal
#' 
checkDupSignals <- function(input, bio, tag.list){
  appendTo("debug", "Starting dupSignalsCheck.")  
  failsafe <- match(tag.list, bio$Signal)
  if (any(table(failsafe) > 1)) {
    appendTo(c("Screen", "Report"), "Error: One or more signals match more than one tag in the detections! Showing relevant signals/tags.")
    t1 <- cbind(names(input), bio$Signal[failsafe])
    t2 <- t1[complete.cases(t1), ]
    t3 <- table(t2[, 1], t2[, 2])
    rm(t1, t2)
    dupsig <- data.frame(Signal = colnames(t3)[apply(t3, 2, sum) > 1], Tags = NA, stringsAsFactors = FALSE)
    for (i in seq_len(nrow(dupsig))) dupsig$Tags[i] <- paste(row.names(t3)[t3[, dupsig$Signal[i]] == 1], collapse = ", ")
    rm(t3)
    for (i in seq_len(nrow(dupsig))) appendTo(c("Screen", "Report"), paste("   Signal ", dupsig$Signal[i], " was found on tags ", dupsig$Tags[i], ".", sep = ""))
    emergencyBreak()
    stop("Fatal exception found. Stopping analysis.\n")
  }
  appendTo("debug", "Terminating dupSignalsCheck.")  
}
