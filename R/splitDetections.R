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
splitDetections <- function(detections, bio, spatial, exclude.tags = NULL, silent = FALSE) {
  appendTo("debug", "Starting splitDetections.")
  my.list <- split(detections, detections$Transmitter)
  my.list <- excludeTags(input = my.list, exclude.tags = exclude.tags, silent = silent)
  
  tags <- noDetectionsCheck(input = my.list, bio = bio)
  dupSignalsCheck(input = my.list, bio = bio, tag.list = tags$list)
  
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

#' Collect summary information on the tags detected but that are not part of the study.
#'
#' @param input list of detections for the tags to be excluded.
#' @param restart logical: if TRUE, remove file 'temp_strays.csv' from the working directory.
#' 
#' @keywords internal
#' 
collectStrays <- function(input, restart = FALSE){
  appendTo("debug", "Starting collectStrays.")
  if (restart && file.exists("temp_strays.csv")) {
    file.remove("temp_strays.csv")
  }
  if (length(input) > 0) {
    recipient <- data.frame(Transmitter = names(input), 
      N.detections = unlist(lapply(input,nrow)), 
      First.detection = unlist(lapply(input, function(x) as.character(head(x$Timestamp,1)))),
      Last.detection = unlist(lapply(input, function(x) as.character(tail(x$Timestamp,1)))),
      Receivers = unlist(lapply(input, function(x) paste(unique(x$Receiver), collapse = ", ")))
      )
    write.table(recipient, file = "temp_strays.csv", sep = ",", append = file.exists("temp_strays.csv"), row.names = FALSE, col.names = !file.exists("temp_strays.csv"))
  }
  appendTo("debug", "Terminating collectStrays.")
}

#' Store summary information on the stray tags detected in a permanent file.
#'
#' @keywords internal
#'
storeStrays <- function(){
  appendTo("debug", "Starting storeStrays.")
  if (file.exists("temp_strays.csv")) {
    if (file.exists(newname <- "stray_tags.csv")) {
      continue <- TRUE
      index <- 1
      while (continue) {
        if (file.exists(newname <- paste("stray_tags", index, "csv", sep = "."))) {
          index <- index + 1
        } else {
          continue <- FALSE
        }
      }
    }
    file.rename("temp_strays.csv", newname)
  }
  appendTo("debug", "Terminating storeStrays.")

}

#' Collect summary information on the tags detected but that are not part of the study.
#'
#' @param input list of detections
#' @inheritParams actel
#' @inheritParams splitDetections
#'
#' @keywords internal
#' 
excludeTags <- function(input, exclude.tags, silent){
  appendTo("debug", "Starting excludeTags.")  
  if (length(exclude.tags) != 0) {
    link <- match(exclude.tags, names(input))
    if (!silent){
      appendTo(c("Screen", "Report", "Warning"), paste("M: Excluding tag(s) ", paste(exclude.tags, collapse = ", "), " from the analysis per used command (detections removed: ", paste(unlist(lapply(input[link], nrow)), collapse = ", "), ", respectively).", sep = ""))
      collectStrays(input = input[link], restart = TRUE)
    }
    appendTo("debug", "Terminating excludeTags.")  
    return(input[-link])
  }
  appendTo("debug", "Terminating excludeTags.")  
  return(input)
}

#' Check if there are detections matching the target tags.
#'
#' @param input list of detections
#' @inheritParams splitDetections
#' 
#' @keywords internal
#' 
noDetectionsCheck <- function(input, bio){
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

#' Check if there are detections for the target tags before release.
#'
#' @param input list of detections
#' @inheritParams splitDetections
#' 
#' @keywords internal
#' 
#' @return The detections list without invalid detections.
#' 
detectionBeforeReleaseCheck <- function(input, bio){
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
          decision <- commentCheck(line = "Decision:(a/b/comment) ", tag = status.df$Transmitter[i])
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
            remove.tag <- c(remove.tag, i)
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

#' Remove Code Spaces from transmitter names
#' 
#' @param input A vector of transmitter names
#' 
#' @keywords internal
#' 
stripCodeSpaces <- function(input) {
  output <- vector()
  for (i in seq_len(length(input))) output[i] <- tail(unlist(strsplit(input[i], "-")), 1)
  return(output)
}

#' Check if there are duplicated signals in the detected tags.
#'
#' @param input list of detections
#' @inheritParams splitDetections
#' @param tag.list list of the target signals
#' 
#' @keywords internal
#' 
dupSignalsCheck <- function(input, bio, tag.list){
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

#' Match the detection's receiver to its respective array.
#'
#' @param input list of detections
#' @inheritParams loadDetections
#' 
#' @keywords internal
#' 
assignArrays <- function(input, spatial){
  appendTo("debug", "Starting assignArrays.")  
  to.combine <- list()
  for (j in spatial$receiver.columns) {
    to.combine[[length(to.combine) + 1]] <- match(input$Receiver, spatial$stations[, j])
  }
  rm(j)
  input$Array <- spatial$stations$Array[combine(to.combine)]
  appendTo("debug", "Terminating assignArrays.")  
  return(input)
}