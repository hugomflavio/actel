#' Create the timetable
#'
#' Crawls trough the movement events of each fish to find when it entered and left each section of the study area.
#' 
#' @inheritParams actel
#' @inheritParams simplifyMovements
#' @inheritParams loadDetections
#' @inheritParams groupMovements
#' @param movements A list of movements for each target tag, created by groupMovements.
#' 
#' @return A table of the entering and leaving points for each section per target tag
#' 
#' @keywords internal
#' 
assembleTimetable <- function(movements, sections, spatial, minimum.detections, 
  dist.mat, invalid.dist, speed.method, if.last.skip.section, success.arrays, override, cautious.assignment) {
  appendTo("debug", "Starting assembleTimetable.")
  appendTo(c("Screen", "Report"), "M: Initiating timetable development. Your assistance may be needed during the process.")

  # NOTE: The NULL variables below are actually column names used by data.table.
  # This definition is just to prevent the package check from issuing a note due unknown variables.
  Valid <- NULL 
  Array <- NULL
  
  recipient <- vector()
  if (invalid.dist) {
    for (i in seq_len(length(sections))) {
      recipient <- c(recipient, paste(c("Time.until", "First.station",
        "Arrived", "Time.in", "Last.station", "Left"), sections[i], sep = "."))
    }
  } else {
    for (i in seq_len(length(sections))) {
      recipient <- c(recipient, paste(c("Time.until", "Speed.to", "First.station",
        "Arrived", "Time.in", "Speed.in", "Last.station", "Left"), sections[i], sep = "."))
    }
  }
  recipient <- c(recipient, "Very.last.array", "Status", "Detections", "Backwards.movements", "Max.cons.back.moves", "P.type")
  if (!invalid.dist && speed.method == "first to first")
    recipient <- recipient[!grepl("Speed.in",recipient)]

  timetable <- matrix(nrow = length(movements), ncol = length(recipient))
  timetable <- as.data.frame(timetable)
  
  colnames(timetable) <- recipient
  rm(recipient)
  rownames(timetable) <- names(movements)
  
  for (i in names(movements)) {
    if (!is.null(override) && any(grepl(i, override))) {
      ## If the fish is marked for manual mode
      processing.type <- "Overridden"
      events <- overrideDefaults(i, movements[[i]], sections)
      ## Jump to updating movements and deploying values
    } else {
      if (movements[[i]][, any(Valid)]) {
        ## If there are any valid events (invalid could only be caused by upstream detection)
        processing.type <- "Auto"
        ## Find the last events for each section If there is more than one detection event
        if (movements[[i]][(Valid), all(Array == "Unknown")]) {
          ## If all the events are in unknown arrays
          appendTo(c("Screen","Report", "Warning"), paste("W: Fish ", i, " only moved through unknown hydrophones. Considered invalid.", sep = ""))
          last.events <- rep(NA, length(sections))
          movements[[i]]$Valid <- FALSE
          ## Jump to find first events
        } else {
          ## If all or some events are in known locations
          if (movements[[i]][(Valid), any(Array == "Unknown")]) {
            ## If some unknown events must be discarded
            appendTo(c("Screen","Report"), paste("M: Disregarding ", sum(movements[[i]][, "Array"] == "Unknown"), " unknown events from fish ", i, "'s movement table.", sep = ""))
            movements[[i]][(Array == "Unknown")]$Valid <- FALSE
          }
          recipient <- findLastEvents(i = i, movements = movements[[i]], 
            sections = sections, minimum.detections = minimum.detections, 
            processing.type = processing.type, cautious.assignment = cautious.assignment)
          last.events <- recipient[[1]]
          processing.type <- recipient[[2]]
        }
      } else {
        ## If all the events are invalid
        last.events <- rep(NA, length(sections))
        processing.type <- "Manual"
      }      
      recipient <- findFirstEvents(i = i, last.events = last.events, 
        movements = movements[[i]], sections = sections, 
        processing.type = processing.type, cautious.assignment = cautious.assignment)
      events <- recipient[[1]]
      processing.type <- recipient[[2]]
    }
    ## First update movement validity based on selected events
    movements[[i]] <- updateMovementValidity(movements = movements[[i]], events = events, sections = sections)
    ## Second, deploy the values
    timetable <- deployValues(i = i, timetable = timetable, movements = movements[[i]], 
      events = events, sections = sections, spatial = spatial, dist.mat = dist.mat, 
      invalid.dist = invalid.dist, if.last.skip.section = if.last.skip.section, 
      success.arrays = success.arrays, processing.type = processing.type, speed.method = speed.method)
  }
  timetable$Transmitter <- rownames(timetable)
  appendTo("debug", "Terminating assembleTimetable.")
  return(list(timetable = timetable, movements = movements))
}

#' Update movement validity based on the chosen first and last events for each section
#' 
#' @inheritParams actel
#' @inheritParams deployValues
#' @inheritParams assembleTimetable
#' 
#' @keywords internal
#' 
#' @return the updated movement's table
#' 
updateMovementValidity <- function(movements, events, sections){
  # NOTE: The NULL variables below are actually column names used by data.table.
  # This definition is just to prevent the package check from issuing a note due unknown variables.
  Array <- NULL
    
  appendTo("Debug", "Starting updateMovementValidity.")
  for (j in seq_len(length(sections))) {
    link <- movements[, grepl(sections[j], Array)]
    if (is.na(events$last.events[j])) {
      if (any(link))
        movements[link, "Valid"] <- FALSE
    } else {
      if (any(which(link) > events$last.events[j]))
        movements[which(link)[which(link) > events$last.events[j]]]$Valid <- FALSE
      if (any(which(link) < events$first.events[j]))
        movements[which(link)[which(link) < events$first.events[j]]]$Valid <- FALSE
    }
    rm(link)
  }
  appendTo("Debug", "Terminating updateMovementValidity.")
  return(movements)
}

#' Override automated logics
#'
#' If the user has called for an override for a specific tag, this function will allow for a fully manual choice of events.
#' 
#' @inheritParams actel
#' @inheritParams simplifyMovements
#' @param i The tag number currently under scrutiny. Only used for messaging purposes.
#' 
#' @return The first and last events for each section for the target fish.
#' 
#' @keywords internal
#' 
overrideDefaults <- function(i, movements, sections) {
  appendTo("debug", "Starting overrideDefaults.")
  cat("----------------------------\n")
  appendTo(c("Screen", "Report", "Warning"), paste("M: Override has been triggered for fish ", i, ". Entering full manual mode.", sep = ""))
  last.events <- vector()
  first.events <- vector()
  cat("Opening movements list for inspection.\n\n")
  print(movements, topn = nrow(movements))
  cat("\n")
  for (j in seq_len(length(sections))) {
    check = TRUE
    while (check) {
      last.events[j] <- suppressWarnings(as.numeric(commentCheck(line = paste("Last ", sections[j], " event: ", sep = ""), tag = i)))
      if (!is.na(last.events[j])) {
        valid = TRUE
        check <- !overrideEventCheck(type = "last", the.event = last.events[j], 
                    up.boundary = tail(which(grepl(sections[j], movements$Array)), 1),
                    t = checkPrevious(last.events = last.events, j = j),
                    last.events = last.events,
                    j = j, sections = sections, movements = movements)
      } else {
        decision <- commentCheck(line = paste("Confirm that the fish did not pass this section?(y/N/comment) "), tag = i)
        if (decision == "y" | decision == "Y") {
          check = FALSE
        } else {
          check = TRUE
        }
      }
    }
    appendTo("UD", last.events[j])
    if (is.na(last.events[j]))
      appendTo("UD", "Y")
    rm(check)
  }
  rm(j)
  for (j in seq_len(length(sections))) {
    # If the fish left the section
    if (is.na(last.events[j])) {
      cat(paste("M: Skipping first event selection for ", sections[j], " as this fish never left this section.\n", sep = ""))
      first.events[j] <- NA
    } else {
      check = TRUE
      while (check) {
        first.events[j] <- suppressWarnings(as.numeric(commentCheck(line = paste("First ", sections[j], " event: ", sep = ""), tag = i)))
        if (is.na(first.events[j])) {
          cat("M: The inserted value is not numeric. Please try again.\n")
        } else {
          valid = TRUE
          check <- !overrideEventCheck(type = "first", the.event = first.events[j], 
                      up.boundary = last.events[j],
                      t = checkPrevious(last.events = last.events, j = j),
                      last.events = last.events,
                      j = j, sections = sections, movements = movements)
        }
      }
      appendTo("UD", first.events[j])
      rm(check)
    }
  }
  cat("Terminating full manual mode\n----------------------------\n")
  appendTo("debug", "Terminating overrideDefaults.")
  return(list(first.events = first.events, last.events = last.events))
}

#' Find previous valid last event index
#' 
#' @param last.events A vector of the movement numbers corresponding to each last event. Is supplied by the function findLastEvents.
#' @param j the row being analysed
#' 
#' @return The previous valid last event index
#' 
#' @keywords internal
#' 
checkPrevious <- function(last.events, j){
  t <- j-1
  failsafe = TRUE
  while (failsafe) {
    if (t > 0 && is.na(last.events[t])) {
      t <- t-1
    } else {
      failsafe = FALSE
    }
  }
  return(t)
}

#' Check event validity during manual mode
#' 
#' @inheritParams actel
#' @inheritParams simplifyMovements
#' @inheritParams checkPrevious
#' @param type The type of event being checked, only affects message display
#' @param the.event The event being evaluated (chosen by the user during overrideDefaults)
#' @param up.boundary The last event for the target section
#' @param t The previous valid last event index
#' @param j The current section index
#' 
#' @return TRUE if the event is valid, FALSE if the event is not valid
#' 
#' @keywords internal
#' 
overrideEventCheck <- function(type, the.event, up.boundary, t, last.events, j, sections, movements){
  valid = TRUE
  if (t == 0 && (the.event < 1 | the.event > up.boundary)) {
    cat(paste("M: Invalid row (", type ," event must be between 1 and ", up.boundary, "). Please try again.\n", sep = ""))
    if (type == "last") cat("   Leave empty to indicate that the fish did not pass by this section.\n")
    valid = FALSE
  }
  if (t > 0 && (the.event <= last.events[t] | the.event > up.boundary)) {
   cat(paste("M: Invalid row (", type ," event must be between ", (last.events[t] + 1), " and ", up.boundary, "). Please try again.\n", sep = ""))
    if (type == "last") cat("   Leave empty to indicate that the fish did not pass by this section.\n")
    valid = FALSE
  }
  if (valid && !grepl(sections[j], movements[the.event, "Array"])) {
    cat(paste("M: Invalid row (not a ", sections[j], " event), please try again.\n", sep = ""))
    valid = FALSE
  }
  return(valid)
}

#' Find last events
#'
#' Finds the last event for each section in the study area.
#' 
#' @inheritParams actel
#' @inheritParams simplifyMovements
#' @inheritParams overrideDefaults
#' @param processing.type the current processing type for the fish. Is supplied by the function assembleTimetable.
#' 
#' @return A vector of the movement numbers corresponding to each last event.
#' 
#' @keywords internal
#' 
findLastEvents <- function(i, movements, sections, minimum.detections, processing.type, cautious.assignment) {
  # NOTE: The NULL variables below are actually column names used by data.table.
  # This definition is just to prevent the package check from issuing a note due unknown variables.
  Array <- NULL
  Valid <- NULL
  Detections <- NULL

  appendTo("debug", paste("Starting findLastEvents for fish ", i, ".", sep = ""))
  if (nrow(movements) > 1) {
    last.events <- rep(NA, length(sections))
    for (l in seq_len(length(sections))) {
      trigger <- FALSE
      matching.events <- movements[, which(grepl(sections[l], Array) & Valid)]
      if(length(matching.events) >= 1){
        if(cautious.assignment){
          trigger <- TRUE
          for(j in rev(matching.events)){
            if (movements[j, Detections] > 1) {
              last.events[l] <- j
              (break)
            } 
          }
        } else {
          last.events[l] <- tail(matching.events, 1)
        }
      }
      if (trigger && is.na(last.events[l])) {
        processing.type <- "Manual"
        ## I used cat here because the appendTo will be used later on, when the findFirstEvents comes up
        cat(paste0("W: No ", sections[l], " detection events with more than one detection were found for fish ", i, ". Opening movements list for inspection.\n"))
        print(movements[(Valid), -c("Valid")], topn = nrow(movements[(Valid), -c("Valid")]))
        cat("\n")
        check = TRUE
        while (check) {
          new.value <- suppressWarnings(as.numeric(commentCheck(line = paste("Which event should be considered the LAST", sections[l], "detection event?(comment) "), tag = i)))
          appendTo("UD", new.value)
          if (is.na(new.value)) {
            decision <- commentCheck(line = paste("W: The inserted value is not numeric. Continuing will erase all", sections[l], "related timestamps. Proceed?(y/N/comment) "), tag = i)
            appendTo("UD", decision)
            if (decision == "Y" | decision == "y") {
              appendTo("Screen", paste("M: Erasing ", sections[l], " related timestamps for fish ", i, ".", sep = ""))
              last.events[l] <- NA
              appendTo(c("Report", "Warning"), paste("W: No ", sections[l], " detection events with more than one detection were found for fish ", i, ". Erasing ", sections[l], " timestamps per user command.", 
              sep = ""))
              check = FALSE
            }
            rm(decision)
          } else {
            valid.row = TRUE
            new.value <- movements[(Valid), which = TRUE][new.value]
            if (!grepl(sections[l], movements[new.value, "Array"])) {
              cat("M: Invalid row (not a", sections[l], "event), please try again.\n")
              valid.row = FALSE
            }
            if (valid.row) {
              check = FALSE
              last.events[l] <- new.value
              appendTo(c("Report", "Warning"), paste("W: No ", sections[l], " detection events with more than one detection were found for fish ", i, ". Last ", sections[l], " row manually set to ", 
              new.value, ".", sep = ""))
            }
          }
        }
      }
    }
    recipient <- eventOrderCheck(i = i, last.events = last.events, 
      sections = sections, movements = movements, processing.type = processing.type)
    last.events <- recipient[[1]]
    processing.type <- recipient[[2]]
    rm(recipient)
  } else {
    # = there is only one detection event
    if (movements[1, "Detections"] >= minimum.detections) {
      last.events <- pmatch(sections, movements[, "Array"])
      # last.events <- last.events[1:grep(1, last.events)]
    } else {
      # = not enough detections
      appendTo(c("Screen", "Report"), paste("M: Fish ", i, " only has one movement entry (", movements[1, "Array"], ") with ", movements[1, "Detections"], " detections. Considered invalid.", sep = ""))
      last.events <- rep(NA, length(sections))
    }
  }
  appendTo("debug", paste("Terminating findLastEvents for fish ", i, ".", sep = ""))
  return(list(last.events = last.events, processing.type = processing.type))
}

#' Check event order
#'
#' Looks for evidence of backwards movements.
#' 
#' @inheritParams actel
#' @inheritParams simplifyMovements
#' @inheritParams overrideDefaults
#' @inheritParams findLastEvents
#' 
#' @return A vector of the movement numbers corresponding to each last event.
#' 
#' @keywords internal
#' 
eventOrderCheck <- function(i, last.events, sections, movements, processing.type) {
  # NOTE: The NULL variables below are actually column names used by data.table.
  # This definition is just to prevent the package check from issuing a note due unknown variables.
  Valid <- NULL
  Array <- NULL  
  appendTo("debug", paste("Starting eventOrderCheck for fish ", i, ".", sep = ""))
  trigger <- vector()
  for (l in seq_len(length(sections))) {
    trigger[movements[(Valid), grep(sections[l], Array)]] <- l
  }
  rm(l)
  if (is.unsorted(trigger)) {
    ## If Inter-section backward movements detected
    if (is.unsorted(last.events, na.rm = T)) {
      ## If last events are not ordered
      appendTo(c("Screen", "Report", "Warning"), paste("W: Inter-section backwards movements were detected for Fish ", i, " and the last events are not ordered!", sep = ""))
      not.ordered.trigger <- TRUE
      decision <- "Y"
      cat("   Opening movements list for inspection.\n")
    } else {
      ## If last events are ordered
      appendTo(c("Screen", "Report", "Warning"), paste("W: Inter-section backwards movements were detected for Fish ", i, ".", sep = ""))
      not.ordered.trigger <- FALSE
      decision <- commentCheck(line = paste("Would you like to see the movement table for fish", i, "?(y/N/comment) "), tag = i)
      appendTo("UD", decision)
    }
    if (decision == "Y" | decision == "y") {
      ## If the user decides to see the movements
      print(movements[(Valid), -c("Valid")], topn = nrow(movements[(Valid), -c("Valid")]))
      rm(decision)
      cat("\n")
      appendTo("Screen", paste("Current last events: ", paste(last.events, collapse = ", "), " (", paste(sections, collapse = ", "), ").", sep = ""))
      cat("\n")
      if (not.ordered.trigger) {
        appendTo(c("Screen"), "The last movement event of a section must NOT precede the last movement event of a \nprevious section (i.e. the migration analysis cannot cope with inter-section U turns).\nPlease edit the last valid events so this is not the case anymore.\n")
        decision <- "Y"
      } else {
        decision <- commentCheck(line = "Would you like to edit the last valid events?(y/N/comment) ", tag = i)
      }
      appendTo("UD", decision)
      if (decision == "Y" | decision == "y") {
        ## If the user decides to edit the last events
        while (decision == "Y" | decision == "y") {
          position <- suppressWarnings(as.numeric(commentCheck(line = paste("Which last valid event would you like to edit?(1-", length(last.events), "/comment) ", sep = ""), tag = i)))
          ## The position is the last event to edit
          appendTo("UD", position)
          if (is.na(position)) {
            ## If no position was set
            decision <- commentCheck(line = "W: The inserted value is not numeric. Abort last valid events' edition?(y/N/comment) ", tag = i)
            appendTo("UD", decision)
            if (decision != "Y" & decision != "y") {
              decision <- "Y"
            } else {
              if (is.unsorted(last.events, na.rm = T)) {
                ## If the user decides to abort but last events are still not ordered
                cat("\n")
                appendTo("Screen", paste("Current last events: ", paste(last.events, collapse = ", "), " (", paste(sections, collapse = ", "), ").", sep = ""))
                cat("\n")
                appendTo(c("Screen"), "The last movement event of a section must NOT precede the last movement event\nof a previous section (i.e. Actel cannot cope with inter-section U turns).\nPlease edit the last valid events so this is not the case anymore.\n")
                decision <- "Y"
              } else {
                decision <- "N"
              }
            }
          } else {
            ## If the position is numeric
            if (position > 0 & position <= length(last.events)) {
              ## If the position is within the limits of the last events
              check = TRUE
              invalidate = FALSE
              while (check) {
                new.value <- suppressWarnings(as.numeric(commentCheck(line = paste("New last valid event for ", sections[position], ": ", sep = ""), tag = i)))
                appendTo("UD", new.value)
                if (is.na(new.value)) {
                  ## If the selected row is not numeric
                  decision <- commentCheck(line = paste("W: The inserted value is not numeric. Continuing will render ", sections[position], " events invalid. Proceed?(y/N/comment) ", sep = ""), tag = i)
                  appendTo("UD", decision)
                  if (decision == "Y" | decision == "y") {
                    ## Decides to remove the last event
                    check = FALSE
                    invalidate = TRUE
                  }
                  rm(decision)
                } else {
                  # If the selected row is numeric
                  new.value <- movements[(Valid), which = TRUE][new.value]
                  if (!grepl(sections[position], movements[new.value, "Array"])) {
                    ## If the elected row does not belong to the right section
                    cat("M: Invalid row (not a", sections[position], "event), please try again.\n")
                  } else {
                    ## else terminate the while
                    check = FALSE
                  }
                }
              }
              rm(check)
              if (invalidate) {
                ## If the user decided to remove the last event
                last.events[position] <- NA
              } else {
                ## If a new value was set
                last.events[position] <- new.value
              }
              rm(position, new.value, invalidate)
              if (is.unsorted(last.events, na.rm = T)) {
                ## if after the edition the values are still not ordered, edits must continue
                cat("\n")
                appendTo("Screen", paste("Current last events: ", paste(last.events, collapse = ", "), " (", paste(sections, collapse = ", "), ").", sep = ""))
                cat("\n")
                appendTo("Screen", "The last events are not ordered. Please continue editing the last events until the last movement \nevent of each section is NOT lower than the last movement event of a section that precedes it.")
                decision <- "Y"
              } else {
                ## Else the user can decide if he wants to continue or stop
                decision <- commentCheck(line = "Continue editing last valid events?(y/N/comment) ", tag = i)
              }
              appendTo("UD", decision)
            } else {
              ## If the position is outside the limits of the last events
              cat("M: That event is not available, please try again.\n")
            }
          }
        }
        appendTo("Report", paste0("M: Last valid events for fish ", i, " were manually edited to: ", paste(last.events, collapse = ", "), "."))
        processing.type <- "Manual"
      } else {
        appendTo("Report", paste0("M: Default last valid events for fish ", i, " were kept upon inspection."))
      }
    } else {
      appendTo("Report", paste0("M: Default last valid events for fish ", i, " were kept without inspection."))
    }
  }
  appendTo("debug", paste0("Terminating eventOrderCheck for fish ", i, "."))
  return(list(last.events = last.events, processing.type = processing.type))
}

#' Find starting row
#'
#' Finds the row from which the function findFirstEvents should start looking for the first event.
#' 
#' @inheritParams actel
#' @inheritParams simplifyMovements
#' @inheritParams checkPrevious
#' @param l The section currently being analysed. Supplied by findFirstEvents
#' 
#' @return The row number
#' 
#' @keywords internal
#' 
findFirstRow <- function(l, last.events, movements, sections) {
  # NOTE: The NULL variables below are actually column names used by data.table.
  # This definition is just to prevent the package check from issuing a note due unknown variables.
  Array <- NULL
  Valid <- NULL

  appendTo("debug", "Starting findFirstRow.")
  if (l > 1) {
    for (k in rev(seq_len(l - 1))) {
      # Prevent script from breaking if the fish was not detected on the previous section
      if (!is.na(last.events[k])) {
        first.row = last.events[k] + 1
        (break)
      }
    }
    rm(k)
  }
  if (!exists("first.row")) {
    for (k in seq_len(nrow(movements))) {
      if (movements[k, grepl(sections[l], Array) & Valid]) {
        first.row = k
        (break)
      }
    }
    rm(k)
  }
  appendTo("debug", "Terminating findFirstRow.")
  return(first.row)
}

#' Find first events
#'
#' Finds the first event for each section in the study area that has a last event.
#' 
#' @inheritParams actel
#' @inheritParams simplifyMovements
#' @inheritParams overrideDefaults
#' @inheritParams checkPrevious
#' @inheritParams findLastEvents
#' 
#' @return A list containing both the first and last events.
#' 
#' @keywords internal
#' 
findFirstEvents <- function(i, last.events, movements, sections, processing.type, cautious.assignment) {
  # NOTE: The NULL variables below are actually column names used by data.table.
  # This definition is just to prevent the package check from issuing a note due unknown variables.
  Array <- NULL
  Valid <- NULL
  Detections <- NULL

  appendTo("debug", paste("Starting findFirstEvents for fish ", i, ".", sep = ""))
  first.events <- rep(NA, length(sections))
  for (l in seq_len(length(sections))) {
    # If the fish left the section
    trigger <- FALSE
    if (!is.na(last.events[l])) {
      first.row <- findFirstRow(l, last.events, movements, sections)
      matching.events <- movements[, which(grepl(sections[l], Array) & Valid)]
      valid.events <- matching.events[matching.events >= first.row]
      if(cautious.assignment){
        trigger <- TRUE
        for(j in valid.events){
          if (movements[j, Detections] > 1) {
            first.events[l] <- j
            (break)
          } 
        }
      } else {
        first.events[l] <- head(valid.events, 1)
      }
    }
    if (trigger && is.na(first.events[l])) {
      check = TRUE
      while (check) {
        new.value <- suppressWarnings(as.numeric(commentCheck(line = paste("Which event should be considered the FIRST", sections[l], "detection event?(comment) "), tag = i)))
        appendTo("UD", new.value)
        if (is.na(new.value)) {
          decision <- commentCheck(line = paste("W: The inserted value is not numeric. Continuing will erase all", sections[l], "related timestamps. Proceed?(y/N) "), tag = i)
          appendTo("UD", decision)
          if (decision == "Y" | decision == "y") {
            appendTo("Screen", paste("M: Erasing ", sections[l], " related timestamps for fish ", i, ".\n", sep = ""))
            first.events[l] <- last.events[l] <- NA
            appendTo(c("Report", "Warning"), paste("W: No ", sections[l], " detection events with more than one detection were found for fish ", i, ". Erasing ", sections[l], 
            " timestamps per user command.", sep = ""))
            check = FALSE
          }
          rm(decision)
        } else {
          valid.row = TRUE
          new.value <- movements[(Valid), which = TRUE][new.value]
          if (!grepl(sections[l], movements[new.value, "Array"])) {
            cat("M: Invalid row (not a", sections[l], "event), please try again.\n")
            valid.row = FALSE
          }
          if (new.value < first.row | new.value > last.events[l]) {
            cat("M: Invalid row (out of bounds), please try again. Valid range is ", first.row, " to ", last.events[l], ".\n", sep = "")
            valid.row = FALSE
          }
          if (valid.row) {
            check = FALSE
            first.events[l] <- new.value
            appendTo(c("Report", "Warning"), paste("W: No ", sections[l], " detection events with more than one detection were found for fish ", i, ". First ", sections[l], " row manually set to ", 
            new.value, ".", sep = ""))
          }
        }
      }
    }
  }
  appendTo("debug", paste("Terminating findFirstEvents for fish ", i, ".", sep = ""))
  return(list(events = list(first.events = first.events, last.events = last.events),
        processing.type = processing.type))
}

#' Deploy chosen values
#'
#' Compiles the information supplied and deploys it into the correct timetable row.
#' 
#' @param timetable A table of the entering and leaving points for each section per target tag, created by assembleTimetable.
#' @inheritParams actel
#' @inheritParams simplifyMovements
#' @inheritParams overrideDefaults
#' @inheritParams loadDetections
#' @inheritParams groupMovements
#' @inheritParams findLastEvents
#' @param events A list containing both the first and last events. Supplied by findFirstEvents.
#' 
#' @return A list containing both the first and last events.
#' 
#' @keywords internal
#' 
deployValues <- function(i, timetable, movements, events, sections, spatial, 
  dist.mat, invalid.dist, if.last.skip.section, success.arrays, processing.type, speed.method) {
  # NOTE: The NULL variables below are actually column names used by data.table.
  # This definition is just to prevent the package check from issuing a note due unknown variables.
  Detections <- NULL
  Array <- NULL

  appendTo("debug", paste("Starting deployValues for fish ", i, ".", sep = ""))
  if (any(!is.na(events$last.events))) {
    last.last <- tail(events$last.events[!is.na(events$last.events)], 1)
    for (j in seq_len(length(sections))) {
      if (!is.na(events$first.events[j])) {
        timetable[i, paste("Arrived", sections[j], sep = ".")] <- paste(movements[[events$first.events[j], "First.time"]])
        timetable[i, paste("First.station", sections[j], sep = ".")] <- movements[[events$first.events[j], "First.station"]]
        timetable[i, paste("Left", sections[j], sep = ".")] <- paste(movements[[events$last.events[j], "Last.time"]])
        timetable[i, paste("Last.station", sections[j], sep = ".")] <- movements[[events$last.events[j], "Last.station"]]
        timetable[i, paste("Time.in", sections[j], sep = ".")] <- as.vector(difftime(timetable[i, paste("Left", sections[j], sep = ".")], timetable[i, paste("Arrived", sections[j], sep = ".")], 
          units = "secs"))
        if (speed.method == "last to first" && !invalid.dist) {
          to.col <- paste("Speed.in", sections[j], sep = ".")
          dist.row <- timetable[i, paste("First.station", sections[j], sep = ".")]
          dist.col <- timetable[i, paste("Last.station", sections[j], sep = ".")]
          from.col <- paste("Time.in", sections[j], sep = ".")
          timetable[i, to.col] <- dist.mat[dist.row, dist.col]/timetable[i, from.col]
        }
      }
      if (j > 1) {
        testA <- !is.na(timetable[i, paste("Arrived", sections[j], sep = ".")])
        testB <- !is.na(timetable[i, paste("Left", sections[j - 1], sep = ".")])
        if (testA & testB) {
          to.col <- paste("Time.until", sections[j], sep = ".")
          from.colA <- paste("Arrived", sections[j], sep = ".")
          from.colB <- paste("Left", sections[j - 1], sep = ".")
          AtoB <- difftime(timetable[i, from.colA], timetable[i, from.colB], units = "secs")
          timetable[i, to.col] <- as.vector(AtoB)
          if (!invalid.dist) {
            to.col <- paste("Speed.to", sections[j], sep = ".")
            dist.row <- timetable[i, paste("First.station", sections[j], sep = ".")]
            if (speed.method == "last to first"){
              dist.col <- timetable[i, paste("Last.station", sections[j - 1], sep = ".")]
              total.time <- timetable[i, paste("Time.until", sections[j], sep = ".")]
            }
            if (speed.method == "first to first"){
              dist.col <- timetable[i, paste("First.station", sections[j - 1], sep = ".")]
              total.time <- timetable[i, paste("Time.in", sections[j - 1], sep = ".")] + 
                            timetable[i, paste("Time.until", sections[j], sep = ".")]
            }
            timetable[i, to.col] <- dist.mat[dist.row, dist.col]/ total.time
          }
        }
      }
      # If the last last event was on the active section
      if (grepl(sections[j], movements$Array[last.last])) {
        # If we are not on the last section and the last last event was on the last array of the section
        partA <- movements$Array[last.last]
        partB <- spatial$array.order[[sections[j]]]
        long.test <- match(partA, partB) == length(spatial$array.order[[sections[j]]])
        if (if.last.skip.section && j < length(sections) && long.test) {
          timetable[i, "Status"] <- paste("Disap. in", sections[j + 1])
        } else {
          timetable[i, "Status"] <- paste("Disap. in", sections[j])
        }
      }
      timetable[i, "Very.last.array"] <- movements$Array[last.last]
    }
  } else {
    timetable[i, "Status"] <- paste("Disap. in", sections[1])
    timetable[i, "Very.last.array"] <- "Release"
  }
  timetable[i, "Detections"] <- movements[, sum(Detections)]

  if (movements[, any(Array != "Unknown")]) {
    temp <- movements[Array != "Unknown", ]
    recipient <- countUpMoves(movements = temp, spatial = spatial)
    timetable[i, "Backwards.movements"] <- recipient[[1]]
    timetable[i, "Max.cons.back.moves"] <- recipient[[2]]
  } else {
    timetable[i, "Backwards.movements"] <- 0
    timetable[i, "Max.cons.back.moves"] <- 0
  }
  timetable[i, "P.type"] <- processing.type
  testA <- !is.na(timetable[i, paste("Left", tail(sections, 1), sep = ".")])
  testB <- any(!is.na(match(success.arrays, movements[tail(events$last.events, 1), Array])))
  if (testA & testB) 
    timetable[i, "Status"] <- "Succeeded"
  appendTo("debug", paste("Terminating deployValues for fish ", i, ".", sep = ""))
  return(timetable)
}

#' Count backwards movements
#' 
#' @inheritParams simplifyMovements
#' @inheritParams loadDetections
#' 
#' @keywords internal
#' 
#' @return The number of backwards movements and the maximum consecutive backwards movements
#' 
countUpMoves <- function(movements, spatial){
  # NOTE: The NULL variables below are actually column names used by data.table.
  # This definition is just to prevent the package check from issuing a note due unknown variables.
  Array <- NULL

  appendTo("debug", "Starting countUpMoves.")
  if (nrow(movements) > 1) {# Determine number of backwards movements
    array.sequence <- vector()
    for (i in seq_len(length(unlist(spatial$array.order)))) {
      array.sequence[grep(unlist(spatial$array.order)[i], movements[, Array])] <- i
    }
    backwards.movement <- vector()
    for (i in 2:length(array.sequence)) {
      backwards.movement[i - 1] <- array.sequence[i] < array.sequence[i - 1]
    }
    up.moves <- sum(backwards.movement)
    # Find out maximum consecutive backwards movements
    recipient <- vector()
    restart <- TRUE
    if (length(backwards.movement) > 1) {
      for (i in 2:length(backwards.movement)) {
        if (restart) {
          counter <- 1
          store <- FALSE
        }
        if (backwards.movement[i]) {
          store <- TRUE
          if (backwards.movement[i - 1]) {
            restart <- FALSE
            counter <- counter + 1
          } else {
            restart <- TRUE
          }
        }
        if (store) {
          recipient[length(recipient) + 1] <- counter
        }
      }
      if (length(recipient) == 0) {
        if (up.moves == 0) {
          Max.back.moves <- 0
        } else {
          Max.back.moves <- 1
        }
      } else {
        Max.back.moves <- max(recipient)
      }
    } else {
      if (backwards.movement) {
        Max.back.moves <- 1
      } else {
        Max.back.moves <- 0
      }
    }
  } else {
    up.moves <- 0
    Max.back.moves <- 0
  }
  appendTo("debug", "Terminating countUpMoves.")
  return(list(up.moves = up.moves, Max.back.moves = Max.back.moves))
}

#' Create status.df
#'
#' Combines the timetable and the original biometrics.
#' 
#' @inheritParams actel
#' @inheritParams deployValues
#' @inheritParams splitDetections
#' @inheritParams simplifyMovements
#' @inheritParams loadDetections
#' @inheritParams groupMovements
#' 
#' @return A data frame containing all the final data for each fish.
#' 
#' @keywords internal
#' 
assembleOutput <- function(timetable, bio, movements, spatial, sections, dist.mat, invalid.dist, tz.study.area) {
  appendTo("debug", "Merging 'bio' and 'timetable'.")
  status.df <- merge(bio, timetable, by = "Transmitter", all = T)
  
  appendTo("debug", "Completing entries for fish that were never detected.")
  status.df$Status[is.na(status.df$Status)] <- paste("Disap. in", sections[1])
  status.df$Status <- factor(status.df$Status, levels = c(paste("Disap. in", sections), "Succeeded"))
  status.df$Very.last.array[is.na(status.df$Very.last.array)] <- "Release"
  status.df$Very.last.array <- factor(status.df$Very.last.array, levels = c("Release", levels(spatial$stations$Array)))
  status.df$P.type[is.na(status.df$P.type)] <- "Skipped"
  status.df$Detections[is.na(status.df$Detections)] <- 0

  the.cols <- grepl("Release.date", colnames(status.df)) | grepl("Arrived",colnames(status.df)) | grepl("Left",colnames(status.df))
  for (i in colnames(status.df)[the.cols]) {
    status.df[, i] <- as.POSIXct(status.df[,i], tz = tz.study.area)
  }
  rm(the.cols)
  
  appendTo("debug", "Calculating time from release to first detection.")
  for (i in 1:nrow(status.df)) {
    appendTo("debug", paste("(status.df) Analysing fish ", status.df$Signal[i], " (", i, ").", sep = ""))
    arriving.points <- status.df[i, paste("Arrived", sections, sep = ".")]
    if (any(!is.na(arriving.points))) {
      first.section <- sections[head(which(!is.na(arriving.points)), 1)]
      pointA <- as.POSIXct(status.df[i, paste("Arrived", first.section, sep = ".")], tz = tz.study.area)
      pointB <- as.POSIXct(status.df[i, "Release.date"], tz = tz.study.area)
      AtoB <- as.vector(difftime(pointA, pointB, units = "secs"))
      status.df[i, paste("Time.until", first.section, sep = ".")] <- AtoB
      if (!invalid.dist) {
        dist.row <- status.df[i, paste("First.station", first.section, sep = ".")]
        dist.col <- as.character(status.df[i, "Release.site"])
        df.to.col <- paste("Speed.to", first.section, sep = ".")
        df.from.col <- paste("Time.until", first.section, sep = ".")
        status.df[i, df.to.col] <- dist.mat[dist.row, dist.col]/status.df[i, df.from.col]
      }
      rm(AtoB)
    }
  }
  rm(i)
  
  if (file.exists("temp_comments.txt")) {
    temp <- read.table("temp_comments.txt", header = F, sep = "\t")
    status.df[, "Script.comments"] <- NA
    for (i in seq_len(nrow(temp))) {
      link <- match(temp[i, 1], status.df$Transmitter)
      if (is.na(status.df$Script.comments[link])) {
        status.df$Script.comments[link] <- paste(temp[i, 2])
      } else {
        status.df$Script.comments[link] <- paste(status.df$Script.comments[link], temp[i, 2], sep = "// ")
      }
    }
  }
  appendTo("debug", "Done.")
  return(status.df)
}

#' Create section.overview
#'
#' Produces a table with the survival per group of fish present in the biometrics.
#' 
#' @inheritParams actel
#' @inheritParams simplifyMovements
#' 
#' @return A data frame containing the survival per group of fish present in the biometrics.
#' 
#' @keywords internal
#' 
assembleSectionOverview <- function(status.df, sections) {
  appendTo("debug", "Starting assembleSectionOverview.")
  section.overview <- as.data.frame.matrix(with(status.df, table(Group, Status)))
  section.overview$Total <- as.vector(with(status.df, table(Group)))
  colnames(section.overview) <- gsub(" ", ".", colnames(section.overview))
  if (length(sections) >= 2) {
    to.col <- paste("Migrated.to", sections[2], sep = ".")
    from.col <- paste("Disap..in", sections[1], sep = ".")
    section.overview[, to.col] <- section.overview$Total - section.overview[, from.col]
    recipient <- vector()
    for (i in 2:length(sections)) {
      recipient <- c(recipient, paste(c("Migrated.to", "Disap..in"), sections[i], sep = "."))
    }
  } else {
    recipient <- NULL
  }
  if (length(sections) > 2) {
    for (i in 3:length(sections)) {
      to.col <- paste("Migrated.to", sections[i], sep = ".")
      from.colA <- paste("Migrated.to", sections[i - 1], sep = ".")
      from.colB <- paste("Disap..in", sections[i - 1], sep = ".")
      section.overview[, to.col] <- section.overview[, from.colA] - section.overview[, from.colB]
    }
  }
  recipient <- c("Total", paste("Disap..in", sections[1], sep = "."), recipient, "Succeeded")
  appendTo("debug", "Terminating assembleSectionOverview.")
  return(section.overview[, recipient])
}


#' Create array.overview
#'
#' @return A data frame containing the progression per group of fish present in the biometrics.
#' 
#' @keywords internal
#' 
assembleArrayOverview <- function(group.CJS) {
  appendTo("debug", "Starting assembleArrayOverview.")
  recipient <- lapply(group.CJS, function(x) x$absolutes)

  for (i in 1:length(recipient)) {
    recipient[[i]][1, ] <- apply(recipient[[i]][c(1,3), ], 2, sum, na.rm = TRUE)
    recipient[[i]][2, ] <- recipient[[i]][4, ]
    recipient[[i]][3, ] <- recipient[[i]][2, ] - recipient[[i]][1, ]
    recipient[[i]] <- recipient[[i]][1:3, ]
    rownames(recipient[[i]]) <- c("Known", "Estimated", "Difference")
  }
  appendTo("debug", "Terminating assembleArrayOverview.")  
  return(recipient)
}

#' Create array.overview
#'
#' @return A data frame containing the progression per group of fish present in the biometrics.
#' 
#' @keywords internal
#' 
mbAssembleArrayOverview <- function(input) {
  appendTo("debug", "Starting mbAssembleArrayOverview.")
  for (i in 1:length(input)) {
    input[[i]][1, ] <- apply(input[[i]][c(1,3), ], 2, sum, na.rm = TRUE)
    input[[i]][2, ] <- input[[i]][4, ]
    input[[i]][3, ] <- input[[i]][2, ] - input[[i]][1, ]
    input[[i]] <- input[[i]][1:3, ]
    rownames(input[[i]]) <- c("Known", "Estimated", "Difference")
  }
  appendTo("debug", "Terminating mbAssembleArrayOverview.")  
  return(input)
}
