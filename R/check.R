#' check.R arguments
#' 
#' @param arrays a list containing information for each array
#' @param bio A table with the tags and biometrics of the studied fish.
#' @param detections.list A list of the detections split by each target tag, 
#'  created by \code{\link{splitDetections}}.
#' @param dotmat The matrix of distances between arrays
#' @param fish The fish being analysed
#' @param GUI One of "needed", "always" or "never". If "needed", a new window is
#'  opened to inspect the movements only if the movements table is too big to be
#'  displayed in R's console. If "always", a graphical interface is always created
#'  when the possibility to invalidate events emerges. If "never", a graphical
#'  interface is never invoked. In this case, if the table to be displayed does
#'  not fit in R's console, a temporary file will be saved and the user will be
#'  prompted to open and examine that file. Defaults to "needed".
#' @param movements,moves The movements table
#' @param release The release location of the fish
#' @param secmoves the section movements list
#' @param spatial The spatial data frame, as loaded by loadSpatial
#' @param valid.movements The valid movements table
#' 
#' @name check_args
#' @keywords internal
#' 
NULL

#' Check that section names are not duplicated and that sections are not contained 
#' within other section names (e.g. section "N" and section "NW").
#' 
#' @inheritParams migration
#' 
#' @return No return value, called for side effects.
#' 
#' @keywords internal
#' 
checkSectionsUnique <- function(sections) {
  if (any(table(sections) > 1))
    stop("Some section names are duplicated. Please include each section only once in the 'sections' argument.\n", call. = FALSE)
  if (any(link <- sapply(sections, function(i) length(grep(i, sections))) > 1))
    stop(paste0(
      ifelse(sum(link) == 1, "Section '", "Sections '"), 
      paste(sections[link], collapse = "', '"), 
      ifelse(sum(link) == 1, "' is", "' are"), 
      " contained within other section names. Sections must be unique and independent.\n       Please rename your sections and arrays so that section names are not contained within each other.\n"), call. = FALSE)
}

#' Handler for table interaction events
#' 
#' @inheritParams check_args
#' @param trigger The message/warning that triggered the interaction
#' @param force Logical: If TRUE, the user is moved directly to indicating which movements should be invalidated.
#' 
#' @return A data frame with the updated movements table
#' 
#' @keywords internal
#' 
tableInteraction <- function(moves, fish, trigger, GUI, force = FALSE) { # nocov start
  if (GUI == "never")
    popup <- FALSE
  if (GUI == "needed") {
    if (nrow(moves) > min(800, (getOption("max.print") - 2)))
      popup <- TRUE
    else
      popup <- FALSE
  }
  if (GUI == "always")
    popup <- TRUE

  if (popup) {
    output <- graphicalInvalidate(moves = moves, fish = fish, trigger = trigger)
    decision <- readline(paste0("Would you like to leave a comment for fish ", fish, "?(y/N) "))
    appendTo(c("UD"), decision)
    if (decision == "y" | decision == "Y") {
      appendTo(c("UD", "Comment"), readline(paste0("New comment on fish ", fish, ": " )), fish)
      appendTo("Screen", "M: Comment successfully stored, returning to the previous interaction.")
    }
  } else {
    if (nrow(moves) > min(800, (getOption("max.print") - 2))) {
      message("The movements table for fish '", fish, "' is too large to display on the console and GUI is set to 'never'.\nTemporarily saving the table to '", paste0(tempdir(), '/actel_inspect_movements.csv'), "'. Please inspect this file and decide if any events should be considered invalid.\nPlease use the 'Event' column as a reference for the event number.")
      to.print <- cbind(data.frame(Event = 1:nrow(moves)), moves)
      write.csv(to.print, paste0(tempdir(), "/actel_inspect_movements.csv"), row.names = FALSE)
      if (force) {
        output <- invalidateEvents(movements = moves, fish = fish)
      } else {  
        decision <- commentCheck(line = "Would you like to render any movement event invalid?(y/N/comment) ", tag = fish)
        appendTo("UD", decision)
        if (decision == "y" | decision == "Y") {
          output <- invalidateEvents(movements = moves, fish = fish)
        } else {
          output <- moves
        }
      }  
      first.try <- TRUE
      while (file.exists(paste0(tempdir(), "/actel_inspect_movements.csv"))) {
        if (!suppressWarnings(file.remove(paste0(tempdir(), "/actel_inspect_movements.csv")))) {
          if (first.try) {
            warning("Please close the currently open 'actel_inspect_movements.csv' file so the analysis can continue", immediate. = TRUE, call. = FALSE); flush.console()
            first.try <- FALSE
          }
        }
      }
    } else {
      message("Opening movements list for inspection:\n")
      message(paste0(capture.output(print(moves, topn = nrow(moves))), collapse = "\n"))
      if (nrow(moves) >= 100)
        message("\nM: Long table detected, repeating warning(s) that triggered the interaction:\n-----\n", trigger, "\n-----")
      if (nrow(moves) < 100 & nrow(moves) >= 30)
        message("\nM: Please find the exception which triggered this interaction at the top of the table.")
        message("")
      if (force) {
        output <- invalidateEvents(movements = moves, fish = fish)
      } else {
        decision <- commentCheck(line = "Would you like to render any movement event invalid?(y/N/comment) ", tag = fish)
        appendTo("UD", decision)
        if (decision == "y" | decision == "Y") {
          output <- invalidateEvents(movements = moves, fish = fish)
        } else {
          output <- moves
        }
      }
    }
  }
  return(output)
} # nocov end

#' Check if the dependencies required to open a GUI are installed, and if
#' opening a GUI does not throw an error. Some code adapted from RGtk2's 
#' .onLoad function.
#' 
#' @inheritParams explore
#' 
#' @keywords internal
#' 
#' @return An updated GUI argument (character string)
#'
#' 
checkGUI <- function(GUI = c("needed", "always", "never")) {
  if (!is.character(GUI))
    stop("'GUI' should be one of 'needed', 'always' or 'never'.\n", call. = FALSE)
  GUI <- match.arg(GUI)
  if (GUI != "never") {
    aux <- c(
      length(suppressWarnings(packageDescription("gWidgets2"))),
      length(suppressWarnings(packageDescription("gWidgets2RGtk2"))),
      length(suppressWarnings(packageDescription("RGtk2"))))
    missing.packages <- sapply(aux, function(x) x == 1)
    if (any(missing.packages)) {
      appendTo(c("Screen", "Warning", "Report"), 
        paste0("GUI is set to '", GUI, "' but ", 
          ifelse(sum(missing.packages) == 1, "package '", "packages '"),
          paste(c("gWidgets2", "gWidgets2RGtk2", "RGtk2")[missing.packages], collapse = "', '"),
          ifelse(sum(missing.packages) == 1, "' is", "' are"),
          " not available. Please install ",
          ifelse(sum(missing.packages) == 1, "it", "them"),
          " if you intend to run GUI.\n         Disabling GUI (i.e. GUI = 'never') for the current run."))
      GUI <- "never"
    } else {
      if (.Platform$OS.type == "windows") {
        dllpath <- Sys.getenv("RGTK2_GTK2_PATH")
        if (!nzchar(dllpath))
          dllpath <- file.path(file.path(system.file(package = "RGtk2"), "gtk", .Platform$r_arch), "bin")
        dll <- try(library.dynam("RGtk2", "RGtk2", sub("/RGtk2", "", find.package("RGtk2")), DLLpath = dllpath), silent = TRUE)
      } else {
       dll <- try(library.dynam("RGtk2", "RGtk2", sub("/RGtk2", "", find.package("RGtk2"))), silent = TRUE)
      }
      if (is.character(dll)) {
       appendTo(c("Screen", "Warning", "Report"), 
        paste0("GUI is set to '", GUI,
        "' but loading of RGtk2 dll failed. Please run e.g. gWidgets2::gtext() to trigger the installation of RGtk2's dll and then restart R.\n         Disabling GUI (i.e. GUI = 'never') for the current run."))
       GUI <- "never"
      }
    }
  }
  return(GUI)
}

#' Check if there are duplicated detection in the input data
#' 
#' @param input The detections data frame
#' 
#' @return The detections data frame, without duplicated detections.
#' 
#' @keywords internal
#' 
checkDupDetections <- function(input) {
  appendTo("debug", "Running overrideDefaults.")
  aux <- data.frame(
    TimestampA = input$Timestamp[-nrow(input)],
    TimestampB = input$Timestamp[-1],
    ReceiverA = input$Receiver[-nrow(input)],
    ReceiverB = input$Receiver[-1],
    TransmitterA = input$Transmitter[-nrow(input)],
    TransmitterB = input$Transmitter[-1]
    )
  dups <- c(FALSE, aux[,1] == aux[,2] & aux[,3] == aux[,4] & aux[,5] == aux[,6])
  if (any(dups)) {
    appendTo(c("Screen", "Report", "Warning"), paste0(sum(dups), " duplicated detection", ifelse(sum(dups) == 1, " was", "s were"), " found. Could an input file be duplicated?"))
    message("")
    appendTo("Screen", "Possible options:\n   a) Stop and double-check the data\n   b) Remove duplicated detections\n   c) Continue without changes")
    message("")
    if (interactive()) { # nocov start
      unknown.input = TRUE
      while (unknown.input) {
        decision <- readline("Decision:(a/b/c) ")
        if (decision == "a" | decision == "A") {
          unknown.input = FALSE
          appendTo("UD", decision)
          emergencyBreak()
          stop("Function stopped by user command.", call. = FALSE)
        }
        if (decision == "b" | decision == "B") {
          appendTo("UD", decision)
          unknown.input = FALSE
          appendTo(c("Screen", "Report"), "M: Removing duplicated detections from the analysis per user command.")
          output <- input[!dups, ]
        }
        if (decision == "c" | decision == "C") {
          appendTo("UD", decision)
          unknown.input = FALSE
          appendTo(c("Screen", "Report"), "M: Continuing analysis with duplicated detections per user command.")
          output <- input
        }
        if (unknown.input) {
          appendTo("Screen", "Option not recognized, please input either 'a', 'b', 'c' or 'comment'.")
        }
      } 
    } else { # nocov end
      appendTo("Report", "M: Not running in interactive mode, deleting duplicated detections by default.")
      output <- input[!dups, ]
    }
  } else {
    output <- input
  }
  return(output)
}


#' Skips all validity checks for a fish and allows the user to freely invalidate events
#' 
#' @inheritParams check_args
#' 
#' @return A data frame containing the movements for the target fish
#' 
#' @keywords internal
#' 
overrideValidityChecks <- function(moves, fish, GUI) { # nocov start
  appendTo("debug", "Starting overrideDefaults.")
  message("----------------------------")
  appendTo(c("Screen", "Report"), trigger <- paste0("M: Override has been triggered for fish ", fish, ". Entering full manual mode."))
  moves <- tableInteraction(moves = moves, fish = fish, trigger = trigger, GUI = GUI)
  attributes(moves)$p.type <- "Overridden"
  message("Terminating full manual mode\n----------------------------")
  return(moves) # nocov end
}

#' Check that the fish have enough detections to be valid
#' 
#' @inheritParams check_args
#' @inheritParams explore
#' 
#' @return A data frame containing the movements with valid/invalid notes
#' 
#' @keywords internal
#' 
checkMinimumN <- function(movements, minimum.detections, fish) {
  if (nrow(movements) == 1 && movements$Detections < minimum.detections) {
    appendTo(c("Screen", "Report", "Warning"), paste0("Fish ", fish, " only has one movement event (", movements$Array, ") with ", movements$Detections, " detections. Considered invalid."))
    movements$Valid <- FALSE
  }
  return(movements)
}

#' check fish speeds against defined thresholds (in m/s)
#' 
#' @inheritParams check_args
#' @inheritParams explore
#' 
#' @return A list of movements with updated 'Valid' columns
#' 
#' @keywords internal
#' 
checkSpeeds <- function(movements, fish, valid.movements, speed.warning, speed.error, GUI) {
  appendTo("debug", "Running checkSpeeds")
  the.warning <- NULL
  vm <- valid.movements
  if (any(na.as.false(vm$Average.speed.m.s >= speed.warning))) {
    link <- which(vm$Average.speed.m.s >= speed.warning)
    if (link[1] == 1) {
      appendTo(c("Report", "Warning", "Screen"), 
        the.warning <- paste0("Fish ", fish, " had an average speed of ", round(vm$Average.speed.m.s[1], 2),
          " m/s from release to first valid event (Release -> ", vm$Array[1], ")."))
      the.warning <- paste("Warning:", the.warning)
      link <- link[-1]
    }
    if (length(link) > 0) {
      for (i in 1:length(link)) {
        appendTo(c("Report", "Warning", "Screen"), 
          other.warning <- paste0("Fish ", fish, " had an average speed of ", round(vm$Average.speed.m.s[link[i]], 2),
            " m/s from valid event ", link[i] - 1, " to ", link[i], " (",vm$Array[link[i] - 1], " -> ", vm$Array[link[i]], ")."))
        other.warning <- paste("Warning:", other.warning)
        the.warning <- paste0(the.warning, "\n", other.warning)
      }
    }
  }
  # Trigger user interaction
  if (any(na.as.false(vm$Average.speed.m.s >= speed.error))) { # nocov start
    aux <- tableInteraction(moves = movements, fish = fish, trigger = the.warning, GUI = GUI)
    movements <- transferValidity(from = aux, to = movements)
  } # nocov end
  return(movements)   
}

#' Find if a fish is standing still in an array
#' 
#' @inheritParams check_args
#' @inheritParams explore
#' 
#' @return A list of movements with updated 'Valid' columns
#' 
#' @keywords internal
#' 
checkInactiveness <- function(movements, fish, detections.list, 
  inactive.warning, inactive.error, invalid.dist, dist.mat, GUI) {
  Valid <- NULL
  appendTo("debug", "Running checkInactiveness")
  if (any(movements$Valid)) {
    valid.moves <- movements[(Valid)]
    # Find first and last potentially inactive movement
    breaks <- rle(valid.moves$Array)
    if(length(breaks$lengths) > 1) {
      Start <- sum(breaks$lengths[1:(length(breaks$lengths) - 1)]) + 1
    } else {
      Start <- 1
    }
    Stop <- nrow(valid.moves)
    # Fetch respective detection rows
    valid.row.list <- lapply(Start:Stop, function(j) {
      Start <- min(which(detections.list$Timestamp == valid.moves$First.time[j] & detections.list$Standard.name == valid.moves$First.station[j]))
      Stop <- Start + (valid.moves$Detections[j] - 1)
      return(Start:Stop)
    })
    # Start trimming from the Start to see if a period of inactiveness is present
    continue <- TRUE
    iteration <- 1
    while (continue) {
      the.warning <- NULL
      # count days spent to compare with arguments
      start_i <- Start + iteration - 1
      # Stop if we have reached the end of the possible iterations
      if (start_i > Stop)
        break()
      days.spent <- round(as.numeric(difftime(valid.moves$Last.time[Stop], valid.moves$First.time[start_i], units = "days")), 2)
      # Stop if the days.spent are lesser than enough to trigger a warning
      if (days.spent < inactive.warning)
        break()
      valid.rows <- unlist(valid.row.list[iteration:length(valid.row.list)])
      the.detections <- detections.list[valid.rows, ]
      # find all stations
      the.stations <- as.character(sort(unique(the.detections$Standard.name)))
      trigger.error <- FALSE
      if (invalid.dist) {
        # Trigger warning
        if (length(the.stations) <= 3) {
          n.detections <- sum(valid.moves$Detections[start_i:Stop])
          appendTo(c("Report", "Warning", "Screen"), 
            the.warning <- paste0("Fish ", fish, " was detected ", n.detections, 
              " times at three or less stations of array '", tail(breaks$values, 1), 
              "' (", paste(the.stations, collapse = ", "), ") over ", days.spent, 
              " days and then disappeared. Could it be inactive?"))
          the.warning <- paste("Warning:", the.warning)
          continue <- FALSE
        }
        if (length(the.stations) <= 3 & days.spent >= inactive.error)
          trigger.error <- TRUE # nocov
      } else {
        aux <- dist.mat[the.stations, the.stations]
        if (all(aux <= 1500)) {
          n.detections <- sum(valid.moves$Detections[start_i:Stop])
          appendTo(c("Report", "Warning", "Screen"), 
            the.warning <- paste0("Fish ", fish, " was detected ", n.detections, 
              " times at stations less than 1.5 km apart in array '", tail(breaks$values, 1), 
              "' (", paste(the.stations, collapse = ", "), "), over ", days.spent, 
              " days and then disappeared. Could it be inactive?"))
          the.warning <- paste("Warning:", the.warning)
          continue <- FALSE
        }
        if (all(aux <= 1500) & days.spent >= inactive.error)
          trigger.error <- TRUE # nocov
      }
      # Trigger user interaction
      if (trigger.error) { # nocov start
        appendTo("Screen", paste0("M: ", fish, " has been inactive for more than ", inactive.error," days (inactiveness started on ", as.Date(valid.moves$First.time[start_i]),")"))
        aux <- tableInteraction(moves = valid.moves, fish = fish, trigger = the.warning, GUI = GUI)
        movements <- transferValidity(from = aux, to = movements)
      } # nocov end
      iteration <- iteration + 1
    }
  }
  return(movements)
}

#' Find out if a fish moved in an impossible direction
#' 
#' @inheritParams check_args
#' 
#' @return A list of movements with updated 'Valid' columns
#' 
#' @keywords internal
#' 
checkImpassables <- function(movements, fish, dotmat, GUI){
  appendTo("debug", "Running checkImpassables")
  Valid <- NULL
  restart <- TRUE
  while (restart) {
    restart <- FALSE
    if (sum(movements$Valid) > 1) {
      valid.moves <- movements[(Valid)]
      shifts <- data.frame(
        A = valid.moves$Array[-nrow(valid.moves)],
        B = valid.moves$Array[-1])
      distances <- apply(shifts, 1, function(x) dotmat[x[1], x[2]])
      if (any(is.na(distances))) {
        the.warning <- NULL
        sapply(which(is.na(distances)), function(i) {
          appendTo(c("Screen", "Warning", "Report"), aux <- paste0("Fish ", fish, " made an impassable jump: It is not possible to go from array ", shifts[i, 1], " to ", shifts[i, 2], ".\n         Please resolve this either by invalidating events or by adjusting your 'spatial.txt' file and restarting."))
          the.warning <- c(the.warning, aux)
        })
        if (interactive()) { # nocov start
          the.warning <- paste("Warning:", the.warning, collapse = "\n")
          aux <- tableInteraction(moves = valid.moves, fish = fish, trigger = the.warning, GUI = GUI, force = TRUE)
          movements <- transferValidity(from = aux, to = movements)
          restart <- TRUE
        } else { # nocov end
          stop("Preventing analysis from entering interactive mode in a non-interactive session.\n", call. = FALSE)
        }
      }
    }
  }
  return(movements)
}

#' Verify number of detections in section movements
#' 
#' @inheritParams check_args
#' @inheritParams residency
#' 
#' @return A list of section movements with updated 'Valid' columns
#' 
#' @keywords internal
#' 
checkSMovesN <- function(secmoves, fish, section.minimum, GUI) {
  appendTo("debug", "Running checkSMovesN")
  if (any(link <- secmoves$Detections < section.minimum)) {
    appendTo(c("Screen", "Report", "Warning"), the.warning <- paste0("Section movements with less than ", section.minimum, " detections are present for fish ", fish, "."))
    if (interactive())
      secmoves <- tableInteraction(moves = secmoves, fish = fish, trigger = the.warning, GUI = GUI) # nocov
  }
  return(secmoves)
}

#' Check that the fish linearly moved along the sections
#' 
#' @inheritParams check_args
#' @inheritParams migration
#' 
#' @return A list of section movements with updated 'Valid' columns
#' 
#' @keywords internal
#' 
checkLinearity <- function(secmoves, fish, sections, arrays, GUI) {
  hide <- c("First.station", "Last.station")
  vsm <- secmoves[secmoves$Valid, !hide, with = FALSE]
  back.check <- match(vsm$Section, sections)
  turn.check <- rev(match(sections, rev(vsm$Section))) # captures the last event of each section. Note, the values count from the END of the events
  if (is.unsorted(back.check)) {
      if (is.unsorted(turn.check, na.rm = TRUE))
        appendTo(c("Screen", "Report", "Warning"), the.warning <- paste0("Inter-section backwards movements were detected for fish ", fish, " and the last events are not ordered!"))
      else
        appendTo(c("Screen", "Report", "Warning"), the.warning <- paste0("Inter-section backwards movements were detected for fish ", fish, "."))
    if (interactive()) { # nocov start
      aux <- tableInteraction(moves = vsm, fish = fish, trigger = the.warning, GUI = GUI, force = FALSE)
      secmoves <- transferValidity(from = aux, to = secmoves)
    } # nocov end
  }
  return(secmoves)
}

#' Check report compatibility
#' 
#' Checks if pandoc is up and running and creates a "Report" folder, if necessary
#'
#' @inheritParams explore
#' 
#' @return An updated report argument (logical)
#' 
#' @keywords internal
#' 
checkReport <- function(report){
  appendTo("debug", "Running checkReport")
  if (report) {
    appendTo("Report", "M: 'report' option has been activated.")
    if (!rmarkdown::pandoc_available()) {
      appendTo(c("Screen", "Report", "Warning"), "'report' can only be activated if pandoc is installed. You can find how to install pandoc at: https://pandoc.org/installing.html\n   You can also check if pandoc is available to R by running rmarkdown::pandoc_available()")
      message("Would you like to:\n\n  a) Continue with 'report' set to FALSE\n  b) Stop the analysis and install pandoc.\n")
      if (interactive()) { # nocov start
        check <- TRUE
        while (check) {
          decision <- readline("Decision:(a/b) ")
          appendTo("UD", decision)
          if (decision == "a" | decision == "A") {
            appendTo(c("Screen", "Report", "Warning"), "Deactivating 'report' to prevent function failure.")
            report <- FALSE
            check <- FALSE
          }
          if (decision == "b" | decision == "B") {
            emergencyBreak()
            stop("Analysis stopped per user command.\n", call. = FALSE)        
          }
          if (check)
            appendTo("Screen", "Option not recognised; please input either 'a' or 'b'.\n")
        }
      } else { # nocov end
        report <- FALSE
      }
    }
  }
  return(report)
}

#' Check for movements upstream of the release site.
#'
#' @inheritParams check_args
#' 
#' @return A data frame with the movements for the target fish with updated 'Valid' column.
#' 
#' @keywords internal
#' 
checkUpstream <- function(movements, fish, release, arrays, GUI) {
  appendTo("debug", "Running checkUpstream")
  # NOTE: The NULL variables below are actually column names used by data.table.
  # This definition is just to prevent the package check from issuing a note due unknown variables.
  Valid <- NULL
  Array <- NULL

  the.warning <- NULL
  after.arrays <- unique(c(release, unlist(lapply(release, function(x) arrays[[x]]$all.after.and.par))))
  
  if (any(movements$Valid))
    vm <- movements[(Valid)]
  else
    return(movements)

  if (any(is.na(match(vm$Array, after.arrays)))) {
    appendTo(c("Screen", "Report", "Warning"), the.warning <- paste0("Fish ", fish, " was detected in an array that is not after its release site! Opening relevant data for inspection.\nExpected first array: ", release))
    the.warning <- paste("Warning:", the.warning)
    if (interactive()) { # nocov start
      aux <- tableInteraction(moves = vm, fish = fish, trigger = the.warning, GUI = GUI)
      movements <- transferValidity(from = aux, to = movements)
    } # nocov end
  }
  return(movements)
}

#' Check if fish are jumping over arrays
#' 
#' @inheritParams check_args
#' @inheritParams explore
#' 
#' @return A list of movements with updated 'Valid' columns
#' 
#' @keywords internal
#' 
checkJumpDistance <- function(movements, fish, release, dotmat, jump.warning, jump.error, GUI) {
  appendTo("debug", "Running checkJumpDistance")
  # NOTE: The NULL variables below are actually column names used by data.table.
  # This definition is just to prevent the package check from issuing a note due unknown variables.
  Valid <- NULL
  the.warning <- NULL
  if (any(movements$Valid)) {
    vm <- movements[(Valid)]
    # Check release-to-first
    release.jump <- min(dotmat[as.character(release), as.character(vm$Array[1])] + 1)
    if (release.jump > jump.warning) {
      # Trigger warning
      appendTo(c("Report", "Warning", "Screen"), 
        the.warning <- paste0("Fish ", fish, " jumped through ", release.jump - 1, 
          ifelse(release.jump > 2, " arrays ", " array "), 
          "from release to first valid event (Release -> ", vm$Array[1], ")."))
      the.warning <- paste("Warning:", the.warning)
    }
    if (release.jump > jump.error)
      trigger.error <- TRUE # nocov
    else
      trigger.error <- FALSE
    # Check event-to-event
    if (nrow(vm) > 1) {
      A <- vm$Array[-nrow(vm)]
      B <- vm$Array[-1]
      aux <- cbind(A, B)
      jumps <- apply(aux, 1, function(x) dotmat[x[1], x[2]])
      names(jumps) <- paste(A, "->", B)
      if (any(is.na(jumps))) {
        emergencyBreak()
        stop("There are unresolved impassable jumps in the movements.\n")
      }
      if (any(jumps > jump.warning)) {
        link <- which(jumps > jump.warning)
        for (i in 1:length(link)) {
          # Trigger warning
          appendTo(c("Report", "Warning", "Screen"), 
            other.warning <- paste0("Fish ", fish, " jumped through ", jumps[link[i]] - 1, 
              ifelse(jumps[link[i]] > 2, " arrays ", " array "), 
              "in valid events ", link[i], " -> ", link[i] + 1, " (", names(jumps)[link[i]], ")."))
          other.warning <- paste("Warning:", other.warning)
        the.warning <- paste0(the.warning, "\n", other.warning)
        }
      if (any(jumps[link] > jump.error))
        trigger.error <- TRUE # nocov
      }
    }
    # Trigger user interaction
    if (interactive() && trigger.error) { # nocov start
      aux <- tableInteraction(moves = vm, fish = fish, trigger = the.warning, GUI = GUI)
      movements <- transferValidity(from = aux, to = movements)
    } # nocov end
  }
  return(movements)
}

#' Confirm that receivers were not re-deployed before being retrieved
#' 
#' @param input The table of deployments
#' 
#' @return No return value, called for side effects.
#' 
#' @keywords internal
#' 
checkDeploymentTimes <- function(input) {
  appendTo("debug", "Running checkDeploymentTimes")
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
        message(paste0(capture.output(aux[[i]])[-1], collapse = "\n"))
        message("")
        emergencyBreak()
        stop("Fatal exception found. Read lines above for more details.\n", call. = FALSE)      
      }
    }
  }
}

#' Confirm that the station names in the deployments table match those listed in the spatial file
#' 
#' @param input The table of deployments
#' @inheritParams check_args
#' 
#' @return A data frame with the deployment locations
#' 
#' @keywords internal
#' 
checkDeploymentStations <- function(input, spatial) {
  appendTo("debug","Running checkDeploymentStations")
  aux <- spatial[spatial$Type == "Hydrophone", ]
  link <- match(unique(input$Station.name), aux$Station.name)
  if (any(is.na(link))) {
    appendTo(c("Screen", "Report", "Warning"), paste0("", ifelse(sum(is.na(link)) > 1, "Stations", "Station"), " '", paste(unique(input$Station.name)[is.na(link)], collapse = "', '"), "' ", ifelse(sum(is.na(link)) > 1, "are", "is"), " listed in the deployments but ", ifelse(sum(is.na(link)) > 1, "are", "is"), " not part of the study's stations. Discarding deployments at unknown stations."))
    to.remove <- match(input$Station.name, unique(input$Station.name)[is.na(link)])
    input <- input[is.na(to.remove), ]
  }
  link <- match(aux$Station.name, unique(input$Station.name))
  if (any(is.na(link))) {
    emergencyBreak()
    stop(paste0(ifelse(sum(is.na(link)) > 1, "Stations '", "Station '"), 
      paste(aux$Station.name[is.na(link)], collapse = "', '"), 
      ifelse(sum(is.na(link)) > 1, "' are", "' is"),
      " listed in the spatial file but no receivers were ever deployed there.\n"), call. = FALSE)      
  }
  return(input)
}

#' Find detections from unknown receivers
#' 
#' @param input The detections data frame
#' 
#' @return No return value, called for side effects.
#' 
#' @keywords internal
#' 
checkUnknownReceivers <- function(input) {
  appendTo("debug", "Running checkUnknownReceivers")
  unknown <- is.na(input$Standard.name)
  if (any(unknown)) {
    appendTo(c("Screen", "Report", "Warning"), paste0("Detections from receivers ", paste(unique(input$Receiver[unknown]), collapse = ", "), " are present in the data, but these receivers are not part of the study's stations. Double-check potential errors."))
  }
}

#' Check for target data in the unknown receivers
#' 
#' @inheritParams check_args
#' 
#' @return A list containing an updated spatial list and a TRUE/FALSE object indicating whether or not Standard station names must be reprocessed.
#' 
#' @return A list containing:
#' \itemize{
#'  \item \code{spatial}: A list containing the spatial elements of the study, with data for the unknown receivers, if relevant.
#'  \item \code{deployments}: A list containing the receiver deployments, with data for the unknown receivers, if relevant.
#'  \item \code{detections.list}: A list containing the detections for each fish, with updated station names.
#' }
#' 
#' @keywords internal
#' 
checkTagsInUnknownReceivers <- function(detections.list, deployments, spatial) {
  # NOTE: The NULL variables below are actually column names used by data.table.
  # This definition is just to prevent the package check from issuing a note due unknown variables.
  Receiver <- NULL

  appendTo("debug", "Running tagsInUnknownReceivers")
  for (i in names(detections.list)) {
    if (any(is.na(detections.list[[i]]$Standard.name))) {
      A <- detections.list[[i]]$Receiver
      B <- names(deployments)
      unknown.receivers <- unique(detections.list[[i]][is.na(match(A, B)), Receiver])
      if (length(unknown.receivers) > 0) {
        appendTo(c("Screen", "Report", "Warning"), paste0("Fish ", i, " was detected in one or more receivers that are not listed in the study area (receiver(s): ", paste(unknown.receivers, collapse = ", "), ")!"))
        message("Possible options:\n   a) Stop and double-check the data (recommended)\n   b) Temporarily include the receiver(s) to the stations list")
        if (interactive()) { # nocov start
          check <- TRUE
          while (check) {
            decision <- commentCheck(line = "Which option should be followed?(a/b/comment) ", tag = i)
            if (decision == "a" | decision == "A" | decision == "b" | decision == "B") 
              check <- FALSE 
            else 
              message("Option not recognized, please try again.")
            appendTo("UD", decision)
          }
        } else { # nocov end
          decision <- "b"
        }
        if (decision == "a" | decision == "A") {
          emergencyBreak() # nocov
          stop("Stopping analysis per user command.\n", call. = FALSE) # nocov
        }
        if (decision == "b" | decision == "B") {
          recipient <- includeUnknownReceiver(spatial = spatial, deployments = deployments, unknown.receivers = unknown.receivers)
          spatial <- recipient[[1]]
          deployments <- recipient[[2]]
        }
      }
      link <- is.na(detections.list[[i]]$Standard.name)
      levels(detections.list[[i]]$Standard.name) <- c(levels(detections.list[[i]]$Standard.name), "Ukn.")
      detections.list[[i]]$Standard.name[link] <- "Ukn."
      levels(detections.list[[i]]$Array) <- c(levels(detections.list[[i]]$Array), "Unknown")
      detections.list[[i]]$Array[link] <- "Unknown"
    }
  }
  return(list(spatial = spatial, deployments = deployments, detections.list = detections.list))
}

#' Temporarily include missing receivers in the spatial object
#' 
#' @param unknown.receivers serial number of the receivers to be included
#' @inheritParams check_args
#' 
#' @return A list containing:
#' \itemize{
#'  \item \code{spatial}: A list containing the spatial elements of the study, with data for the unknown receivers, if relevant.
#'  \item \code{deployments}: A list containing the receiver deployments, with data for the unknown receivers, if relevant.
#' }
#' 
#' @keywords internal
#' 
includeUnknownReceiver <- function(spatial, deployments, unknown.receivers){
  appendTo("debug", "Running includeUnknownReceiver.")
  appendTo(c("Screen", "Report"), "M: Including missing receiver(s) in the deployments and stations. Assigning to array 'Unknown' and standard name 'Ukn.'.")
  if (is.na(match("Unknown", levels(spatial$stations$Station.name)))) {
    levels(spatial$stations$Station.name) <- c(levels(spatial$stations$Station.name), "Unknown")
    levels(spatial$stations$Array) <- c(levels(spatial$stations$Array), "Unknown")
    spatial$stations[nrow(spatial$stations) + 1, "Station.name"] <- "Unknown"
    spatial$stations[nrow(spatial$stations), "Array"] <- "Unknown"
    spatial$stations[nrow(spatial$stations), "Standard.name"] <- "Ukn."
  }
  for (i in unknown.receivers) {
    if (is.na(match(i, names(deployments)))) {
      deployments[[length(deployments) + 1]] <- data.frame(
        Receiver = i,
        Station.name = "Unknown",
        Start = NA_character_,
        Stop = NA_character_)
      names(deployments)[length(deployments)] <- i
    }
  }
  return(list(spatial = spatial, deployments = deployments))
}

#' Check if there are detections for the target tags before release.
#'
#' @param input The list of detections
#' @inheritParams check_args
#' 
#' @keywords internal
#'
#' @return A list containing the detections without invalid data.
#' 
checkDetectionsBeforeRelease <- function(input, bio, discard.orphans = FALSE){
  appendTo("debug", "Running detectionBeforeReleaseCheck.")  
  remove.tag <- NULL
  link <- match(bio$Transmitter, names(input))
  for(i in seq_len(length(link))) {
    if (!is.na(link[i])) {
      if (any(to.remove <- !(input[[link[i]]]$Timestamp > bio$Release.date[i]))) {
        appendTo(c("Screen", "Warning", "Report"), paste0("Fish ", names(input)[link[i]], " was detected before being released!"))
        if (!discard.orphans) {
          appendTo("Screen", paste0("  Release time: ", bio$Release.date[i]))
          appendTo("Screen", paste0("  First detection time: ", input[[link[i]]]$Timestamp[1]))
          appendTo("Screen", paste0("  Number of detections before release: ", sum(to.remove)))
          message("\nPossible options:\n   a) Stop and double-check the data (recommended)\n   b) Discard orphan detections in this instance.\n   c) Discard orphan detections for all instances.\n")
          if (interactive()) { # nocov start
            unknown.input = TRUE
            while (unknown.input) {
              decision <- commentCheck(line = "Decision:(a/b/c/comment) ", tag = bio$Transmitter[i])
              if (decision == "a" | decision == "A") {
                unknown.input = FALSE
                emergencyBreak()
                stop("Function stopped by user command.", call. = FALSE)
              }
              if (decision == "b" | decision == "B")
                unknown.input = FALSE
              if (decision == "c" | decision == "C") {
                unknown.input = FALSE
                discard.orphans = TRUE
              }
              if (unknown.input)
                message("Option not recognised, please input either 'a' or 'b'.")
            }
            appendTo("UD", decision)
          } # nocov end
        }
        if (all(to.remove)) {
          appendTo(c("Screen", "Report"), paste0("ALL detections from Fish ", names(input)[link[i]], " were removed per user command."))
          remove.tag <- c(remove.tag, link[i])
        } else {
          input[[link[i]]] <- input[[link[i]]][!to.remove, ]
          appendTo(c("Screen", "Report"), paste0("M: ", sum(to.remove), " detections from Fish ", names(input)[link[i]], " were removed per user command."))
        }
      }
    }
  }
  if (!is.null(remove.tag)) {
    input <- input[-remove.tag]
  }
  return(input)
}

#' Check if there are detections matching the target tags.
#'
#' @param input The list of detections
#' @inheritParams check_args
#' 
#' @return No return value, called for side effects.
#' 
#' @keywords internal
#' 
checkNoDetections <- function(input, bio){
  appendTo("debug", "Running noDetectionsCheck.")  
  tag.list <- stripCodeSpaces(names(input))
  signal_check <- suppressWarnings(as.numeric(unlist(strsplit(as.character(bio$Signal), "|", fixed = TRUE))))
  link <- match(signal_check, tag.list)
  if (all(is.na(link))) {
    appendTo(c("Report"), "Error: No detections were found in the input data which matched the target signals.")
    emergencyBreak()
    stop("No detections were found in the input data which matched the target signals.\n", call. = FALSE)
  }
}

#' Check if there are duplicated signals in the detected tags.
#'
#' @param input list of detections
#' @param tag.list list of the target signals
#' @inheritParams check_args
#' 
#' @return No return value, called for side effects.
#' 
#' @keywords internal
#' 
checkDupSignals <- function(input, bio){
  appendTo("debug", "Running dupSignalsCheck.")
  tag.list <- stripCodeSpaces(names(input))
  signal_check <- suppressWarnings(as.numeric(unlist(strsplit(as.character(bio$Signal), "|", fixed = TRUE))))  
  failsafe <- match(tag.list, signal_check)
  if (any(table(failsafe) > 1)) {
    appendTo(c("Report"), "Error: One or more signals match more than one tag in the detections! Showing relevant signals/tags.")
    t1 <- cbind(names(input), signal_check[failsafe])
    t2 <- t1[complete.cases(t1), ]
    t3 <- table(t2[, 1], t2[, 2])
    rm(t1, t2)
    dupsig <- data.frame(Signal = colnames(t3)[apply(t3, 2, sum) > 1], Tags = NA, stringsAsFactors = FALSE)
    for (i in seq_len(nrow(dupsig))) {
      dupsig$Tags[i] <- paste(row.names(t3)[t3[, dupsig$Signal[i]] == 1], collapse = ", ")
    }
    rm(t3)
    rest.of.message <- NULL
    for (i in seq_len(nrow(dupsig))) {
      rest.of.message <- paste0(rest.of.message, "\n   Signal ", dupsig$Signal[i], " was found on tags ", dupsig$Tags[i], ".")
    }
    appendTo(c("Report"), rest.of.message)
    emergencyBreak()
    stop(paste0("One or more signals match more than one tag in the detections! Showing relevant signals/tags.", rest.of.message, "\n"), call. = FALSE)
  }
}

#' Allow the user to determine a given movement event invalid
#' 
#' @inheritParams check_args
#' 
#' @return A data frame with the movement events for the target fish and an updated 'Valid' column.
#' 
#' @keywords internal
#' 
invalidateEvents <- function(movements, fish) { # nocov start
    appendTo("Screen", "Note: You can select event ranges by separating them with a ':' and/or multiple events at once by separating them with a space or a comma.")
    check <- TRUE
    while (check) {
      the.string <- commentCheck(line = "Events to be rendered invalid: ", tag = fish)
      the.inputs <- unlist(strsplit(the.string, "\ |,"))
      the.rows <- the.inputs[grepl("^[0-9]*$", the.inputs)]
      n.rows <- length(the.rows)
      if (length(the.rows) > 0)
        the.rows <- as.integer(the.rows)
      else
        the.rows <- NULL
      the.ranges <- the.inputs[grepl("^[0-9]*[0-9]:[0-9][0-9]*$", the.inputs)]
      n.ranges <- length(the.ranges)
      if (length(the.ranges) > 0) {
        the.ranges <- strsplit(the.ranges, ":")
        the.ranges <- unlist(lapply(the.ranges, function(x) {
          r <- as.integer(x)
          r[1]:r[2]
        }))
      } else {
        the.ranges <- NULL
      }
      the.rows <- sort(unique(c(the.rows, the.ranges)))
      appendTo("UD", the.string)
      if (is.null(the.rows)) {
        decision <- readline("The input could not be recognised as row numbers, would you like to abort invalidation the process?(y/N) ")
        appendTo("UD", decision)
        if (decision == "y" | decision == "Y") {
          appendTo("Screen", "Aborting.")                 
          check <- FALSE
        } else {
          check <- TRUE
        }
      } else {
        if (sum(n.rows, n.ranges) < length(the.inputs))
          appendTo("Screen", "Part of the input could not be recognised as a row number.")
        if (all(the.rows > 0 & the.rows <= nrow(movements))) {
          if (length(the.rows) <= 10)
            decision <- readline(paste0("Confirm: Would you like to render event(s) ", paste(the.rows, collapse = ", "), " invalid?(y/N) "))
          else
            decision <- readline(paste0("Confirm: Would you like to render ", length(the.rows), " events invalid?(y/N) "))
          appendTo("UD", decision)
          if (decision == "y" | decision == "Y") {
            movements$Valid[the.rows] <- FALSE
            attributes(movements)$p.type <- "Manual"
            if (any(movements$Valid)) {
              if (length(the.rows) <= 10)
                appendTo(c("Screen", "Report"), paste0("M: Movement event(s) ", paste(the.rows, collapse = ", "), " from fish ", fish," were rendered invalid per user command."))
              else
                appendTo(c("Screen", "Report"), paste0("M: ", length(the.rows), " movement event(s) from fish ", fish," were rendered invalid per user command."))
              decision <- readline("Would you like to render any more movements invalid?(y/N) ")
              appendTo("UD", decision)
              if (decision == "y" | decision == "Y") {
                check <- TRUE
                appendTo("Screen", paste0("M: Updated movement table of fish ", fish, ":"))
                message(paste0(capture.output(print(movements, topn = nrow(movements))), collapse = "\n"))
                appendTo("Screen", "Note: You can select multiple events at once by separating them with a space.")
              } else {
                check <- FALSE
              }
            } else {
              appendTo(c("Screen", "Report"), paste0("M: ALL movement events from fish ", fish," were rendered invalid per user command."))
              check <- FALSE
            }
          }
        } else {
          appendTo("Screen", paste0("Please select only events within the row limits (1-", nrow(movements),")."))
          check <- TRUE
        }
      }
    } # end while
  return(movements)
} # nocov end

#' Opens a new window that allows the user to determine movement event invalidity
#' 
#' @inheritParams check_args
#' @param trigger The warning/message that triggered the interaction
#' 
#' @return A data frame with the movement events for the target fish and an updated 'Valid' column.
#' 
#' @keywords internal
#' 
graphicalInvalidate <- function(moves, fish, trigger) { # nocov start
  on.exit({if(gWidgets2::isExtant(w)) gWidgets2::dispose(w)}, add = TRUE)
  
  graphical_valid <- NULL
  
  to.print <- cbind(data.frame(Event = 1:nrow(moves)), moves)
  to.print$First.time <- as.character(to.print$First.time)
  to.print$Last.time <- as.character(to.print$Last.time)
  
  w <- gWidgets2::gwindow(paste("Events for fish", fish), width = 800, height = 300)
  g <- gWidgets2::ggroup(horizontal = FALSE, container = w)
  hdr <- gWidgets2::glayout(container = g)
  hdr[1, 1] <- gWidgets2::glabel("<b>Warning message:</b>", markup = TRUE)
  hdr[1, 2, expand = TRUE] <- ""
  hdr[2, 1:2, expand = TRUE] <- gWidgets2::gtext(trigger, handler = NULL)
  hdr[3, 1:2, expand = TRUE] <- gWidgets2::glabel("<b>Usage note:</b> Edit event validity by selecting event and choosing the desired action below.", markup = TRUE)
  
  tbl <- gWidgets2::gtable(to.print, multiple = TRUE, expand = TRUE, container = g)
  
  btns <- gWidgets2::glayout(container = g)

  invalid_function <- function(h, ...) {
    tbl[tbl$get_value(), "Valid"] <- FALSE
  }
  btns[1, 1] <- gWidgets2::gbutton(text = "Invalidate selected", handler = invalid_function, action = NULL)

  reset_function <- function(h, ...) {
    tbl[tbl$get_value(), "Valid"] <- TRUE
  }
  btns[1, 2] <- gWidgets2::gbutton(text = "Revalidate selected", handler = reset_function, action = NULL)

  btns[1, 3, expand = TRUE] <- ""

  btn_function <- function(h, ...) {
    x <- as.data.frame(tbl[, c("Event", "Valid")])
    graphical_valid <<- x$Valid[order(x$Event)]
    gWidgets2::dispose(w)
  }
  btns[1, 4] <- gWidgets2::gbutton(text = "Submit and close", handler = btn_function, action = NULL)

  message("M: Make any necessary edits in the external visualization window and submit the result to continue the analysis.\nNote: You can use Ctrl and Shift to select multiple events, and Ctrl+A to select all events at once."); flush.console()
  while (gWidgets2::isExtant(w)) {}

  if (is.null(graphical_valid)) {
    appendTo(c("Screen", "Warning", "Report"), "External visualization window was closed before result submission. Assuming all movement events are valid.")
    graphical_valid <- rep(TRUE, nrow(moves))
  }

  moves$Valid <- graphical_valid

  aux <- rle(!graphical_valid)
  aux <- data.frame(Value = aux[[2]], n = aux[[1]])
  aux$stop <- cumsum(aux$n)
  aux$start <- c(1, aux$stop[-1] - (aux$n[-1] - 1))
  aux <- aux[aux$Value, ]
  aux$combine <- aux$start != aux$stop
  aux$final <- aux$start
  aux$final[aux$combine] <- paste(aux$start[aux$combine], aux$stop[aux$combine], sep = ":")
  appendTo("UD", paste("from_gui:", aux$final, collapse = " "))
  return(moves)
} # nocov end

#' Transfer validity updates from valid movements to all movements
#' 
#' @param from The valid movements table with newly invalidated moves
#' @param to The target movements table
#' 
#' @return A data frame with the movement events for the target fish and an updated 'Valid' column.
#' 
#' @keywords internal
#' 
transferValidity <- function(from, to) {
  Valid <- NULL
  if (any(!from$Valid)) {
    aux <- from[!(Valid)]
    link <- apply(aux, 1, function(x) which(to[, 1] == x[1] & grepl(x["First.time"], to$First.time, fixed = TRUE)))
    to$Valid[link] <- FALSE
    attributes(to)$p.type <- "Manual"
  }
  return(to)
}
