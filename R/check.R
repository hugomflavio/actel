#' check.R arguments
#'
#' @param arrays a list containing information for each array.
#' @param bio A table with the tags and biometrics of the studied animals.
#' @param deployments a list of deployments.
#' @param detections The detections data.frame for a specific tag.
#' @param detections.list A list of the detections split by each target tag,
#'  created by \code{\link{splitDetections}}.
#' @param dist.mat A distances matrix.
#' @param discard.orphans Logical: Should actel automatically discard
#'  detections that do not fall within receiver deployment periods, or that
#'  were recorded before the respective animals were released?
#' @param dotmat The matrix of distances between arrays.
#' @param GUI One of "needed", "always" or "never". If "needed", a new window is
#'  opened to inspect the movements only if the movements table is too big to be
#'  displayed in R's console. If "always", a graphical interface is always created
#'  when the possibility to invalidate events emerges. If "never", a graphical
#'  interface is never invoked. In this case, if the table to be displayed does
#'  not fit in R's console, a temporary file will be saved and the user will be
#'  prompted to open and examine that file. Defaults to "needed".
#' @param movements The movements table for a specific tag.
#' @param n A string indicating the overall progress.
#' @param release The release location of the animal.
#' @param secmoves the section movements list.
#' @param spatial A list of spatial objects in the study area.
#' @param tag The tag being analysed.
#' @param unknown.receivers serial number of the receivers to be included.
#' @param valid.movements The valid movements table for a specific tag.
#'
#' @name check_args
#' @keywords internal
#'
NULL

#' Check argument quality
#'
#' @param dp A preloaded datapack (or NULL if no data was preloaded).
#' @inheritParams explore
#' @inheritParams migration
#' @inheritParams residency
#'
#' @keywords internal
#'
#' @return updated parameters
#'
checkArguments <- function(dp, tz, min.total.detections, min.per.event, max.interval, speed.method = c("last to first", "last to last", "first to first"),
  speed.warning, speed.error, start.time, stop.time, report, auto.open, save.detections, jump.warning, jump.error,
  inactive.warning, inactive.error, exclude.tags, override, print.releases, detections.y.axis = c("auto", "stations", "arrays"),
  if.last.skip.section = NULL, replicates = NULL, section.warning, section.error, section.order = NULL, timestep = c("days", "hours")) {
  event(type = "debug", "Running checkArguments.")

  # Note: Checks only relevant for migration() or residency() are listed at the bottom!

  no.dp.args <- c("tz", "section.order", "start.time", "stop.time", "save.detections", "exclude.tags")
  link <- c(!is.null(tz),
            !is.null(section.order),
            !is.null(start.time),
            !is.null(stop.time),
            !(is.logical(save.detections) && !save.detections), !is.null(exclude.tags))

  if (!is.null(dp) & any(link)) {
    event(type = c("warning", "screen", "report"), 
          "Argument", ifelse(sum(link) > 1, "s '", " '"), 
          paste(no.dp.args[link], collapse = "', '"),
          ifelse(sum(link) > 1, "' were ", "' was "), 
          "set but a datapack was provided. Disregarding set arguments.")
  }

  if (is.null(dp) & is.null(tz)) {
    event(type = "stop",
          'argument "tz" is missing, with no default')
  }

  if (is.null(dp) && is.na(match(tz, OlsonNames()))) {
    event(type = "stop",
          "'tz' could not be recognized as a time zone.",
          " Check available time zones with OlsonNames()")
  }

  if (!is.numeric(min.total.detections)) {
    event(type = "stop",
          "'min.total.detections' must be numeric.")
  }

  if (!is.numeric(min.per.event)) {
    event(type = "stop",
          "'min.per.event' must be numeric.")
  }

  if (min.total.detections <= 0) {
    event(type = "stop",
          "'min.total.detections' must be positive.")
  }

  if (any(min.per.event <= 0)) {
    event(type = "stop",
          "'min.per.event' must be positive.")
  }

  if (length(min.per.event) == 1) {
    min.per.event <- rep(min.per.event, 2)
  }

  if (!is.numeric(max.interval)) {
    event(type = "stop",
          "'max.interval' must be numeric.")
  }

  if (max.interval <= 0) {
    event(type = "stop",
          "'max.interval' must be positive.")
  }

  if (max.interval < 10) {
    event(type = c("Screen", "Warning"),
          "Setting 'max.interval' to less than 10 minutes is not recommended!",
          "\n         This can lead to the creation of an immense number of",
          " movement events,\n         which will be hardly manageable if any",
          " quality check is triggered.")
  }

  if (!is.character(speed.method)) {
    event(type = "stop",
          "'speed.method' should be one of 'last to first',",
          " 'last to last', or 'first to first'.")
  }
  speed.method <- match.arg(speed.method)

  if (!is.null(speed.warning) && !is.numeric(speed.warning)) {
    event(type = "stop",
          "'speed.warning' must be numeric.")
  }

  if (!is.null(speed.warning) && speed.warning <= 0) {
    event(type = "stop",
          "'speed.warning' must be positive.")
  }

  if (!is.null(speed.error) && !is.numeric(speed.error)) {
    event(type = "stop",
          "'speed.error' must be numeric.")
  }

  if (!is.null(speed.error) && speed.error <= 0) {
    event(type = "stop",
          "'speed.error' must be positive.")
  }

  if (!is.null(speed.error) & is.null(speed.warning))
    speed.warning <- speed.error

  if (!is.null(speed.error) && speed.error < speed.warning) {
    event(type = "stop",
          "'speed.error' must not be lower than 'speed.warning'.")
  }

  if (!is.null(speed.warning) & is.null(speed.error))
    speed.error <- Inf

  if (is.null(dp) && !is.null(start.time) && !grepl("^[1-2][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9] [0-2][0-9]:[0-5][0-9]:[0-5][0-9]", start.time)) {
    event(type = "stop",
          "'start.time' must be in 'yyyy-mm-dd hh:mm:ss' format.")
  }

  if (is.null(dp) && !is.null(stop.time) && !grepl("^[1-2][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9] [0-2][0-9]:[0-5][0-9]:[0-5][0-9]", stop.time)) {
    event(type = "stop",
          "'stop.time' must be in 'yyyy-mm-dd hh:mm:ss' format.")
  }

  if (!is.logical(report)) {
    event(type = "stop",
          "'report' must be logical.")
  }

  if (!is.logical(auto.open)) {
    event(type = "stop",
          "'auto.open' must be logical.")
  }

  if (is.null(dp) && !is.logical(save.detections)) {
    event(type = "stop",
          "'save.detections' must be logical.")
  }

  if (!is.numeric(jump.warning)) {
    event(type = "stop",
          "'jump.warning' must be numeric.")
  }

  if (jump.warning < 1) {
    event(type = "stop",
          "'jump.warning' must not be lower than 1.")
  }

  if (!is.numeric(jump.error)) {
    event(type = "stop",
          "'jump.error' must be numeric.")
  }

  if (jump.error < 1) {
    event(type = "stop",
          "'jump.error' must not be lower than 1.")
  }

  if (jump.error < jump.warning) {
    if (jump.warning == 2) { # this happens if someone changed jump error but didn't set jump warning.
      event(type = c("screen", "warning"), 
            "Adjusting default 'jump.warning' to match set 'jump.error'.")
      jump.warning <- jump.error
    } else {
      if (jump.error == 3) { # this happens if someone changed jump warning but didn't change jump error.
        event(type = c("screen", "warning"), 
              "Adjusting default 'jump.error' to match set 'jump.warning'.")
        jump.error <- jump.warning
      } else { # this happens if someone set both jump warning and jump error. In this case, stop and let the user decide.
        event(type = "stop",
              "'jump.error' must not be lower than 'jump.warning'.")
      }
    }
  }

  if (!is.null(inactive.warning) && !is.numeric(inactive.warning)) {
    event(type = "stop",
          "'inactive.warning' must be numeric.")
  }

  if (!is.null(inactive.warning) && inactive.warning <= 0) {
    event(type = "stop",
          "'inactive.warning' must be positive.")
  }

  if (!is.null(inactive.error) && !is.numeric(inactive.error)) {
    event(type = "stop",
          "'inactive.error' must be numeric.")
  }

  if (!is.null(inactive.error) && inactive.error <= 0) {
    event(type = "stop",
          "'inactive.error' must be positive.")
  }

  if (!is.null(inactive.error) & is.null(inactive.warning)) {
    inactive.warning <- inactive.error
  }

  if (!is.null(inactive.error) && inactive.error < inactive.warning) {
    event(type = "stop",
          "'inactive.error' must not be lower than 'inactive.warning'.")
  }

  if (!is.null(inactive.warning) & is.null(inactive.error)) {
    inactive.error <- Inf
  }

  if (is.null(dp) && !is.null(exclude.tags) && any(!grepl("-", exclude.tags, fixed = TRUE))) {
    event(type = "stop",
          "Not all contents in 'exclude.tags' could be recognized as tags",
          " (i.e. 'codespace-signal').",
          " Valid examples: 'R64K-1234', 'A69-1303-1234'")
  }

  if (!is.null(override) && is.numeric(override)) {
    event(type = c('report', 'screen', 'warning'), 
          "Override is numeric (i.e. the code space has",
          " not been included). Will attempt to identify",
          " tags to be excluded based on signal alone.")
  }

  if (!is.logical(print.releases)) {
    event(type = "stop",
          "'print.releases' must be logical.")
  }

  if (!is.character(detections.y.axis)) {
    event(type = "stop",
          "'detections.y.axis' should be one of 'stations' or 'arrays'")
  }
  detections.y.axis <- match.arg(detections.y.axis)

  # Check that all the overridden tags are part of the study
  if (!is.null(dp) && !is.null(override)) {
    if (is.numeric(override)) {
      lowest_signals <- sapply(dp$bio$Signal, lowestSignal)
      if (any(link <- is.na(match(override, dp$bio$Signal)))) {
        event(type = "stop",
              "Some tags listed in 'override' (",
              paste0(override[link], collapse = ", "),
              ") are not listed in the biometrics data.")
      }
    } else {
      if (any(link <- is.na(match(override, dp$bio$Transmitter)))) {
        event(type = "stop",
              "Some tags listed in 'override' (",
              paste0(override[link], collapse = ", "),
              ") are not listed in the biometrics data.")
      }
    }
  }

  # NON-explore checks
  if (!is.character(timestep)) {
    event(type = "stop",
          "'timestep' should be one of 'days' or 'hours'")
  }
  timestep <- match.arg(timestep)

  if (!is.null(section.order) && any(table(section.order) > 1)) {
    event(type = "stop",
          "Some section names are duplicated in the 'section.order' argument.",
          " Please include each section only once.")
  }

  if (!is.null(if.last.skip.section) && !is.logical(if.last.skip.section)) {
    event(type = "stop",
          "'if.last.skip.section' must be logical.")
  }

  if (!is.null(replicates) && !is.list(replicates)) {
    event(type = "stop",
          "'replicates' must be a list.")
  }

  if (!is.null(replicates) && length(names(replicates)) != length(replicates)) {
    event(type = "stop",
          "All list elements within 'replicates' must be named",
          " (i.e. list(Array = 'St.1') rather than list('St.1')).")
  }

  # only run these in residency calls.
  if (!missing(section.warning)) {
    if (section.error > section.warning) {
      if (section.warning == 1) { # this happens if someone changed section error but didn't set section warning.
        event(type = c("screen", "warning"), 
              "Adjusting default 'section.warning' to",
              " match set 'section.error'.")
        section.warning <- section.error
      } else {
        if (section.error == 1) { # this happens if someone changed section warning but didn't change section error.
          event(type = c("screen", "warning"),
                "Adjusting default 'section.error' to",
                " match set 'section.warning'.")
          section.error <- section.warning
        } else { # this happens if someone set both section warning and section error. In this case, stop and let the user decide.
          event(type = "stop",
                "'section.error' must not be lower than 'section.warning'.")
        }
      }
    }

    if (min.per.event[2] > section.error) {
      event(type = c('screen', 'warning', 'report'),
            "The minimum number of detections per valid section event is",
            " higher than 'section.warning'. Section warnings will",
            " never occur.")
    }

    if (min.per.event[2] > section.error) {
      event(type = c('screen', 'warning', 'report'),
            "The minimum number of detections per valid section event is",
            " higher than 'section.error'. Section errors will never occur.")
    }
  } else {
    # placeholder values so that explore and migration don't crash when attempting
    # to export below. These two variables are only used by residency.
    section.error <- 0
    section.warning <- 0
  }

  return(list(min.per.event = min.per.event,
              speed.method = speed.method,
              speed.warning = speed.warning,
              speed.error = speed.error,
              jump.warning = jump.warning,
              jump.error = jump.error,
              inactive.warning = inactive.warning,
              inactive.error = inactive.error,
              detections.y.axis = detections.y.axis,
              section.warning = section.warning,
              section.error = section.error,
              timestep = timestep))
}


#' Verify that the source data has been compiled using actel's preload function
#'
#' @param token The token generated by preload
#' @param timestamp The time when the token was issued
#'
#' @keywords internal
#'
#' @return No return value. Called for side effects.
#'
checkToken <- function(token, timestamp) {
  event(type = "debug", "Running checkToken.")
  if (file.exists(paste0(tempdir(), "/actel_token_list.csv")))
    x <- read.csv(paste0(tempdir(), "/actel_token_list.csv"))
  else
    x <- NULL
  if ((is.null(token) | is.null(timestamp) | is.null(x)) || !any(x$Token == token & x$Timestamp == timestamp)) {
    event(type = "stop",
          "The datapack's token is invalid or missing.",
          " Please use the function preload() to compile the input data.\n",
          "Data must be compiled during the current R session",
          " to avoid complications.")
  }
}


#' Check if the dependencies required to open a GUI are installed, and if
#' opening a GUI does not throw an error. Some code adapted from RGtk2's
#' .onLoad function.
#'
#' @inheritParams check_args
#'
#' @keywords internal
#'
#' @return An updated GUI argument (character string)
#'
#'
checkGUI <- function(GUI = c("needed", "always", "never"), save.tables.locally) {
  event(type = "debug", "Running checkGUI.")

  if (!is.character(GUI)) {
    event(type = "stop",
          "'GUI' should be one of 'needed', 'always' or 'never'.")
  }

  GUI <- match.arg(GUI)

  if (GUI != "never" && length(suppressWarnings(packageDescription("gWidgets2tcltk"))) == 1) {
    event(type = c("screen", "warning"), 
          "GUI is set to '", GUI, "' but package 'gWidgets2tcltk' is not",
          " available. Please install it if you intend to run GUI.\n",
          "         Disabling GUI (i.e. GUI = 'never') for the current run.")
    GUI <- "never"
  }

  if (GUI == "never" & save.tables.locally & file.exists("actel_inspect_movements.csv")) {
    event(type = "stop",
          "GUI is set to 'never' and save.tables.locally is set to TRUE, but",
          " a file 'actel_inspect_movements.csv' already exists in the",
          " current work directory.\n       Stopping to prevent data loss.",
          " Delete/rename the existing 'actel_inspect_movements.csv' to",
          " be able to run this analysis.")
  }

  if (GUI == "never" & save.tables.locally & file.exists("actel_inspect_detections.csv")) {
    event(type = "stop",
          "GUI is set to 'never' and save.tables.locally is set to TRUE, but",
          " a file 'actel_inspect_detections.csv' already exists in the",
          " current work directory.\n       Stopping to prevent data loss.",
          " Delete/rename the existing 'actel_inspect_detections.csv' to",
          " be able to run this analysis.")
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
  event(type = "debug", "Running checkDupDetections.")
  paired <- data.frame(
    TimestampA = input$Timestamp[-nrow(input)],
    TimestampB = input$Timestamp[-1],
    ReceiverA = input$Receiver[-nrow(input)],
    ReceiverB = input$Receiver[-1],
    TransmitterA = input$Transmitter[-nrow(input)],
    TransmitterB = input$Transmitter[-1]
    )

  if (any(colnames(input) == "Source.file")) {
    paired$Source.FileA <- input$Source.file[-nrow(input)]
    paired$Source.fileB <- input$Source.file[-1]
  }

  dups <- c(FALSE, paired[,1] == paired[,2] & paired[,3] == paired[,4] & paired[,5] == paired[,6])
  if (any(dups)) {
    event(type = c("screen", "report", "warning"),
          sum(dups), " duplicated detection", 
          ifelse(sum(dups) == 1, " was", "s were"),
          " found. Could an input file be duplicated?")
    message("")
    message("Possible options:\n   a) Stop and double-check the data\n   b) Remove duplicated detections\n   c) Continue without changes\n   d) Save duplicated detections to a file and re-open dialogue.")
    message("")
    restart <- TRUE
    while (restart) {
      decision <- userInput("Decision:(a/b/c/d) ", choices = letters[1:4], hash = "# dup. detections")
      if (decision == "a") {
        event(type = "stop",
              "Function stopped by user command.") # nocov
      }
      if (decision == "b") {
        event(type = c("screen", "report"),
              "M: Removing duplicated detections from the analysis",
              " per user command.")
        output <- input[!dups, ]
        restart <- FALSE
      }
      if (decision == "c") { # nocov start
        event(type = c("screen", "report"),
              "M: Continuing analysis with duplicated detections",
              " per user command.")
        output <- input
        restart <- FALSE
      } # nocov end
      if (decision == "d") { # nocov start
        file.name <- userInput("Please specify a file name (leave empty to abort saving): ", hash = "# save dups to this file")
        # break if empty
        if (file.name == "")
          next()
        # confirm extension
        if (!grepl("\\.csv$", file.name))
          file.name <- paste0(file.name, ".csv")
        # prevent auto-overwrite
        if (file.exists(file.name)) {
          aux <- userInput(paste0("File '", file.name, "' already exists. Overwrite contents?(y/n) "),
                           choices = c("y", "n"),
                           hash = "# overwrite file with same name?")
          if (aux == "y")
            overwrite <- TRUE
          else
            overwrite <- FALSE
        }
        else
          overwrite <- TRUE
        # save
        if (overwrite) {
          success <- TRUE
          # recover if saving fails
          tryCatch(data.table::fwrite(paired[dups[-1], ], file.name, , dateTimeAs = "write.csv"), error = function(e) {
            event(type = c("screen", "report"),
                  "Error: Could not save file (reason: '",
                  sub("\n$", "", e), 
                  "').\n       Reopening previous interaction.")
            success <<- FALSE
          })
          if (success)
            event(type = c("screen", "report"),
                  "M: A copy of the duplicated detections has been saved to '",
                  file.name, "'.\n   Reopening previous interaction.")
        }
      } # nocov end
    }
  } else {
    output <- input
  }
  return(output)
}


#' Check the number of detections (total and per event) of a given tag. Works
#' for both array and section movements.
#'
#' @inheritParams check_args
#' @inheritParams explore
#'
#' @return A data frame containing the movements with valid/invalid notes
#'
#' @keywords internal
#'
checkMinimumN <- function(movements, min.total.detections, min.per.event, tag, n) {
  event(type = "debug", "Running checkMinimumN")
  if (sum(movements$Detections) < min.total.detections) {
    event(type = c("screen", "report", "warning"),
          "Tag ", tag, " ", n, " has fewer than ", min.total.detections,
          " detections in total. Discarding this tag.")
    movements$Valid <- FALSE # invalidate the tag
  }
  if (any(movements$Valid) & any(movements$Detections < min.per.event)) {
    if (any(colnames(movements) == 'Array'))
      event(type = c("screen", "report", "warning"),
            "Tag ", tag, " ", n, " has ", tolower(colnames(movements)[1]),
            " movement events with fewer than ", min.per.event,
            " detections. Invalidating those events.")
    movements$Valid[movements$Detections < min.per.event] <- FALSE
  }
  return(movements)
}


#' check tag speeds against defined thresholds (in m/s)
#'
#' @inheritParams check_args
#' @inheritParams explore
#'
#' @return A list of movements with updated 'Valid' columns
#'
#' @keywords internal
#'
checkSpeeds <- function(movements, tag, detections, valid.movements,
  speed.warning, speed.error, GUI, save.tables.locally, n) {
  event(type = "debug", "Running checkSpeeds.")
  the_warning <- NULL
  vm <- valid.movements
  warning_counter <- 0
  if (any(na.as.false(vm$Average.speed.m.s >= speed.warning))) {
    link <- which(vm$Average.speed.m.s >= speed.warning)
    if (link[1] == 1) {
      # This function produces complex warnings. 
      # We need to build them gradually.
      the_warning <- paste0("Tag ", tag, " ", n, " had an average speed of ",
                            round(vm$Average.speed.m.s[1], 2),
                            " m/s from release to first valid event",
                            " (Release -> ", vm$Array[1], ").")
      event(type = c("report", "warning", "screen"),
            the_warning)
      the_warning <- paste("Warning:", the_warning)
      link <- link[-1]
      warning_counter <- warning_counter + 1
    }
    if (length(link) > 0) {
      for (i in 1:length(link)) {
        warning_counter <- warning_counter + 1
        if (warning_counter < 5) {
          other_warning <- paste0("Tag ", tag, " ", n, 
                                  " had an average speed of ", 
                                  round(vm$Average.speed.m.s[link[i]], 2),
                                  " m/s from valid event ", link[i] - 1,
                                  " to ", link[i], " (",vm$Array[link[i] - 1],
                                  " -> ", vm$Array[link[i]], ").")
          event(type = c("report", "warning", "screen"),
                other_warning)
          other_warning <- paste("Warning:", other_warning)
          the_warning <- paste0(the_warning, "\n", other_warning)
        } else {
          final_warning <- paste0("Warning: Tag ", tag, " ", n,
                                  " had an average speed higher than ",
                                  speed.warning, " m/s in ", warning_counter,
                                  " events (of which the first 4 are",
                                  " displayed above).")
          message("\r", final_warning, appendLF = FALSE)
          flush.console()
        }
      }
      if (warning_counter >= 5) {
        message() # print and empty line
        event(type = c("report", "warning"),
              sub("Warning: ", "", final_warning))
        link <- createEventRanges(link)
        event_list <- paste0("         Events that raised warnings: ",
                             paste(link, collapse = ", "))
        event(type = c("screen", "report"),)
        the_warning <- paste0(the_warning, "\n", 
                              final_warning, "\n", 
                              event_list)
      }
    }
  }
  # Trigger user interaction
  if (any(na.as.false(vm$Average.speed.m.s >= speed.error))) { # nocov start
    movements <- tableInteraction(moves = movements, tag = tag, 
                                  detections = detections,
                                  trigger = the_warning, GUI = GUI, 
                                  save.tables.locally = save.tables.locally)
  } # nocov end
  return(movements)
}

#' Find if a tag is standing still in an array
#'
#' @inheritParams check_args
#' @inheritParams explore
#'
#' @return A list of movements with updated 'Valid' columns
#'
#' @keywords internal
#'
checkInactiveness <- function(movements, tag, detections, n,
  inactive.warning, inactive.error, dist.mat, GUI, save.tables.locally) {
  event(type = "debug", "Running checkInactiveness.")
  Valid <- NULL
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
      Start <- min(which(detections$Timestamp == valid.moves$First.time[j] & detections$Standard.name == valid.moves$First.station[j]))
      Stop <- Start + (valid.moves$Detections[j] - 1)
      return(Start:Stop)
    })
    # Start trimming from the Start to see if a period of inactiveness is present
    continue <- TRUE
    iteration <- 1
    while (continue) {
      the_warning <- NULL
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
      the.detections <- detections[valid.rows, ]
      # find all stations
      the.stations <- as.character(sort(unique(the.detections$Standard.name)))
      trigger.error <- FALSE
        # Trigger warning
      if (attributes(dist.mat)$valid) {
        aux <- dist.mat[the.stations, the.stations]
        if (all(aux <= 1500)) {
          n.detections <- sum(valid.moves$Detections[start_i:Stop])
          the_warning <- paste0("Tag ", tag, " ", n, " was detected ", 
                                n.detections, " times at stations less than",
                                " 1.5 km apart in array '",
                                tail(breaks$values, 1),
                                "' (", paste(the.stations, collapse = ", "), 
                                "), over ", days.spent,
                                " days and then disappeared.",
                                " Could it be inactive?")
          event(type = c("report", "warning", "screen"),
                the_warning)
          the_warning <- paste("Warning:", the_warning)
          continue <- FALSE
        }
        if (all(aux <= 1500) & days.spent >= inactive.error)
          trigger.error <- TRUE # nocov
      } else {
        if (length(the.stations) <= 3) {
          n.detections <- sum(valid.moves$Detections[start_i:Stop])
          the_warning <- paste0("Tag ", tag, " ", n, " was detected ", 
                                n.detections, " times at three or fewer",
                                " stations of array '", tail(breaks$values, 1),
                                "' (", paste(the.stations, collapse = ", "),
                                ") over ", days.spent, " days and then",
                                " disappeared. Could it be inactive?")
          event(type = c("Report", "Warning", "Screen"),
                the_warning)
          the_warning <- paste("Warning:", the_warning)
          continue <- FALSE
        }
        if (length(the.stations) <= 3 & days.spent >= inactive.error)
          trigger.error <- TRUE # nocov
      }
      # Trigger user interaction
      if (trigger.error) { # nocov start
        error_message <- paste0("M: Tag ", tag, " ", n, 
                                " has been inactive for more than ", 
                                inactive.error, 
                                " days. Inactiveness started on event ",
                                start_i, " (", 
                                as.Date(valid.moves$First.time[start_i]),").")
        event(type = "screen", 
              error_message)
        movements <- tableInteraction(moves = movements, tag = tag, detections = detections,
                                      trigger = paste0(the_warning, "\n", error_message), GUI = GUI,
                                      save.tables.locally = save.tables.locally)
      } # nocov end
      iteration <- iteration + 1
    }
  }
  return(movements)
}

#' Find out if a tag moved in an impossible direction
#'
#' @inheritParams check_args
#'
#' @return A list of movements with updated 'Valid' columns
#'
#' @keywords internal
#'
checkImpassables <- function(movements, tag, bio, spatial, detections, dotmat, GUI, save.tables.locally, n){
  event(type = "debug", "Running checkImpassables.")
  Valid <- NULL

  release <- as.character(bio$Release.site[na.as.false(bio$Transmitter == tag)])
  release <- unlist(strsplit(with(spatial, release.sites[release.sites$Standard.name == release, "Array"]), "|", fixed = TRUE))

  restart <- TRUE
  first.time <- TRUE
  while (restart) {
    restart <- FALSE
    warning_counter <- 0
    the_warning <- NULL
    valid.moves <- movements[(Valid)]
    if (!first.time) {
      message("------------------------------------")
      event(type = c("screen", "warning"), 
            "The last interaction did not solve the impassable problem! See",
            " remaining problems below.\n         You can also press ESC to",
            " abort the current run and alter your spatial.txt file.")
      message("------------------------------------")
    }
    # check release movement
    if (sum(movements$Valid) > 0) {
      if(all(is.na(dotmat[as.character(release), as.character(valid.moves$Array[1])]))) {
        the_warning <- paste0("Tag ", tag, " ", n, " made an impassable",
                              " jump: It is not possible to go from",
                              " release site ", as.character(release), 
                              " to ", as.character(valid.moves$Array[1]), 
                              ".\n         Please resolve this either by",
                              " invalidating events or by adjusting your",
                              " 'spatial.txt' file and restarting.")
        event(type = c("screen", "warning", "report"), 
              the_warning)
        the_warning <- paste("Warning:", the_warning)
        warning_counter <- warning_counter + 1
      }
    }
    # check other movements
    if (sum(movements$Valid) > 1) {
      shifts <- data.frame(
        A = valid.moves$Array[-nrow(valid.moves)],
        B = valid.moves$Array[-1])
      distances <- apply(shifts, 1, function(x) dotmat[x[1], x[2]])
      if (any(is.na(distances))) {
        for (i in which(is.na(distances))) {
          warning_counter <- warning_counter + 1
          if (warning_counter < 5) {
            other_warning <- paste0("Tag ", tag, " ", n, 
                                    " made an impassable jump in events ", i,
                                    " to ", i + 1, 
                                    ": It is not possible to go from array ", 
                                    shifts[i, 1], " to ", shifts[i, 2], ".")
            event(type = c("screen", "warning", "report"),
                  other_warning)
            other_warning <- paste("Warning:", other_warning)
            the_warning <- paste0(the_warning, "\n", other_warning)
          } else {
            final_warning <- paste0("Warning: Tag ", tag, " made ", 
                                    warning_counter, " impassable jumps (of",
                                    " which the first 4 are displayed above).")
            message(paste0("\r", final_warning), appendLF = FALSE)
            flush.console()
          }
        }
        if (warning_counter >= 5) {
          message() # empty line spacer
          event(type = c("report", "warning"),
                sub("Warning: ", "", final_warning))
          link <- createEventRanges(which(is.na(distances)))
          event_list <- paste0("         Events that raised warnings: ",
                               paste(link, collapse = ", "))
          event(type = c("screen", "report"),
                event_list)
          the_warning <- paste0(the_warning, "\n",
                                final_warning, "\n",
                                event_list)
        }
      }
    }
    if (!is.null(the_warning)) {
      message("Please resolve this either by invalidating events or by adjusting your 'spatial.txt' file and restarting.")
      if (interactive()) { # nocov start
        the_warning <- paste(the_warning, "\nPlease resolve this either by invalidating events or by adjusting your 'spatial.txt' file and restarting.", collapse = "\n")
        movements <- tableInteraction(moves = movements, tag = tag, detections = detections,
                                      trigger = the_warning, GUI = GUI, force = TRUE,
                                      save.tables.locally = save.tables.locally)
        restart <- TRUE
        first.time <- FALSE
      } else { # nocov end
        stop("Preventing analysis from entering interactive mode in a non-interactive session.")
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
checkSMovesN <- function(secmoves, tag, section.warning, section.error, GUI, save.tables.locally, n) {
  event(type = "debug", "Running checkSMovesN.")

  if (any(secmoves$Detections <= section.warning & secmoves$Valid)) {
    the_warning <- paste0("Section movements with ", section.warning,
                          " or fewer detections are present for tag ",
                          tag, " ", n, ".")
    event(type = c("screen", "report", "warning"),
          the_warning)
  }

  if (any(secmoves$Detections <= section.error & secmoves$Valid)) {
    secmoves <- tableInteraction(moves = secmoves, tag = tag, trigger = the_warning, # nocov
                                 GUI = GUI, save.tables.locally = save.tables.locally) # nocov
  }

  return(secmoves)
}

#' Check that the tag linearly moved along the sections
#'
#' @inheritParams check_args
#' @inheritParams migration
#'
#' @return A list of section movements with updated 'Valid' columns
#'
#' @keywords internal
#'
checkLinearity <- function(secmoves, tag, spatial, arrays, GUI, save.tables.locally, n) {
  event(type = "debug", "Running checkLinearity.")

  valid.secmoves <- secmoves[secmoves$Valid, ]

  sections <- names(spatial$array.order)
  back.check <- match(valid.secmoves$Section, sections)
  turn.check <- rev(match(sections, rev(valid.secmoves$Section))) # captures the last event of each section. Note, the values count from the END of the events
  if (is.unsorted(back.check)) {
      if (is.unsorted(turn.check, na.rm = TRUE)) {
        the_warning <- paste0("Inter-section backwards movements were",
                              " detected for tag ", tag, " ", n,
                              " and the last events are not ordered!")
        event(type = c("screen", "report", "warning"),
              the_warning)
      }
      else {
        the_warning <- paste0("Inter-section backwards movements were",
                              " detected for tag ", tag, " ", n, ".")
        event(type = c("screen", "report", "warning"),
              the_warning)
      }
    if (interactive()) { # nocov start
      secmoves <- tableInteraction(moves = secmoves, tag = tag, trigger = the_warning,
                                   GUI = GUI, force = FALSE, save.tables.locally = save.tables.locally)
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
  event(type = "debug", "Running checkReport.")
  if (report) {
    event(type = "report", 
          "M: 'report' option has been activated.")
    if (!rmarkdown::pandoc_available()) {
      event(type = c("screen", "report", "warning"), 
            "'report' can only be activated if pandoc is installed.",
            " You can find how to install pandoc at:",
            " https://pandoc.org/installing.html\n",
            "   You can also check if pandoc is available to R by running",
            " rmarkdown::pandoc_available()")
      message("Would you like to:\n\n  a) Stop the analysis and install pandoc.\n  b) Continue with 'report' set to FALSE\n")
      decision <- userInput("Decision:(a/b) ", choices = letters[1:2], hash = "# pandoc warning")
      if (decision == "a") {
        event(type = "stop",
              "Analysis stopped per user command.")
      }
      if (decision == "b") {
        event(type = c("screen", "report", "warning"),
              "Deactivating 'report' to prevent function failure.")
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
#' @return A data frame with the movements for the target tag with updated 'Valid' column.
#'
#' @keywords internal
#'
checkFirstDetBackFromRelease <- function(movements, tag, bio, detections, arrays, spatial, GUI, save.tables.locally, n) {
  event(type = "debug", "Running checkFirstDetBackFromRelease.")
  # NOTE: The NULL variables below are actually column names used by data.table.
  # This definition is just to prevent the package check from issuing a note due unknown variables.
  Valid <- NULL
  Array <- NULL

  release <- as.character(bio$Release.site[na.as.false(bio$Transmitter == tag)])
  release <- unlist(strsplit(with(spatial, release.sites[release.sites$Standard.name == release, "Array"]), "|", fixed = TRUE))

  the_warning <- NULL
  after.arrays <- unique(c(release, unlist(lapply(release, function(x) arrays[[x]]$all.after.and.par))))

  if (any(movements$Valid))
    vm <- movements[(Valid)]
  else
    return(movements)

  if (any(is.na(match(vm$Array, after.arrays)))) {
    the_warning <- paste0("Tag ", tag, " ", n, " was detected in an array",
                          " that is not after its release site! Opening",
                          " relevant data for inspection.",
                          "\nExpected first array: ", release)
    event(type = c("screen", "report", "warning"),
          the_warning)
    the_warning <- paste("Warning:", the_warning)
    if (interactive()) { # nocov start
      movements <- tableInteraction(moves = movements, tag = tag, detections = detections,
                                    trigger = the_warning, GUI = GUI, save.tables.locally = save.tables.locally)
    } # nocov end
  }
  return(movements)
}

#' Check if tags are jumping over arrays
#'
#' @inheritParams check_args
#' @inheritParams explore
#'
#' @return A list of movements with updated 'Valid' columns
#'
#' @keywords internal
#'
checkJumpDistance <- function(movements, tag, bio, detections, spatial, arrays,
  dotmat, paths, jump.warning, jump.error, GUI, save.tables.locally, n) {
  event(type = "debug", "Running checkJumpDistance.")
  # NOTE: The NULL variables below are actually column names used by data.table.
  # This definition is just to prevent the package check from issuing a note due unknown variables.
  Valid <- NULL
  the_warning <- NULL
  warning_counter <- 0
  events.to.complain.about <- NULL

  release <- as.character(bio$Release.site[na.as.false(bio$Transmitter == tag)])
  release <- unlist(strsplit(with(spatial, release.sites[release.sites$Standard.name == release, "Array"]), "|", fixed = TRUE))
  release.time <- bio$Release.date[na.as.false(bio$Transmitter == tag)]

  if (any(movements$Valid)) {
    vm <- movements[(Valid)]
    #########################
    # CHECK RELEASE TO FIRST
    #########################
    # remove any potential first array that is not connected do the first move (this should never happen, but still...)
    release <- release[!is.na(dotmat[as.character(release), as.character(vm$Array[1])])]
    # stop if no arrays are connected to the first move. This should also never happen actually.
    if (length(release) == 0) {
        event(type = "stop",
              "There are unresolved impassable jumps in the movements",
              " (Found at release).")
    }
    # minimum jump size
    release.steps <- min(dotmat[as.character(release), as.character(vm$Array[1])] + 1)
    # Check release-to-first
    if (release.steps > 1) {
      # determine if any arrays were not active during the movement.
      path.dist <- list()
      for (r in release) {
        sub.paths <- paths[[paste0(r, "_to_", vm$Array[1])]]
        if (is.null(sub.paths)) { # failsafe in case step size is 2
          aux <- list(list(n = 2, names = r))
        } else {
          aux <- lapply(sub.paths, function(p) {
            xarrays <- unlist(strsplit(p, " -> "))
            xarrays <- sapply(xarrays, function(a) {
              # if the movement ocurred before the start
              v1 <- arrays[[a]]$live$Start > release.time &
                    arrays[[a]]$live$Start > vm$First.time[1]
              # or after the stop
              v2 <- arrays[[a]]$live$Stop < release.time &
                    arrays[[a]]$live$Stop < vm$First.time[1]
              # then that period does not qualify
              v3 <- !(v1 | v2)
              # and if any period qualifies, then the array was up during the movememnt
              any(v3)
            })
            return(list(n = sum(xarrays) + 2, names = names(xarrays)[xarrays]))
            # +2 because the release site and the array that picked up the animal also
            # count as steps, so the minimal step size is two.
          })
        }
        names(aux) <- sub.paths
        path.dist <- c(path.dist, aux)
        rm(aux)
      }
      new.steps <- sapply(path.dist, function(x) x$n)
      release.steps <- min(new.steps)
      say.at.least <- min(new.steps) != max(new.steps)
      rm(new.steps)
    }
    if (release.steps > jump.warning) {
      # Trigger warning
      the_warning <- paste0("Tag ", tag, " ", n, " jumped through ",
                            ifelse(say.at.least, "at least ", ""), 
                            release.steps - 1,
                            ifelse(release.steps > 2, " arrays ", " array "),
                            "from release to first valid event (Release -> ", 
                            vm$Array[1], ").")
      event(type = c("report", "warning", "screen"),
            the_warning)
      the_warning <- paste("Warning:", the_warning)
      warning_counter <- warning_counter + 1
      events.to.complain.about <- c(events.to.complain.about, 1)
      rm(say.at.least)
    }
    if (release.steps > jump.error)
      trigger.error <- TRUE # nocov
    else
      trigger.error <- FALSE

    #########################
    # CHECK EVENT TO EVENT
    #########################
    if (nrow(vm) > 1) {
      A <- vm$Array[-nrow(vm)]
      B <- vm$Array[-1]
      aux <- cbind(A, B)
      move.steps <- apply(aux, 1, function(x) dotmat[x[1], x[2]])
      names(move.steps) <- paste(A, "->", B)

      if (any(is.na(move.steps))) {
        event(type = "stop",
              "There are unresolved impassable jumps in the movements",
              " (Found during moves).")
      }

      if (any(move.steps > 1)) {
        # isolate troublesome events
        steps.to.check <- which(move.steps > 1)
        # confirm that arrays were active
        for (i in steps.to.check) {
          sub.paths <- paths[[paste0( vm$Array[i], "_to_", vm$Array[i + 1])]]
          path.dist <- lapply(sub.paths, function(p) {
            xarrays <- unlist(strsplit(p, " -> "))
            xarrays <- sapply(xarrays, function(a) {
              # if the movement ocurred before the start
              v1 <- arrays[[a]]$live$Start > vm$Last.time[i] &
                    arrays[[a]]$live$Start > vm$First.time[i + 1]
              # or after the stop
              v2 <- arrays[[a]]$live$Stop < vm$Last.time[i] &
                    arrays[[a]]$live$Stop < vm$First.time[i + 1]
              # then that period does not qualify
              v3 <- !(v1 | v2)
              # and if any period qualifies, then the array was up during the movement
              any(v3)
            })
            return(list(n = sum(xarrays) + 1, names = names(xarrays)[xarrays]))
            # +1 because the target array also counts as a step
          })
          names(path.dist) <- sub.paths
          new.steps <- sapply(path.dist, function(x) x$n)
          move.steps[i] <- min(new.steps)
          say.at.least <- min(new.steps) != max(new.steps)
          if (move.steps[i] > jump.warning) { # because ">", then if jump.warning = 1, actel only complains if steps = 2.
            warning_counter <- warning_counter + 1
            events.to.complain.about <- c(events.to.complain.about, i + 1)
            if (warning_counter < 5) {
              # Trigger warning
              other_warning <- paste0("Tag ", tag, " ", n, " jumped through ",
                                      ifelse(say.at.least, "at least ", ""),
                                      move.steps[i] - 1,
                                      ifelse(move.steps[i] > 2,
                                             " arrays ",
                                             " array "),
                                      "in valid events ", i, " -> ", i + 1, 
                                      " (", names(move.steps)[i], ").")
              event(type = c("Report", "Warning", "Screen"),
                    other_warning)
              other_warning <- paste("Warning:", other_warning)
              the_warning <- paste0(the_warning, "\n", other_warning)
            } else {
              final_warning <- paste0("Warning: Tag ", tag, " ", n, " jumped ", jump.warning, " or more arrays on ", warning_counter, " occasions (of which the first 4 are displayed above).")
              message(paste0("\r", final_warning), appendLF = FALSE)
              flush.console()
            }
          }
          rm(say.at.least)
          if (move.steps[i] > jump.error) { # same as if above.
            trigger.error <- TRUE # nocov
          }
        }
        if (warning_counter >= 5) {
          message()
          event(type = c("report", "warning"), 
                sub("Warning: ", "", final_warning))
          link <- createEventRanges(events.to.complain.about)
          event_list <- paste0("         Events that raised warnings: ", 
                               paste(link, collapse = ", "))
          event(type = c("screen", "report"),
                event_list)
          the_warning <- paste0(the_warning, "\n", final_warning, "\n", event_list)
        }
      }
    }
    # Trigger user interaction
    if (interactive() && trigger.error) { # nocov start
      movements <- tableInteraction(moves = movements, tag = tag, detections = detections,
                                    trigger = the_warning, GUI = GUI, save.tables.locally = save.tables.locally)
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
  event(type = "debug", "Running checkDeploymentTimes.")
  aux <- split(input, input$Receiver)
  for (i in 1:length(aux)) {
    if (nrow(aux[[i]]) > 1) {
      A <- aux[[i]]$Start[-1]
      B <- aux[[i]]$Stop[-nrow(aux[[i]])]
      AtoB <- as.numeric(difftime(A, B))
      if (any(AtoB < 0)) {
        event(type = c("screen", "report"), 
              "Error: Receiver ", names(aux)[i], 
              " was re-deployed before being retrieved:\n")
        aux[[i]][, "placeholder"] <- ""
        aux[[i]][c(FALSE, AtoB < 0), "placeholder"] <- " <- !!!"
        names(aux[[i]])[ncol(aux[[i]])] <- ""
        message(paste0(capture.output(aux[[i]])[-1], collapse = "\n"))
        message()
        event(type = "stop",
              "Fatal exception found. Read lines above for more details.")
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
  event(type = "debug","Running checkDeploymentStations.")
  aux <- spatial[spatial$Type == "Hydrophone", ]
  link <- match(unique(input$Station.name), aux$Station.name)
  if (any(is.na(link))) {
    event(type = c("screen", "report", "warning"),
          "The following station", ifelse(sum(is.na(link)) > 1, "s are", " is"),
          " listed in the deployments but ",
          ifelse(sum(is.na(link)) > 1, "are", "is"),
          " not part of the study's stations: '",
          paste(unique(input$Station.name)[is.na(link)], collapse = "', '"),
          "'\n",
          "Discarding deployments at unknown stations.")
    to.remove <- match(input$Station.name, unique(input$Station.name)[is.na(link)])
    input <- input[is.na(to.remove), ]
  }
  link <- match(aux$Station.name, unique(input$Station.name))
  if (any(is.na(link))) {
    event(type = "stop",
          "The following station",
          ifelse(sum(is.na(link)) > 1, "s are", " is"),
          " listed in the spatial file but no receivers",
          " were ever deployed there: '",
          paste(aux$Station.name[is.na(link)], collapse = "', '"), "'\n")
  }
  input$Standard.name <- aux$Standard.name[match(input$Station.name, aux$Station.name)]
  input$Array <- aux$Array[match(input$Station.name, aux$Station.name)]

  return(input[, c("Receiver", "Array", "Station.name", "Standard.name", "Start", "Stop")])
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
  event(type = "debug", "Running checkUnknownReceivers.")
  unknown <- is.na(input$Standard.name)
  if (any(unknown)) {
    how.many <- length(unique(input$Receiver[unknown]))
    event(type = c("screen", "report", "warning"),
          "Detections from receiver",
          ifelse(how.many > 1, "s ", " "),
          paste(unique(input$Receiver[unknown]), collapse = ", "),
          " are present in the data, but ",
          ifelse(how.many > 1, "these receivers are", "this receiver is"),
          " not part of the study's stations. Double-check potential errors.")
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
#'  \item \code{detections.list}: A list containing the detections for each tag, with updated station names.
#' }
#'
#' @keywords internal
#'
checkTagsInUnknownReceivers <- function(detections.list, deployments, spatial) {
  event(type = "debug", "Running tagsInUnknownReceivers")
  # NOTE: The NULL variables below are actually column names used by data.table.
  # This definition is just to prevent the package check from issuing a note due unknown variables.
  include.all.unknowns <- FALSE
  exclude.all.unknowns <- FALSE
  processed.unknowns <- NULL
  excluded <- NULL
  included <- NULL

  deployed.receivers <- unlist(lapply(deployments, function(x) x$Receiver))
  for (i in names(detections.list)) {
    if (any(is.na(detections.list[[i]]$Standard.name))) {
      # If this is not the first iteration, some receivers may have already been processed.
      if (!is.null(processed.unknowns))
        detections.list[[i]] <- detections.list[[i]][!(detections.list[[i]]$Receiver %in% processed.unknowns), ]

      receivers <- detections.list[[i]]$Receiver
      link <- is.na(match(receivers, deployed.receivers))
      new.unknowns <- unique(detections.list[[i]]$Receiver[link])

      if (length(new.unknowns) > 0) {
        event(type = c("screen", "report", "warning"),
              "Tag ", i, " was detected in one or more receivers that are not",
              " listed in the study area (receiver(s): ",
              paste(new.unknowns, collapse = ", "), ")!")
        if (!include.all.unknowns & !exclude.all.unknowns) {
          message("Possible options:\n   a) Stop and double-check the data (recommended)\n   b) Temporarily include receiver(s) \n      (Unknown events will be considered invalid, but will be visible in the detection plots)\n   c) Repeat option b for all unknown receivers\n   d) Discard detections for this unknown receiver (not recommended)\n   e) Repeat option d for all unknown receivers (very much not recommended!)")
          decision <- userInput("Which option should be followed?(a/b/c/d/e/comment) ", choices = c("a", "b", "c", "d", "e", "comment"),
                                tag = i, hash = "# unknown receivers")
        } else {
          decision <- "ignore"
        }

        if (decision == "a") {
          event(type = "stop",
                "Stopping analysis per user command.") # nocov
        }

        if (decision == "c") {
          include.all.unknowns <- TRUE # nocov
        }

        if (include.all.unknowns || decision == "b") {
          recipient <- includeUnknownReceiver(spatial = spatial, deployments = deployments, unknown.receivers = new.unknowns)
          spatial <- recipient[[1]]
          deployments <- recipient[[2]]
          link <- is.na(detections.list[[i]]$Standard.name)
          levels(detections.list[[i]]$Standard.name) <- c(levels(detections.list[[i]]$Standard.name), "Ukn.")
          detections.list[[i]]$Standard.name[link] <- "Ukn."
          levels(detections.list[[i]]$Array) <- c(levels(detections.list[[i]]$Array), "Unknown")
          detections.list[[i]]$Array[link] <- "Unknown"
          levels(detections.list[[i]]$Section) <- c(levels(detections.list[[i]]$Section), "Unknown")
          detections.list[[i]]$Section[link] <- "Unknown"
          processed.unknowns <- c(processed.unknowns, as.character(new.unknowns))
          included <- c(included, as.character(new.unknowns))
        }

        if (decision == "e") {
          exclude.all.unknowns <- TRUE # nocov
        }

        if (exclude.all.unknowns || decision == "d") {
          detections.list[[i]] <- detections.list[[i]][is.na(match(receivers, new.unknowns)), ] # keep the ones that are not unknown
          processed.unknowns <- c(processed.unknowns, as.character(new.unknowns))
          excluded <- c(excluded, as.character(new.unknowns))
        }
        rm(decision)
      }
    }
  }
  # remove empty data frames (for tags detected only at excluded unknown receivers)
  detections.list <- detections.list[sapply(detections.list, nrow) > 0]
  # append summary to spatial
  if (!is.null(included) | !is.null(excluded))
    spatial$unknowns <- list(included = included, excluded = excluded)

  return(list(spatial = spatial, deployments = deployments, detections.list = detections.list))
}

#' Temporarily include missing receivers in the spatial object
#'
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
  event(type = "debug", "Running includeUnknownReceiver.")
  event(type = c("screen", "report"),
        "M: Including missing receiver(s) in the deployments and stations.",
        " Assigning to array 'Unknown' and standard name 'Ukn.'.")
  if (is.na(match("Unknown", levels(spatial$stations$Station.name)))) {
    levels(spatial$stations$Station.name) <- c(levels(spatial$stations$Station.name), "Unknown")
    levels(spatial$stations$Array) <- c(levels(spatial$stations$Array), "Unknown")
    levels(spatial$stations$Section) <- c(levels(spatial$stations$Section), "Unknown")
    spatial$stations[nrow(spatial$stations) + 1, "Station.name"] <- "Unknown"
    spatial$stations[nrow(spatial$stations), "Array"] <- "Unknown"
    spatial$stations[nrow(spatial$stations), "Section"] <- "Unknown"
    spatial$stations[nrow(spatial$stations), "Standard.name"] <- "Ukn."
  }
  for (i in unknown.receivers) {
    if (is.na(match(i, names(deployments)))) {
      deployments[[length(deployments) + 1]] <- data.frame(
        Receiver = i,
        Array = "Unknown",
        Station.name = "Unknown",
        Standard.name = "Ukn.",
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
  event(type = "debug", "Running checkDetectionsBeforeRelease.")
  remove.tag <- NULL
  link <- match(bio$Transmitter, names(input))
  for(i in seq_len(length(link))) {
    if (!is.na(link[i])) {
      if (any(to.remove <- !(input[[link[i]]]$Timestamp > bio$Release.date[i]))) {
        event(type = c("screen", "warning", "report"), 
              "Tag ", names(input)[link[i]], 
              " was detected before being released!")
        if (!discard.orphans) {
          event(type = "screen", 
                "  Release time: ", bio$Release.date[i])
          event(type = "screen", 
                "  First detection time: ", input[[link[i]]]$Timestamp[1])
          event(type = "screen", 
                "  Number of detections before release: ", sum(to.remove))
          event(type = "screen", 
                "\nPossible options:\n",
                "   a) Stop and double-check the data (recommended)\n",
                "   b) Discard orphan detections in this instance.\n",
                "   c) Discard orphan detections for all instances.\n",
                "   d) Save orphan detections to a file and re-open dialogue.")

          restart <- TRUE
          while (restart) {
            decision <- userInput("Decision:(a/b/c/d/comment) ",
                                  choices = c("a", "b", "c", "d", "comment"),
                                  tag = bio$Transmitter[i],
                                  hash = paste("# detections before release for tag", bio$Transmitter[i]))

            if (decision == "a") {
              event(type = "stop",
                    "Function stopped by user command.") # nocov
            }

            if (decision == "b") {
              restart <- FALSE
            }

            if (decision == "c") {
              discard.orphans <- TRUE # nocov
              restart <- FALSE # nocov
            }

            if (decision == "d") { # nocov start
              file.name <- userInput("Please specify a file name (leave empty to abort saving): ", hash = "# save pre-release orphans to this file")
              # break if empty
              if (file.name == "")
                next()
              # confirm extension
              if (!grepl("\\.csv$", file.name))
                file.name <- paste0(file.name, ".csv")
              # prevent auto-overwrite
              if (file.exists(file.name)) {
                aux <- userInput(paste0("File '", file.name, "' already exists. Overwrite contents?(y/n) "),
                                 choices = c("y", "n"),
                                 hash = "# overwrite file with same name?")
                if (aux == "y")
                  overwrite <- TRUE
                else
                  overwrite <- FALSE
              }
              else
                overwrite <- TRUE
              # save
              if (overwrite) {
                success <- TRUE
                # recover if saving fails
                tryCatch(data.table::fwrite(input[[link[i]]][to.remove, ], file.name, , dateTimeAs = "write.csv"), error = function(e) {
                  event(type = c("screen", "report"),
                        "Error: Could not save file (reason: '",
                        sub("\n$", "", e), "').\n",
                        "       Reopening previous interaction.")
                  success <<- FALSE
                })
                if (success)
                  event(type = c("screen", "report"),
                        "M: A copy of the orphan detections has been",
                        " saved to '", file.name,
                        "'.\n   Reopening previous interaction.")
              }
            } # nocov end
          }
        }
        if (all(to.remove)) {
          event(type = c("screen", "report"),
                "M: ALL detections from tag ", names(input)[link[i]],
                " removed per user command.")
          remove.tag <- c(remove.tag, link[i])
        } else {
          input[[link[i]]] <- input[[link[i]]][!to.remove, ]
          event(type = c("screen", "report"),
                "M: ", sum(to.remove), " detection(s) from tag ",
                names(input)[link[i]], " removed per user command.")
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
  event(type = "debug", "Running checkNoDetections.")
  if ('Code.space' %in% colnames(bio))
    link <- match(paste0(bio$Code.space, '-', lowestSignal(bio$Signal)), names(input))
  else
    link <- match(lowestSignal(bio$Signal), extractSignals(names(input)))

  if (all(is.na(link))) {
    event(type = "stop",
          "No detections were found in the input data which",
          " matched the target signals.")
  }
}

#' Check if there are duplicated signals in the detected tags.
#'
#' @param input list of detections
#' @param bio the biometrics table
#'
#' @return No return value, called for side effects.
#'
#' @keywords internal
#'
checkDupSignals <- function(input, bio){
  event(type = "debug", "Running checkDupSignals.")

  if (any(colnames(bio) == "Code.space")) {
    event(type = 'debug', 
          'Debug: Skipping checkDupSignals as there is a codespace column')
  }
  else {
    signals <- extractSignals(names(input))
    expected <- suppressWarnings(as.numeric(unlist(strsplit(as.character(bio$Signal), "|", fixed = TRUE))))
    to.check <- match(signals, expected)
    if (any(table(to.check) > 1)) {
        bind_list_with_expected <- cbind(names(input), expected[to.check])
        remove_non_detected <- bind_list_with_expected[complete.cases(bind_list_with_expected), ]
        table_the_signals <- table(remove_non_detected[, 1], remove_non_detected[, 2])

        dupsig <- data.frame(Signal = colnames(table_the_signals)[apply(table_the_signals,
            2, sum) > 1], Tags = NA, stringsAsFactors = FALSE)
        for (i in seq_len(nrow(dupsig))) {
            dupsig$Tags[i] <- paste(row.names(table_the_signals)[table_the_signals[, dupsig$Signal[i]] == 1], collapse = ", ")
        }

        rest.of.message <- NULL
        for (i in seq_len(nrow(dupsig))) {
            rest.of.message <- paste0(rest.of.message, "\n   Signal ",
                                      dupsig$Signal[i], " was found on tags ",
                                      dupsig$Tags[i], ".")
        }

        event(type = "stop",
              "One or more signals match more than one tag in the detections!",
              " Showing relevant signals/tags.", rest.of.message)
    }
  }
}

#' warn users if they are about to run into an unfixed bug.
#'
#' @return No return value, called for side effects.
#'
#' @keywords internal
#'
checkIssue79 <- function(arrays, spatial) {
  event(type = "debug", "Running checkIssue79.")

  if (length(arrays) < 3) {
    # not enough arrays to have a problem
    return()
  }

  output <- lapply(names(arrays), function(i) {
    link <- match(i, spatial$stations$Array)
    this.section <- as.character(unique(spatial$stations$Section[link]))

    link <- match(arrays[[i]]$before, spatial$stations$Array)
    before <- as.character(unique(spatial$stations$Section[link]))

    if (length(before) > 0) {
      before <- data.frame(
        Section = this.section,
        Neighbour = before,
        type = "Before")
    } else {
      before <- NULL
    }

    link <- match(arrays[[i]]$after, spatial$stations$Array)
    after <- as.character(unique(spatial$stations$Section[link]))

    if (length(after) > 0) {
      after <- data.frame(
        Section = this.section,
        Neighbour = after,
        type = "After")
    } else {
      after <- NULL
    }

    return(rbind(before, after))
  })

  x <- do.call(rbind, output)
  x <- x[apply(x, 1, function(i) i[1] != i[2]), ]
  x$tmp <- paste(x[,1], x[,2], x[,3])
  x[!duplicated(x$tmp), ]
  x$tmp <- NULL

  link <- aggregate(x$type, list(x$Section), function(i) any(duplicated(i)))$x

  if (any(link)) {
    if (getOption("actel.bypass_issue_79", default = FALSE)) {
      event(type = c("Screen", "warning", "report"), 
            "This study area seems to trigger issue 79.",
            " However, you have activated the bypass,",
            " so the analysis will continue.")
    } else {
      event(type = "stop",
            "\nREAD CAREFULLY\n",
            "It seems your study area contains parallel sections. actel is",
            " currently incapable of comprehending parallel sections during",
            " migration. This bug (issue 79) was brought to light in version",
            " 1.2.1, but a fix has not been found yet. This bug is easy to",
            " circumvent: you must make sure that the study area sections are",
            " sequential. Otherwise, the survival values per section could",
            " become compromised. If you wish to read more about this issue",
            " (when it appears, what it causes, and how to avoid it), please",
            " visit https://hugomflavio.github.io/actel-website/issue_79.html",
            "\n\n",
            "If you really want to run your analysis as is, or if you think",
            " this error came up unnecessarily, you can force the analysis by",
            " running the following command and restarting the analysis:",
            "\n\n",
            "options(actel.bypass_issue_79 = TRUE)",
            "\n\n",
            "If this issue impacts you and you want to help me fix it,",
            " feel free to reach out.",
            "\n\n")
    }
  }
}
