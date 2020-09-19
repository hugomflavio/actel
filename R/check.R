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
checkArguments <- function(dp, tz, minimum.detections, max.interval, speed.method = c("last to first", "last to last"),
  speed.warning, speed.error, start.time, stop.time, report, auto.open, save.detections, jump.warning, jump.error,
  inactive.warning, inactive.error, exclude.tags, override, print.releases, plot.detections.by = c("stations", "arrays"),
  if.last.skip.section = NULL, replicates = NULL, section.minimum = NULL, section.order = NULL, timestep = c("days", "hours")) {
  appendTo("debug", "Running checkArguments.")

  no.dp.args <- c("tz", "section.order", "start.time", "stop.time", "save.detections", "exclude.tags")
  link <- c(!is.null(tz), 
            !is.null(section.order),
            !is.null(start.time), 
            !is.null(stop.time),
            !(is.logical(save.detections) && !save.detections), !is.null(exclude.tags))

  if (!is.null(dp) & any(link))
    appendTo(c("Screen", "Warning", "Report"), paste0("Argument", ifelse(sum(link) > 1, "s '", " '"), paste(no.dp.args[link], collapse = "', '"),
      ifelse(sum(link) > 1, "' were ", "' was "), "set but a datapack was provided. Disregarding set arguments."))

  if (is.null(dp) && is.na(match(tz, OlsonNames())))
    stopAndReport("'tz' could not be recognized as a timezone. Check available timezones with OlsonNames()")

  if (!is.numeric(minimum.detections))
    stopAndReport("'minimum.detections' must be numeric.")

  if (minimum.detections <= 0)
    stopAndReport("'minimum.detections' must be positive.")

  if (!is.numeric(max.interval))
    stopAndReport("'max.interval' must be numeric.")

  if (max.interval <= 0)
    stopAndReport("'max.interval' must be positive.")

  if (max.interval < 10)
    appendTo(c("Screen", "Warning"), "Setting 'max.interval' to less than 10 minutes is not recommended!\n         This can lead to the creation of an immense number of movement events,\n         which will be hardly manageable if any quality check is triggered.")

  if (!is.character(speed.method))
    stopAndReport("'speed.method' should be one of 'last to first' or 'last to last'.")
  speed.method <- match.arg(speed.method)

  if (!is.null(speed.warning) && !is.numeric(speed.warning))
    stopAndReport("'speed.warning' must be numeric.")

  if (!is.null(speed.warning) && speed.warning <= 0)
    stopAndReport("'speed.warning' must be positive.")

  if (!is.null(speed.error) && !is.numeric(speed.error))
    stopAndReport("'speed.error' must be numeric.")

  if (!is.null(speed.error) && speed.error <= 0)
    stopAndReport("'speed.error' must be positive.")

  if (!is.null(speed.error) & is.null(speed.warning))
    speed.warning <- speed.error

  if (!is.null(speed.error) && speed.error < speed.warning)
    stopAndReport("'speed.error' must not be lower than 'speed.warning'.")

  if (!is.null(speed.warning) & is.null(speed.error))
    speed.error <- Inf

  if (is.null(dp) && !is.null(start.time) && !grepl("^[1-2][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9] [0-2][0-9]:[0-5][0-9]:[0-5][0-9]", start.time))
    stopAndReport("'start.time' must be in 'yyyy-mm-dd hh:mm:ss' format.")

  if (is.null(dp) && !is.null(stop.time) && !grepl("^[1-2][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9] [0-2][0-9]:[0-5][0-9]:[0-5][0-9]", stop.time))
    stopAndReport("'stop.time' must be in 'yyyy-mm-dd hh:mm:ss' format.")

  if (!is.logical(report))
    stopAndReport("'report' must be logical.")

  if (!is.logical(auto.open))
    stopAndReport("'auto.open' must be logical.")

  if (is.null(dp) && !is.logical(save.detections))
    stopAndReport("'save.detections' must be logical.")

  if (!is.numeric(jump.warning))
    stopAndReport("'jump.warning' must be numeric.")

  if (jump.warning < 1)
    stopAndReport("'jump.warning' must not be lower than 1.")

  if (!is.numeric(jump.error))
    stopAndReport("'jump.error' must be numeric.")

  if (jump.error < 1)
    stopAndReport("'jump.error' must not be lower than 1.")

  if (jump.error < jump.warning)
    stopAndReport("'jump.error' must not be lower than 'jump.warning'.")

  if (!is.null(inactive.warning) && !is.numeric(inactive.warning))
    stopAndReport("'inactive.warning' must be numeric.")

  if (!is.null(inactive.warning) && inactive.warning <= 0)
    stopAndReport("'inactive.warning' must be positive.")

  if (!is.null(inactive.error) && !is.numeric(inactive.error))
    stopAndReport("'inactive.error' must be numeric.")

  if (!is.null(inactive.error) && inactive.error <= 0)
    stopAndReport("'inactive.error' must be positive.")

  if (!is.null(inactive.error) & is.null(inactive.warning))
    inactive.warning <- inactive.error

  if (!is.null(inactive.error) && inactive.error < inactive.warning)
    stopAndReport("'inactive.error' must not be lower than 'inactive.warning'.")

  if (!is.null(inactive.warning) & is.null(inactive.error))
    inactive.error <- Inf

  if (is.null(dp) && !is.null(exclude.tags) && any(!grepl("-", exclude.tags, fixed = TRUE)))
    stopAndReport("Not all contents in 'exclude.tags' could be recognized as tags (i.e. 'codespace-signal'). Valid examples: 'R64K-1234', A69-1303-1234'")

  if (!is.null(override) && !is.numeric(override))
    stopAndReport("'override' must be numeric. Please include only the tag signals in the 'override' argument.")

  if (!is.logical(print.releases))
    stopAndReport("'print.releases' must be logical.")

  if (!is.character(plot.detections.by))
    stopAndReport("'plot.detections.by' should be one of 'stations' or 'arrays'")
  plot.detections.by <- match.arg(plot.detections.by)

  if (!is.character(timestep))
    stopAndReport("'timestep' should be one of 'days' or 'hours'")
  timestep <- match.arg(timestep)

  # Check that all the overridden tags are part of the study
  if (!is.null(dp) && !is.null(override)) {
    lowest_signals <- sapply(dp$bio$Signal, function(i) min(as.numeric(unlist(strsplit(as.character(i), "|", fixed = TRUE)))))
    if (any(link <- is.na(match(override, lowest_signals))))
      stopAndReport("Some tag signals listed in 'override' (", paste0(override[link], collapse = ", "), ") are not listed in the biometrics data.")
  }

  # NON-explore checks
  if (!is.null(section.order) && any(table(section.order) > 1))
    stopAndReport("Some section names are duplicated in the 'section.order' argument. Please include each section only once.")

  if (!is.null(if.last.skip.section) && !is.logical(if.last.skip.section))
    stopAndReport("'if.last.skip.section' must be logical.")

  if (!is.null(replicates) && !is.list(replicates))
    stopAndReport("'replicates' must be a list.")

  if (!is.null(replicates) && length(names(replicates)) != length(replicates))
    stopAndReport("All list elements within 'replicates' must be named (i.e. list(Array = 'St.1') rather than list('St.1')).")

  if (!is.null(section.minimum) && !is.numeric(section.minimum))
    stopAndReport("'section.minimum' must be numeric.")

  return(list(speed.method = speed.method,
              speed.warning = speed.warning,
              speed.error = speed.error,
              inactive.warning = inactive.warning,
              inactive.error = inactive.error,
              plot.detections.by = plot.detections.by,
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
  appendTo("debug", "Running checkToken.")
  if (file.exists(paste0(tempdir(), "/actel_token_list.csv")))
    x <- read.csv(paste0(tempdir(), "/actel_token_list.csv"))
  else
    x <- NULL
  if ((is.null(token) | is.null(timestamp) | is.null(x)) || !any(x$Token == token & x$Timestamp == timestamp))
    stopAndReport("The datapack's token is invalid or missing. Please the function preload() to compile the input data.\nAdditionally, data must to be compiled during the current R session.")
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
checkGUI <- function(GUI = c("needed", "always", "never")) {
  appendTo("debug", "Running checkGUI.")
  if (!is.character(GUI))
    stopAndReport("'GUI' should be one of 'needed', 'always' or 'never'.")
  GUI <- match.arg(GUI)
  if (GUI != "never") {
    aux <- c(
      length(suppressWarnings(packageDescription("gWidgets2"))),
      length(suppressWarnings(packageDescription("gWidgets2RGtk2"))),
      length(suppressWarnings(packageDescription("RGtk2"))))
    missing.packages <- sapply(aux, function(x) x == 1)
    if (any(missing.packages)) {
      appendTo(c("Screen", "Warning"),
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
       appendTo(c("Screen", "Warning"),
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
  appendTo("debug", "Running checkDupDetections.")
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
    decision <- userInput("Decision:(a/b/c) ", choices = letters[1:3], hash = "# dup. detections")
    if (decision == "a")
      stopAndReport("Function stopped by user command.") # nocov
    if (decision == "b") {
      appendTo(c("Screen", "Report"), "M: Removing duplicated detections from the analysis per user command.")
      output <- input[!dups, ]
    }
    if (decision == "c") { # nocov start
      appendTo(c("Screen", "Report"), "M: Continuing analysis with duplicated detections per user command.")
      output <- input
    } # nocov end
  } else {
    output <- input
  }
  return(output)
}


#' Check that the tag have enough detections to be valid
#'
#' @inheritParams check_args
#' @inheritParams explore
#'
#' @return A data frame containing the movements with valid/invalid notes
#'
#' @keywords internal
#'
checkMinimumN <- function(movements, minimum.detections, tag) {
  appendTo("debug", "Running checkMinimumN.")  
  if (nrow(movements) == 1 && movements$Detections < minimum.detections) {
    appendTo(c("Screen", "Report", "Warning"), paste0("Tag ", tag, " only has one movement event (", movements$Array, ") with ", movements$Detections, " detections. Considered invalid."))
    movements$Valid <- FALSE
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
checkSpeeds <- function(movements, tag, detections, valid.movements, speed.warning, speed.error, GUI) {
  appendTo("debug", "Running checkSpeeds.")
  the.warning <- NULL
  vm <- valid.movements
  if (any(na.as.false(vm$Average.speed.m.s >= speed.warning))) {
    link <- which(vm$Average.speed.m.s >= speed.warning)
    if (link[1] == 1) {
      appendTo(c("Report", "Warning", "Screen"),
        the.warning <- paste0("Tag ", tag, " had an average speed of ", round(vm$Average.speed.m.s[1], 2),
          " m/s from release to first valid event (Release -> ", vm$Array[1], ")."))
      the.warning <- paste("Warning:", the.warning)
      link <- link[-1]
    }
    if (length(link) > 0) {
      for (i in 1:length(link)) {
        appendTo(c("Report", "Warning", "Screen"),
          other.warning <- paste0("Tag ", tag, " had an average speed of ", round(vm$Average.speed.m.s[link[i]], 2),
            " m/s from valid event ", link[i] - 1, " to ", link[i], " (",vm$Array[link[i] - 1], " -> ", vm$Array[link[i]], ")."))
        other.warning <- paste("Warning:", other.warning)
        the.warning <- paste0(the.warning, "\n", other.warning)
      }
    }
  }
  # Trigger user interaction
  if (any(na.as.false(vm$Average.speed.m.s >= speed.error))) { # nocov start
    movements <- tableInteraction(moves = movements, tag = tag, detections = detections, trigger = the.warning, GUI = GUI)
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
checkInactiveness <- function(movements, tag, detections,
  inactive.warning, inactive.error, dist.mat, GUI) {
  Valid <- NULL
  appendTo("debug", "Running checkInactiveness.")
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
        # Trigger warning
      if (attributes(dist.mat)$valid) {
        aux <- dist.mat[the.stations, the.stations]
        if (all(aux <= 1500)) {
          n.detections <- sum(valid.moves$Detections[start_i:Stop])
          appendTo(c("Report", "Warning", "Screen"),
            the.warning <- paste0("Tag ", tag, " was detected ", n.detections,
              " times at stations less than 1.5 km apart in array '", tail(breaks$values, 1),
              "' (", paste(the.stations, collapse = ", "), "), over ", days.spent,
              " days and then disappeared. Could it be inactive?"))
          the.warning <- paste("Warning:", the.warning)
          continue <- FALSE
        }
        if (all(aux <= 1500) & days.spent >= inactive.error)
          trigger.error <- TRUE # nocov
      } else {
        if (length(the.stations) <= 3) {
          n.detections <- sum(valid.moves$Detections[start_i:Stop])
          appendTo(c("Report", "Warning", "Screen"),
            the.warning <- paste0("Tag ", tag, " was detected ", n.detections,
              " times at three or less stations of array '", tail(breaks$values, 1),
              "' (", paste(the.stations, collapse = ", "), ") over ", days.spent,
              " days and then disappeared. Could it be inactive?"))
          the.warning <- paste("Warning:", the.warning)
          continue <- FALSE
        }
        if (length(the.stations) <= 3 & days.spent >= inactive.error)
          trigger.error <- TRUE # nocov
      }
      # Trigger user interaction
      if (trigger.error) { # nocov start
        appendTo("Screen", error.message <- paste0("M: Tag ", tag, " has been inactive for more than ", inactive.error," days. Inactiveness started on event ", start_i, " (", as.Date(valid.moves$First.time[start_i]),")."))
        movements <- tableInteraction(moves = movements, tag = tag, detections = detections.list, trigger = paste0(the.warning, "\n", error.message), GUI = GUI)
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
checkImpassables <- function(movements, tag, detections, dotmat, GUI){
  appendTo("debug", "Running checkImpassables.")
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
          appendTo(c("Screen", "Warning", "Report"), aux <- paste0("Tag ", tag, " made an impassable jump: It is not possible to go from array ", shifts[i, 1], " to ", shifts[i, 2], ".\n         Please resolve this either by invalidating events or by adjusting your 'spatial.txt' file and restarting."))
          the.warning <- c(the.warning, aux)
        })
        if (interactive()) { # nocov start
          the.warning <- paste("Warning:", the.warning, collapse = "\n")
          movements <- tableInteraction(moves = movements, tag = tag, detections = detections, trigger = the.warning, GUI = GUI, force = TRUE)
          restart <- TRUE
        } else { # nocov end
          stop("Preventing analysis from entering interactive mode in a non-interactive session.\n")
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
checkSMovesN <- function(secmoves, tag, section.minimum, GUI) {
  appendTo("debug", "Running checkSMovesN.")
  if (any(link <- secmoves$Detections < section.minimum)) {
    appendTo(c("Screen", "Report", "Warning"), the.warning <- paste0("Section movements with less than ", section.minimum, " detections are present for tag ", tag, "."))
    if (interactive())
      secmoves <- tableInteraction(moves = secmoves, tag = tag, trigger = the.warning, GUI = GUI) # nocov
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
checkLinearity <- function(secmoves, tag, spatial, arrays, GUI) {
  appendTo("debug", "Running checkLinearity.")
  sections <- names(spatial$array.order)
  back.check <- match(secmoves$Section, sections)
  turn.check <- rev(match(sections, rev(secmoves$Section))) # captures the last event of each section. Note, the values count from the END of the events
  if (is.unsorted(back.check)) {
      if (is.unsorted(turn.check, na.rm = TRUE))
        appendTo(c("Screen", "Report", "Warning"), the.warning <- paste0("Inter-section backwards movements were detected for tag ", tag, " and the last events are not ordered!"))
      else
        appendTo(c("Screen", "Report", "Warning"), the.warning <- paste0("Inter-section backwards movements were detected for tag ", tag, "."))
    if (interactive()) { # nocov start
      secmoves <- tableInteraction(moves = secmoves, tag = tag, trigger = the.warning, GUI = GUI, force = FALSE)
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
  appendTo("debug", "Running checkReport.")
  if (report) {
    appendTo("Report", "M: 'report' option has been activated.")
    if (!rmarkdown::pandoc_available()) {
      appendTo(c("Screen", "Report", "Warning"), "'report' can only be activated if pandoc is installed. You can find how to install pandoc at: https://pandoc.org/installing.html\n   You can also check if pandoc is available to R by running rmarkdown::pandoc_available()")
      message("Would you like to:\n\n  a) Stop the analysis and install pandoc.\n  b) Continue with 'report' set to FALSE\n")
      decision <- userInput("Decision:(a/b) ", choices = letters[1:2], hash = "# pandoc warning")
      if (decision == "a")
        stopAndReport("Analysis stopped per user command.")
      if (decision == "b") {
        appendTo(c("Screen", "Report", "Warning"), "Deactivating 'report' to prevent function failure.")
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
checkUpstream <- function(movements, tag, release, detections, arrays, GUI) {
  appendTo("debug", "Running checkUpstream.")
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
    appendTo(c("Screen", "Report", "Warning"), the.warning <- paste0("Tag ", tag, " was detected in an array that is not after its release site! Opening relevant data for inspection.\nExpected first array: ", release))
    the.warning <- paste("Warning:", the.warning)
    if (interactive()) { # nocov start
      movements <- tableInteraction(moves = movements, tag = tag, detections = detections, trigger = the.warning, GUI = GUI)
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
checkJumpDistance <- function(movements, tag, detections, release, dotmat, jump.warning, jump.error, GUI) {
  appendTo("debug", "Running checkJumpDistance.")
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
        the.warning <- paste0("Tag ", tag, " jumped through ", release.jump - 1,
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
      if (any(is.na(jumps)))
        stopAndReport("There are unresolved impassable jumps in the movements.")
      if (any(jumps > jump.warning)) {
        link <- which(jumps > jump.warning)
        for (i in 1:length(link)) {
          # Trigger warning
          appendTo(c("Report", "Warning", "Screen"),
            other.warning <- paste0("Tag ", tag, " jumped through ", jumps[link[i]] - 1,
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
      movements <- tableInteraction(moves = movements, tag = tag, detections = detections, trigger = the.warning, GUI = GUI)
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
  appendTo("debug", "Running checkDeploymentTimes.")
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
        stopAndReport("Fatal exception found. Read lines above for more details.")
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
  appendTo("debug","Running checkDeploymentStations.")
  aux <- spatial[spatial$Type == "Hydrophone", ]
  link <- match(unique(input$Station.name), aux$Station.name)
  if (any(is.na(link))) {
    appendTo(c("Screen", "Report", "Warning"), paste0("", ifelse(sum(is.na(link)) > 1, "Stations", "Station"), " '", paste(unique(input$Station.name)[is.na(link)], collapse = "', '"), "' ", ifelse(sum(is.na(link)) > 1, "are", "is"), " listed in the deployments but ", ifelse(sum(is.na(link)) > 1, "are", "is"), " not part of the study's stations. Discarding deployments at unknown stations."))
    to.remove <- match(input$Station.name, unique(input$Station.name)[is.na(link)])
    input <- input[is.na(to.remove), ]
  }
  link <- match(aux$Station.name, unique(input$Station.name))
  if (any(is.na(link))) {
    stopAndReport(paste0(ifelse(sum(is.na(link)) > 1, "Stations '", "Station '"),
      paste(aux$Station.name[is.na(link)], collapse = "', '"),
      ifelse(sum(is.na(link)) > 1, "' are", "' is"),
      " listed in the spatial file but no receivers were ever deployed there.\n"))
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
  appendTo("debug", "Running checkUnknownReceivers.")
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
#'  \item \code{detections.list}: A list containing the detections for each tag, with updated station names.
#' }
#'
#' @keywords internal
#'
checkTagsInUnknownReceivers <- function(detections.list, deployments, spatial) {
  # NOTE: The NULL variables below are actually column names used by data.table.
  # This definition is just to prevent the package check from issuing a note due unknown variables.
  Receiver <- NULL
  include.all.unknowns <- FALSE
  exclude.all.unknowns <- FALSE

  appendTo("debug", "Running tagsInUnknownReceivers")
  for (i in names(detections.list)) {
    if (any(is.na(detections.list[[i]]$Standard.name))) {
      A <- detections.list[[i]]$Receiver
      B <- names(deployments)
      unknown.receivers <- unique(detections.list[[i]][is.na(match(A, B)), Receiver])
      if (length(unknown.receivers) > 0) {
        appendTo(c("Screen", "Report", "Warning"), paste0("Tag ", i, " was detected in one or more receivers that are not listed in the study area (receiver(s): ", paste(unknown.receivers, collapse = ", "), ")!"))
        message("Possible options:\n   a) Stop and double-check the data (recommended)\n   b) Temporarily include receiver(s) \n      (Unknown events will be considered invalid, but will show on detection plots)\n   c) Repeat option b for all unknown receivers\n   d) Discard unknown detections for this tag (not recommended)\n   e) Repeat option d for all tags (very much not recommended!)")
        decision <- userInput("Which option should be followed?(a/b/c/d/comment) ", choices = c("a", "b", "c", "d", "e", "comment"),
                              tag = i, hash = "# unknown receivers")
        if (decision == "a")
          stopAndReport("Stopping analysis per user command.") # nocov

        if (decision == "c") 
          include.all.unknowns <- TRUE # nocov
        
        if (include.all.unknowns || decision == "b") {
          recipient <- includeUnknownReceiver(spatial = spatial, deployments = deployments, unknown.receivers = unknown.receivers)
          spatial <- recipient[[1]]
          deployments <- recipient[[2]]
          link <- is.na(detections.list[[i]]$Standard.name)
          levels(detections.list[[i]]$Standard.name) <- c(levels(detections.list[[i]]$Standard.name), "Ukn.")
          detections.list[[i]]$Standard.name[link] <- "Ukn."
          levels(detections.list[[i]]$Array) <- c(levels(detections.list[[i]]$Array), "Unknown")
          detections.list[[i]]$Array[link] <- "Unknown"
        }
        
        if (decision == "e")
          exclude.all.unknowns <- TRUE # nocov
        
        if (exclude.all.unknowns || decision == "d")
          detections.list[[i]] <- detections.list[[i]][!is.na(match(A, B)), ] # nocov

        rm(decision)
      }
    }
  }
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
  appendTo("debug", "Running checkDetectionsBeforeRelease.")
  remove.tag <- NULL
  link <- match(bio$Transmitter, names(input))
  for(i in seq_len(length(link))) {
    if (!is.na(link[i])) {
      if (any(to.remove <- !(input[[link[i]]]$Timestamp > bio$Release.date[i]))) {
        appendTo(c("Screen", "Warning", "Report"), paste0("Tag ", names(input)[link[i]], " was detected before being released!"))
        if (!discard.orphans) {
          appendTo("Screen", paste0("  Release time: ", bio$Release.date[i]))
          appendTo("Screen", paste0("  First detection time: ", input[[link[i]]]$Timestamp[1]))
          appendTo("Screen", paste0("  Number of detections before release: ", sum(to.remove)))
          message("\nPossible options:\n   a) Stop and double-check the data (recommended)\n   b) Discard orphan detections in this instance.\n   c) Discard orphan detections for all instances.\n")
          decision <- userInput("Decision:(a/b/c/comment) ", 
                                choices = c("a", "b", "c", "comment"),
                                tag = bio$Transmitter[i], 
                                hash = paste("# detections before release for tag", bio$Transmitter[i]))
          
          if (decision == "a")
            stopAndReport("Function stopped by user command.") # nocov
          
          if (decision == "c")
            discard.orphans <- TRUE # nocov
        }
        
        if (all(to.remove)) {
          appendTo(c("Screen", "Report"), paste0("ALL detections from tag ", names(input)[link[i]], " were removed per user command."))
          remove.tag <- c(remove.tag, link[i])
        } else {
          input[[link[i]]] <- input[[link[i]]][!to.remove, ]
          appendTo(c("Screen", "Report"), paste0("M: ", sum(to.remove), " detections from tag ", names(input)[link[i]], " were removed per user command."))
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
  appendTo("debug", "Running checkNoDetections.")
  tag.list <- extractSignals(names(input))
  signal_check <- suppressWarnings(as.numeric(unlist(strsplit(as.character(bio$Signal), "|", fixed = TRUE))))
  link <- match(signal_check, tag.list)
  if (all(is.na(link)))
    stopAndReport("No detections were found in the input data which matched the target signals.\n")
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
  appendTo("debug", "Running checkDupSignals.")
  tag.list <- extractSignals(names(input))
  signal_check <- suppressWarnings(as.numeric(unlist(strsplit(as.character(bio$Signal), "|", fixed = TRUE))))
  failsafe <- match(tag.list, signal_check)
  if (any(table(failsafe) > 1)) {
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
    stopAndReport("One or more signals match more than one tag in the detections! Showing relevant signals/tags.", rest.of.message)
  }
}

