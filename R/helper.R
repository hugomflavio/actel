#' collapse event indexes into ranges
#'
#' @param x a numerical vector
#'
#' @return a string of isolated events and event ranges
#'
#' @keywords internal
#'
createEventRanges <- function(x) {
  overlaps <- c(FALSE, x[-length(x)] == x[-1]-1)
  starts <- which(!overlaps)
  stops <- c(starts[-1] - 1, length(x))
  aux <- data.frame(Start = x[starts],
                    Stop = x[stops])
  aux$Combine <- aux$Start != aux$Stop
  aux$Final <- aux$Start
  aux$Final[aux$Combine] <- paste(aux$Start[aux$Combine], aux$Stop[aux$Combine], sep = ":")
  return(aux$Final)
}

#' Split a dataframe every nth row
#'
#' Idea from here: https://stackoverflow.com/questions/7060272/split-up-a-dataframe-by-number-of-rows
#'
#' @param x the dataframe
#' @param n the number of rows to keep in each chunk
#'
#' @return A list of equal-sized dataframes
#'
#' @keywords internal
#'
splitN <- function(x, n, row.names = FALSE) {
  r <- nrow(x)
  z <- rep(1:ceiling(r / n), each = n)[1:r]
  output <- split(x, z)

  # reset row names?
  if (!row.names)
    output <- lapply(output, function(x) {
      rownames(x) <- 1:nrow(x)
      return(x)
    })

  # make individual names for list elements
  # showing the original rows. e.g. "1:100"
  aux <- unique(z) * n
  breaks <- aux[-length(aux)]
  starts <- c(1, breaks + 1)
  ends <- c(breaks, r)
  names(output) <- paste0(starts, ":", ends)
  return(output)
}

#' darken colours
#'
#' Copied from https://gist.github.com/Jfortin1/72ef064469d1703c6b30
#'
#' @param color The colour to be darkened
#' @param factor The level of darkening
#'
#' @return The darker colour code
#'
#' @keywords internal
#'
darken <- function(color, factor = 1.4){
  col <- grDevices::col2rgb(color)

  if (factor > 1)
    col <- col / factor
  else
    col <- col + (255 - col) * (1 - factor)

  col <- grDevices::rgb(t(col), maxColorValue = 255)
  return(col)
}

#' Match POSIX values
#'
#' @param this the vector of posix to be match
#' @param there the vector of posix to be matched against
#'
#' @return a vector with the matches
#'
#' @keywords internal
#'
match.POSIXt <- function(this, there) {
  sapply(this, function(i) {
    x <- which(i == there)
    if (length(x) == 0)
      return(NA)
    if (length(x) > 1)
      return(min(x))
    return(x)
  })
}

#' Find original station name
#'
#' @param input The results of an actel analysis (either explore, migration or residency).
#' @param station The station standard name or number.
#'
#' @examples
#' stationName(example.results, 1)
#'
#' # or
#'
#' stationName(example.results, "St.2")
#'
#' @return The original station name
#'
#' @export
#'
stationName <- function(input, station) {
  if (!inherits(input, "list"))
    stop("Could not recognise the input as an actel results object.", call. = FALSE)

  if (is.null(input$valid.movements) | is.null(input$spatial) | is.null(input$rsp.info))
    stop("Could not recognise the input as an actel results object.", call. = FALSE)

  if (is.numeric(station))
    station <- paste0("St.", station)

  link <- match(station, input$spatial$stations$Standard.name)

  return(input$spatial$stations$Station.name[link])
}

#' nearsq helper
#'
#' Finds the largest x for which n %% x == 0
#'
#' Obtained here: <https://stackoverflow.com/questions/32017327/calculate-the-optimal-grid-layout-dimensions-for-a-given-amount-of-plots-in-r>
#'
#' @param n number of plots to fit.
#'
#' @return An integer
#'
#' @keywords internal
#'
fact <- function(n) {
  k <- floor(sqrt(n))
  for (i in k:1) {
    if (n %% i == 0)
      return(i)
  }
}

#' Find optimum plotting grid
#'
#' Calculates the optimal distribution of plots in a square grid.
#'
#' Obtained here: <https://stackoverflow.com/questions/32017327/calculate-the-optimal-grid-layout-dimensions-for-a-given-amount-of-plots-in-r>
#'
#' @param n number of plots to fit.
#'
#' @return a vector of the number of rows and number of columns needed to distribute the desired number of plots.
#'
#' @keywords internal
#'
nearsq <- function(n, tol = 5/3+0.001) {
  m <- ceiling(sqrt(n))^2
  for (i in n:m) {
    a <- fact(i)
    b <- i / a
    if(b / a < tol)
      return(c(a, b))
  }
}

#' Import RData in a list format
#'
#' @param source A RData file.
#'
#' @examples
#' # Dummy example:
#' # Create two objects:
#' object_1 <- "This"
#' object_2 <- "Worked!"
#'
#' # Save them as an RData file in R's temporary directory
#' save(object_1, object_2, file = paste0(tempdir(), "/dataToList_example.RData"))
#'
#' # Remove the dummy objects as we don't need them any more
#' rm(object_1, object_2)
#'
#' # Load the RData file as a single object
#' x <- dataToList(paste0(tempdir(), "/dataToList_example.RData"))
#'
#' # inspect x
#' x
#'
#' @return A list containing the objects present in the source RData file.
#'
#' @export
#'
dataToList <- function(source){
  e <- new.env()
  load(source, envir = e)
  return(as.list(e))
}

#' Extract signals from transmitter names
#'
#' @param input A vector of transmitter names
#'
#' @examples
#' # create dummy string
#' x <- c("R64K-1234", "A69-1303-12")
#'
#' # run extractSignals
#' extractSignals(x)
#'
#' @return A vector of transmitter signals
#'
#' @export
#'
extractSignals <- function(input) {
  output <- lapply(input, function(x) {
    aux <- unlist(strsplit(as.character(x), "-"))
    return(as.numeric(tail(aux, 1)))
  })
  return(unlist(output))
}

#' Extract Code Spaces from transmitter names
#'
#' @param input A vector of transmitter names
#'
#' @examples
#' # create dummy string
#' x <- c("R64K-1234", "A69-1303-12")
#'
#' # run extractCodeSpaces
#' extractCodeSpaces(x)
#'
#' @return A vector of transmitter signals
#'
#' @export
#'
extractCodeSpaces <- function(input) {
  unname(sapply(input, function(x) sub("-[0-9]*$", "", as.character(x))))
}


#' Calculate the standard error of the mean for circular data
#'
#' @param x input data
#' @param na.rm Logical: Should missing values be removed?
#' @param silent Logical: Should the number of NA's removed be displayed?
#'
#' @return The standard error of the mean (numeric value).
#'
#' @keywords internal
#'
std.error.circular <- function(x, na.rm = TRUE, silent = FALSE){
  a <- length(x)
  if(na.rm) {
    x <- x[!is.na(x)]
  }
  output <- circular::sd.circular(x) / sqrt(length(x))
  if (!silent && a != length(x)) {
    message("M: Ommited ", a - length(x), " missing ", ifelse((a - length(x)) > 1, "values.", "value."))
  }
  return(output)
}


#' Convert hh:mm:ss time to hh.hhh
#'
#' @param input Single string or a vector of strings containing hours:minutes or hours:minutes:seconds.
#' @param unit the desired units of the output, one of "h" (hours), "m", (minutes) or "s" (seconds).
#'
#' @return A number or vector of numbers corresponding to the decimal hour equivalent of the character input.
#'
#' @keywords internal
#'
decimalTime <- function(input, unit = c("h", "m", "s")) {
  unit <- match.arg(unit)
  .converter <- function(x, unit) {
    x <- as.numeric(unlist(strsplit(x, ":")))
    if (length(x) == 2)
      x <- x[1] + x[2]/60
    if (length(x) == 3)
      x <- x[1] + x[2]/60 + x[3]/3600
    if (unit == "h")
      return(x)
    if (unit == "m")
      return(x * 60)
    if (unit == "s")
      return(x * 3600)
  }
  if (missing(input))
    stop("Input appears to be empty.")
  if (length(input) == 1) {
    output <- .converter(as.character(input), unit = unit)
    names(output) <- input
  }
  if (length(input) > 1)
    output <- sapply(as.character(input), function(i) .converter(i, unit = unit))
  return(output)
}

#' Convert numeric time to HH:MM
#'
#' @param x Single string or a vector of strings containing hours:minutes or hours:minutes:seconds.
#' @param format the format of x, one of "h" (hours), "m", (minutes) or "s" (seconds).
#' @param seconds Logical; If TRUE, output is returned in HH:MM:SS format.
#'
#' @return a string or vector of strings corresponding to the character hour equivalent of the numeric input.
#'
#' @keywords internal
#'
minuteTime <- function(x, format = c("h", "m", "s"), seconds = TRUE) {
  format <- match.arg(format)
  .converter <- function(x) {
    if(!is.na(x)){
      if(x < 0){
        x <- abs(x)
        neg = TRUE
      } else neg = FALSE
      if(format == "h")
        x = x
      if(format == "m")
        x = x/60
      if(format == "s")
        x = x/3600
      m = x %% 1
      h = x - m
      m = 60 * m
      s = m %% 1
      m = m - s
      s = round(60 * s, 0)
      if (h < 10) h <- paste0(0, h)
      if (!seconds & s>30) m = m + 1
      if (m < 10) m <- paste0(0, m)
      if (s < 10) s <- paste0(0, s)
      if (seconds)
        x <- paste(h, m, s, sep = ":")
      else
        x <- paste(h, m, sep = ":")
      if (neg) x <- paste0("-", x)
    }
    return(x)
  }
  if (missing(x)) stop("Input appears to be empty.")
  if (!is.numeric(x)) stop("Input is not numeric.")
  if (length(x) == 1) output <- .converter(x)
  if (length(x) > 1) output <- unlist(lapply(x, .converter))
  return(output)
}

#' Subset a character string counting from the right end
#'
#' Allows to quickly remove a given number of characters from the end of a string
#'
#' @param input Character string to be trimmed.
#' @param n Number of characters to be removed.
#'
#' @return A trimmed string or vector of trimmed strings.
#'
#' @keywords internal
#'
substrRight <- function(input, n) {
  substr(input, nchar(input) - n + 1, nchar(input))
}

#' TRUE/FALSE wrapper for match
#'
#' Looks for matches of a list on a target, but returns TRUE/FALSE instead of the position where the match was found.
#'
#' @param input vector of elements to be matched.
#' @param match vector of elements where to look for the input.
#'
#' @return A logical vector.
#'
#' @keywords internal
#'
matchl <- function(input, match) {
  !is.na(match(input, match))
}

#' Consider NA's as FALSE
#'
#' Aimed to be used in a vector of TRUE/FALSE's, where NA's are present and should be considered as false.
#'
#' @param input vector containing NA's.
#'
#' @return A logical vector.
#'
#' @keywords internal
#'
na.as.false <- function(input) {
  input[is.na(input)] <- FALSE
  return(input)
}

#' Combine a list of vectors
#'
#' Intended to combine vectors where, for each position, only one of the vectors contains data (i.e. the remaining are NA's).
#'
#' @param input a list of vectors with non-overlapping data.
#'
#' @return A vectorized combination of the data in the different list vectors.
#'
#' @keywords internal
#'
combine <- function(input) {
  if (!inherits(input, "list"))
    stop("'combine' is only intended to combine a list of vectors to a single vector.")
  if (length(input) == 1) {
    output <- input[[1]]
  } else {
    if (var(unlist(lapply(input, length))) != 0)
      stop("All vectors to combine should have the same length.")
    output <- input[[1]]
    for (i in 2:length(input)) {
      to.replace <- !is.na(input[[i]])
      if (any(!is.na(output)[to.replace]))
        stop("Trying to combine value to an already used position.")
      output[to.replace] <- input[[i]][to.replace]
    }
  }
  return(output)
}

#' Log a new event
#'
#' Logs useful information as the analysis progresses. May be called to abort
#' the run if type includes "stop"
#'
#' @param type A vector with one of more of several options:
#'  \itemize{
#'    \item 'crash' special case to be used only together with on.exit(). Meant
#'       to capture function failures not expressly handled by our coded stops.
#'    \item 'stop' appends the message to temp_log.txt then stops the execution.
#'    \item 'warning' appends the message to temp_warnings.txt,
#'    \item 'screen' displays the message on screen,
#'    \item 'report' appends the message to temp_log.txt,
#'    \item 'ud' appends the message to temp_UD.txt,
#'    \item 'comment' appends the message to temp_comments.txt,
#'    \item 'debug' appends the message only to the debug log. NOTE: does not have to be
#'      explicitly called; everything goes into the debug log. Use when you
#'      _only_ want the event to be logged in as debug.
#'  }
#'  The same message may be appended to multiple types at once.
#'  Preferred order is "stop", "warning", "screen", "report".
#' @param tag Only relevant one 'comment' is one of the types. Put the
#'  transmitter ID here so the comment is attached to the right animal.
#' @param ... The text fragments that compose the event message.
#'
#' @return No return value, called for side effects.
#'
#' @keywords internal
#'
event <- function(..., type, tag) {
  if (missing(type)) {
    stop("event was called without expressly defining argument 'type'.",
         call. = FALSE)
  }
  type <- tolower(type)

  if ("warning" %in% type & "error" %in% type) { # nocov start
    # this should never happen, this is a dev error
    stop("event() was called with both warning and error flags.",
         call. = FALSE)
  } # nocov end

  if ("crash" %in% type) {
    type <- c("crash", "screen", "report")
    event_text <- paste0("\nA fatal exception occurred!\n",
                         "Found a bug? Report it here:",
                         " https://github.com/hugomflavio/actel/issues",
                         "\n\n",
                         "You can recover latest the job log (including your",
                         " comments and decisions) by running recoverLog().\n",
                         "-------------------")
    function_call <- paste0(...)
  } else {
    event_text <- paste0(...)
  }

  if ("screen" %in% type) {
    if ("warning" %in% type) {
      warning(event_text, immediate. = TRUE, call. = FALSE)
    } else {
      message(event_text)
    }
    flush.console() # for buffered consoles.
  }

  # stop calls should always be logged to the report
  if ("report" %in% type | "stop" %in% type) {
    if ("warning" %in% type) {
      write(paste("Warning:", event_text),
            file = paste(tempdir(), "temp_log.txt", sep = "/"),
            append = file.exists(paste(tempdir(), "temp_log.txt", sep = "/")))
    } else {
      write(event_text,
            file = paste(tempdir(), "temp_log.txt", sep = "/"),
            append = file.exists(paste(tempdir(), "temp_log.txt", sep = "/")))
    }
  }

  if ("warning" %in% type) {
    write(paste("Warning:", event_text),
      file = paste(tempdir(), "temp_warnings.txt", sep = "/"),
      append = file.exists(paste(tempdir(), "temp_warnings.txt", sep = "/")))
  }

  # "ud" stands for "user decision"
  if ("ud" %in% type) { # nocov start
    write(event_text,
      file = paste(tempdir(), "temp_UD.txt", sep = "/"),
      append = file.exists(paste(tempdir(), "temp_UD.txt", sep = "/")))
  } # nocov end

  if ("comment" %in% type) {
    write(paste(tag, event_text, sep = "\t"),
      file = paste(tempdir(), "temp_comments.txt", sep = "/"),
      append = file.exists(paste(tempdir(), "temp_comments.txt", sep = "/")))
  }

  # everything goes into debug
  if ("debug" %in% type) {
    event_text <- paste("Debug:", event_text)
    if (getOption("actel.debug", default = FALSE)) {
      message(event_text)
    }
  }
  write(paste(format(Sys.time(), "%H:%M:%S:"), event_text),
    file = paste(tempdir(), "actel_debug_file.txt", sep = "/"),
    append = file.exists(paste(tempdir(), "actel_debug_file.txt", sep = "/")))

  if ("stop" %in% type) {
    stop(event_text, call. = FALSE)
  }

  if ("crash" %in% type) {
    # transfer comments, decisions, and function call to job log
    if (file.exists(paste0(tempdir(), "/temp_comments.txt"))) {
      comments <- readr::read_file(paste0(tempdir(), "/temp_comments.txt"))
      comments <- gsub("\r", "", comments)
      comments <- gsub("\t", ": ", comments)
    } else {
      comments <- ""
    }
    if (file.exists(paste0(tempdir(), "/temp_UD.txt"))) {
      uds <- readr::read_file(paste0(tempdir(), "/temp_UD.txt"))
      uds <- gsub("\r", "", uds)
    } else {
      uds <- ""
    }
    text_block <- paste0("User comments:\n",
                         "-------------------\n",
                         comments,
                         "-------------------\n",
                         "User interventions:\n",
                         "-------------------\n",
                         uds,
                         "-------------------\n",
                         "Function call:\n",
                         "-------------------\n",
                         function_call,
                         "\n",
                         "-------------------")
      write(text_block,
            file = paste(tempdir(), "temp_log.txt", sep = "/"),
            append = file.exists(paste(tempdir(), "temp_log.txt", sep = "/")))

    # rename the log file
    if (getOption("actel.debug", default = FALSE)) {
      # if in debug mode, export the debug log
      file.copy(paste0(tempdir(), "/actel_debug_file.txt"),
                paste0(tempdir(), "/latest_actel_error_log.txt"),
                overwrite = TRUE)
    } else {
      # if not in debug mode, export the regular log
      file.rename(paste0(tempdir(), "/temp_log.txt"),
                  paste0(tempdir(), "/latest_actel_error_log.txt"))
      # and clean up
      deleteHelpers()
    }
  }
}

#' Delete temporary files
#'
#' At the end of the function actel or event(type = "crash"),
#' removes temporary files.
#'
#' @return No return value, called for side effects.
#'
#' @keywords internal
#'
deleteHelpers <- function() {
  temp_files <- paste0("/temp_", c("log", "warnings", "UD", "comments"), ".txt")
  helper.list <- paste0(tempdir(), temp_files)
  link <- unlist(lapply(helper.list, file.exists))
  for (file in helper.list[link]) {
    file.remove(file)
  }
}

#' Recover latest actel crash log
#'
#' @param file Name of the file to which the log should be saved.
#' @param overwrite Logical: If 'file' already exists, should its content be overwritten?
#'
#' @examples
#' \dontshow{
#' sink(paste0(tempdir(), "/latest_actel_error_log.txt"))
#' cat(
#' "This is an example file
#' -
#' -
#' -
#' Timestamp: ", Sys.Date(), "
#' Function: example()")
#' sink()
#' }
#'
#' recoverLog(file = paste0(tempdir(), "/new_log.txt"))
#'
#' \dontshow{
#' file.remove(paste0(tempdir(), "/latest_actel_error_log.txt"))
#' file.remove(paste0(tempdir(), "/new_log.txt"))
#' }
#'
#' @return No return value, called for side effects.
#'
#' @export
#'
recoverLog <- function(file, overwrite = FALSE) {
  if (!file.exists(paste0(tempdir(), "/latest_actel_error_log.txt")))
    stop("No crash logs found.", call. = FALSE)

  if (missing(file)) {
    warning("'file' argument is missing. Attempting to save log to 'actel_job_log.txt'. To specify a different target, use the 'file' argument.", immediate. = TRUE, call. = FALSE)
    file <- "actel_job_log.txt"
  }

  if (!is.character(file) | length(file) != 1)
    stop("'file' must be a single character string.", call. = FALSE)

  if (!grepl("\\.txt$", file))
    file <- paste0(file, ".txt")

  if (file.exists(file) & !overwrite)
    stop("File '", file, "' already exists and overwrite = FALSE", call. = FALSE)

  file.copy(paste0(tempdir(), "/latest_actel_error_log.txt"), file, overwrite = overwrite)

  x <- readLines(file)
  function_line <- grep("^Function: ", x)
  timestamp_line <- grep("^Timestamp: ", x)
  message("M: Job log for ",
          sub("Function: ", "", x[function_line]),
              " analysis run on ",
              sub("Timestamp: ", "", x[timestamp_line]),
              " recovered to ", file)
}

#' Convert a data frame with timestamps into a list of circular objects
#'
#' @param x A data frame where the first column is an identifier, the second column
#'   is a grouping structure, and columns three and onwards are timestamps at different
#'   locations. Can be generated automatically by \code{\link{getTimes}}.
#' @param by.group Logical: Should the times at each location be divided by the group column (second column of x)?
#'
#' @examples
#' # create dummy input data frame.
#' # Note: the names of the columns are irrelevant.
#' x <- data.frame(ID = c(1:5),
#'  Group = c("A", "A", "B", "B", "B"),
#'  A1 = as.POSIXct(
#'    c("2019-01-03 11:21:12",
#'      "2019-01-04 12:22:21",
#'      "2019-01-05 13:31:34",
#'      "2019-01-06 14:32:43",
#'      "2019-01-07 15:23:52")),
#'  A2 = as.POSIXct(
#'    c("2019-01-08 16:51:55",
#'      "2019-01-09 17:42:42",
#'      "2019-01-10 18:33:33",
#'      "2019-01-11 19:24:32",
#'      "2019-01-12 20:15:22")),
#'  stringsAsFactors = TRUE)
#'
#' # run timesToCircular
#' timesToCircular(x)
#'
#' # optionally, split results by group:
#' timesToCircular(x, by.group = TRUE)
#'
#' @return A list of circular objects for each data column and, optionally, for each group.
#'
#' @export
#'
timesToCircular <- function(x, by.group = FALSE) {
  if (is.null(dim(x)) || ncol(x) < 3 || !all(sapply(x[, 3:ncol(x)], function(i) any(class(i) == "POSIXt"))))
    stop("timesToCircular only works on data frames where the second column is a grouping structure and columns three and onwards are timestamps.", call. = FALSE)

  output <- list()
  cols.with.data <- apply(x, 2, function(x) !all(is.na(x)))
  x <- x[, cols.with.data]

  if (by.group) {
    aux <- split(x, x[, 2])
    capture <- lapply(1:length(aux), function(i) {
      for (j in 3:ncol(x)) {
        output[[length(output) + 1]] <<- circular::circular(decimalTime(substrRight(as.character(aux[[i]][, j]), 8)), units = "hours", template = "clock24")
        names(output[[length(output)]]) <<- aux[[i]][, 1]
        names(output)[length(output)] <<- paste0(names(aux)[i], ".", colnames(aux[[i]])[j])
      }
    })
  } else {
    for (i in 3:ncol(x)) {
      output[[length(output) + 1]] <- circular::circular(decimalTime(substrRight(as.character(x[, i]), 8)), units = "hours", template = "clock24")
      names(output[[length(output)]]) <- x[, 1]
    }
    names(output) <- colnames(x)[3:ncol(x)]
  }
  return(output)
}


#' cleanly extract lowest signal from signals string
#'
#' @param x
#'
#' @return a vector of lowest signals
#'
#' @keywords internal
#'
lowestSignal <- function(x) {
  sapply(as.character(x), function(i)
    min(
      as.numeric(
        unlist(
          strsplit(i,
            "|",
            fixed = TRUE)
          )
        )
      )
    )
}

#' Split signals from multi-signal input
#'
#' @param x
#'
#' @return a list of split signals
#'
#' @keywords internal
#'
#'
splitSignals <- function(x) {
  if(length(x) != 1)
    stop('splitSignals can only split one input at a time')
  as.numeric(unlist(strsplit(as.character(x), "|", fixed = TRUE)))
}


#' Convert Lotek CDMA log to csv
#'
#' Lotek CDMA logs are exported in TXT, and contain several chunks of
#' of information. Importantly, the detections may be saved with a GMT offset,
#' as opposed to the more common UTC standard.
#' Additionally, the date format isn't the standard yyyy-mm-dd.
#'
#' This function extracts the detections from the CDMA file, converts the
#' dates to yyyy-mm-dd, binds the time to the date and resets it to UTC, and
#' ultimately converts the dataframe into the standard format accepted by actel.
#'
#' @param file the file name.
#' @param date_format the format used by the computer that generated the file
#'
#' @examples
#' # create a dummy detections file to read
#' dummy_file <- tempfile()
#' sink(dummy_file)
#' cat(
#' "WHS FSK Receiver Data File
#'
#' Receiver Configuration:
#' Working Frequency:  76 KHz
#' Bit Rate:           2400 bps
#' Code Type:          FSK
#' Serial Number:      WHS3K-1234567
#' Node ID:            10000
#'
#' Receiver Settings:
#' GMT Correction:     00:00
#'
#' Decoded Tag Data:
#' Date      Time             TOA       Tag ID    Type     Value     Power
#' =======================================================================
#' 04/09/24  22:50:03     0.43875        37910       P       9.1        12
#' 08/21/24  12:45:18     0.99646        55606       M         0         1
#' 08/23/24  15:01:04     0.76042        55778       P       0.0         2
#'
#' Receiver Sensor Messages:
#' Date      Time      Sensor   Temp     Press   Battery  Tilt-X  Tilt-Y  Tilt-Z
#' =============================================================================
#' 04/11/24  21:44:00  T / P    1534         0
#'
#' Receiver Setup Messages:
#' Date      Time      Type                    Details
#' =============================================================================
#' 08/22/24  18:50:11  Change Logging Mode     New Mode: SETUP
#' ")
#' sink()
#'
#' # now read it
#' x <- convertLotekCDMAFile(dummy_file)
#'
#' # the dummy file will be deleted automatically once you close this R session.
#'
#' @return A data frame of standardized detections from the input file.
#'
#' @export
#'
convertLotekCDMAFile <- function(file, date_format = "%m/%d/%y") {
  file_raw <- readLines(file)

  serial_n <- file_raw[grep("^Serial Number:", file_raw)]
  serial_n <- extractSignals(serial_n)

  code_type <- file_raw[grep("^Code Type:", file_raw)]
  code_type <- sub("Code Type:\\s*", "", code_type)
  if (code_type == "") {
    code_type <- NA
  }

  gmt_cor <- file_raw[grep("^GMT Correction:", file_raw)]
  gmt_cor <- sub("GMT Correction:\\s*", "", gmt_cor)
  gmt_cor <- decimalTime(gmt_cor)

  # importing this file is not easy. We must extract the detection lines
  # and the detection headers and then work with them. To find those lines:
  det_start <- grep("=========", file_raw)[1]
  det_end <- grep("Receiver Sensor Messages:", file_raw)[1] - 2

  # To properly parse the columns, we must use the column names, otherwise the
  # import will fail if any of the columns only has NAs in it. But we must
  # remove the "===" row before working with it. We must also replace any single
  # spaces in the column names with underscores.
  det_lines <- file_raw[(det_start-1):det_end]
  det_lines <- det_lines[-2] # <- the "=====" line
  det_lines[1] <- stringr::str_replace_all(det_lines[1],
                                           pattern = "(?<!\\s)\\s(?!\\s)",
                                           replacement = "_")

  # Now that we have the detection lines, we need to work some magic to find
  # the right column widths. This is because read_fwf doesn't allow using the
  # first row as column names, but without knowing the widths of that first row,
  # we'll run into the NA issue I mentioned above.
  # All this can be probably be changed _if_ this issue is addressed:
  # https://github.com/tidyverse/readr/issues/1393#issuecomment-2408682861
  #
  # So:
  # find out how fwf would read the headers
  head_fw <- readr::fwf_empty(I(det_lines[1]))
  # add the column names to head_fw
  aux <- sub("Tag_ID[^ ]*", "Signal", det_lines[1])
  head_fw$col_names <- unlist(strsplit(aux, "\\s+"))
  # find out how fwf would read the table content
  body_fw <- readr::fwf_empty(I(det_lines), skip = 1)
  # now the fun part, compare the two and expand head_fw as needed.
  # at the end of the foor loop below, head_fw will have the right
  # column positions to import the detection lines correctly.

  # the code below works for this example:
  # head_fw:
  #       1            2         3                4        5             6          7         8
  # 0           13  17  21    27  31           44 4749         60   65      73  77    83   88   93
  # |------------|   |---|     |---|            |--| |----------|    |-------|   |-----|    |----|
  # |Serial_Number    Date      Time             TOA  Tag_ID[dec]     Tag_Type    Sensor     Power

  # body_fw:
  #       1              2         3            4             5            6       !!!          7
  # 0           13  17      2527      35   40     47       56  60        70 73               90 93
  # |------------|   |-------| |-------|    |------|        |---|         |--|                |--|
  # |WHS4K-1900132    03/25/24  12:22:25     0.19490         1132          PSK                 512

  # Both combined:
  #       1              2         3            4          5             6          7         8
  # 0           13  17      2527      35   40     4749         60   65      73  77    83   88   93
  # |------------|   |-------| |-------|    |------| |----------|    |-------|   |-----|    |----|
  # |Serial_Number    Date      Time             TOA  Tag_ID[dec]     Tag_Type    Sensor     Power
  # |WHS4K-1900132    03/25/24  12:22:25     0.19490         1132          PSK                 512

  # i_b is set manually because some columns may be missing
  # in body_fw, so we need to be able to make body_fw lag
  # behind in comparison to head_fw.
  i_b <- 0
  for (i_h in 1:length(head_fw$begin)) {
    i_b <- i_b + 1
    while (TRUE) {
      if (head_fw$begin[i_h] < body_fw$begin[i_b]) {
        if (is.na(head_fw$end[i_h])) {
          # we've reached the end
          break()
        }
        if (head_fw$end[i_h] <= body_fw$begin[i_b]) {
          # if A starts and ends before/when b begins
          # then this is a missing column, so for the
          # coming comparisons we must lag behind in b.
          i_b <- i_b - 1
          break()
        } else {
          # a starts earlier, which is fine.
          if (head_fw$end[i_h] >= body_fw$end[i_b]) {
            # a already encompasses b.
            break()
          } else {
            # expand a to encompass b.
            head_fw$end[i_h] <- body_fw$end[i_b]
          }
        }
      }
      if (head_fw$begin[i_h] == body_fw$begin[i_b]) {
        if (is.na(head_fw$end[i_h])) {
          # we've reached the end
          break()
        }
        if (head_fw$end[i_h] == body_fw$end[i_b]) {
          # a and b are the same.
          break()
        } else {
          if (head_fw$begin[i_h + 1] >= body_fw$end[i_b]) {
            head_fw$end[i_h] <- body_fw$end[i_b]
          } else {
            # should never happen?
            stop("something went wrong, contact developer")
          }
        }
      }
      if (head_fw$begin[i_h] > body_fw$begin[i_b]) {
        head_fw$begin[i_h] <- body_fw$begin[i_b]
      }
    }
  }

  # now that we know the fixed widths, we can import the data
  output <- readr::read_fwf(I(det_lines),
    col_positions = head_fw,
    skip = 1, # skip the column headers to get the data types right.
    show_col_types = FALSE)

  # convert to data.table to stay compatible
  # with the rest of the import functions
  output <- as.data.table(output)

  # work on the columns we want to keep
  output$CodeSpace <- code_type
  output$Receiver <- as.numeric(serial_n)
  output$Date <- as.Date(output$Date, format = date_format)
  output$Timestamp <- paste(output$Date, output$Time)
  output$Signal <- suppressWarnings(as.numeric(output$Signal))
  # a more elegant warning is thrown at the end if NAs are formed here

  link <- matchl(colnames(output), c("Type", "Sensor"))
  if (sum(link) > 1) {
    stop("too many sensor type matches. contact developer")
  }
  if (sum(link) == 1) {
    output$Sensor.Unit <- output[, which(link), with = FALSE]
  } else {
    output$Sensor.Unit <- NA
  }

  if ("Value" %in% colnames(output)) {
    output$Sensor.Value <- output$Value
  } else {
    output$Sensor.Value <- NA
  }

  # extract the standard columns
  std_cols <- c("Timestamp", "Receiver", "CodeSpace",
                "Signal", "Sensor.Value", "Sensor.Unit")
  output <- output[, std_cols, with = FALSE]
  output$Timestamp <- fasttime::fastPOSIXct(as.character(output$Timestamp),
                                            tz = "UTC")
  output$Timestamp <- output$Timestamp - (gmt_cor * 3600)

  # final checks
  if (any(is.na(output$Timestamp))) {
    warning(
      paste0("Some timestamp values are NA. This must be fixed before these ",
      "detections are used in an actel analysis."),
      call. = FALSE)
  }
  if (any(is.na(output$Receiver))) {
    warning(
      paste0("Some receiver serial number values are NA. This must be fixed ",
        "before these detections are used in an actel analysis."),
      call. = FALSE)
  }
  if (any(is.na(output$CodeSpace))) {
    warning(
      paste0("Some code space values are NA. This must be fixed before these ",
      "detections are used in an actel analysis."),
      call. = FALSE)
  }
  if (any(is.na(output$Signal))) {
    warning(
      paste0("Some signal values are NA. This must be fixed before these ",
      "detections are used in an actel analysis."),
      call. = FALSE)
  }

  return(output)
}
