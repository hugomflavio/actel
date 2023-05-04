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

#' stop function but paste error to the report too
#' 
#' @param ... parts of the error string
#' 
#' @return No return value, called for side effects.
#' 
#' @keywords internal
#' 
stopAndReport <- function(...) {
  the.string <- paste0(...)
  appendTo("Report", paste0("Error: ", the.string))
  stop(the.string, call. = FALSE)
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
  unlist(lapply(input, function(x) tail(unlist(strsplit(as.character(x), "-")), 1)))
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
 if(na.rm)
  x <- x[!is.na(x)]
 output <- circular::sd.circular(x) / sqrt(length(x))
 if (!silent && a != length(x))
  message("M: Ommited ", a - length(x), " missing ", ifelse((a - length(x)) > 1, "values.", "value."))
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

#' Append to ...
#'
#' Appends a note/comment to the specified recipient, which in turn corresponds to a temporary helper file.
#'
#' @param recipient 'Screen' displays the message on screen, 'Report' appends the message to 'temp_log.txt', 'Warning' appends the message to 'temp_warnings.txt', 'UD' appends the message to 'temp_UD.txt', 'Comment' appends the message to 'temp_comments.txt'. The same message may be appended to multiple recipients at once.
#' @param line The text to be appended.
#' @param tag the tag number to which the comment belongs. Only used when recipient = 'Comment'.
#'
#' @return No return value, called for side effects.
#'
#' @keywords internal
#'
appendTo <- function(recipient, line, tag) {
  recipient <- tolower(recipient)
  for (i in recipient) {
    if (i == "screen") {
      if (any(recipient == "warning"))
        warning(line, immediate. = TRUE, call. = FALSE)
      else
        message(line)
      flush.console()
    }
    if (i == "report") {
      if (any(recipient == "warning")) {
        write(paste("Warning:", line),
          file = paste(tempdir(), "temp_log.txt", sep = "/"),
          append = file.exists(paste(tempdir(), "temp_log.txt", sep = "/")))
      } else {
        write(line,
          file = paste(tempdir(), "temp_log.txt", sep = "/"),
          append = file.exists(paste(tempdir(), "temp_log.txt", sep = "/")))
      }
    }
    if (i == "warning") {
      write(paste("Warning:", line),
        file = paste(tempdir(), "temp_warnings.txt", sep = "/"),
        append = file.exists(paste(tempdir(), "temp_warnings.txt", sep = "/")))
    }
    if (i == "ud") { # nocov start
      write(line,
        file = paste(tempdir(), "temp_UD.txt", sep = "/"),
        append = file.exists(paste(tempdir(), "temp_UD.txt", sep = "/")))
    } # nocov end
    if (i == "comment") {
      write(paste(tag, line, sep = "\t"),
        file = paste(tempdir(), "temp_comments.txt", sep = "/"),
        append = file.exists(paste(tempdir(), "temp_comments.txt", sep = "/")))
    }
  }
  write(paste(format(Sys.time(), "%H:%M:%S.:"), line),
    file = paste(tempdir(), "actel_debug_file.txt", sep = "/"),
    append = file.exists(paste(tempdir(), "actel_debug_file.txt", sep = "/")))
}

#' Delete temporary files
#'
#' At the end of the function actel or emergencyBreak, removes temporary files.
#'
#' @return No return value, called for side effects.
#'
#' @keywords internal
#'
deleteHelpers <- function() {
  helper.list <- paste0(tempdir(), paste0("/temp_", c("log", "warnings", "UD", "comments"), ".txt"))
  link <- unlist(lapply(helper.list, file.exists))
  for (file in helper.list[link]) {
    file.remove(file)
  }
}

#' Standard procedure when aborting
#'
#' Wraps up the report in R's temporary folder before the function end.
#'
#' @return No return value, called for side effects.
#'
#' @keywords internal
#'
emergencyBreak <- function(the.function.call) { # nocov start
  appendTo("Report", "\nA fatal exception occurred, stopping the process!\nFound a bug? Report it here: https://github.com/hugomflavio/actel/issues\n\n-------------------")
  if (file.exists(paste0(tempdir(), "/temp_comments.txt")))
    appendTo("Report", paste0("User comments:\n-------------------\n", gsub("\t", ": ", gsub("\r", "", readr::read_file(paste0(tempdir(), "/temp_comments.txt")))), "-------------------"))

  if (file.exists(paste0(tempdir(), "/temp_UD.txt")))
    appendTo("Report", paste0("User interventions:\n-------------------\n", gsub("\r", "", readr::read_file(paste0(tempdir(), "/temp_UD.txt"))), "-------------------"))

  appendTo("Report", paste0("Function call:\n-------------------\n", the.function.call, "\n-------------------"))

  message("\nM: The analysis errored. You can recover latest the job log (including your comments and decisions) by running recoverLog().")
  
  if (getOption("actel.debug", default = FALSE)) {
    file.copy(paste0(tempdir(), "/temp_log.txt"), paste0(tempdir(), "/latest_actel_error_log.txt"))
  } else {
    file.rename(paste0(tempdir(), "/temp_log.txt"), paste0(tempdir(), "/latest_actel_error_log.txt"))
    deleteHelpers()
  }
} # nocov end

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

  if (!grepl(".txt$", file))
    file <- paste0(file, ".txt")

  if (file.exists(file) & !overwrite)
    stop("File '", file, "' already exists and overwrite = FALSE", call. = FALSE)

  file.copy(paste0(tempdir(), "/latest_actel_error_log.txt"), file, overwrite = overwrite)

  x <- readLines(file)
  message("M: Job log for ", sub("Function: ", "", x[6]), " analysis run on ", sub("Timestamp: ", "", x[5]), " recovered to ", file)
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

