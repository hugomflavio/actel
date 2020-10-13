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
  
  if (!row.names)
    output <- lapply(output, function(x) {
      rownames(x) <- 1:nrow(x)
      return(x)
    })

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
    col <- col / factor
    col <- grDevices::rgb(t(col), maxColorValue = 255)
    col
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

#' @title Remove Code Spaces from transmitter names
#'
#' @param input A vector of transmitter names
#'
#' @return A vector of transmitter signals
#'
#' @name stripCodeSpaces-deprecated
#' @usage stripCodeSpaces(input)
#' @seealso \code{\link{actel-deprecated}}
#' @keywords internal
NULL

#' @rdname stripCodeSpaces-deprecated
#'
#' @section \code{stripCodeSpaces}:
#' For \code{stripCodeSpaces}, use \code{\link{extractSignals}}.
#'
#' @export
#'
stripCodeSpaces <- function(input) {
  .Deprecated("extractSignals")
  extractSignals(input)
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
  .converter <- function(input, unit) {
    x = as.character(input)
    x = as.numeric(unlist(strsplit(x, ":")))
    if (length(x) == 2)
      x = x[1] + x[2]/60
    if (length(x) == 3)
      x = x[1] + x[2]/60 + x[3]/3600
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
    output <- .converter(input, unit = unit)
    names(output) <- input
  }
  if (length(input) > 1)
    output <- sapply(input, function(i) .converter(i, unit = unit))
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

#' Forcefully round a number up
#'
#' Forces the rounding of the input to the next higher rounded value.
#'
#' @param input The value to be rounded.
#' @param to The level of rounding to be applied (i.e. to=10 will round 14.2 to 20; to=1 will round i to 15).
#'
#' @return A numeric value or string.
#'
#' @keywords internal
#'
roundUp <- function(input, to = 10) {
  if (inherits(input, "list"))
    lapply(input, function(input) to * (input %/% to + as.logical(input %% to)))
  else
    to * (input %/% to + as.logical(input %% to))
}

#' Forcefully round a number down
#'
#' Forces the rounding of the input to the next lower rounded value.
#'
#' @param input The value to be rounded.
#' @param to The level of rounding to be applied (i.e. to=10 will round 14.8 to 10; to=1 will round i to 14).
#'
#' @return A numeric value or string.
#'
#' @keywords internal
#'
roundDown <- function(input, to = 10) {
  to * (input%/%to)
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
  for (i in recipient) {
    if (i == "Screen") {
      if (any(recipient == "Warning"))
        warning(line, immediate. = TRUE, call. = FALSE)
      else
        message(line)
      flush.console()
    }
    if (i == "Report") {
      if (any(recipient == "Warning")) {
        write(paste("Warning:", line),
          file = paste(tempdir(), "temp_log.txt", sep = "/"),
          append = file.exists(paste(tempdir(), "temp_log.txt", sep = "/")))
      } else {
        write(line,
          file = paste(tempdir(), "temp_log.txt", sep = "/"),
          append = file.exists(paste(tempdir(), "temp_log.txt", sep = "/")))
      }
    }
    if (i == "Warning") {
      write(paste("Warning:", line),
        file = paste(tempdir(), "temp_warnings.txt", sep = "/"),
        append = file.exists(paste(tempdir(), "temp_warnings.txt", sep = "/")))
    }
    if (i == "UD") { # nocov start
      write(line,
        file = paste(tempdir(), "temp_UD.txt", sep = "/"),
        append = file.exists(paste(tempdir(), "temp_UD.txt", sep = "/")))
    } # nocov end
    if (i == "Comment") {
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
  logname <- paste(gsub(":", ".", sub(" ", ".", as.character(Sys.time()))), "actel.log-ERROR.txt", sep = ".")

  if (file.exists(paste(tempdir(), "temp_comments.txt", sep = "/")))
    appendTo("Report", paste0("User comments:\n-------------------\n", gsub("\t", ": ", gsub("\r", "", readr::read_file(paste(tempdir(), "temp_comments.txt", sep = "/")))), "-------------------")) # nocov

  if (file.exists(paste(tempdir(), "temp_UD.txt", sep = "/")))
    appendTo("Report", paste0("User interventions:\n-------------------\n", gsub("\r", "", readr::read_file(paste(tempdir(), "temp_UD.txt", sep = "/"))), "-------------------")) # nocov

  appendTo("Report", paste0("Function call:\n-------------------\n", the.function.call, "\n-------------------"))

  message("")
  decision <- userInput(paste0("\nThe analysis errored. Save job log (including comments and decisions) to ", logname, "?(y/n) "), choices = c("y", "n"))
  
  if (decision == "y")
    file.copy(paste(tempdir(), "temp_log.txt", sep = "/"), logname)
  else
    file.rename(paste(tempdir(), "temp_log.txt", sep = "/"), paste(tempdir(), logname, sep = "/"))

  deleteHelpers()
} # nocov end

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
  if (is.null(dim(x)) | ncol(x) < 3 | !all(sapply(x[, 3:ncol(x)], function(i) any(class(i) == "POSIXt"))))
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

#' Load shapefile and convert to a raster object.
#'
#' loadShape can also perform early quality checks on the shape file, to ensure it is compatible
#' with the remaining study data. To activate these, set the names of the columns in the spatial.csv
#' file that contain the x and y coordinates of the stations using coord.x and coord.y. By default,
#' loadShape looks for a spatial.csv file in the current working directory, but this can be
#' personalized using the spatial argument.
#'
#' It is highly recommended to read the vignette regarding distances matrix before running this function.
#' You can find it by running \code{vignette('a-2_distances_matrix', 'actel')} or \code{browseVignettes('actel')}
#'
#' @param path The system path to the 'shape' file. Defaults to the current directory.
#' @param shape A shape file containing land polygons of the study area.
#' @param size The pixel size, in metres.
#' @param spatial Either a character string specifying the path to a spatial.csv file or a spatial data frame.
#'  This argument is not mandatory, but can be used to perform an early check of the shape file's compatibility
#'  with the study stations and release sites.
#' @param coord.x,coord.y The names of the columns containing the x and y positions of the stations
#'  in the spatial.csv file. these coordinates MUST BE in the same coordinate system as the shape file.
#' @param buffer Artificially expand the shape file edges. Can be a single value (applied to all edges)
#'  or four values (xmin, xmax, ymin, ymax). The unit of the buffer depends on the shape file's
#'  coordinate system.
#' @param type The type of shapefile being loaded. One of "land" or "water".
#'
#' @examples
#' \donttest{
#' # check if R can run the distance functions
#' aux <- c(
#'   length(suppressWarnings(packageDescription("raster"))),
#'   length(suppressWarnings(packageDescription("gdistance"))),
#'   length(suppressWarnings(packageDescription("sp"))),
#'   length(suppressWarnings(packageDescription("tools"))),
#'   length(suppressWarnings(packageDescription("rgdal"))))
#' missing.packages <- sapply(aux, function(x) x == 1)
#'
#' if (any(missing.packages)) {
#'   message("Sorry, this function requires packages '",
#'     paste(c("raster", "gdistance", "sp", "tools", "rgdal")[missing.packages], collapse = "', '"),
#'     "' to operate. Please install ", ifelse(sum(missing.packages) > 1, "them", "it"),
#'     " before proceeding.")
#' } else {
#'   if (suppressWarnings(require("rgdal"))) {
#'     # Fetch actel's example shapefile location
#'     aux <- system.file(package = "actel")[1]
#'
#'     # import the shape file
#'     x <- loadShape(path = aux, shape = "example_shapefile.shp", size = 20)
#'
#'     # have a look at the resulting raster,
#'     # where the blank spaces are the land areas
#'     raster::plot(x)
#'   } else {
#'     message("Sorry, it appears that rgdal is not being able to load.")
#'   }
#' }
#' rm(aux, missing.packages)
#' }
#' @return A raster object.
#'
#' @export
#'
loadShape <- function(path = ".", shape, size, spatial = "spatial.csv",
  coord.x = NULL, coord.y = NULL, buffer = NULL, type = c("land", "water")){
  # initial checks on package presence
  aux <- c(
    length(suppressWarnings(packageDescription("raster"))),
    length(suppressWarnings(packageDescription("gdistance"))),
    length(suppressWarnings(packageDescription("sp"))),
    length(suppressWarnings(packageDescription("tools"))),
    length(suppressWarnings(packageDescription("rgdal"))))
  missing.packages <- sapply(aux, function(x) x == 1)
  if (any(missing.packages)) {
    stop(paste0("This function requires packages '", paste(c("raster", "gdistance", "sp", "tools", "rgdal")[missing.packages], collapse = "', '"),
      "' to operate. Please install ", ifelse(sum(missing.packages) > 1, "them", "it"), " before proceeding.\n"), call. = FALSE)
  }

  type <- match.arg(type)

  if (!is.null(buffer) & length(buffer) != 4 & length(buffer) != 1)
    stop("'buffer' must either contain one value (applied to all four corners), or four values (applied to xmin, xmax, ymin and ymax, respectively).\n", call. = FALSE)
  if (!is.null(buffer) & !is.numeric(buffer))
    stop("'buffer' must be numeric (in metres or degrees, depending on the shape coordinate system).\n", call. = FALSE)
  if (any(buffer < 0))
    stop("'buffer' values cannot be negative.\n", call. = FALSE)

  if (is.null(coord.x) & !is.null(coord.y))
    warning("'coord.y' was set but 'coord.x' was not. Skipping range check.", call. = FALSE, immediate. = TRUE)
  if (!is.null(coord.x) & is.null(coord.y))
    warning("'coord.x' was set but 'coord.y' was not. Skipping range check.", call. = FALSE, immediate. = TRUE)

  # check if spatial information is present
  if (!is.null(coord.x) & !is.null(coord.y)) {
    check.spatial <- TRUE
    if (is.character(spatial)) {
      if (file.exists(spatial)) {
        spatial <- loadSpatial(spatial)
      } else {
        warning("Could not find a ", spatial, " file in the current working directory. Skipping range check.", call. = FALSE, immediate. = TRUE)
        check.spatial <- FALSE
        spatial <- NULL
      }
    }
    if (check.spatial) {
      if (any(is.na(xy <- match(c(coord.x, coord.y), colnames(spatial))))) {
        if (all(is.na(xy))) {
          warning("Could not find columns '", coord.x,"' and '", coord.y,"' in the spatial data frame. Skipping range check.", call. = FALSE, immediate. = TRUE)
        } else {
          if (is.na(xy[1]))
            warning("Could not find column '", coord.x,"' in the spatial data frame. Skipping range check.", call. = FALSE, immediate. = TRUE)
          else
            warning("Could not find column '", coord.y,"' in the spatial data frame. Skipping range check.", call. = FALSE, immediate. = TRUE)
        }
        spatial <- NULL
      }
    }
  } else {
    spatial <- NULL
  }

  # remaining input quality checks
  if (path != "." & !file.exists(paste(path, shape, sep = "/")))
    stop(paste0("Could not find file '", paste(path, shape, sep = "/"), "'.\n"), call. = FALSE)

  if (path == "." & !file.exists(shape))
    stop(paste0("Could not find file '", shape, "'.\n"), call. = FALSE)

  # load shape file
  if (tools::file_ext(shape) == "shp") {
    shape <- sub(".shp", "", shape)
    shape <- rgdal::readOGR(dsn = path, layer = shape, verbose = FALSE) #study area shapefile
  } else {
    stop("'shape' must be a .shp file.\n", call. = FALSE)
  }

  # extend ranges with the buffer
  if (!is.null(buffer)) {
    if (length(buffer) == 1){
      shape@bbox[, 1] <- shape@bbox[, 1] - buffer
      shape@bbox[, 2] <- shape@bbox[, 2] + buffer
    } else {
      shape@bbox[1, 1] <- shape@bbox[1, 1] - buffer[1]
      shape@bbox[1, 2] <- shape@bbox[1, 2] + buffer[2]
      shape@bbox[2, 1] <- shape@bbox[2, 1] - buffer[3]
      shape@bbox[2, 2] <- shape@bbox[2, 2] + buffer[4]
    }
  }

  # Compare shape range with station positioning
  issue.message.1 <- FALSE
  if (!is.null(spatial)) {
    xmax <- max(spatial[, xy[1]]) + size
    xmin <- min(spatial[, xy[1]]) - size
    ymax <- max(spatial[, xy[2]]) + size
    ymin <- min(spatial[, xy[2]]) - size

    if (shape@bbox[1, 1] > xmin) {
      issue.message.1 <- TRUE
      shape@bbox[1, 1] <- xmin
    }
    if (shape@bbox[1, 2] < xmax) {
      issue.message.1 <- TRUE
      shape@bbox[1, 2] <- xmax
    }
    if (shape@bbox[2, 1] > ymin) {
      issue.message.1 <- TRUE
      shape@bbox[2, 1] <- ymin
    }
    if (shape@bbox[2, 2] < ymax) {
      issue.message.1 <- TRUE
      shape@bbox[2, 2] <- ymax
    }
    if (issue.message.1)
    message("M: Extending the shape ranges with open water to ensure the stations fit inside it.")
  }

  # ensure range allows for integer pixels
  issue.message.2 <- FALSE

  fix.x <- ((shape@bbox[1, 2] - shape@bbox[1, 1]) %% size) / 2
  if (fix.x != 0) {
    issue.message.2 <- TRUE
    shape@bbox[1, 2] <- shape@bbox[1, 2] - fix.x
    shape@bbox[1, 1] <- shape@bbox[1, 1] + fix.x
  }

  fix.y <- ((shape@bbox[2, 2] - shape@bbox[2, 1]) %% size) / 2
  if (fix.y != 0) {
    issue.message.2 <- TRUE
    shape@bbox[2, 2] <- shape@bbox[2, 2] - fix.y
    shape@bbox[2, 1] <- shape@bbox[2, 1] + fix.y
  }

  if (issue.message.2)
    message("M: Applying a small correction to the shape extent to ensure an integer number of pixels.")

  if (issue.message.1 | issue.message.2) {
    message("M: New shape extent:")
    message(paste0(capture.output(print(shape@bbox)), collapse = "\n"), "\n")
  }

  pixel.res <- (shape@bbox[,2] - shape@bbox[,1]) / size
  message(paste("M: Chosen pixel size:", size, "\nM: Resulting pixel dimensions:"))
  message(paste0(capture.output(print(pixel.res)), collapse = "\n"), "\n")

  ras <- suppressWarnings(raster::raster(nrow = pixel.res["y"], ncol = pixel.res["x"], crs = raster::crs(shape))) # create a recipient raster
  raster::extent(ras) <- raster::extent(shape) #Make the raster have the same extent as the shapefile

  #### "Burn" the shapefile into the raster
  message("M: Burning the shape into a raster. This process may take several minutes depending on the shape size and chosen pixel size."); flush.console()
  shape.mask <- suppressWarnings(raster::rasterize(shape, ras))
  project.raster <- is.na(shape.mask)
  project.raster[project.raster == 0] <- NA # make land impossible to cross

  if (type == "water") {
    # invert raster
    project.raster[is.na(project.raster)] <- 2
    project.raster[project.raster == 1] <- NA
    project.raster[project.raster == 2] <- 1
  }

  # check if stations are in water
  if (!is.null(spatial)) {
    sp_points <- sp::SpatialPoints(data.frame(
          x = spatial[, coord.x],
          y = spatial[, coord.y]))

    check <- raster::extract(x = project.raster,
      y = sp_points)

    if (any(is.na(check))) {
      one <- sum(is.na(check)) == 1
      warning(ifelse(one, "Station '", "Stations '"), paste(spatial$Station.name[is.na(check)], collapse = "', '"),
        ifelse(one, "' is", "' are"), " not placed in water! This can cause several problems.", call. = FALSE, immediate. = TRUE)
    }
  }

  return(project.raster)
}


#' Calculate Transition Layer
#'
#' Using a previously imported shape file that has been converted to a raster (see \code{\link{loadShape}}),
#' Prepares a TransitionLayer object to be used in distance
#' estimations (see \code{\link{distancesMatrix}}). Adapted from Grant Adams' script "distance to closest mpa".
#'
#' It is highly recommended to read the vignette regarding distances matrix before running this function.
#' You can find it by running \code{vignette('a-2_distances_matrix', 'actel')} or \code{browseVignettes('actel')}
#'
#' @param x A water raster; for example the output of \code{\link{loadShape}}
#' @param directions The number of directions considered for every movement situation during cost
#'  calculation. See the vignettes for more details.
#'
#' @examples
#' \donttest{
#' # check if R can run the distance functions
#' aux <- c(
#'   length(suppressWarnings(packageDescription("raster"))),
#'   length(suppressWarnings(packageDescription("gdistance"))),
#'   length(suppressWarnings(packageDescription("sp"))),
#'   length(suppressWarnings(packageDescription("tools"))),
#'   length(suppressWarnings(packageDescription("rgdal"))))
#' missing.packages <- sapply(aux, function(x) x == 1)
#'
#' if (any(missing.packages)) {
#'   message("Sorry, this function requires packages '",
#'     paste(c("raster", "gdistance", "sp", "tools", "rgdal")[missing.packages], collapse = "', '"),
#'     "' to operate. Please install ", ifelse(sum(missing.packages) > 1, "them", "it"),
#'     " before proceeding.")
#' } else {
#'   if (suppressWarnings(require("rgdal"))) {
#'     # Fetch actel's example shapefile location
#'     aux <- system.file(package = "actel")[1]
#'
#'     # import the shape file
#'     x <- loadShape(path = aux, shape = "example_shapefile.shp", size = 20)
#'
#'     # Build the transition layer
#'     t.layer <- transitionLayer(x)
#'
#'     # inspect the output
#'     t.layer
#'
#'   } else {
#'     message("Sorry, it appears that rgdal is not being able to load.")
#'   }
#' }
#' rm(aux, missing.packages)
#' }
#' @return A TransitionLayer object.
#'
#' @export
#'
transitionLayer <- function(x, directions = c(16, 8, 4)){
  # initial checks on package presence
  aux <- c(
    length(suppressWarnings(packageDescription("raster"))),
    length(suppressWarnings(packageDescription("gdistance"))),
    length(suppressWarnings(packageDescription("sp"))),
    length(suppressWarnings(packageDescription("tools"))),
    length(suppressWarnings(packageDescription("rgdal"))))
  missing.packages <- sapply(aux, function(x) x == 1)
  if (any(missing.packages)) {
    stop(paste0("This function requires packages '", paste(c("raster", "gdistance", "sp", "tools", "rgdal")[missing.packages], collapse = "', '"),
      "' to operate. Please install ", ifelse(sum(missing.packages) > 1, "them", "it"), " before proceeding.\n"), call. = FALSE)
  }

  # argument quality
  directions <- as.character(directions[1])
  directions <- match.arg(directions)

  #### The transition layer will be used as the shape for calculating least-cost distance
  message("M: Constructing the transition layer. This process may take several minutes depending on the study area size and chosen pixel size."); flush.console()
  transition.layer <- gdistance::transition(1 / x, transitionFunction = mean, directions = as.numeric(directions))
  transition.layer <- gdistance::geoCorrection(transition.layer, type = "c") # correct for shape distortion, as well as for diagonal connections between grid cells
  return(transition.layer)
}

#' Calculate Distances Matrix
#'
#' Using a previously created transition layer (see \code{\link{transitionLayer}}), calculates the distances
#' between spatial points. Adapted from Grant Adams' script "distance to closest mpa". if the argument 'actel'
#' is set to TRUE (default), an actel-compatible matrix is generated, and the user will be asked if they would
#' like to save the matrix as 'distances.csv' in the current directory.
#'
#' It is highly recommended to read the vignette regarding distances matrix before running this function.
#' You can find it by running \code{vignette('a-2_distances_matrix', 'actel')} or \code{browseVignettes('actel')}
#'
#' @param t.layer A TransitionLayer object, generated by \code{\link{transitionLayer}}.
#' @param starters A data frame with the points from which to start measuring the distance. Ignored if actel = TRUE (default), as the 'spatial.csv' is loaded as starters.
#' @param targets A data frame with the points to which a way must be found. Ignored if actel = TRUE (default), as the 'spatial.csv' is loaded as targets.
#' @param coord.x,coord.y The names of the columns containing the x and y coordinates in the starters and targets. Must be identical in the starters and targets.
#' @param id.col The name of the column containing the IDs of the points to be used as starters and targets. Must be identical in both files. Ignored if actel = TRUE (default), as the stations' standard names are used.
#' @param actel Logical: Should the distance matrix be optimized for actel? Defaults to TRUE.
#'
#' @examples
#' \donttest{
#' # check if R can run the distance functions
#' aux <- c(
#'   length(suppressWarnings(packageDescription("raster"))),
#'   length(suppressWarnings(packageDescription("gdistance"))),
#'   length(suppressWarnings(packageDescription("sp"))),
#'   length(suppressWarnings(packageDescription("tools"))),
#'   length(suppressWarnings(packageDescription("rgdal"))))
#' missing.packages <- sapply(aux, function(x) x == 1)
#'
#' if (any(missing.packages)) {
#'   message("Sorry, this function requires packages '",
#'     paste(c("raster", "gdistance", "sp", "tools", "rgdal")[missing.packages], collapse = "', '"),
#'     "' to operate. Please install ", ifelse(sum(missing.packages) > 1, "them", "it"),
#'     " before proceeding.")
#' } else {
#'   if (suppressWarnings(require("rgdal"))) {
#'     # move to a temporary directory
#'     old.wd <- getwd()
#'     setwd(tempdir())
#'
#'     # Fetch location of actel's example files
#'     aux <- system.file(package = "actel")[1]
#'
#'     # create a temporary spatial.csv file
#'     file.copy(paste0(aux, "/example_spatial.csv"), "spatial.csv")
#'
#'     # import the shape file and use the spatial.csv
#'     # to check the extents.
#'     x <- loadShape(path = aux, shape = "example_shapefile.shp",
#'       coord.x = "x", coord.y = "y", size = 20)
#'
#'     # Build the transition layer
#'     t.layer <- transitionLayer(x)
#'
#'     # compile the distances matrix. Columns x and y in the spatial dataframe
#'     # contain the coordinates of the stations and release sites.
#'     distancesMatrix(t.layer, coord.x = 'x', coord.y = 'y')
#'
#'     # return to original directory
#'     setwd(old.wd)
#'     rm(old.wd)
#'   } else {
#'     message("Sorry, it appears that rgdal is not being able to load.")
#'   }
#' }
#' rm(aux, missing.packages)
#' }
#'
#' @return A matrix with the distances between each pair of points.
#'
#' @export
#'
distancesMatrix <- function(t.layer, starters = NULL, targets = starters,
  coord.x = "x", coord.y = "y", id.col = NULL, actel = TRUE){
  aux <- c(
    length(suppressWarnings(packageDescription("raster"))),
    length(suppressWarnings(packageDescription("gdistance"))),
    length(suppressWarnings(packageDescription("sp"))),
    length(suppressWarnings(packageDescription("tools"))),
    length(suppressWarnings(packageDescription("rgdal"))))
  missing.packages <- sapply(aux, function(x) x == 1)
  if (any(missing.packages)) {
    stop(paste0("This function requires packages '", paste(c("raster", "gdistance", "sp", "tools", "rgdal")[missing.packages], collapse = "', '"),
      "' to operate. Please install ", ifelse(sum(missing.packages) > 1, "them", "it"), " before proceeding.\n"), call. = FALSE)
  }

  if (class(t.layer) != "TransitionLayer")
    stop("Could not recognise 't.layer' as a TransitionLayer object. Make sure to compile it using the function transitionLayer.\n", call. = FALSE)

  if (!is.null(id.col) && length(id.col) > 1)
    stop("Please provide only one column name in 'id.col'", call. = FALSE)
  if (!is.null(id.col) && is.numeric(id.col))
    stop("Please refer to the column name in 'id.col', rather than the column index.\n", call. = FALSE)

  if (actel) {
    message("M: Creating actel-compatible distances matrix."); flush.console()
    if (!is.null(starters) | !is.null(targets))
      warning("starters' or 'targets' were set but will be ignored because 'actel' is set to TRUE. Set 'actel' to FALSE to use the 'starters' and 'targets' arguments.", call. = FALSE, immediate. = TRUE)
    starters <- targets <- loadSpatial()
    if (!is.null(id.col))
      warning("id.col' was set but will be ignored because 'actel' is set to TRUE. Set 'actel' to FALSE to use the 'id.col' argument.", call. = FALSE, immediate. = TRUE)
    id.col <- "Standard.name"
  }

  if (!inherits(starters, "data.frame"))
    stop("'starters' must be a data frame.\n", call. = FALSE)
  if (!inherits(targets, "data.frame"))
    stop("'targets' must be a data frame.\n", call. = FALSE)

  if (is.na(match(coord.x, colnames(starters))))
    stop(paste0("Could not find a column '", coord.x, "' in 'starters'."), call. = FALSE)
  if (is.na(match(coord.y, colnames(starters))))
    stop(paste0("Could not find a column '", coord.y, "' in 'starters'."), call. = FALSE)
  if (is.na(match(coord.x, colnames(targets))))
    stop(paste0("Could not find a column '", coord.x, "' in 'targets'."), call. = FALSE)
  if (is.na(match(coord.y, colnames(targets))))
    stop(paste0("Could not find a column '", coord.y, "' in 'targets'."), call. = FALSE)

    starters <- starters[, c(id.col, coord.x, coord.y)]
    colnames(starters) <- c(id.col, "longitude", "latitude")

    targets <- targets[, c(id.col, coord.x, coord.y)]
    colnames(targets) <- c(id.col, "longitude", "latitude")
    
  if (!is.null(id.col)) {
    if (!is.na(match(id.col, colnames(starters)))) {
      outputRows <- starters[, id.col]
      if (any(duplicated(outputRows))) {
        warning("The '", id.col, "' column in 'starters' contains duplicated values; skipping row naming.", immediate. = TRUE, call. = FALSE)
        row.rename <- FALSE
      } else {
        row.rename <- TRUE
      }
    } else {
      warning("Could not find a '", id.col, "' column in 'starters'; skipping row naming.", immediate. = TRUE, call. = FALSE)
      row.rename <- FALSE
    }
    if (!is.na(match(id.col, colnames(targets)))) {
      outputCols <- targets[, id.col]
      if (any(duplicated(outputCols))) {
        warning("The '", id.col, "' column in 'targets' contains duplicated values; skipping column naming.", immediate. = TRUE, call. = FALSE)
        col.rename <- FALSE
      } else {
        col.rename <- TRUE
      }
    } else {
      warning("Could not find a '", id.col, "' column in 'targets'; skipping column naming.", immediate. = TRUE, call. = FALSE)
      col.rename <- FALSE
    }
  } else {
    row.rename <- FALSE
    col.rename <- FALSE
  }

  #### Process the "from" coordinates (this would be the starters ".csv" file)
  sp::coordinates(starters) <- ~ longitude + latitude # converts the file to a spatialPoints object
  raster::crs(starters) <- raster::crs(t.layer) # sets the crs
  
  #### Process the "to" coordinates (this would be the targets ".csv" file)
  sp::coordinates(targets) <- ~ longitude + latitude # converts the file to a spatialPoints object
  raster::crs(targets) <- raster::crs(t.layer)
  
  #### Calculate a matrix of distances to each object
  dist.mat <- data.frame(gdistance::costDistance(t.layer, starters, targets))
  if (any(dist.mat == Inf)) {
    warning("At least one station is completely blocked off from the remaining stations by land. Filling
the respective fields with NA. If your animals were expected to travel around the areas present
in the shape file, consider applying a 'buffer' when calculating the transition layer. This
will artificially add water space around the shape file.", call. = FALSE)
    dist.mat[dist.mat == Inf] <- NA
  }
  
  if (row.rename)
    rownames(dist.mat) <- outputRows
  
  if (col.rename)
    colnames(dist.mat) <- outputCols
  
  if (interactive() & actel) { # nocov start
    decision <- userInput("Would you like to save an actel-compatible distances matrix as 'distances.csv' in the current work directory?(y/n) ",
                          choices = c("y", "n"))
    if (decision == "y") {
      if (file.exists('distances.csv')) {
        warning("A file 'distances.csv' is already present in the current directory.", call. = FALSE, immediate. = TRUE)
        decision <- userInput("Continuing will overwrite this file. Would you like to continue?(y/n) ", choices = c("y", "n"))
      }
      if (decision == "y")
        write.csv(round(dist.mat, 0), "distances.csv", row.names = TRUE)
    }
  } # nocov end
  return(round(dist.mat, 0))
}

#' Create a Template Distances Matrix
#'
#' Creates an empty matrix based on the local 'spatial.csv' file and saves it to 'distances.csv' so the
#' user can manually fill it.
#'
#' It is highly recommended to read the vignette regarding distances matrix before running this function.
#' You can find it by running \code{vignette('a-2_distances_matrix', 'actel')} or \code{browseVignettes('actel')}
#'
#' @param input Either a data frame with spatial data or the path to the file containing the spatial information.
#'
#' @examples
#' # This function requires a file with spatial information
#'
#' # Fetch location of actel's example files
#' aux <- system.file(package = "actel")[1]
#'
#' # run emptyMatrix on the temporary spatial.csv file
#' emptyMatrix(paste0(aux, "/example_spatial.csv"))
#'
#' @return An empty matrix with the rows and columns required to
#'  operate with the target spatial file.
#'
#' @export
#'
emptyMatrix <- function(input = "spatial.csv"){
  if (is.character(input)) {
    if(!file.exists(input))
      stop("Could not find file '", input, "'.\n", call. = FALSE)
  }

  input <- loadSpatial(input = input)

  output <- matrix(nrow = nrow(input), ncol = nrow(input))
  colnames(output) <- rownames(output) <- input$Standard.name

  for(i in 1:nrow(output))
    output[i, i] = 0

  return(output)
}

#' Complete a Distances Matrix
#'
#' Completes the bottom diagonal of a matrix with the same number of rows and columns.
#'
#' It is highly recommended to read the vignette regarding distances matrix before running this function.
#' You can find it by running \code{vignette('a-2_distances_matrix', 'actel')} or \code{browseVignettes('actel')}
#'
#' @param x A distances matrix to be completed.
#'
#' @examples
#' # Create dummy matrix with upper diagonal filled.
#' x <- matrix(
#'  c( 0,  1,  2,  3,  4, 5,
#'    NA,  0,  1,  2,  3, 4,
#'    NA, NA,  0,  1,  2, 3,
#'    NA, NA, NA,  0,  1, 2,
#'    NA, NA, NA, NA,  0, 1,
#'    NA, NA, NA, NA, NA, 0),
#'  ncol = 6, byrow = TRUE)
#'
#' # inspect x
#' x
#'
#' # run completeMatrix
#' completeMatrix(x)
#'
#' @return A matrix of distances between pairs of points.
#'
#' @export
#'
completeMatrix <- function(x){
  if (!inherits(x, "matrix"))
    stop("The input must be a matrix", call. = FALSE)
  if (nrow(x) != ncol(x))
    stop("The matrix does not contain the same number of columns and rows. Aborting.", call. = FALSE)

  for (i in 1:ncol(x)) {
    x[i:ncol(x), i] <- t(x[i, i:ncol(x)])
  }
  return(x)
}
