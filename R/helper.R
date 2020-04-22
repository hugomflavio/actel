#' nearsq helper
#' 
#' Obtained here: https://stackoverflow.com/questions/32017327/calculate-the-optimal-grid-layout-dimensions-for-a-given-amount-of-plots-in-r
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

#' find optimum plotting grid
#' 
#' obtained here: https://stackoverflow.com/questions/32017327/calculate-the-optimal-grid-layout-dimensions-for-a-given-amount-of-plots-in-r
#' 
#' @param n number of plots to fit.
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

#' Import actel Results to a List
#' 
#' @param source A RData file containing actel results from a previous run
#' 
#' @export
#' 
dataToList <- function(source){
  e <- new.env()
  load(source, envir = e)
  return(as.list(e))
}

#' Remove Code Spaces from transmitter names
#' 
#' @param input A vector of transmitter names
#' 
#' @keywords internal
#' 
stripCodeSpaces <- function(input) {
  unlist(lapply(input, function(x) tail(unlist(strsplit(x, "-")), 1)))
}


#' Calculate the standard error of the mean
#' 
#' @param x input data
#' @param na.rm Logical: Should missing values be removed?
#' @param silent Logical: Should the number of NA's removed be displayed?
#' 
#' @return Standard Error of the Mean
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
#' @return Decimal hour equivalent (single value or vector)
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
#' @return Decimal hour equivalent (single value or vector)
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
#' @return Trimmed character string
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
#' @return TRUE/FALSE vector for the input values.
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
#' @return TRUE/FALSE vector
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
#' @return A single vector where all data has been combined.
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
#' @return The rounded value
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
#' @return The rounded value
#' 
#' @keywords internal
#' 
roundDown <- function(input, to = 10) {
  to * (input%/%to)
}


#' Append to ...
#'
#' Appends a note/comment to the specified recipient, which in turn corresponds to a helper file (later on deleted by deleteHelpers).
#' 
#' @param recipient 'Screen' displays the message on screen, 'Report' appends the message to 'temp_log.txt', 'Warning' appends the message to 'temp_warnings.txt', 'UD' appends the message to 'temp_UD.txt', 'Comment' appends the message to 'temp_comments.txt'. The same message may be appended to multiple recipients at once.
#' @param line The text to be appended.
#' @param fish the tag number to which the comment belongs. Only used when recipient = 'Comment'.
#' 
#' @keywords internal
#' 
appendTo <- function(recipient, line, fish) {
  for (i in recipient) {
    if (i == "Screen") {
      if (any(recipient == "Warning"))
        warning(line, immediate. = TRUE, call. = FALSE)
      else
        message(line)
      flush.console()
    } 
    if (i == "Report") {
      if (any(recipient == "Warning"))
        write(paste("Warning:", line), file = "temp_log.txt", append = file.exists("temp_log.txt"))
      else
        write(line, file = "temp_log.txt", append = file.exists("temp_log.txt"))
    }
    if (i == "Warning") 
      write(paste("Warning:", line), file = "temp_warnings.txt", append = file.exists("temp_warnings.txt"))
    if (i == "UD") 
      write(line, file = "temp_UD.txt", append = file.exists("temp_UD.txt"))
    if (i == "Comment") 
      write(paste(fish, line, sep = "\t"), file = "temp_comments.txt", append = file.exists("temp_comments.txt"))
  }
  write(line, file = "temp_debug.txt", append = file.exists("temp_debug.txt"))
}

#' Delete temporary files
#' 
#' At the end of the function actel or emergencyBreak, removes temporary files.
#' 
#' @param emergency Logical: Should temp_debug.txt be kept?
#' 
#' @keywords internal
#' 
deleteHelpers <- function(emergency = FALSE) {
  helper.list <- paste0("temp_", c("log", "warnings", "UD", "comments", "debug"), ".txt")
  if (emergency) 
    helper.list <- helper.list[helper.list != "temp_debug.txt"]
  link <- unlist(lapply(helper.list, file.exists))
  for (file in helper.list[link]) file.remove(file)
}

#' Standard procedure when aborting
#' 
#' Saves a copy of the report in a permanent file, where the user decisions taken up to the abort are recorded.
#' 
#' @keywords internal
#' 
emergencyBreak <- function() {
  appendTo("Report", "\nA fatal exception occurred, stopping the process!\n\n-------------------")
  logname <- paste(gsub(":", ".", sub(" ", ".", as.character(Sys.time()))), "actel.log-STOP.txt", sep = ".")
  appendTo("Screen", paste("M: A fatal exception occurred, saving emergency log as", logname))
  if (file.exists("temp_UD.txt")) 
    appendTo("Report", paste0("User interventions:\n-------------------\n", gsub("\r", "", readr::read_file("temp_UD.txt")), "-------------------"))
  file.rename("temp_log.txt", logname)
  deleteHelpers(TRUE)
}

#' Move helper files to new directory
#' 
#' @param my.home The working directory where the main function was triggered.
#' 
#' @keywords internal
#' 
moveHelpers <- function(my.home) {
  if (getwd() != my.home) {
    if (file.exists(paste(my.home, "temp_log.txt", sep = "/")))
      fs::file_move(paste(my.home, "temp_log.txt", sep = "/"), paste(getwd(), "temp_log.txt", sep = "/"))
    if (file.exists(paste(my.home, "temp_warnings.txt", sep = "/"))) 
      fs::file_move(paste(my.home, "temp_warnings.txt", sep = "/"), paste(getwd(), "temp_warnings.txt", sep = "/"))
    if (file.exists(paste(my.home, "temp_debug.txt", sep = "/"))) 
      fs::file_move(paste(my.home, "temp_debug.txt", sep = "/"), paste(getwd(), "temp_debug.txt", sep = "/"))
  }
}

#' Write in comments
#' 
#' Checks if the user has invoked the comment command for a specific fish, and stores the comment.
#' 
#' @param line The text of the interaction in which the user may or may not request a comment.
#' @param tag The tag number currently being analysed.
#' 
#' @keywords internal
#' 
commentCheck <- function(line, tag) { # nocov start
  comment.check = TRUE
  while (comment.check) {
    decision <- readline(line)
    if (any(matchl(decision, c("Comment", "comment")))) {
      appendTo(c("UD"), "Comment")
      {
        appendTo(c("UD", "Comment"), readline(paste0("New comment on fish ", tag, ": ")), tag)
      }
      appendTo("Screen", "M: Comment successfully stored, returning to the previous interaction.")
    } else {
      comment.check = FALSE
    }
  }
  return(decision)
} # nocov end

#' Clean Current Folder
#' 
#' Deletes previous analysis files from the current workspace. Input files are not deleted.
#' 
#' @param skip A vector of files to be ignored.
#' 
#' @export
#' 
clearWorkspace <- function(skip = NA){
  deleteHelpers()
  files <- c(list.files(pattern = "*actel*"), list.files(pattern = "stray"))
  if (file_test("-d", "Report"))
    files <- c(files, "Report")
  files <- files[!files == "actel.R"]
  files <- files[!files == "actel.detections.RData"]
  files <- files[!matchl(files, skip)]
  if (length(files) > 0) {
    message("Proceeding will eliminate the following files/folders:")
    print(files)
    if (interactive()) {
      decision <- readline("Proceed?(y/N) ") # nocov
    } else {
      decision <- "y"
    }
    if(decision == "y" | decision == "Y"){
      unlink(files, recursive = TRUE)
    } else {
      message("Aborted.") # nocov
    }
  } else {
    message("Workspace already clean.")
  }
}

#' Display Update Help
#' 
#' @export
#' 
updateActel <- function() { # nocov start
  rep.ver <- tryCatch(unlist(strsplit(readLines('https://raw.githubusercontent.com/hugomflavio/actel/master/DESCRIPTION')[3], " "))[2], error = function(e) NULL, warning = function(w) NULL)
  if (!is.null(rep.ver)) {
    message("M: Opening actel's installation instructions.")
    browseURL("https://github.com/hugomflavio/actel#installing-actel")
  } else {
    message("M: Could not detect an internet connection. Find installation instructions in this webpage:\n   https://github.com/hugomflavio/actel#installing-actel")
  }
} # nocov end


#' Extract time stamp of valid entry or exit in each array
#' 
#' @param spatial The list of spatial objects.
#' @param movements The list of valid movements.
#' @param type The point to be recorded: one of "arrival" or "departure".
#' @param events The number of events to record. if "one" and type is "arrival", the very first arrival is returned;
#' if "one" and type is "departure", the very last departure is returned.
#' 
#' @keywords internal
#' 
#' @return A data frame with the timestamps for each fish (rows) and array (columns)
#' 
getTimes <- function(movements, spatial, type = c("arrival", "departure"), events = c("first", "all", "last")){
  # appendTo("Debug", "Running getTimes.")
  type <- match.arg(type)
  events <- match.arg(events)

  if (type == "arrival")
    the.column <- "First.time"
  else
    the.column <- "Last.time"

  the.times <- list()

  # extrat arrivals or departures
  capture.output <- lapply(movements, function(x) {
    # cat(".\n")
    aux <- x[[the.column]] # data.table syntax
    names(aux) <- x[[1]] # data.table syntax
    the.times[[length(the.times) + 1]] <<- aux
    return(NULL)
  })
  names(the.times) <- names(movements)

  # allow array/section movement flexibility
  if (colnames(movements[[1]])[1] == "Array")
    col.order <- unlist(spatial$array.order)
  else
    col.order <- names(spatial$array.order)

  # find maximum nchar for each tag
  max.char <- sapply(the.times, function(x) nchar(max(table(names(x)))))

  # shuffle data into the right format
  aux <- lapply(col.order, function(i) {
    aux <- lapply(names(the.times), function(j) {
      # cat(j, "\n")
      if (any(link <- names(the.times[[j]]) == i)) {
        output <- data.frame(
          Event = paste(j, stringr::str_pad(1:sum(link), max.char[j], pad = "0"), sep = "_"),
          V1 = the.times[[j]][link]
        )
        colnames(output)[2] <- i
        if (events == "first") {
          first.row <- output[1, , drop = FALSE]
          first.row$Event <- j
          return(first.row)
        }
        if (events == "last") {
          last.row <- output[nrow(output), , drop = FALSE]
          last.row$Event <- j
          return(last.row)
        }
        if (events == "all")
          return(output)
      } else {
        return(NULL)
      }
    })
    names(aux) <- names(the.times)
    output <- as.data.frame(data.table::rbindlist(aux))
    rownames(output) <- output$Event
    output <- output[, -1, drop = FALSE]
    return(output)
  })
  # Ensure all data frames contain the same rows, by the same order
  the.rows <- sort(unique(unlist(lapply(aux, row.names))))
  aux <- lapply(aux, function(x) {
    rows.to.add <- the.rows[!the.rows %in% row.names(x)]
    x[rows.to.add, ] <- NA
    x <- x[the.rows, , drop = FALSE]
    return(x)
  })
  output <- do.call(cbind, aux)
  output$Transmitter <- gsub("_[0-9]*$", "", rownames(output))
  output <- output[, c(ncol(output), 1:(ncol(output) - 1))]
  return(output)
}


#' Convert times data frame into a list of circular objects
#' 
#' @param times A data frame with the time stamps for reach fish and array.
#' 
#' @keywords internal
#' 
#' @return A list of circular objects
#' 
convertTimesToCircular <- function(times) {
  appendTo("Debug", "Running convertTimesToCircular.")
  output <- list()
  cols.with.data <- apply(times, 2, function(x) !all(is.na(x)))
  times <- times[, cols.with.data]
  for (i in 2:ncol(times)) {
    output[[i - 1]] <- circular::circular(decimalTime(substrRight(as.character(times[, i]), 8)), units = "hours", template = "clock24")
    names(output[[i - 1]]) <- times$Transmitter
  }
  names(output) <- colnames(times)[2:ncol(times)]
  return(output)
}

#' Calculate Transition Layer
#' 
#' \code{transitionLayer()} imports a shape file into R and prepares it to be used in distance
#' estimations. Adapted from Grant Adams' script "distance to closest mpa".
#' 
#' @param shape A shape file projected in a metric coordinate system.
#' @param size The pixel size, in metres.
#' @param EPSGcode The EPSG code of the shape file's coordinate system. DO NOT USE degree-based coordinate systems.
#' @param coord.x,coord.y The names of the columns containing the x and y positions of the stations in the spatial.csv file. Must be in the same coordinate system as the shape file.
#' @param buffer Artificially expand the shape file edges. Can be a single value (applied to all edges) or four values (xmin, xmax, ymin, ymax).
#' @param directions The number of directions considered for every movement situation during cost calculation. See the vignettes for more details.
#' @param force Logical: Should the process continue even if the transition layer has 2000 pixels on one or both axes?
#' 
#' @export
#' 
#' @return A RData file with the transition layer is stored in the current directory.
#' 
transitionLayer <- function(shape, size, EPSGcode, coord.x = NULL, coord.y = NULL, buffer = NULL, directions = c(16, 8, 4), force = FALSE){
  list.of.packages <- c("raster", "gdistance", "sp", "tools", "rgdal")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
  if (length(new.packages) > 0) {
    stop(paste0("This function requires packages '", paste(new.packages,collapse="', '"), 
      "' to operate. Please install them before proceeding.\n"), call. = FALSE)
  }
  directions <- as.character(directions)
  directions <- match.arg(directions)

  if (!is.numeric(EPSGcode))
    stop("'EPSGcode' must be numeric.\n", call. = FALSE)

  if (length(EPSGcode) != 1)
    stop("Please provide only one EPSG code.\n", call. = FALSE)

  aux <- rgdal::make_EPSG()$code
  to.check <- aux[!is.na(aux)]
  if (is.na(match(EPSGcode, to.check)))
    stop("Could not recognize the selected EPSG code. You can find a list of available EPSG codes by running rgdal::make_EPSG()\n", call. = FALSE)
  
  if (!is.null(buffer) & length(buffer) != 4 & length(buffer) != 1)
    stop("'buffer' must either contain one value (applied to all four corners), or four values (applied to xmin, xmax, ymin and ymax, respectively).\n", call. = FALSE)
  if (!is.null(buffer) & !is.numeric(buffer))
    stop("'buffer' must be numeric (in metres).\n", call. = FALSE)
  if (any(buffer < 0))
    stop("'buffer' values cannot be negative.\n", call. = FALSE)

  spatial <- NULL
  if (is.null(coord.x) & !is.null(coord.y))
    warning("'coord.y' was set but 'coord.x' was not. Skipping spatial.csv check.", call. = FALSE, immediate. = TRUE)
  if (!is.null(coord.x) & is.null(coord.y))
    warning("'coord.x' was set but 'coord.y' was not. Skipping spatial.csv check.", call. = FALSE, immediate. = TRUE)
  if (!is.null(coord.x) & !is.null(coord.y)) {
    if (file.exists("spatial.csv")) {
      spatial <- loadSpatial()
      if (any(is.na(xy <- match(c(coord.x, coord.y), colnames(spatial))))) {
        if (all(is.na(xy))) {
          warning("Could not find columns '", coord.x,"' and '", coord.y,"' in the spatial.csv file. Skipping spatial.csv check.", call. = FALSE, immediate. = TRUE)
        } else {
          if (is.na(xy[1]))
            warning("Could not find column '", coord.x,"' in the spatial.csv file. Skipping spatial.csv check.", call. = FALSE, immediate. = TRUE)
          else
            warning("Could not find column '", coord.y,"' in the spatial.csv file. Skipping spatial.csv check.", call. = FALSE, immediate. = TRUE)
        }
        spatial <- NULL
      }
    } else {
      warning("'coord.x' and 'coord.y' were set but could not find a spatial.csv file in the current working directory. Skipping spatial.csv check.", call. = FALSE, immediate. = TRUE)
    }
  }
  if (!file.exists(shape))
    stop(paste0("Could not find file '", shape, "' in the working directory.\n"), call. = FALSE)
  if (tools::file_ext(shape) == "shp") {
    shape <- sub(".shp", "", shape)
    shape <- rgdal::readOGR(dsn = ".", layer = shape, verbose = FALSE) #study area shapefile
  } else {
    stop("'shape' must be a .shp file.\n", call. = FALSE)
  }
  data.crs <- raster::crs(paste0("+init=epsg:", EPSGcode))
  shape <- sp::spTransform(x = shape, CRSobj = data.crs) # Set CRS 

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
  if (!is.null(spatial)) {
    xmax <- max(spatial[, xy[1]]) + size
    xmin <- min(spatial[, xy[1]]) - size
    ymax <- max(spatial[, xy[2]]) + size
    ymin <- min(spatial[, xy[2]]) - size

    if (shape@bbox[1, 1] > xmin) {
      message("Extending shape's minimum X range to ensure the stations fit in the range."); flush.console()
      shape@bbox[1, 1] <- xmin
    }
    if (shape@bbox[1, 2] < xmax) {
      message("Extending shape's maximum X range to ensure the stations fit in the range."); flush.console()
      shape@bbox[1, 2] <- xmax
    }
    if (shape@bbox[2, 1] > ymin) {
      message("Extending shape's minimum Y range to ensure the stations fit in the range."); flush.console()
      shape@bbox[2, 1] <- ymin
    }
    if (shape@bbox[2, 2] < ymax) {
      message("Extending shape's maximum Y range to ensure the stations fit in the range."); flush.console()
      shape@bbox[2, 2] <- ymax
    }
  }

  # ensure range allows for integer pixels
  fix.x <- ((shape@bbox[1, 2] - shape@bbox[1, 1]) %% size) / 2
  if (fix.x != 0) {
    message("M: Applying a correction of +- ", fix.x, " metres on each X edge of the shape to ensure an integer number of pixels.")
    shape@bbox[1, 2] <- shape@bbox[1, 2] - fix.x
    shape@bbox[1, 1] <- shape@bbox[1, 1] + fix.x
  }

  fix.y <- ((shape@bbox[2, 2] - shape@bbox[2, 1]) %% size) / 2
  if (fix.y != 0) {
    message("M: Applying a correction of +- ", fix.y, " metres on each Y edge of the shape to ensure an integer number of pixels.")
    shape@bbox[2, 2] <- shape@bbox[2, 2] - fix.y
    shape@bbox[2, 1] <- shape@bbox[2, 1] + fix.y
  }

  if (fix.y != 0 | fix.y != 0) {
    message("M: New shape extent:\n")
    print(shape@bbox)
  }
  
  pixel.res <- (shape@bbox[,2] - shape@bbox[,1]) / size
  message(paste("\nChosen pixel size:", size, "\n\nNumber of resulting pixels:\n"))
  print(pixel.res)
  message("")

  # start working
  if (!force && any(pixel.res > 2000)) {
    stop("The chosen pixel size creates a transition layer with one or two axes greater than
2000 pixels. This can lead to very long computing times and ultimately the function  may
fail due to lack of free RAM to allocate the results. If you really want to use this pixel
size, rerun the function with force = TRUE.\n", call. = FALSE)
  } else {
    ras <- raster::raster(nrow = pixel.res["y"], ncol = pixel.res["x"], crs = raster::crs(shape)) # create a recipient raster
    raster::extent(ras) <- raster::extent(shape) #Make the raster have the same extent as the shapefile
    #### "Burn" the shapefile into the raster
    message("M: Constructing the transition layer. This process may take several minutes depending on the study area size and chosen pixel size."); flush.console()
    shape.mask <- raster::rasterize(shape, ras)
    project.raster <- is.na(shape.mask)
    project.raster[project.raster == 0] <- NA # make land impossible to cross
    #### The transition layer will be used as the shape for calculating least-cost distance
    transition.layer <- gdistance::transition(1 / project.raster, transitionFunction = mean, directions = as.numeric(directions))
    transition.layer <- gdistance::geoCorrection(transition.layer, type = "c") # correct for shape distortion, as well as for diagonal connections between grid cells
    save(transition.layer, file = "transition.layer.RData")
    message("M: The transition layer was saved to 'transition.layer.RData' in the current work directory.")
  }
}

#' Calculate Distances Matrix
#' 
#' Using a previously created transition layer (see \code{\link{transitionLayer}}), calculates the distances
#' between spatial points. Adapted from Grant Adams' script "distance to closest mpa".
#' 
#' @param t.layer An RData file containing a transition layer.
#' @param starters The points from which to start measuring the distance.
#' @param targets The points to which a way must be found.
#' @param EPSGcode The EPSG code corresponding to the coordinate system of the input data. All inputs must be in the same metric system. DO NOT use degree-based coordinates.
#' @param coord.x,coord.y The names of the columns containing the x and y information. Must be identical in the starters and targets.
#' @param id.col The name of the column containing the IDs of the points to be used as starters and targets. Must be identical in both files.
#' @param actel Logical: Should the distance matrix be optimized for actel and saved in the working directory?
#'
#' @return The distances matrix. If actel = TRUE, the distance matrix is also stored in a 'distances.csv' file.
#' 
#' @export
#' 
distancesMatrix <- function(t.layer = "transition.layer.RData", starters = NULL, targets = starters, EPSGcode, 
  coord.x = "x", coord.y = "y", id.col = NULL, actel = TRUE){
  list.of.packages <- c("raster", "gdistance", "sp", "tools", "rgdal")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
  if (length(new.packages) > 0) {
    stop(paste0("This function requires packages '", paste(new.packages,collapse="', '"), 
      "' to operate. Please install them before proceeding.\n"), call. = FALSE)
  }

  if (!is.numeric(EPSGcode))
    stop("'EPSGcode' must be numeric.\n", call. = FALSE)
  if (length(EPSGcode) != 1)
    stop("Please provide only one EPSG code.\n", call. = FALSE)

  if (!is.null(id.col) && length(id.col) > 1)
    stop("Please provide only one column name in 'id.col'", call. = FALSE)
  if (!is.null(id.col) && is.numeric(id.col))
    stop("Please refer to the column name in 'id.col', rather than the column index.\n", call. = FALSE)

  aux <- rgdal::make_EPSG()$code
  to.check <- aux[!is.na(aux)]
  if (is.na(match(EPSGcode, to.check)))
    stop("Could not recognize the selected EPSG code. You can find a list of available EPSG codes by running rgdal::make_EPSG()\n", call. = FALSE)

  data.crs <- raster::crs(paste0("+init=epsg:", EPSGcode))

  if (!file.exists(t.layer))
    stop(paste0("Could not find file '", t.layer, "' in the working directory.\n"), call. = FALSE)

  if (tools::file_ext(t.layer) == "RData") {
    transition.layer <- NULL
    load(t.layer)
    if (is.null(transition.layer)) 
      stop(paste0("Could not find a transition layer in '", t.layer, "'.\n"), call. = FALSE)
  } else {
    stop(paste0("'", t.layer, "' could not be recognised as .RData file, please make sure the include the file extension in 't.layer'.\n"), call. = FALSE)
  }

  if (actel)
    starters <- targets <- "spatial.csv"

  if (!file.exists(starters))
    stop("Could not find a '", starters, "' file in the working directory.\n", call. = FALSE)
  if (!file.exists(targets))
    stop("Could not find a '", targets, "' file in the working directory.\n", call. = FALSE)

  if (tools::file_ext(starters) != "csv" | tools::file_ext(targets) != "csv")
    stop("One of the point files (starters or targets) does not appear to be written in csv format. Please make sure to include the '.csv' extension in the file name.\n", call. = FALSE)

  
  if (actel) {
    message("M: Creating actel-compatible distances matrix."); flush.console()
    id.col <- "Standard.name"
    starters.df <- loadSpatial()
    targets.df <- starters.df
  } else {
    starters.df <- read.csv(starters) 
    targets.df <- read.csv(targets)
  }

  if (is.na(match(coord.x, colnames(starters.df))))
    stop(paste0("Could not find a column '", coord.x, "' in the file '", starters,"'."), call. = FALSE)
  if (is.na(match(coord.y, colnames(starters.df))))
    stop(paste0("Could not find a column '", coord.y, "' in the file '", starters,"'."), call. = FALSE)
  if (is.na(match(coord.x, colnames(targets.df))))
    stop(paste0("Could not find a column '", coord.x, "' in the file '", targets,"'."), call. = FALSE)
  if (is.na(match(coord.y, colnames(targets.df))))
    stop(paste0("Could not find a column '", coord.y, "' in the file '", targets,"'."), call. = FALSE)

  colnames(starters.df)[colnames(starters.df) == coord.x] <- "longitude"
  colnames(starters.df)[colnames(starters.df) == coord.y] <- "latitude"
  colnames(targets.df)[colnames(targets.df) == coord.x] <- "longitude"
  colnames(targets.df)[colnames(targets.df) == coord.y] <- "latitude"

  if (!missing(id.col)) {
    if (!is.na(match(id.col, colnames(starters.df)))) {
      outputRows <- starters.df[, id.col] 
      if (any(duplicated(outputRows))) {
        warning("The '", id.col, "' column in the '", starters, "' file contains duplicated values; skipping row naming.", immediate. = TRUE, call. = FALSE)
        row.rename <- FALSE
      } else {
        row.rename <- TRUE
      }
    } else {
      warning("Could not find a '", id.col, "' column in the '", starters, "' file; skipping row naming.", immediate. = TRUE, call. = FALSE)
      row.rename <- FALSE
    }
    if (!is.na(match(id.col, colnames(targets.df)))) {
      outputCols <- targets.df[, id.col] 
      if (any(duplicated(outputCols))) {
        warning("The '", id.col, "' column in the '", targets, "' file contains duplicated values; skipping column naming.", immediate. = TRUE, call. = FALSE)
        col.rename <- FALSE
      } else {
        col.rename <- TRUE
      }
    } else {
      warning("Could not find a '", id.col, "' column in the '", targets, "' file; skipping column naming.", immediate. = TRUE, call. = FALSE)
      col.rename <- FALSE
    }
  } else {
    row.rename <- FALSE
    col.rename <- FALSE
  }
  #### Process the "from" coordinates (this would be the starters ".csv" file)
  sp::coordinates(starters.df) <- ~ longitude + latitude # converts the file to a spatialPoints object
  raster::crs(starters.df) <- raster::crs(data.crs) # sets the crs to metres
  #### Process the "to" coordinates (this would be the targets ".csv" file)
  sp::coordinates(targets.df) <- ~ longitude + latitude # converts the file to a spatialPoints object
  raster::crs(targets.df) <- raster::crs(data.crs)
  #### Calculate a matrix of distances to each object
  dist.mat <- data.frame(gdistance::costDistance(transition.layer, starters.df, targets.df))
  if (any(dist.mat == Inf)) {
    warning("At least one station is completely blocked off from the remaining stations by land. Filling 
the respective fields with NA. If your fish was expected to travel around the areas present 
in the shape file, consider applying a 'buffer' when calculating the transition layer. This
will artificially add water space around the shape file.", call. = FALSE)
    dist.mat[dist.mat == Inf] <- NA
  }
  if (row.rename)
    rownames(dist.mat) <- outputRows
  if (col.rename)
    colnames(dist.mat) <- outputCols
  if (actel) {
    message("M: Saving actel-compatible distances matrix as 'distances.csv'."); flush.console()
    write.csv(dist.mat, "distances.csv", row.names = TRUE)
  }
  return(dist.mat)
}

#' Create a Template Distances Matrix
#' 
#' Creates an empty matrix based on the local 'spatial.csv' file and saves it to 'distances.csv' so the
#' user can manually fill it.
#' 
#' @export
#' 
emptyMatrix <- function(){
  if(!file.exists("spatial.csv"))
    stop("Could not find a 'spatial.csv' file in the current working directory.\n", call. = FALSE)

  input <- loadSpatial(file = "spatial.csv")

  output <- matrix(nrow = nrow(input), ncol = nrow(input))
  colnames(output) <- rownames(output) <- input$Standard.name

  for(i in 1:nrow(output))
    output[i,i] = 0

  if (file.exists("distances.csv"))
    decision <- readline("A file named 'distances.csv' is already present in the working directory. Do you want to overwrite it?(y/N) ") # nocov
  else 
    decision <- "Y"

  if(decision == "Y" | decision == "y")
    write.csv(output, file = "distances.csv", na = "", row.names = TRUE)
  else
    message("Aborting.") # nocov
}

#' Complete a Distances Matrix
#' 
#' Completes a matrix that has the upper diagonal half filled.
#' 
#' @export
#' 
completeMatrix <- function(){
  if (!file.exists("distances.csv"))
    stop("Could not find a 'distances.csv' file in the current working directory.\n", call. = FALSE)

  input <- read.csv("distances.csv", row.names = 1)

  for (i in 1:ncol(input)) {
    input[i:ncol(input), i] <- t(input[i, i:ncol(input)])
  }

  write.csv(input, file = "distances.csv", row.names = TRUE)
  message("M: Distances matrix successfully completed and stored in 'distances.csv'.")
  return(input)
}
