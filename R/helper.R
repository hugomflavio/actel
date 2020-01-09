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
  message("M: Ommited", a - length(x), "missing values.")
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
  if (length(input) < 1) 
    stop("Input appears to be empty.")
  if (length(input) == 1) 
    output <- .converter(input, unit = unit)
  if (length(input) > 1) 
    output <- unlist(lapply(input, function(i) .converter(i, unit = unit)))
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
      if (neg) x <- paste("-", x)
    }
    return(x)
  }
  if (length(x) < 1) stop("Input appears to be empty.")
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
    appendTo("Report", paste0("User inverventions:\n-------------------\n", gsub("\r", "", readr::read_file("temp_UD.txt")), "-------------------"))
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
commentCheck <- function(line, tag) {
  comment.check = TRUE
  while (comment.check) {
    decision <- readline(line)
    if (any(matchl(decision, c("Comment", "comment")))) {
      appendTo(c("UD"), "Comment")
      {
        appendTo(c("UD", "Comment"), readline(paste0("New comment on fish ", tag, ":")), tag)
      }
      appendTo("Screen", "M: Comment successfully stored, returning to the previous interaction.")
    } else {
      comment.check = FALSE
    }
  }
  return(decision)
}

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
    decision <- readline("Proceed?(y/N) ")
    if(decision == "y" | decision == "Y"){
      unlink(files, recursive = TRUE)
    } else
      message("Aborted.")
  } else {
    message("Workspace already clean.")
  }
}

#' Display Update Help
#' 
#' @export
#' 
updateActel <- function() {
  rep.ver <- tryCatch(unlist(strsplit(readLines('https://raw.githubusercontent.com/hugomflavio/actel/master/DESCRIPTION')[3], " "))[2], error = function(e) NULL, warning = function(w) NULL)
  if (!is.null(rep.ver)) {
    message("M: Opening actel's installation instructions.")
    browseURL("https://github.com/hugomflavio/actel#installing-actel")
  } else {
    message("M: Could not detect an internet connection. Find installation instructions in this webpage:\n   https://github.com/hugomflavio/actel#installing-actel")
  }
}


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
getTimes <- function(movements, spatial, type = c("arrival", "departure"), events = c("one", "all")){
  # appendTo("Debug", "Running getTimes.")
  type <- match.arg(type)
  events <- match.arg(events)

  the.times <- list()

  if (type == "arrival")
    the.column <- "First.time"
  else
    the.column <- "Last.time"

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

  # shuffle data into the right format
  aux <- lapply(col.order, function(i) {
    aux <- lapply(names(the.times), function(j) {
      # cat(j, "\n")
      if (any(link <- names(the.times[[j]]) == i)) {
        output <- data.frame(
          Event = paste(j, 1:sum(link), sep = "_"),
          V1 = the.times[[j]][link]
        )
        colnames(output)[2] <- i
        if (events == "one") {
          if (type == "arrival")
            return(output[1, , drop = FALSE])
          else
            return(output[nrow(output), , drop = FALSE])
        } else {
          return(output)
        }
      } else {
        return(NULL)
      }
    })
    names(aux) <- names(the.times)
    output <- as.data.frame(data.table::rbindlist(aux))
    rownames(output) <- output$Event
    output <- output[, -1, drop = FALSE]
    if (events == "one")
      rownames(output) <- gsub("_[0-9]*$", "", rownames(output))
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
#' \code{transitionLayer} imports a shape file into R and prepares it to be used in distance
#' estimations. Adapted from Grant Adams' script "distance to closest mpa". 
#' (grant.adams@@eagles.usm.edu)
#' 
#' \url{https://cran.r-project.org/web/packages/gdistance/vignettes/gdistance1.pdf}
#'
#' @param shape A shape file projected in a metric coordinate system.
#' @param size The pixel size, in metres.
#' @param EPSGcode The EPSG code of the shape file's coordinate system. DO NOT USE degree-based coordinate systems.
#' @param directions The number of directions considered for every movement situation during cost calculation. See the vignettes for more details.
#' @param force Logical: Should the process continue even if the transition layer has 2000 pixels on one or both axes?
#' 
#' @export
#' 
#' @return A RData file with the transition layer is stored in the current directory.
#' 
transitionLayer <- function(shape, size, EPSGcode, directions = c(16, 8, 4), force = FALSE){
  directions <- as.character(directions)
  directions <- match.arg(directions)
  if (!file.exists(shape))
    stop(paste0("Could not find file '", shape, "' in the working directory.\n"), call. = FALSE)
  if (tools::file_ext(shape) == "shp") {
    shape <- sub(".shp", "", shape)
    shape <- rgdal::readOGR(dsn = ".", layer = shape, verbose = FALSE) #study area shapefile
  } else {
    stop("'shape' must be a .shp file.\n", call. = FALSE)
  }
  data.crs <- raster::crs(paste0("+init=epsg:", EPSGcode))
  raster::crs(shape)<-raster::crs(data.crs) # Set CRS 
  pixel.res <- (shape@bbox[,2] - shape@bbox[,1]) / size
  if (any(pixel.res %% 1 != 0)) {
    message("The chosen pixel size does not allow for an integer number of pixels\n\nShapefile resolution:")
    print(shape@bbox)
    message(paste("\nChosen pixel size:", size, "\nNumber of resulting pixels:"))
    print(pixel.res)
    message("")
    stop("The extent of the shapefile divided by the pixel size must result in an integer.\n", call. = FALSE)
  }
  if (!force && any(pixel.res > 2000)) {
    warning("The chosen pixel size creates a transition layer with one or two axes greater 
  than 2000 pixels. This can lead to very long computing times and ultimately the function 
  may fail due to lack of free RAM to allocate the results. If you really want to use this 
  pixel size, rerun the function with force = TRUE.")
  } else {
    ras <- raster::raster(nrow = pixel.res["y"], ncol = pixel.res["x"], crs = raster::crs(shape)) # create a recipient raster
    raster::extent(ras) <- raster::extent(shape) #Make the raster have the same extent as the shapefile
    #### "Burn" the shapefile into the raster
    message("M: Constructing the transition layer. This process may take several minutes depending on the study area size and chosen pixel size."); flush.console()
    shape.mask <- raster::rasterize(shape, ras)
    project.raster <- is.na(shape.mask)
    project.raster[project.raster == 0] <- 10000 # make land extremely hard to cross
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
#' between spatial points. Adapted from Grant Adams' script "distance to closest mpa". (grant.adams@@eagles.usm.edu)
#' 
#' \url{https://cran.r-project.org/web/packages/gdistance/vignettes/gdistance1.pdf}
#'
#' @param t.layer An RData file containing a transition layer.
#' @param starters The points from which to start measuring the distance.
#' @param targets The points to which a way must be found.
#' @param EPSGcode The EPSG code corresponding to the coordinate system of the input data. All inputs must be in the same metric system. DO NOT use degree-based coordinates.
#' @param coord.x,coord.y The names of the columns containing the x and y information. Must be identical in the starters and targets.
#' @param PointIDCol The name of the column containing the IDs of the points to be used as starters and targets. Must be identical in both files.
#' @param actel Logical: Should the distance matrix be optimized for actel and saved in the working directory?
#'
#' @return If actel = TRUE, The distance matrix is stored in the 
#' 
#' @export
#' 
distancesMatrix <- function(t.layer = "transition.layer.RData", starters = NULL, targets = starters, EPSGcode, 
  coord.x = "x", coord.y = "y", PointIDCol = NA, actel = TRUE){
  list.of.packages <- c("raster", "gdistance", "sp", "tools", "rgdal")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
  if (length(new.packages)>0) {
    stop(paste0("This function requires packages '", paste(new.packages,collapse="', '"), 
      "' to operate. Please install them before proceeding.\n"), call. = FALSE)
  }
  data.crs <- raster::crs(paste0("+init=epsg:", EPSGcode))

  if (tools::file_ext(t.layer) == "RData") {
    load(t.layer)
    if (!exists("transition.layer")) stop(paste0("Could not find a transition layer in '", t.layer, "'.\n"), call. = FALSE)
  } else {
    stop(paste0("'", t.layer, "' could not be recognised as .RData file, please make sure the file name is correct.\n"), call. = FALSE)
  }

  if (actel)
    starters <- targets <- "spatial.csv"

  if (tools::file_ext(starters) != "csv" | tools::file_ext(targets) != "csv"){
    stop("One of the point files (starters or targets) does not appear to be writen in csv format.\n", call. = FALSE)
  }
  starters <- read.csv(starters) 
  
  if (actel) {
    message("M: Creating actel-compatible distances matrix."); flush.console()
    PointIDCol <- "Standard.name"
    starters$Standard.name <- as.character(starters$Station.name)
    link <- starters$Type == "Hydrophone"
    starters$Standard.name[link] <- paste0("St.", seq_len(sum(starters$Type == "Hydrophone")))
    targets = starters
  } else {
    targets <- read.csv(targets)
  }
  colnames(starters)[colnames(starters) == coord.x] <- "longitude"
  colnames(starters)[colnames(starters) == coord.y] <- "latitude"
  colnames(targets)[colnames(targets) == coord.x] <- "longitude"
  colnames(targets)[colnames(targets) == coord.y] <- "latitude"
  if (!is.na("PointIDCol")) {
    rename <- TRUE
    if (!is.na(match(PointIDCol, colnames(starters)))) outputRows <- starters[, PointIDCol] else rename <- FALSE
    if (!is.na(match(PointIDCol, colnames(targets )))) outputCols <- targets[, PointIDCol] else rename <- FALSE
  } else {
    rename <- FALSE
  }
  #### Process the "from" coordinates (this would be the starters ".csv" file)
  sp::coordinates(starters) <- ~ longitude + latitude # converts the file to a spatialPoints object
  raster::crs(starters) <- raster::crs(data.crs) # sets the crs to metres
  #### Process the "to" coordinates (this would be the targets ".csv" file)
  sp::coordinates(targets) <- ~ longitude + latitude # converts the file to a spatialPoints object
  raster::crs(targets) <- raster::crs(data.crs)
  #### Calculate a matrix of distances to each object
  dist.mat <- data.frame(gdistance::costDistance(t.layer, starters, targets))
  if (rename) {
    rownames(dist.mat) <- outputRows
    colnames(dist.mat) <- outputCols
  }
  if (actel) {
    message("M: Saving actel-compatible distances matrix as 'distances.csv'."); flush.console()
    write.csv(dist.mat, "distances.csv", row.names = TRUE)
  }
  return(dist.mat)
}

#' Create a Template Distances Matrix
#' 
#' Creates a matrix based on the local 'spatial.csv' file and saves it to 'distances.csv' so the
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
    decision <- readline("A file named 'distances.csv' is already present in the working directory. Do you want to overwrite it?(y/N) ")
  else 
    decision <- "Y"

  if(decision == "Y" | decision == "y")
    write.csv(output, file = "distances.csv", na = "", row.names = TRUE)
  else
    message("Aborting.")
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

  if (any(is.na(input)))
    stop("There are NA's in the upper diagonal of the matrix. Please fill in all values above the diagonal 0 line.\n")

  write.csv(input, file = "distances.csv", row.names = TRUE)
  message("M: Distances matrix successfully completed and stored in 'distances.csv'.")
  print(input)
}
