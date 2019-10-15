#' Update study area for actel v.0.0.4
#' 
#' Converts the spatial.csv in itself + a deployments.csv file.
#' 
#' @inheritParams migration
#' 
#' @export
#' 
updateStudy <- function(tz.study.area) {
  if (file.exists("deployments.csv")) {
    cat("M: A 'deployments.csv' file is already present in the current directory.\n")
  } else {
    spatial <- loadSpatial(file = "spatial.csv")
    detections <- loadDetections(tz.study.area = tz.study.area, force = TRUE)
    stations <- spatial[spatial$Type == "Hydrophone", ]
    deployments <- stations[, c("Receiver", "Station.Name")]
    if (sum(grepl("Receiver", colnames(stations))) >= 1) {
      the.rows <- which(grepl("Receiver", colnames(stations)))[-1]
      for (i in the.rows) {
        recipient <- stations[!is.na(stations[, i]) , c(i, "Station.Name")]
        rbind(deployments, recipient)
      }
    }
    deployments[, "Start"] <- min(detections$Timestamp) - 1
    deployments[,  "Stop"] <- max(detections$Timestamp) + 1
    write.csv(deployments, "deployments.csv", row.names = FALSE)
    write.csv(spatial, "spatial_obsulete.csv", row.names = FALSE)
    write.csv(spatial[, !grepl("Receiver", colnames(spatial))], "spatial.csv", row.names = FALSE)
    cat("M: Study area updated. A copy of the original 'spatial.csv' file was stored as 'spatial_obsulete.csv'.\n")
    deleteHelpers()
  }
}

#' Calculate the standard error of the mean
#' 
#' @param x input data
#' @param na.rm logical: if TRUE, missing values are removed.
#' @param silent logica: if TRUE, The number of NA's removed is not displayed.
#' 
#' @return SDM
#' 
#' @export
#' 
std.error <- function(x, na.rm = TRUE, silent = FALSE){
 a <- length(x)
 if(na.rm) 
  x <- x[!is.na(x)]
 output <- sd(x) / sqrt(length(x))
 if (!silent && a != length(x)) 
  cat("M: Ommited", a - length(x), "missing values.\n")
 return(output)
}

#' Calculate the standard error of the mean
#' 
#' @param x input data
#' @param na.rm logical: if TRUE, missing values are removed.
#' @param silent logica: if TRUE, The number of NA's removed is not displayed.
#' 
#' @return SDM
#' 
#' @export
#' 
std.error.circular <- function(x, na.rm = TRUE, silent = FALSE){
 a <- length(x)
 if(na.rm) 
  x <- x[!is.na(x)]
 output <- circular::sd.circular(x) / sqrt(length(x))
 if (!silent && a != length(x)) 
  cat("M: Ommited", a - length(x), "missing values.\n")
 return(output)
}


#' Calculate the absolute values by which X can be divided
#' 
#' @param x the value to be divided
#' 
#' @return a vector of values that, when dividing x, return integers
#' 
#' @export
#' 
divisors <- function(x){
    y <- seq_len(x)
    return(y[x %% y == 0])
}

#' Convert hh:mm:ss time to hh.hhh
#'
#' Wrapper for timeConverter
#' 
#' @param input Single string or a vector of strings containing hours:minutes or hours:minutes:seconds
#' 
#' @return Decimal hour equivalent (single value or vector)
#' 
#' @export
#' 
decimalTime <- function(input) {
  if (length(input) < 1) 
    stop("Input appears to be empty.")
  if (length(input) == 1) 
    output <- timeConverter(input)
  if (length(input) > 1) 
    output <- unlist(lapply(input, timeConverter))
  return(output)
}

#' Convert hh:mm:ss time to hh.hhh
#'
#' Converts hour:minute:second text strings to decimal hours
#' 
#' @param input a string containing either hours:minutes or hours:minutes:seconds data.
#' 
#' @return Decimal hour equivalent (single value or vector)
#'
#' @keywords internal
#' 
timeConverter <- function(input) {
  x = as.character(input)
  x = as.numeric(unlist(strsplit(x, ":")))
  if (length(x) == 2) 
    x = x[1] + x[2]/60
  if (length(x) == 3) 
    x = x[1] + x[2]/60 + x[3]/3600
  return(x)
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
#' @export
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
#' Aimed to be used in a vector of TRUE/FALSE's, where NA's are present and should be considered as false
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
#' @param input The value to be rounded
#' @param to The level of rounding to be applied (i.e. to=10 will round 14.2 to 20; to=1 will round i to 15)
#' 
#' @return The rounded value
#' 
#' @export
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
#' @param input The value to be rounded
#' @param to The level of rounding to be applied (i.e. to=10 will round 14.8 to 10; to=1 will round i to 14)
#' 
#' @return The rounded value
#' 
#' @export
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
    if (i == "Screen") 
      cat(paste(line, "\n", sep = ""))
    flush.console()
    if (i == "Report") 
      write(line, file = "temp_log.txt", append = file.exists("temp_log.txt"))
    if (i == "Warning") 
      write(line, file = "temp_warnings.txt", append = file.exists("temp_warnings.txt"))
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
#' @keywords internal
#' 
deleteHelpers <- function(emergency = FALSE) {
  helper.list <- paste("temp_", c("log", "warnings", "UD", "comments", "debug"), ".txt", sep = "")
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
  appendTo("Report", "\nAn exception occurred, stopping the process!\n\n-------------------")
  if (file.exists("temp_UD.txt")) 
    appendTo("Report", paste("User inverventions:\n-------------------\n", gsub("\r", "", readr::read_file("temp_UD.txt")), "-------------------", sep = ""))
  # appendTo(c("Screen", "Report"), paste("M: Saving report as '", paste(gsub(":", ".", sub(" ", ".", as.character(Sys.time()))), "actel.log-STOP.txt", sep = "."), "'.", sep = ""))
  file.rename("temp_log.txt", paste(gsub(":", ".", sub(" ", ".", as.character(Sys.time()))), "actel.log-STOP.txt", sep = "."))
  deleteHelpers(TRUE)
}

#' Check path validity
#' 
#' Confirms that the target directory exists.
#' 
#' @inheritParams actel
#' 
#' @keywords internal
#' 
pathCheck <- function(my.home, path) {
  appendTo("debug", "Starting pathCheck.")
  if (!is.null(path)) {
    if (dir.exists(path)) {
      setwd(path)
    } else {
      appendTo(c("Screen", "Report"), paste0("Error: The selected path does not exist. Details:\n  Current directory: ", getwd(), "\n  Path: ", path, "\n\nYou may either:\n  a) continue the analysis in the current directory;\n  b) restart the function."))
      unknown.input = TRUE
      while (unknown.input) {
        decision <- readline("Decision:(a/b) ")
        if (decision == "a" | decision == "A") {
          unknown.input = FALSE
          path <- my.home
        }
        if (decision == "b" | decision == "B") {
          unknown.input = FALSE
          emergencyBreak()
          stop("Analysis stopped by user command.\n")
        }
        if (unknown.input) {
          cat("Option not recognized, please input either 'a' or 'b'.\n")
        }
      }
    }
  } else {
    path <- my.home
  }
  if (my.home != getwd() )
    deleteHelpers()
  moveHelpers(my.home)
  appendTo("debug", "Done.")
  return(path)
}

#' Move helper files to new directory
#' 
#' @param my.home The working directory where the function was triggered.
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

#' Check report compatibility
#' 
#' Creates a "Report" folder if necessary and silently activates ggplot2 and reshape2 to avoid startup messages
#'
#' @return A TRUE/FALSE decision
#' 
#' @keywords internal
#' 
folderCheck <- function(report, redraw){
  if (report) {
    appendTo("Report", "M: 'report' option has been activated.")
    if (length(setdiff(c("ggplot2", "reshape2"), rownames(installed.packages()))) > 0) {
      appendTo(c("Screen", "Report", "Warning"), "W: 'report' option can only be activated if 'ggplot2' and 'reshape2' are installed. Please install these. Deactivating 'Report' for the current job.")
      report <- FALSE
    } else {
      # suppressPackageStartupMessages(library(ggplot2))
      # suppressPackageStartupMessages(library(reshape2))
      if (!dir.exists("Report")) {
        appendTo("Screen", "M: Creating 'Report' subdirectory to store report files.")
        dir.create("Report")
      } else {
        if (redraw) {
        appendTo("Screen", "W: 'Report' directory already present. Overwriting files already present.")
        } else {
        appendTo("Screen", "W: 'Report' directory already present. Skipping files already present.")
        }
      }
    }
  }
  return(report)
}

#' Write in comments
#' 
#' Checks if the user has invoked the comment command for a specific fish, and stores the comment.
#' 
#' @param line The text of the interaction in which the user may or may not request a comment
#' @param tag The tag number currently being analysed
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
        appendTo(c("UD", "Comment"), readline(paste("New comment on fish ", tag, ":", sep = "")), tag)
      }
      appendTo("Screen", "M: Comment successfully stored, returning to the previous interaction.")
    } else {
      comment.check = FALSE
    }
  }
  return(decision)
}

#' Delete previous analysis files from the current workspace. Input files are not deleted.
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
    cat("Proceeding will eliminate the following files/folders:\n")
    print(files)
    decision <- readline("Proceed?(y/N) ")
    if(decision == "y" | decision == "Y"){
      unlink(files, recursive = TRUE)
    } else
      cat("Aborted.\n")
  } else {
    cat("Workspace already clean.\n")
  }
}

#' Check internet connection
#'
#' For an unknown reason, R will crash if it attempts to open the html report in a pc with no internet connection.
#' The report opens, but the progress freezes and R eventually crashes. Can be removed once figured out why this
#' is happening.
#' 
#' @keywords internal
#' 
havingIP <- function() {
  if (.Platform$OS.type == "windows") {
    ipmessage <- system("ipconfig", intern = TRUE)
  } else {
    ipmessage <- system("ifconfig", intern = TRUE)
  }
  validIP <- "((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)[.]){3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)"
  any(grep(validIP, ipmessage))
}

#' Compare installed version with GitHub version
#' 
#' @keywords internal
#' 
versionCheck <- function() {
  rep.ver <- unlist(strsplit(readLines('https://raw.githubusercontent.com/hugomflavio/actel/master/DESCRIPTION')[3], " "))[2]
  rep.ver.short <- substr(rep.ver, start = 1, stop = nchar(rep.ver) - 5)
  rep.ver.num <- as.numeric(gsub(".", "", rep.ver.short))
  inst.ver <- utils::packageVersion("actel")
  inst.ver.short <- substr(inst.ver, start = 1, stop = nchar(as.character(inst.ver)) - 5) 
  inst.ver.num <- as.numeric(gsub(".", "", inst.ver.short))
  if (rep.ver.short > inst.ver.short)
    cat(paste0("-------------------------------------------------------------\n!!! A NEW VERSION of actel is available! (v.", inst.ver.short, " -> v.", rep.ver.short, ")\n!!! You should update actel before continuing.\n!!! To learn how to update actel, run updateActel()\n-------------------------------------------------------------\n"))
}

#' Open actel installation instructions
#' 
#' @export
#' 
updateActel <- function() {
  if (havingIP()) {
    cat("M: Opening actel's installation instructions.\n")
    browseURL("https://github.com/hugomflavio/actel#installing-actel")
  } else {
    cat("M: Could not detect an internet connection. Find installation instructions in this page:\n   https://github.com/hugomflavio/actel#installing-actel\n")
  }
}