#' Load spatial.dot
#' 
#' @param input The name of the dot file
#' @param spatial A spatial data frame
#' @param string A dot string
#' @inheritParams migration
#' 
#' @return The dot list
#' 
#' @keywords internal
#' 
loadDot <- function(string = NULL, input = NULL, spatial, sections = NULL) {
  if (is.null(string) & is.null(input))
    stop("No dot file or dot string were specified.")
  if (is.null(string)) {
    tryCatch(dot <- readDot(input = input), 
      error = function(e) {
        emergencyBreak()
        stop("The contents of the file '", input, "' could not be recognised by the readDot function.\n", call. = FALSE)
        })
  } else {
    dot <- readDot(string = string)
  }
  mat <- dotMatrix(input = dot)
  if (any(is.na(match(unique(spatial$Array), colnames(mat))))) {
    emergencyBreak()
    stop("Not all the arrays listed in the 'spatial.csv' file are present in the 'spatial.dot' file.\n", call. = FALSE)
  }
  if (any(is.na(match(unique(spatial$Array), colnames(mat))))) {
    emergencyBreak()
    stop("Not all the arrays listed in the 'spatial.dot' file are present in the 'spatial.csv' file.\n", call. = FALSE)
  }
  arrays <- dotList(input = dot, sections = sections)
  arrays <- dotPaths(input = arrays, dotmat = mat)
  return(list(dot = dot, arrays = arrays, dotmat = mat))
}

#' Read dot file
#' 
#' @inheritParams loadDot
#' 
#' @return A table with A to B rows
#' 
#' @export
#' 
readDot <- function (input = NULL, string = NULL) {
  if (is.null(string) & is.null(input))
    stop("No dot file or data were specified.")
  if (is.null(string))
    lines <- readLines(input)
  else
    lines <- string
  paths <- lines[grepl("-[->]", lines)]
  paths <- gsub("[ ;]", "", paths)
  paths <- gsub("\\[label=[^\\]]","", paths)
  nodes <- strsplit(paths,"-[->]")
  recipient <- data.frame(
    A = character(), 
    to = character(),
    B = character(), 
    stringsAsFactors = FALSE)
  for (i in 1:length(nodes)) {
    n <- length(nodes[[i]])
    type <- gsub(paste0(nodes[[i]], collapse = "|"), "", paths[[i]])
    aux <- data.frame(
      A = nodes[[i]][1:(n - 1)], 
      to = sapply(seq(from = 1, to = nchar(type), by = 2), function(i) substr(type, i, i + 1)),
      B = nodes[[i]][2:n], 
      stringsAsFactors = FALSE)
    recipient <- rbind(recipient, aux)
  }
  return(recipient)
}

#' Load distances matrix
#' 
#' @param spatial A list of spatial objects in the study area
#' 
#' @keywords internal 
#' 
#' @return a list containing the distances matrix and a TRUE/FALSE value indicating whether or not that distances matrix is valid for the target study area.
#' 
loadDistances <- function(spatial) {
  # Check for distances
  invalid.dist <- TRUE
  appendTo("Debug", "Creating 'dist.mat' if distances file is present.")
  if (file.exists("distances.csv")) {
    appendTo(c("Screen", "Report"), "M: A distances matrix file is present, activating speed calculations.")
    dist.mat <- read.csv("distances.csv", row.names = 1)
    invalid.dist <- FALSE
    if (nrow(dist.mat) != ncol(dist.mat)){
      appendTo(c("Screen", "Report", "Warning"), "Error: The distance matrix appears to be missing data (ncol != nrow). Deactivating speed calculation to avoid function failure.")
      invalid.dist <- TRUE
    }
    if (!invalid.dist && sum(nrow(spatial$stations), nrow(spatial$release.sites)) != nrow(dist.mat)) {
      appendTo(c("Screen", "Report", "Warning"), "Error: The number of spatial points does not match the number of rows in the distance matrix. Deactivating speed calculation to avoid function failure.")
      invalid.dist <- TRUE
    }
    if (!invalid.dist && (any(!matchl(spatial$stations$Standard.Name, colnames(dist.mat))) | any(!matchl(spatial$release.sites$Standard.Name, colnames(dist.mat))))) {
      appendTo(c("Screen", "Report", "Warning"), "Error: Some stations and/or release sites are not present in the distance matrix. Deactivating speed calculation to avoid function failure.")
      invalid.dist <- TRUE
    }    
    if (!invalid.dist && any(rownames(dist.mat) != colnames(dist.mat))) {
      appendTo(c("Screen", "Report", "Warning"), "Error: Some stations and/or release sites are not present in the distance matrix. Deactivating speed calculation to avoid function failure.")
      invalid.dist <- TRUE
    }    
  } else {
    dist.mat <- NA
  }
  return(list(dist.mat = dist.mat, invalid.dist = invalid.dist))  
}


#' Load deployments file and Check the structure
#' 
#' @inheritParams assembleSpatial
#' 
#' @return The deployments dataframe
#' 
#' @keywords internal
#' 
loadDeployments <- function(file, tz.study.area){
  appendTo("debug","Starting loadDeployments.")
  if (file.exists(file))
    input <- read.csv(file)
  else {
    emergencyBreak()
    stop("Could not find a '", file, "' file in the working directory.\n", call. = FALSE)
  }
  default.cols <- c("Receiver", "Station.Name", "Start", "Stop")
  link <- match(default.cols, colnames(input))
  if (any(is.na(link))) {
    appendTo("Report", paste0("Error: Column(s) '", paste(default.cols[is.na(link)], collapse = "', '"), "'are missing in the deployments file."))
    emergencyBreak()
    stop(paste0("Column(s) '", paste(default.cols[is.na(link)], collapse = "', '"), "'are missing in the deployments file.\n"), call. = FALSE)
  }
  if (any(!grepl("^[1-2][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9] [0-2][0-9]:[0-5][0-9]", input$Start))) {
    emergencyBreak()
    stop("Not all values in the 'Start' column appear to be in a 'yyyy-mm-dd hh:mm' format (seconds are optional). Please doublecheck the deployments file.\n", call. = FALSE)
  }
  if (any(!grepl("^[1-2][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9] [0-2][0-9]:[0-5][0-9]", input$Stop))) {
    emergencyBreak()
    stop("Not all values in the 'Stop' column appear to be in a 'yyyy-mm-dd hh:mm' format (seconds are optional). Please doublecheck the deployments file.\n", call. = FALSE)
  }
  if (inherits(try(as.POSIXct(input$Start), silent = TRUE), "try-error")){
    emergencyBreak()
    stop("Could not recognise the data in the 'Release.date' column as POSIX-compatible timestamps. Please doublecheck the biometrics file.\n", call. = FALSE)
  }
  if (inherits(try(as.POSIXct(input$Stop), silent = TRUE),"try-error")){
    emergencyBreak()
    stop("Could not recognise the data in the 'Release.date' column as POSIX-compatible timestamps. Please doublecheck the biometrics file.\n", call. = FALSE)
  }
  input$Receiver <- as.character(input$Receiver)
  input$Receiver <- sapply(input$Receiver, function(x) tail(unlist(strsplit(x, "-")), 1))
  input$Start <- as.POSIXct(input$Start, tz = tz.study.area)
  input$Stop <- as.POSIXct(input$Stop, tz = tz.study.area)
  appendTo("debug","Terminating loadDeployments.")
  return(input)
}

#' Load Spatial file and Check the structure
#' 
#' @inheritParams assembleSpatial
#' @param verbose Logical: If TRUE, the appendTo function is enabled.
#' 
#' @return The spatial dataframe
#' 
#' @export
#' 
loadSpatial <- function(file = "spatial.csv", verbose = FALSE){
  if (verbose)
    appendTo("debug","Starting loadSpatial.")
  if (file.exists(file))
    input <- read.csv(file)
  else {
    emergencyBreak()
    stop("Could not find a '", file, "' file in the working directory.\n", call. = FALSE)
  }
  if (!any(grepl("Station.Name", colnames(input)))) {
    emergencyBreak()
    stop("The spatial file must contain a 'Station.Name' column.\n", call. = FALSE)
  } else {
    if (any(link <- table(input$Station.Name) > 1)) {
      if (verbose)
        appendTo(c("Screen", "Warning", "Report"), "Error: The 'Station.Name' column in the spatial file must not have duplicated values.")
      else
        cat("Error: The 'Station.Name' column in the spatial file must not have duplicated values.")
      cat("Stations appearing more than once:", paste(names(table(input$Station.Name))[link], collapse = ", "), "\n")
      if (verbose)
        emergencyBreak()
      stop("Fatal exception found. Read lines above for more details.\n", call. = FALSE)
    }
  }
  if (!any(grepl("Array", colnames(input)))) {
    if (any(grepl("Group", colnames(input)))) {
      decision <- readline("Error: No 'Array' column found in the spatial file, but a 'Group' column is present. Use the 'Group' column to define the arrays? (Y/n) ")
      appendTo("UD", decision)
      if (decision == "N" & decision == "n") {
        if (verbose)
          emergencyBreak()
        stop("The spatial file must contain an 'Array' column.\n", call. = FALSE)
      } else {
        if (verbose)
          appendTo("Report", "WARNING: No 'Array' column found in the spatial file, but a 'Group' column is present. Using the 'Group' column to define the arrays.")  
        colnames(input)[grepl("Group", colnames(input))] <- "Array"
      }
    }
  }
  if (any(grepl(" ", input$Array))) {
    appendTo("Screen", "W: Replacing spaces in array names to prevent function failure.")
    input$Array <- gsub(" ", "_", input$Array)
  }
  if (!any(grepl("Type", colnames(input)))) {
    if (verbose)
      appendTo("Screen", "M: No 'Type' column found in 'spatial.csv'. Assigning all rows as hydrophones.")
    input$Type <- "Hydrophone"
  } else {
    if (any(is.na(match(unique(input$Type), c("Hydrophone","Release"))))){
      if (verbose)
        emergencyBreak()
      stop("Could not recognise the data in the 'Type' column as only one of 'Hydrophone' or 'Release'. Please doublecheck the spatial file.\n", call. = FALSE)
    }
  }
  input <- setSpatialStandards(input = input) # Create Standard.Name for each station  
  if (verbose)
    appendTo("debug","Terminating loadSpatial.")
  return(input)
}



#' Import biometrics
#' 
#' @param file an input file with biometric data.
#' @inheritParams explore
#' 
#' @keywords internal
#' 
#' @return The biometrics table
#' 
#' @export
#' 
loadBio <- function(file, tz.study.area){
  appendTo("debug", "Starting loadBio.")
  if (file.exists(file))
    bio <- read.csv(file, stringsAsFactors = FALSE)
  else {
    emergencyBreak()
    stop("Could not find a '", file, "' file in the working directory.\n", call. = FALSE)
  }  

  if (!any(grepl("Release.date", colnames(bio)))) {
    emergencyBreak()
    stop("The biometrics file must contain an 'Release.date' column.\n", call. = FALSE)
  }

  if (any(!grepl("^[1-2][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9] [0-2][0-9]:[0-5][0-9]", bio$Release.date))) {
    emergencyBreak()
    stop("Not all values in the 'Release.date' column appear to be in a 'yyyy-mm-dd hh:mm' format (seconds are optional). Please doublecheck the biometrics file.\n", call. = FALSE)
  }
  
  if (inherits(try(bio$Release.date <- as.POSIXct(bio$Release.date, tz = tz.study.area), silent = TRUE),"try-error")){
    emergencyBreak()
    stop("Could not recognise the data in the 'Release.date' column as POSIX-compatible timestamps. Please doublecheck the biometrics file.\n", call. = FALSE)
  }

  if (!any(grepl("Signal", colnames(bio)))){
    emergencyBreak()
    stop("The biometrics file must contain an 'Signal' column.\n", call. = FALSE)
  }

  if (!inherits(bio$Signal,"integer")) {
    emergencyBreak()
    stop("Could not recognise the data in the 'Signal' column as integers. Please doublecheck the biometrics file.\n", call. = FALSE)
  }

  if (any(is.na(bio$Signal))) {
    emergencyBreak()
    stop("Some fish have no 'Signal' information. Please doublecheck the biometrics file.\n", call. = FALSE)
  }

  if (any(link <- table(bio$Signal) > 1)) {
    emergencyBreak()
    stop(ifelse(sum(link) > 1, "Signals ", "Signal "), paste(names(table(bio$Signal))[link], collapse = ", "), ifelse(sum(link) > 1," are ", " is "), "duplicated in the biometrics file.\n", call. = FALSE)
  }

  if (!any(grepl("Release.site", colnames(bio)))) {
    appendTo("Screen", "M: No Release site has been indicated in the biometrics file. Creating a 'Release.site' column to avoid function failure. Filling with 'unspecified'.")
    bio$Release.site <- "unspecified"
  } else {
    bio$Release.site <- factor(bio$Release.site)
    if (any(is.na(bio$Release.site))) {
      appendTo(c("Screen","Report","Warning"),"W: Some fish contain no release site information. You may want to doublecheck the data.\n   Filling the blanks with 'unspecified'.")
      levels(bio$Release.site) <- c(levels(bio$Release.site), "unspecified")
      bio$Release.site[is.na(bio$Release.site)] <- "unspecified"
    }
  }
  if (!any(grepl("Group", colnames(bio)))) {
    appendTo("Screen", "M: No 'Group' column found in the biometrics file. Assigning all fish to group 'All'.")
    bio$Group <- "All"
  } else {
    if (any(is.na(bio$Group))) {
      appendTo(c("Screen","Report","Warning"),"W: Some fish contain no group information. You may want to doublecheck the data.\n   Filling the blanks with 'unspecified'.")
      levels(bio$Group) <- c(levels(bio$Group), "unspecified")
      bio$Group[is.na(bio$Group)] <- "unspecified"
    }
  }
  bio <- bio[order(bio$Signal),]
  appendTo("debug", "Terminating loadbio.")
  return(bio)
}

#' Load ALS detections
#'
#' If there are previously compiled detections present, offers the chance to reuse. Otherwise triggers combineDetections.
#' 
#' @inheritParams actel
#' 
#' @import data.table
#' 
#' @return A dataframe with all the detections
#' 
#' @keywords internal
#' 
loadDetections <- function(start.timestamp = NULL, end.timestamp = NULL, tz.study.area, force = FALSE) {
  # NOTE: The variable actel.detections is loaded from a RData file, if present. To avoid package check
  #   notes, the variable name is created before any use.
  actel.detections <- NULL
    
  recompile <- TRUE
  detection.paths <- c(file.exists("actel.detections.RData"), file.exists("detections/actel.detections.RData"))
  
  if (any(detection.paths)) {
    if (all(detection.paths)) 
      appendTo(c("Screen", "Warning", "Report"), "W: Previously compiled detections were found both in the current directory and in a 'detections' folder.\n   Loading ONLY the compiled detections present in the 'detections' folder.")
    if(detection.paths[2]) 
      load("detections/actel.detections.RData")
    else
      load("actel.detections.RData")
    if (force) {
      decision <- "Y"
    } else {
      appendTo("Screen", paste("M: The detections have been processed on ", actel.detections$timestamp, ".\n   If the input detection files were not changed, it is safe to use these again.", sep = ""))
      decision <- readline("   Reuse processed detections?(Y/n) ")
    }
    appendTo("UD", decision)
    if (decision != "N" & decision != "n"){
      appendTo(c("Screen","Report"), paste("M: Using detections previously compiled on ", actel.detections$timestamp, ".", sep = ""))
      detections <- actel.detections$detections
      attributes(detections$Timestamp)$tzone <- "UTC"
      detections <- convertTimes(input = detections, start.timestamp = start.timestamp, end.timestamp = end.timestamp, tz.study.area = tz.study.area)
      appendTo(c("Screen","Report"), paste("M: Data time range: ", as.character(head(detections$Timestamp, 1)), " to ", as.character(tail(detections$Timestamp, 1)), " (", tz.study.area, ").", sep = ""))
      recompile <- FALSE
    } else {
      appendTo("Screen", "M: Reprocessing the detections.")
    }
    rm(actel.detections)
  }

  if (recompile)
    detections <- compileDetections(path = "detections", start.timestamp = start.timestamp, 
      end.timestamp = end.timestamp, tz.study.area = tz.study.area)
  return(detections)
}

# LOAD DETECTION HELPERS:

#' Combine ALS detections
#'
#' Finds the detections' files and processes them.
#' 
#' @inheritParams actel
#' @param path the path(s) to the detection files
#' 
#' @import data.table
#' 
#' @return A dataframe with all the detections
#' 
#' @keywords internal
#' 
compileDetections <- function(path = "detections", start.timestamp = NULL, end.timestamp = NULL, tz.study.area) {
  appendTo("Screen", "M: Compiling detections...")
  # Find the detection files
  if (file_test("-d", path)) {
    file.list <- findFiles(path = path, pattern = "*.csv")
  } else {
    if (file.exists("detections.csv"))
      file.list <- "detections.csv"
    else
      stop("Could not find a 'detections' folder nor a 'detections.csv' file.\n", call. = FALSE)
  }
  if (file_test("-d", path) & file.exists("detections.csv"))
    appendTo(c("Screen", "Warning", "Report"), "W: Both a 'detections' folder and a 'detections.csv' file are present in the current directory.\n   Loading ONLY the files present in the 'detections' folder.")
  number.of.files <- length(file.list)
  # Prepare the detection files
  data.files <- list()
  for (i in file.list) {
    appendTo("debug", paste("Importing file '", i, "'.", sep = ""))
    data.files[[length(data.files) + 1]] <- data.table::fread(i, fill = TRUE, showProgress = FALSE)
    names(data.files)[length(data.files)] <- i
    if(nrow(data.files[[i]]) > 0){
      unknown.file <- TRUE
      if (unknown.file && any(grepl("CodeType", colnames(data.files[[i]])))) {
        appendTo("debug", paste("File '", i, "' matches a Thelma log.", sep = ""))
        data.files[[i]] <- processThelmaFile(input = data.files[[i]])
        unknown.file <- FALSE
      }
      if (unknown.file && any(grepl("Receiver", colnames(data.files[[i]])))) {
        appendTo("debug", paste("File '", i, "' matches a Vemco log.", sep = ""))
        data.files[[i]] <- processVemcoFile(input = data.files[[i]])
        unknown.file <- FALSE
      }
      if (unknown.file) {
        appendTo(c("Screen", "Report", "Warning"), paste("W: File '", i, "' does not match to any of the supported hydrophone file formats!\n   If your file corresponds to a hydrophone log and actel did not recognize it, please contact the developer team through at www.github.com/hugomflavio/actel/issues/new", 
          sep = ""))
        file.list <- file.list[-grep(i, file.list)]
      }
      rm(unknown.file)
    } else {
      appendTo("debug", paste("File '", i, "' is empty, skipping processing.", sep = ""))
    }
  }
  rm(i)
  if (length(file.list) != number.of.files) {
    if (abs(length(file.list) - number.of.files) > 1) {
      appendTo("Screen", paste("W: ", number.of.files - length(file.list), "files were excluded from further analyses.", sep = ""))
    } else {
      appendTo("Screen", paste("W: One file was excluded from further analyses.", sep = ""))
    }
  }
  # Bind the detection files
  output <- bindDetections(data.files = data.files, file.list = file.list)
  # Convert codespaces
  output <- convertCodes(input = output)
  # Convert time-zones
  output <- convertTimes(input = output, start.timestamp = start.timestamp, 
    end.timestamp = end.timestamp, tz.study.area = tz.study.area)

  actel.detections <- list(detections = output, timestamp = Sys.time())
    save(actel.detections, file = ifelse(file_test("-d", path), paste0(path, "/actel.detections.RData"), "actel.detections.RData"))
  
  appendTo(c("Screen", "Report"), paste("M: Data time range: ", as.character(head(output$Timestamp, 1)), " to ", as.character(tail(output$Timestamp, 1)), " (", tz.study.area, ").", sep = ""))
  appendTo("debug", "Terminating loadDetections.")
  return(output)
}

#' Find file names
#'
#' @inheritParams loadDetections
#' 
#' @return A vector of the file names.
#'
#' @keywords internal
#' 
findFiles <- function(path = NULL, pattern = NULL) {
  appendTo("debug", "Starting findFiles.")
  if (is.null(path)) {
      file.list <- list.files(pattern = pattern)
  } else {
    file.list <- NULL
    for (folder in path) {
      if (file_test("-d", folder)) {
        file.list <- c(file.list, paste0(folder, "/", list.files(folder, pattern = pattern)))
      } else {
        stop("Could not find a '", folder, "' directory in the current working directory.\n", call. = FALSE)
      }
    }
  }
  appendTo("debug", "Terminating findFiles.")
  return(file.list)
}

#' Thelma files
#' 
#' Processes Thelma ALS files.
#' 
#' @param input the file name, supplied by findFiles.
#'
#' @return A data frame of standardized detections from the input file.
#'
#' @keywords internal
#' 
processThelmaFile <- function(input) {
  appendTo("debug", "Starting processThelmaFile.")
  input <- as.data.frame(input)
  output <- data.table(
    Timestamp = fasttime::fastPOSIXct(sapply(input[, 1], function(x) gsub("Z", "", gsub("T", " ", x))), tz = "UTC"),
    Receiver = input[, 8],
    CodeSpace = input[, 3],
    Signal = input[, 4])
  # output <- input[, c(1, 8, 3, 4)]
  # colnames(output) <- c("Timestamp", "Receiver", "CodeSpace", "Signal")
  # output[, "Timestamp"] <- fasttime::fastPOSIXct(output[, "Timestamp"], tz = "UTC")
  appendTo("debug", "Terminating processThelmaFile.")
  return(output)
}

#' Vemco files
#' 
#' Processes Vemco ALS files
#' 
#' @inheritParams processThelmaFile
#'
#' @return A data frame of standardized detections from the input file.
#'
#' @keywords internal
#' 
processVemcoFile <- function(input) {
  appendTo("Debug", "Starting processVemcoFile.")
  # input[, "Transmitter"] <- as.character(input[, "Transmitter"])
  # input[, "Receiver"] <- as.character(input[, "Receiver"])
  appendTo("Debug", "Processing data inside the file...")
  transmitter_aux <- strsplit(input$Transmitter, "-", fixed = TRUE)
  receiver_aux <- strsplit(input$Receiver, "-", fixed = TRUE) # split model and serial
  input[, "CodeSpace"] <- unlist(lapply(transmitter_aux, function(x) paste(x[1:2], collapse = "-"))) # Rejoin code space
  input[, "Signal"] <- unlist(lapply(transmitter_aux, function(x) x[3])) # extract only signal
  input[, "Receiver"] <- unlist(lapply(receiver_aux, function(x) x[2])) # extract only the serial
  appendTo("Debug", "Done!")
  colnames(input)[1] <- c("Timestamp")
  input <- input[, c("Timestamp", "Receiver", "CodeSpace", "Signal")]
  input$Timestamp <- fasttime::fastPOSIXct(input$Timestamp, tz = "UTC")
  appendTo("debug", "Terminating processVemcoFile.")
  return(input)
}

#' Bind detections
#' 
#' Combines all the standardized detections into one data frame.
#' 
#' @param data.files The list of processed files, supplied by loadDetections.
#' @param file.list The list of valid files, supplied by loadDetections.
#'
#' @return A data frame of standardized detections from all the input files.
#'
#' @keywords internal
#' 
bindDetections <- function(data.files, file.list) {
  appendTo("debug", "Starting bindDetections.")
  if (length(file.list) >= 1) {
    temp <- data.files[file.list]
    appendTo("debug", "Compiling the data object.")
    output <- temp[[1]]
    if (length(temp) > 1) 
      for (i in 2:length(temp)) output <- rbind(output, temp[[i]])
  } else {
    stop("No valid detection files were found.\n", call. = FALSE)
  }
  output$Receiver <- as.factor(output$Receiver)
  output$CodeSpace <- as.factor(output$CodeSpace)
  appendTo("debug", "Terminating bindDetections.")
  return(output)
}

#' Convert code spaces
#' 
#' Unifies CodeSpace names, to avoid having different names depending on ALS vendor.
#' 
#' @param input A data frame of standardized detections.
#'
#' @return A data frame with standardized code spaces.
#'
#' @keywords internal
#' 
convertCodes <- function(input) {
  appendTo("debug", "Starting convertCodes.")
  vemco <- c("A69-1008", 
             "A69-1206", 
             "A69-1105", 
             "A69-1303")
             # "A69-1601", 
             # "A69-1602", 
             # "A69-9001", 
             # "A69-9002", 
             # "A69-9004", 
             # "A69-9005", 
             # "A69-9006")
  replace <- c("R256", 
               "R94K", 
               "S256", 
               "R64K") 
               # "A69-1601", 
               # "A69-1602", 
               # "A69-9001", 
               # "A69-9002", 
               # "A69-9004", 
               # "A69-9005", 
               # "A69-9006")
  for (i in seq_len(length(replace))) {
    levels(input$CodeSpace)[levels(input$CodeSpace) == vemco[i]] <- replace[i]
  }
  appendTo("debug", "Terminating convertCodes.")
  return(input)
}

#' Convert Times
#' 
#' Converts the ALS timestamps (UTC) to the designated study area time zone. Can also trim the data by time.
#' 
#' @inheritParams convertCodes
#' @inheritParams actel
#'
#' @return A data frame with corrected timestamps.
#'
#' @keywords internal
#' 
convertTimes <- function(input, start.timestamp, end.timestamp, tz.study.area) {
  # NOTE: The NULL variables below are actually column names used by data.table.
  # This definition is just to prevent the package check from issuing a note due unknown variables.
  Timestamp <- NULL

  appendTo("debug", "Starting convertTimes.")
  attributes(input$Timestamp)$tzone <- tz.study.area
  input <- input[order(input$Timestamp), ]
  if (!is.null(start.timestamp)){
    onr <- nrow(input)
    input <- input[Timestamp >= as.POSIXct(start.timestamp, tz = tz.study.area)]
    appendTo(c("Screen","Report"), paste("M: Discarding detection data previous to ",start.timestamp," per user command (", onr - nrow(input), " detections discarded).", sep = ""))
  }
  if (!is.null(end.timestamp)){
    onr <- nrow(input)
    input <- input[Timestamp <= as.POSIXct(end.timestamp, tz = tz.study.area), ]
    appendTo(c("Screen","Report"), paste("M: Discarding detection data posterior to ",end.timestamp," per user command (", onr - nrow(input), " detections discarded).", sep = ""))
  }
  input$Receiver <- droplevels(input$Receiver)
  input$Transmitter <- as.factor(paste(input$CodeSpace, input$Signal, sep = "-"))
  appendTo("debug", "Terminating convertTimes.")
  return(input)
}
