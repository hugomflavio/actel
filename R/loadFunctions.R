#' Load Spatial file and Check the structure
#' 
#' @inheritParams assembleSpatial
#' 
#' @return The spatial dataframe
#' 
#' @keywords internal
#' 
loadSpatial <- function(file){
  appendTo("debug","Starting loadSpatial.")
  if (file.exists(file))
    input <- read.csv(file)
  else {
    emergencyBreak()
    stop("Could not find a '", file, "' file in the working directory.\n")
  }
  if (!any(grepl("Array", colnames(input)))) {
    if (any(grepl("Group", colnames(input)))) {
      decision <- readline("Error: No 'Array' column found in the spatial file, but a 'Group' column is present. Use the 'Group' column to define the arrays? (Y/n) ")
      appendTo("UD", decision)
      if (decision == "N" & decision == "n") {
        emergencyBreak()
        stop("The spatial file must contain an 'Array' column.\n")
      } else {
        appendTo("Report","Error: No 'Array' column found in the spatial file, but a 'Group' column is present. Using the 'Group' column to define the arrays.")  
        colnames(input)[grepl("Group", colnames(input))] <- "Array"
      }
    }
  }
  if (!any(grepl("Receiver", colnames(input)))) {
    appendTo("Report", "Error: No 'Receiver' columns found in the spatial file.")
    emergencyBreak()
    stop("The spatial file must contain at least one 'Receiver' column.\n")
  }
  if (!any(grepl("Type", colnames(input)))) {
    appendTo("Screen", "M: No 'Type' column found in 'spatial.csv'. Assigning all rows as hydrophones.")
    input$Type <- "Hydrophone"
  } else {
    if (any(is.na(match(unique(input$Type), c("Hydrophone","Release"))))){
      emergencyBreak()
      stop("Could not recognise the data in the 'Type' column as only one of 'Hydrophone' or 'Release'. Please doublecheck the spatial file.\n")
    }
  }
  appendTo("debug","Terminating loadSpatial.")
  return(input)
}


#' Import biometrics
#' 
#' @param file an input file with biometric data.
#' 
#' @keywords internal
#' 
#' @return The biometrics table
#' 
loadBio <- function(file){
  appendTo("debug", "Starting loadBio.")
  if (file.exists(file))
    bio <- read.csv(file, stringsAsFactors = FALSE)
  else {
    emergencyBreak()
    stop("Could not find a '", file, "' file in the working directory.\n")
  }  

  if (!any(grepl("Release.date", colnames(bio)))) {
    emergencyBreak()
    stop("The biometrics file must contain an 'Release.date' column.\n")
  }

  if (any(!grepl("^[1-2][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9] [0-2][0-9]:[0-5][0-9]", bio$Release.date))) {
    emergencyBreak()
    stop("Not all values in the 'Release.date' column appear to be in a 'yyyy-mm-dd hh:mm' format (seconds are optional). Please doublecheck the biometrics file.\n")
  }
  
  if (inherits(try(as.POSIXct(bio$Release.date), silent = TRUE),"try-error")){
    emergencyBreak()
    stop("Could not recognise the data in the 'Release.date' column as POSIX-compatible timestamps. Please doublecheck the biometrics file.\n")
  }

  if (!any(grepl("Signal", colnames(bio)))){
    emergencyBreak()
    stop("The biometrics file must contain an 'Signal' column.\n")
  }

  if (!inherits(bio$Signal,"integer")) {
    emergencyBreak()
    stop("Could not recognise the data in the 'Signal' column as integers. Please doublecheck the biometrics file.\n")
  }

  if (any(is.na(bio$Signal))) {
    emergencyBreak()
    stop("Some fish have no 'Signal' information. Please doublecheck the biometrics file.\n")
  }

  if (any(link <- table(bio$Signal) > 1)) {
    emergencyBreak()
    stop(ifelse(sum(link) > 1, "Signals ", "Signal "), paste(names(table(bio$Signal))[link], collapse = ", "), ifelse(sum(link) > 1," are ", " is "), "duplicated in the biometrics file.\n")
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
loadDetections <- function(start.timestamp = NULL, end.timestamp = NULL, tz.study.area) {
  recompile <- TRUE
  detection.paths <- c(file.exists("actel.detections.RData"), file.exists("detections/actel.detections.RData"))
  
  if (any(detection.paths)) {

    if (all(detection.paths)) 
      appendTo(c("Screen", "Warning", "Report"), "W: Previously compiled detections were found both in the current directory and in a 'detections' folder.\n   Loading ONLY the compiled detections present in the 'detections' folder.")
    
    if(detection.paths[2]) 
      load("detections/actel.detections.RData")
    else
      load("actel.detections.RData")
    
    appendTo("Screen", paste("M: The detections have been processed on ", actel.detections$timestamp, ".\n   If the input detection files were not changed, it is safe to use these again.", sep = ""))
    decision <- readline("   Reuse processed detections?(Y/n) ")
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
  appendTo("debug", "Starting loadDetections.")
  # Find the detection files
  if (file_test("-d", path)) {
    file.list <- findFiles(path = path, pattern = "*.csv")
  } else {
    if (file.exists("detections.csv"))
      file.list <- "detections.csv"
    else
      stop("Could not find a 'detections' folder nor a 'detections.csv' file.\n")
  }
  if (file_test("-d", path) & file.exists("detections.csv"))
    actel:::appendTo(c("Screen", "Warning", "Report"), "W: Both a 'detections' folder and a 'detections.csv' file are present in the current directory.\n   Loading ONLY the files present in the 'detections' folder.")
  number.of.files <- length(file.list)
  # Prepare the detection files
  data.files <- list()
  for (i in file.list) {
    appendTo("debug", paste("Importing file '", i, "'.", sep = ""))
    data.files[[length(data.files) + 1]] <- data.table::fread(i, fill = TRUE)
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
        appendTo(c("Screen", "Report", "Warning"), paste("W: File '", i, "' does not match to any of the supported hydrophone file formats!\n   If your file corresponds to a hydrophone log and actel did not recognize it, please contact the developer team: hdmfla@aqua.dtu.dk", 
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
        stop("Could not find a '", folder, "' directory in the current working directory.\n")
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
#' @return A dataframe of standardized detections from the input file.
#'
#' @keywords internal
#' 
processThelmaFile <- function(input) {
  appendTo("debug", "Starting processThelmaFile.")
  output <- input[, c(1, 8, 3, 4)]
  colnames(output) <- c("Timestamp", "Receiver", "CodeSpace", "Signal")
  output[, "Timestamp"] <- fasttime::fastPOSIXct(output[, "Timestamp"], tz = "UTC")
  appendTo("debug", "Terminating processThelmaFile.")
  return(output)
}

#' Vemco files
#' 
#' Processes Vemco ALS files
#' 
#' @inheritParams processThelmaFile
#'
#' @return A dataframe of standardized detections from the input file.
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
#' @return A dataframe of standardized detections from all the input files.
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
    stop("No valid detection files were found.\n")
  }
  output$Receiver <- as.factor(output$Receiver)
  output$CodeSpace <- as.factor(output$CodeSpace)
  appendTo("debug", "Terminating bindDetections.")
  return(output)
}

#' Convert codespaces
#' 
#' Unifies CodeSpace names, to avoid having different names depending on ALS vendor.
#' 
#' @param input A dataframe of standardized detections.
#'
#' @return A dataframe with standardized code spaces.
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
#' @return A dataframe with corrected timestamps.
#'
#' @keywords internal
#' 
convertTimes <- function(input, start.timestamp, end.timestamp, tz.study.area) {
  appendTo("debug", "Starting convertTimes.")
  input$Timestamp <- fasttime::fastPOSIXct(input$Timestamp, tz = "UTC")
  attributes(input$Timestamp)$tzone <- tz.study.area
  input <- input[order(input$Timestamp), ]
  if (!is.null(start.timestamp)){
    input <- input[counter <- input[, 1] >= as.POSIXct(start.timestamp, tz = tz.study.area), ]
    appendTo(c("Screen","Report"), paste("M: Discarding detection data previous to ",start.timestamp," per user command (", sum(counter), " detections discarded).", sep = ""))
  }
  if (!is.null(end.timestamp)){
    input <- input[counter <- input[, 1] <= as.POSIXct(end.timestamp, tz = tz.study.area), ]
    appendTo(c("Screen","Report"), paste("M: Discarding detection data posterior to ",end.timestamp," per user command (", sum(counter), " detections discarded).", sep = ""))
  }
  input$Receiver <- droplevels(input$Receiver)
  input$Transmitter <- as.factor(paste(input$CodeSpace, input$Signal, sep = "-"))
  appendTo("debug", "Terminating convertTimes.")
  return(input)
}

#' Standardize Stations
#' 
#' Matches the ALS serial number to the stations in the study area, standardizing the ALS station names.
#' 
#' @inheritParams convertCodes
#' @inheritParams loadDetections
#'
#' @return A dataframe with standardized station names.
#'
#' @keywords internal
#' 
standardizeStations <- function(input, spatial) {
  appendTo("debug", "Starting standardizeStations.")
  input$Standard.Name <- NA
  for (i in spatial$receiver.columns) {
    link <- match(input$Receiver, spatial$stations[, i])
    new.names <- spatial$stations$Standard.Name[link]
    places <- seq_len(nrow(input))[!is.na(link)]
    input$Standard.Name[places] <- new.names[!is.na(link)]
  }
  input$Standard.Name <- factor(input$Standard.Name, spatial$stations$Standard.Name)
  appendTo("debug", "Terminating standardizeStations.")
  return(input)
}
