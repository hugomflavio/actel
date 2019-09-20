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

labelUnknowns <- function(detections.list) {
  if (any(unlist(lapply(detections.list, function(x) any(is.na(x$Array)))))) {
    tags <- NULL
    receivers <- NULL
    lapply(detections.list, function(x) {
      if (any(is.na(x$Array))) {
        tags <- c(tags, unique(x$Transmitter))
        receivers <- c(receivers, unique(x$Transmitter[is.na(x$Array)]))
      }})
    appendTo(c("Screen", "Report", "Warning"), paste0("W: Fish ", paste(tags, collapse = ", ") , ifelse(length(tags) > 1, " were", " was"), " detected in one or more receivers that are not listed in the study area (receiver(s): ", paste(unique(receivers), collapse = ", "), ")!"))
    cat("Possible options:\n   a) Stop and doublecheck the data (recommended)\n   b) Temporary include the unknown hydrophone(s) in the analysis\n")
    check <- TRUE
    while (check) {
      decision <- readline("Which option should be followed?(a/b) ")
      if (decision == "a" | decision == "A" | decision == "b" | decision == "B") 
        check <- FALSE else cat("Option not recognized, please try again.\n")
      appendTo("UD", decision)
    }
    if (decision == "a" | decision == "A") {
      emergencyBreak()
      stop("Stopping analysis per user command.\n")
    }
    detections.list <- lapply(detections.list, function(x) {
      levels(x$Standard.Name) <- c(levels(x$Standard.Name), "Unknown")
      x$Standard.Name[is.na(x$Standard.Name)] <- "Unknown"
      levels(x$Array) <- c(levels(x$Array), "Unknown")
      x$Array[is.na(x$Array)] <- "Unknown"
      return(x)
    })
  }
  return(detections.list)
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
    stop("Could not find a '", file, "' file in the working directory.\n")
  }
  default.cols <- c("Receiver", "Station.Name", "Start", "Stop")
  link <- match(default.cols, colnames(input))
  if (any(is.na(link))) {
    appendTo("Report", paste0("Error: Column(s) '", paste(default.cols[is.na(link)], collapse = "', '"), "'are missing in the deployments file."))
    emergencyBreak()
    stop(paste0("Column(s) '", paste(default.cols[is.na(link)], collapse = "', '"), "'are missing in the deployments file.\n"))
  }
  if (any(!grepl("^[1-2][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9] [0-2][0-9]:[0-5][0-9]", input$Start))) {
    emergencyBreak()
    stop("Not all values in the 'Start' column appear to be in a 'yyyy-mm-dd hh:mm' format (seconds are optional). Please doublecheck the deployments file.\n")
  }
  if (any(!grepl("^[1-2][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9] [0-2][0-9]:[0-5][0-9]", input$Stop))) {
    emergencyBreak()
    stop("Not all values in the 'Stop' column appear to be in a 'yyyy-mm-dd hh:mm' format (seconds are optional). Please doublecheck the deployments file.\n")
  }
  if (inherits(try(as.POSIXct(input$Start), silent = TRUE),"try-error")){
    emergencyBreak()
    stop("Could not recognise the data in the 'Release.date' column as POSIX-compatible timestamps. Please doublecheck the biometrics file.\n")
  }
  if (inherits(try(as.POSIXct(input$Stop), silent = TRUE),"try-error")){
    emergencyBreak()
    stop("Could not recognise the data in the 'Release.date' column as POSIX-compatible timestamps. Please doublecheck the biometrics file.\n")
  }
  input$Receiver <- as.character(input$Receiver)
  input$Receiver <- sapply(input$Receiver, function(x) tail(unlist(strsplit(x, "-")), 1))
  input$Start <- fasttime::fastPOSIXct(input$Start, tz = tz.study.area)
  input$Stop <- fasttime::fastPOSIXct(input$Stop, tz = tz.study.area)
  appendTo("debug","Terminating loadDeployments.")
  return(input)
}

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


#' Load Spatial file and Check the structure
#' 
#' @inheritParams assembleSpatial
#' 
#' @return The spatial dataframe
#' 
#' @keywords internal
#' 
new_loadSpatial <- function(file){
  appendTo("debug","Starting new_loadSpatial.")
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
  if (!any(grepl("Type", colnames(input)))) {
    appendTo("Screen", "M: No 'Type' column found in 'spatial.csv'. Assigning all rows as hydrophones.")
    input$Type <- "Hydrophone"
  } else {
    if (any(is.na(match(unique(input$Type), c("Hydrophone","Release"))))){
      emergencyBreak()
      stop("Could not recognise the data in the 'Type' column as only one of 'Hydrophone' or 'Release'. Please doublecheck the spatial file.\n")
    }
  }
  appendTo("debug","Terminating new_loadSpatial.")
  return(input)
}


checkDeploymentTimes <- function(input) {
  appendTo("debug","Terminating checkDeploymentTimes")
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
        print(aux[[i]], row.names = FALSE)
        cat("\n")
        emergencyBreak()
        stop("Fatal exception found. Read lines above for more details.\n")      
      }
    }
  }
  appendTo("debug","Terminating checkDeploymentTimes")
}

checkDeploymentStations <- function(input, spatial) {
  appendTo("debug","Terminating checkDeploymentStations")
  aux <- spatial[spatial$Type == "Hydrophone", ]
  link <- match(unique(input$Station.Name), aux$Station.Name)
  if (any(is.na(link))) {
    appendTo(c("Screen", "Report", "Warning"), paste0("W: ", ifelse(sum(is.na(link)) > 1, "Stations", "Station"), " '", paste(unique(input$Station.Name)[is.na(link)], collapse = "', '"), "' ", ifelse(sum(is.na(link)) > 1, "are", "is"), " listed in the deployments but are not part of the study's stations. Discarding deployments at unknown stations."))
    to.remove <- match(input$Station.Name, unique(input$Station.Name)[is.na(link)])
    input <- input[is.na(to.remove), ]
  }
  link <- match(aux$Station.Name, unique(input$Station.Name))
  if (any(is.na(link))) {
    appendTo(c("Screen", "Report"), paste0("Error: Stations '", paste(aux$Station.Name[is.na(link)], collapse = "', '"), "' are listed in the spatial file but no receivers were ever deployed there."))
    emergencyBreak()
    stop("Fatal exception found. Read lines above for more details.\n")      
  }
  appendTo("debug","Terminating checkDeploymentStations")
  return(input)
}

createUniqueSerials <- function(input) {
  appendTo("debug","Terminating createUniqueSerials")
  output <- split(input, input$Receiver)
  for (i in 1:length(output)) {
    if (nrow(output[[i]]) > 1) {
      output[[i]]$Receiver <- paste0(output[[i]]$Receiver, ".dpl.", 1:nrow(output[[i]]))
    }
  }
  appendTo("debug","Terminating createUniqueSerials")
  return(output)
}


new_standardizeStations <- function(detections, spatial, deployments) {
  appendTo("debug","Terminating new_standardizeStations")
  detections$Receiver <- as.character(detections$Receiver)
  detections$Standard.Name <- NA_character_
  detections$Array <- NA_character_
  empty.receivers <- NULL
  for (i in 1:length(deployments)) {
    receiver.link <- detections$Receiver == names(deployments)[i]
    if (all(!receiver.link)) {
      empty.receivers <- c(empty.receivers, names(deployments)[i])
    } else {
      for (j in 1:nrow(deployments[[i]])) {
        # find target rows in detections
        deployment.link <- detections$Timestamp[receiver.link] >= deployments[[i]]$Start[j] & 
          detections$Timestamp[receiver.link] < deployments[[i]]$Stop[j]
        # rename receiver
        detections$Receiver[receiver.link][deployment.link] <- deployments[[i]]$Receiver[j]
        # find corresponding standard station name
        the.station <- match(deployments[[i]]$Station.Name[j], spatial$Station.Name)
        # include Standard.Name
        detections$Standard.Name[receiver.link][deployment.link] <- spatial$Standard.Name[the.station]
        # include Array
        detections$Array[receiver.link][deployment.link] <- as.character(spatial$Array[the.station])
      }
      if (any(the.error <- is.na(detections$Standard.Name[receiver.link]))) {
        appendTo(c("Screen", "Report"), paste0("Error: ", sum(the.error), " detections for receiver ", names(deployments)[i], " do not fall within deployment periods."))
        cat("\n")
        print(detections[receiver.link][the.error, -c(6, 7)])
        cat("\n")
        cat("Possible options:\n   a) Stop and doublecheck the data (recommended)\n   b) Discard orphan detections.\n")
        check <- TRUE
        while (check) {
          decision <- readline("Which option should be followed?(a/b) ")
          if (decision == "a" | decision == "A" | decision == "b" | decision == "B") 
            check <- FALSE else cat("Option not recognized, please try again.\n")
          appendTo("UD", decision)
        }
        if (decision == "a" | decision == "A") {
          emergencyBreak()
          stop("Stopping analysis per user command.\n")
        } else {
          rows.to.remove <- detections[receiver.link, which = TRUE][the.error]
          detections <- detections[-rows.to.remove]
        }
       }
    }
  }
  appendTo(c("Screen", "Report"), paste0("M: Number of ALS: ", length(deployments), " (of which ", length(empty.receivers), " had no detections)"))
  if (!is.null(empty.receivers))
    appendTo(c("Screen", "Report", "Warning"), paste0("W: No detections were found for receiver(s) ", paste0(empty.receivers, collapse = ", "), "."))
  appendTo("debug","Terminating new_standardizeStations")
  aux <- spatial[spatial$Type == "Hydrophone", ]
  detections$Receiver <- as.factor(detections$Receiver)
  detections$Array <- factor(detections$Array, levels = unique(aux$Array))
  detections$Standard.Name <- factor(detections$Standard.Name, levels = aux$Standard.Name)
  return(detections)
}


unknownReceivers <- function(input) {
  appendTo("debug","Terminating unknownReceivers")
  unknown <- is.na(input$Standard.Name)
  if (any(unknown)) {
    appendTo(c("Screen", "Report", "Warning"), paste0("W: Detections from receivers ", paste(unique(input$Receiver[unknown]), collapse = ", "), " are present in the data, but these receivers are not part of the study's stations. Doublecheck potential errors."))
  }
  appendTo("debug","Terminating unknownReceivers")
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
  appendTo("Screen", "M: Compiling detections...")
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
#' @param spatial A list of spatial objects in the study area
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
