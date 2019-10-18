#' Create numerical distances between dot elements
#' 
#' @param input a dot data frame
#' 
#' @return a matrix of distances between arrays
#' 
#' @keywords internal
#' 
dotMatrix <- function(input) {
  nodes <- unique(unlist(input[, c(1, 3)]))
  graph <- matrix(0, length(nodes), length(nodes), dimnames = list(nodes, nodes))
  for (i in 1:nrow(input)) {
    graph[input$A[i], input$B[i]] <- 1
    graph[input$B[i], input$A[i]] <- 1
  }
  for (i in 1:(length(nodes)-1)) {
    for (A in nodes) {
      for (B in nodes) {
        if (graph[A, B] == i) {
          candidates <- rownames(graph) != B & rownames(graph) != A & graph[, B] == 1
          if (any(candidates)) {
            to.change <- names(candidates)[candidates]
            for (j in to.change) {
              if (graph[A, j] == 0) 
                graph[A, j] <- graph[A, j] + 1 + i
            }
          }
        }
      }
    }
  }
  return(graph)
}

#' Break the dot data frame into a list
#' 
#' @param input a dot data frame
#' @inheritParams migration
#' 
#' @return A list containing, for each array, the arrays that connect to it and to which it connects.
#' 
#' @keywords internal
#' 
dotList <- function(input, sections = NULL) {
  input$SectionA <- rep(NA_character_, nrow(input))
  input$SectionB <- rep(NA_character_, nrow(input))
  if (!is.null(sections)) {
    for (section in sections) {
       input$SectionA[grepl(section, input$A)] <- section
       input$SectionB[grepl(section, input$B)] <- section
    }
    input$Edge <- with(input, SectionA != SectionB)
  }

  arrays <- list()
  for (a in unique(unlist(input[,c(1, 3)]))) {
    auxA <- input[input$A == a, ]
    auxB <- input[input$B == a, ]
    recipient <- list(
      before = if (nrow(auxB) == 0) { NULL  } else { unique(auxB$A) },
      after  = if (nrow(auxA) == 0) { NULL  } else { unique(auxA$B) },
      edge   = if (nrow(auxA) == 0) { FALSE } else { any(auxA$Edge) })
    arrays[[length(arrays) + 1]] <- recipient
    names(arrays)[length(arrays)] <- a
  }
  return(arrays)
}

#' Find arrays valid for efficiency calculation
#' 
#' @param input A dot list
#' @param mat A dot distance matrix
#' 
#' @return The input list, with an extra element for each array with valid efficiency peers
#' 
#' @keywords internal
#' 
dotPaths <- function(input, mat) {
  for (a in names(input)) {
    downstream <- NULL
    to.check <- input[[a]]$after
    while (!is.null(to.check)) {
      new.check <- NULL
      for (b in to.check) {
        # If A and B are adjacent, check that there are no more paths leading to B
        if (mat[a, b] == 1 && length(input[[b]]$before) == 1) {
          if (is.null(downstream) || !grepl(b, downstream)) {
            downstream <- c(downstream, b)
            new.check <- c(new.check, input[[b]]$after)
          }        
        }
        # If B is far away, check that the paths leading to B are in valid the downstream list
        if (mat[a, b] > 1 && all(!is.na(match(input[[b]]$before, downstream)))) {
          if (is.null(downstream) ||!grepl(b, downstream)) {
            downstream <- c(downstream, b)
            new.check <- c(new.check, input[[b]]$after)
          }
        }
        to.check <- unique(new.check)
      }
    }
    input[[a]]$downstream <- downstream
  }
  return(input)
}


#' Process spatial elements
#' 
#' Creates a list containing multiple spatial elements required throughout the analyses
#' 
#' @param file an input file with spatial data.
#' @param first.array Either NULL or the top level array in the study area.
#' @inheritParams splitDetections
#' @inheritParams actel
#' 
#' @return A list of 1) stations, 2) release sites, 3) ALS columns in the spatial file, 4) the Number of ASL, 5) The ALS serial numbers and 6) the array order.
#' 
#' @keywords internal
#' 
transformSpatial <- function(spatial, bio, sections = NULL, first.array = NULL) {
  appendTo("debug", "Starting transformSpatial.")
  # Break the stations away
  appendTo("debug", "Creating 'stations'.")
  stations <- spatial[spatial$Type == "Hydrophone", -match("Type", colnames(spatial))]
  stations$Array <- factor(stations$Array, levels = unique(stations$Array))
  appendTo("debug", "Creating 'release.sites'.")
  # If there is any release site in the spatial file
  if (sum(spatial$Type == "Release") > 0) {
    if (length(unique(bio$Release.site)) == 1 && unique(bio$Release.site) == "unspecified") {
      appendTo(c("Screen", "Report", "Warning"), "W: At least one release site has been indicated in the spatial file, but no release sites were specified in the biometrics file.\n   Discarding release site information and assuming all fish were released at the top level array to avoid function failure.\n   Please doublecheck your data.")
      if (is.null(first.array)) {
        emergencyBreak()
        stop("There is more than one top level array in the study area. Please specify release site(s) in the 'spatial.csv' file and in the 'biometrics.csv' file.\n", call. = FALSE)
      }
      release.sites <- data.frame(Station.Name = "unspecified", 
                                  Longitude = NA_real_, 
                                  Latitude = NA_real_, 
                                  Array = first.array,
                                  Standar.Name = "unspecified")
    } else {
      A <- spatial$Station.Name[spatial$Type == "Release"]
      B <- unique(bio$Release.site)
      if (any(is.na(match(B, A)))) {
        appendTo(c("Screen", "Report", "Warning"), "Error: There is a mismatch between the release sites reported and the release locations for the smolts.")
        cat("   Release sites listed in the spatial file:", paste(A, collapse = ", "), "\n")
        cat("   Sites listed in the biometrics file 'Release.site' column:", paste(B, collapse = ", "), "\n")
        emergencyBreak()
        stop("The release names should be identical in the spatial objects file and in the biometrics file.\n", call. = FALSE)
      } else {
        from.row <- spatial$Type == "Release"
        from.col <- colnames(spatial)[!grepl("Receiver", colnames(spatial))]
        release.sites <- spatial[from.row, from.col]
        row.names(release.sites) <- 1:nrow(release.sites)
      }
      A <- unique(stations$Array)
      B <- unique(release.sites$Array)
      if (any(is.na(match(B, A)))) {
        appendTo(c("Screen", "Report", "Warning"), "Error: There is a mismatch between the expected first array of a release site and the list of arrays.")
        cat("   Arrays listed in the spatial file:", paste(A, collapse = ", "), "\n")
        cat("   Expected first arrays of the release sites:", paste(B, collapse = ", "), "\n")
        emergencyBreak()
        stop("The expected first arrays should match the arrays where stations where deployed in the spatial file.\n", call. = FALSE)
      }
    }
  } else {
    if (length(unique(bio$Release.site)) > 1){
      appendTo("Screen", "M: Release sites were not specified in the spatial file but more than one release site is reported in the biometric data.\n   Assuming all released fish start at the top level array.")
      if (is.null(first.array)) {
        emergencyBreak()
        stop("There is more than one top level array in the study area. Please specify release site(s) in the 'spatial.csv' file and in the 'biometrics.csv' file.\n", call. = FALSE)
      }
    }
    release.sites <- data.frame(Station.Name = unique(bio$Release.site), 
                                Longitude = NA_real_,
                                Latitude = NA_real_, 
                                Array = rep(first.array, length(unique(bio$Release.site))),
                                Standard.Name = unique(bio$Release.site))
  }
  # Wrap up
  if (!is.null(sections)) {
    array.order <- list()  # Used to determine if the fish's last detection was in the last array of a given section
    for (j in sections) {
      array.order[[j]] <- levels(stations$Array)[grepl(j, levels(stations$Array))]
    }
    if (any(trigger <- unlist(lapply(array.order,length)) == 0)) {
      appendTo(c("Screen", "Warning"), decision <- readline(paste("W: No arrays were found that match section(s) ",paste(names(array.order)[trigger], collapse = ", "), ". There could be a typing mistake!\n   Continue the analysis?(y/N) ", sep = "")))
      if (decision != "y" | decision != "Y" ){
        emergencyBreak()
        stop("Stopping analysis per user command.\n", call. = FALSE)
      }
    }
  } else {
    array.order <- list(all = levels(stations$Array))
  }
  # Order release sites by entry point.
  if (!is.ordered(match(release.sites$Array, unlist(array.order))))
    release.sites <- release.sites[order(match(release.sites$Array, unlist(array.order))),]
  # join everything
  output <- list(stations = stations, 
                 release.sites = release.sites, 
                 array.order = array.order)
  appendTo("debug", "Terminating transformSpatial")
  return(output)
}

#' Create Standard Names for spatial elements
#' 
#' Includes standard names and also reprints 'spatial.csv' 
#' 
#' @param input A data frame with spatial information.
#'  
#' @return A data frame with the same information as the input plus the Standard names.
#' 
#' @keywords internal
#' 
setSpatialStandards <- function(input){
  appendTo("debug","Starting setSpatialStandards")
  input$Standard.Name <- as.character(input$Station.Name)
  link <- input$Type == "Hydrophone"
  input$Standard.Name[link] <- paste("St.", seq_len(sum(input$Type == "Hydrophone")), sep = "")
  write.csv(input, "spatial.csv", row.names = F)
  appendTo("debug","Terminating setSpatialStandards")
  return(input)
}

#' Include the deployment in the serial number of the receive
#' 
#' @param input A data frame with the deployments
#' 
#' @return A list of deployments, with unique serial numbers per deployment
#' 
#' @keywords internal
#' 
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

#' Standardize serial numbers, stations and arrays in the detections
#' 
#' Matches the ALS serial number to the deployments to rename the serial number.
#' The corresponding deployment is then used to update the Standard Station name and the array based in the spatial object.
#' 
#' @param detections a data frame of detections
#' @param spatial A list of spatial objects in the study area
#' @param deployments a list of deployments
#'
#' @return A data frame with standardized station names.
#'
#' @keywords internal
#' 
createStandards <- function(detections, spatial, deployments) {
  appendTo("debug","Terminating createStandards")
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
        cat("Possible options:\n   a) Stop and double check the data (recommended)\n   b) Discard orphan detections.\n")
        check <- TRUE
        while (check) {
          decision <- readline("Which option should be followed?(a/b) ")
          if (decision == "a" | decision == "A" | decision == "b" | decision == "B") 
            check <- FALSE else cat("Option not recognized, please try again.\n")
          appendTo("UD", decision)
        }
        if (decision == "a" | decision == "A") {
          emergencyBreak()
          stop("Stopping analysis per user command.\n", call. = FALSE)
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
  appendTo("debug","Terminating createStandards")
  aux <- spatial[spatial$Type == "Hydrophone", ]
  detections$Receiver <- as.factor(detections$Receiver)
  detections$Array <- factor(detections$Array, levels = unique(aux$Array))
  detections$Standard.Name <- factor(detections$Standard.Name, levels = aux$Standard.Name)
  return(detections)
}

#' Split detections by tag
#'
#' Splits the detections' table by tags and selects only detections from target tags
#' 
#' @inheritParams actel
#' @inheritParams loadDetections
#' @param bio A table with the tags and biometrics of the studied fish.
#' @param detections A data frame with all the detections. Supplied by loadDetections.
#' @param silent logical: if TRUE, suppress messages.
#' 
#' @return A list of detections for each tag.
#' 
#' @keywords internal
#' 
splitDetections <- function(detections, bio, exclude.tags = NULL, silent = FALSE) {
  appendTo("debug", "Starting splitDetections.")
  my.list <- split(detections, detections$Transmitter)
  my.list <- excludeTags(input = my.list, exclude.tags = exclude.tags, silent = silent)
  
  tags <- checkNoDetections(input = my.list, bio = bio)
  checkDupSignals(input = my.list, bio = bio, tag.list = tags$list)
  
  appendTo("debug", "Debug: Creating 'trimmed.list'.")
  bio$Transmitter <- names(my.list)[tags[["link"]]]
  trimmed.list <- my.list[tags[["link"]]]
  non.empty <- unlist(lapply(trimmed.list, function(x) length(x) != 0))
  trimmed.list <- trimmed.list[non.empty]
  if (!silent){
    collectStrays(input = my.list[-na.exclude(tags[["link"]])])
    storeStrays()
  }

  appendTo("debug", "Terminating splitDetections.")
  return(list(trimmed.list = trimmed.list, bio = bio))
}

#' Temporarily include unknown receivers with target detections in the study area
#' 
#' @param detections.list a list of the detections for each target tag
#' 
#' @return The detections.list with the unknown receivers labelled
#' 
#' @keywords internal
#' 
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
    cat("Possible options:\n   a) Stop and double check the data (recommended)\n   b) Temporary include the unknown hydrophone(s) in the analysis\n")
    check <- TRUE
    while (check) {
      decision <- readline("Which option should be followed?(a/b) ")
      if (decision == "a" | decision == "A" | decision == "b" | decision == "B") 
        check <- FALSE else cat("Option not recognized, please try again.\n")
      appendTo("UD", decision)
    }
    if (decision == "a" | decision == "A") {
      emergencyBreak()
      stop("Stopping analysis per user command.\n", call. = FALSE)
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

#' Temporarily include missing receivers in the spatial object
#' 
#' @param unknown.receivers serial number of the receivers to be included
#' @inheritParams loadDetections
#' 
#' @keywords internal
#' 
includeUnknownReceiver <- function(spatial, deployments, unknown.receivers){
  appendTo("debug", "Starting includeUnknownReceiver.")
  appendTo(c("Screen", "Report"), "M: Including missing receiver(s) in the deployments and stations. Assigning to array 'Unknown'.")
  if (is.na(match("Unknown", levels(spatial$stations$Station.Name)))) {
    levels(spatial$stations$Station.Name) <- c(levels(spatial$stations$Station.Name), "Unknown")
    levels(spatial$stations$Array) <- c(levels(spatial$stations$Array), "Unknown")
    spatial$stations[nrow(spatial$stations) + 1, "Station.Name"] <- "Unknown"
    spatial$stations[nrow(spatial$stations), "Array"] <- "Unknown"
    spatial$stations[nrow(spatial$stations), "Standard.Name"] <- "Ukn"
  }
  for (i in unknown.receivers) {
    if (is.na(match(i, names(deployments)))) {
      deployments[[length(deployments) + 1]] <- data.frame(
        Receiver = i,
        Station.Name = "Unknown",
        Start = NA_real_,
        Stop = NA_real_)
      names(deployments)[length(deployments)] <- i
    }
  }
  appendTo("debug", "Terminating includeUnknownReceiver.")
  return(list(spatial = spatial, deployments = deployments))
}

#' Collect summary information on the tags detected but that are not part of the study.
#'
#' @param input list of detections
#' @inheritParams actel
#' @inheritParams splitDetections
#'
#' @keywords internal
#' 
excludeTags <- function(input, exclude.tags, silent){
  appendTo("debug", "Starting excludeTags.")  
  if (length(exclude.tags) != 0) {
    link <- match(exclude.tags, names(input))
    if (!silent){
      appendTo(c("Screen", "Report", "Warning"), paste("M: Excluding tag(s) ", paste(exclude.tags, collapse = ", "), " from the analysis per used command (detections removed: ", paste(unlist(lapply(input[link], nrow)), collapse = ", "), ", respectively).", sep = ""))
      collectStrays(input = input[link], restart = TRUE)
    }
    appendTo("debug", "Terminating excludeTags.")  
    return(input[-link])
  }
  appendTo("debug", "Terminating excludeTags.")  
  return(input)
}

#' Remove Code Spaces from transmitter names
#' 
#' @param input A vector of transmitter names
#' 
#' @keywords internal
#' 
stripCodeSpaces <- function(input) {
  output <- vector()
  for (i in seq_len(length(input))) output[i] <- tail(unlist(strsplit(input[i], "-")), 1)
  return(output)
}

#' Match the detection's receiver to its respective array.
#'
#' @param input list of detections
#' @inheritParams loadDetections
#' 
#' @keywords internal
#' 
assignArrays <- function(input, spatial){
  appendTo("debug", "Starting assignArrays.")  
  to.combine <- list()
  for (j in spatial$receiver.columns) {
    to.combine[[length(to.combine) + 1]] <- match(input$Receiver, spatial$stations[, j])
  }
  rm(j)
  input$Array <- spatial$stations$Array[combine(to.combine)]
  appendTo("debug", "Terminating assignArrays.")  
  return(input)
}


#' Extract time stamp of valid entry or exit in each array
#' 
#' @inheritParams assembleMatrices
#' @inheritParams loadDetections
#' @inheritParams actel
#' @param type The point to be recorded: one of "Arrival" or "Departure".
#' 
#' @keywords internal
#' 
#' @return A data frame with the timestamps for each fish (rows) and array (columns)
#' 
getTimes <- function(simple.movements, spatial, tz.study.area, type = c("Arrival", "Departure")){
  appendTo("Debug", "Starting getTimes.")
  type <- match.arg(type)
  output <- matrix(nrow = length(simple.movements), ncol = length(unlist(spatial$array.order)) + 1)
  colnames(output) <- c("Transmitter", unlist(spatial$array.order))
  output <- as.data.frame(output)
  output$Transmitter <- names(simple.movements)
  for (i in 1:length(simple.movements)) {
    for (j in 2:ncol(output)) {
      if (type == "Arrival") the.row <- head(which(grepl(colnames(output)[j], simple.movements[[i]]$Array)), 1)
      if (type == "Departure") the.row <- tail(which(grepl(colnames(output)[j], simple.movements[[i]]$Array)), 1)
      if (length(the.row) != 0) {
        if (type == "Arrival") output[i,j] <- as.character(simple.movements[[i]]$First.time[the.row])
        if (type == "Departure") output[i,j] <- as.character(simple.movements[[i]]$Last.time[the.row])
      }
      if (i == nrow(output))
        output[,j] <- as.POSIXct(output[,j], tz = tz.study.area)
    }
  }
  appendTo("Debug", "Terminating getTimes.")
  return(output)
}

#' Convert times data frame into a list of circular objects
#' 
#' @param times A data frame with the time stamps for reach fish and array
#' 
#' @keywords internal
#' 
#' @return A list of circular objects
#' 
convertTimesToCircular <- function(times) {
  appendTo("Debug", "Starting convertTimesToCircular.")
  output <- list()
  cols.with.data <- apply(times, 2, function(x) !all(is.na(x)))
  times <- times[, cols.with.data]
  for (i in 2:ncol(times)) {
    output[[i - 1]] <- circular::circular(decimalTime(substrRight(as.character(times[, i]), 8)), units = "hours", template = "clock24")
    names(output[[i - 1]]) <- times$Transmitter
  }
  names(output) <- colnames(times)[2:ncol(times)]
  appendTo("Debug", "Terminating convertTimesToCircular.")
  return(output)
}
