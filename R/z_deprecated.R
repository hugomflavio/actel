#' Group movements
#'
#' Crawls trough the detections of each fish and groups them based on ALS arrays and time requirements.
#' 
#' @param detections.list A list of the detections split by each target tag, created by splitDetections.
#' @param dist.mat A matrix of the distances between the deployed ALS.
#' @param invalid.dist Whether or not the distance matrix supplied is valid for the study area.
#' @inheritParams actel
#' @inheritParams splitDetections
#' @inheritParams loadDetections
#' 
#' @return A list of movement events for each fish.
#' 
#' @keywords internal
#' 
deprecated_groupMovements <- function(detections.list, bio, spatial, speed.method, maximum.time, 
  tz.study.area, dist.mat, invalid.dist) {
  appendTo("debug", "Starting groupMovements.")
  movements <- list()
  round.points <- roundDown(seq(from = length(detections.list)/10, to = length(detections.list), length.out = 10), to = 1)
  counter <- 1
  {
    pb <- txtProgressBar(min = 0, max = sum(unlist(lapply(detections.list, nrow))), style = 3, width = 60)
    for (i in names(detections.list)) {
      appendTo("debug", paste("Debug: (Movements) Analysing fish ", i, ".", sep = ""))
      if (invalid.dist) {
        recipient <- data.frame(
          Array = NA_character_, 
          Detections = NA_integer_, 
          First.station = NA_character_, 
          Last.station = NA_character_, 
          First.time = NA_character_, 
          Last.time = NA_character_, 
          Time.travelling = NA_character_, 
          Time.on.array = NA_character_, 
          Valid = NA,
          stringsAsFactors = FALSE
          )
      } else {
        recipient <- data.frame(
          Array = NA_character_, 
          Detections = NA_integer_, 
          First.station = NA_character_, 
          Last.station = NA_character_, 
          First.time = NA_character_, 
          Last.time = NA_character_, 
          Time.travelling = NA_character_, 
          Time.on.array = NA_character_, 
          Average.speed.m.s = NA_real_,
          Valid = NA,
          stringsAsFactors = FALSE
          )
      }
      z = 1
      array.shifts <- c(which(detections.list[[i]]$Array[-1] != detections.list[[i]]$Array[-length(detections.list[[i]]$Array)]), nrow(detections.list[[i]]))
      time.shifts <- which(difftime(detections.list[[i]]$Timestamp[-1], detections.list[[i]]$Timestamp[-length(detections.list[[i]]$Timestamp)], units = "mins") > maximum.time)
      all.shifts <- sort(unique(c(array.shifts, time.shifts)))
      for (j in seq_len(length(all.shifts))) {
        if (j == 1)
          start <- 1
        else
          start <- all.shifts[j - 1] + 1
        stop <- all.shifts[j]
        recipient[z, "Array"] = paste(detections.list[[i]]$Array[start])
        recipient[z, "Detections"] = stop - start + 1
        recipient[z, "First.station"] = paste(detections.list[[i]]$Standard.Name[start])
        recipient[z, "First.time"] = paste(detections.list[[i]]$Timestamp[start])
        recipient[z, "Last.station"] = paste(detections.list[[i]]$Standard.Name[stop])
        recipient[z, "Last.time"] = paste(detections.list[[i]]$Timestamp[stop])
        z = z + 1
        counter <- counter + stop - start + 1
        if (i == tail(names(detections.list), 1)) 
          counter <- sum(unlist(lapply(detections.list, nrow)))
        setTxtProgressBar(pb, counter)
        flush.console()
      }

      recipient$Valid <- TRUE

      recipient <- deprecated_upstreamCheck(i = i, recipient = recipient, bio = bio, spatial = spatial)
      
      if (!is.null(recipient)) {
        recipient[, "First.time"] <- as.POSIXct(recipient[, "First.time"], tz = tz.study.area)
        recipient[, "Last.time"] <- as.POSIXct(recipient[, "Last.time"], tz = tz.study.area)
        
        recipient <- data.table::as.data.table(recipient)
        recipient <- movementTimes(movements = recipient,
            silent = FALSE)
        if (!invalid.dist)
          recipient <- movementSpeeds(movements = recipient, 
            speed.method = speed.method, dist.mat = dist.mat, silent = FALSE)
        
        movements[[length(movements) + 1]] <- recipient
        names(movements)[length(movements)] <- i
        attributes(movements[[length(movements)]])$p.type <- "Auto"
      }
      rm(recipient)
    }
    close(pb)
    rm(pb)
  }
  appendTo("debug", "Done.")
  return(movements)
}

#' Check for movements upstream of the release site.
#'
#' @inheritParams splitDetections
#' @inheritParams loadDetections
#' @param i The tag being analysed.
#' @param recipient The movement events table for the tag.
#' 
#' @return The checked recipient
#' 
#' @keywords internal
#' 
deprecated_upstreamCheck <- function(i, recipient, bio, spatial) {
  appendTo("debug", paste("Starting upstreamCheck for fish ", i, ".", sep = ""))
  t1 <- bio[matchl(bio$Signal, tail(strsplit(i, "-")[[1]], 1)), "Release.site"]  # The release site from the target fish
  t2 <- spatial$release.sites[matchl(spatial$release.sites$Station.Name, t1), "Array"]  # The expected hydrophone array for that release site
  t3 <- match(t2, unlist(spatial$array.order))  # The position of the array relative to the study area
  if (is.na(t3)) {
    appendTo("Report", "Error: There is a mismatch between the expected first array of a release site and the list of arrays.\nPlease correct this before continuing.")
    emergencyBreak()
    stop("There is a mismatch between the expected first array of a release site and the list of arrays.\nPlease correct this before continuing.\n", call. = FALSE)
  }
  t4 <- match(recipient$Array[recipient$Array != "Unknown"], unlist(spatial$array.order))  # The relative position of the arrays where the fish was detected
  if (any(t4 < t3)) {
    cat("\n")
    appendTo(c("Screen", "Report", "Warning"), paste("W: Fish ", i, " was detected behind its release site! Opening relevant data for inspection.", sep = ""))
    appendTo("Screen", paste("   Release site:", t1))
    appendTo("Screen", paste("   Expected first array:", t2))
    cat(paste("\n   Movement table for fish ", i, ":\n", sep =""))
    print(recipient[, -c(7:10)])
    cat("\n")
    appendTo("Screen", "You may either:\n  a) Stop the analysis if the expected first array is wrong;\n  b) Continue as is (does not impact the results);\n  c) Remove a movement event, if you are confident it is a false detection.")
    cat("\n")
    unknown.input = TRUE
    while (unknown.input) {
      decision <- commentCheck(line = "Decision:(a/b/c/comment) ", tag = i)
      if (decision == "a" | decision == "A") {
        unknown.input = FALSE
        appendTo("UD", decision)
        emergencyBreak()
        stop("Script stopped by user command.", call. = FALSE)
      }
      if (decision == "b" | decision == "B") {
        appendTo("UD", decision)
        unknown.input = FALSE
      }
      if (decision == "c" | decision == "C") {
        appendTo("UD", decision)
        unknown.input = FALSE
        check = TRUE
        reprint = FALSE
        while (check) {
          if (reprint) {
            cat(paste("\n   Movement table for fish ", i, ":\n", sep =""))
            print(recipient[recipient$Valid, -c(7:10)])
            cat("\n")
          }
          row.line <- suppressWarnings(as.numeric(readline("Which movement event would you like to remove? ")))
          appendTo("UD", row.line)
          if (is.na(row.line)) {
            decision <- readline("W: The inserted value is not numeric. Abort event removal?(y/N) ")
            appendTo("UD", decision)
            if (decision == "Y" | decision == "y") {
              appendTo("Screen", "M: Aborting event removal")
              check = FALSE
            }
            rm(decision)
          } else {
            if ( row.line < 1 | row.line > nrow(recipient)) {
              appendTo("Screen", paste("Invalid row, please choose a number between 1 and ",nrow(recipient),", or leave blank to abort.", sep = ""))
            } else {
              recipient$Valid[row.line] <- FALSE
              if (any(recipient$Valid)){
                decision <- readline("Do you want to remove more movement events?(y/N) ")
                appendTo("UD", decision)
              } else {
                appendTo(c("Screen","Report"), paste("M: All movement events for fish ",i," were deleted by user command. Excluding it from further analysis.", sep = ""))
                decision <- "N"
              }
              if (decision == "Y" | decision == "y") {
                check = TRUE
                reprint = TRUE
              } else {
                check = FALSE
              }
              rm(decision)
            }
          }
        }
        rm(check)
      }
      if (unknown.input) {
        cat("Option not recognized, please input either 'a', 'b', 'c' or 'comment'.\n")
      }
    }
  }
  if (nrow(recipient) > 0)
    return(recipient)
  else
    return(NULL)
  appendTo("debug", "Done.")
}


#' Compile detection matrix for last array
#'
#' @inheritParams actel
#' @inheritParams loadDetections
#' @inheritParams groupMovements
#' 
#' @return a matrix of detection histories per fish for the last array.
#' 
#' @keywords internal
#' 
lastMatrix <- function(spatial, detections.list, replicate){
  appendTo("debug", "Starting lastMatrix.")
  all.stations <- spatial$stations$Standard.Name[spatial$stations$Array == tail(unlist(spatial$array.order), 1)]
  if (any(link <- !replicate %in% all.stations)) {
    if (sum(link) > 1)
      stop(paste("Stations ", paste(replicate[link], collapse = ", "), " are not part of ", tail(unlist(spatial$array.order), 1), " (available stations: ", paste(all.stations, collapse = ", "), ").", sep = ""))
    else
      stop(paste("Station ", paste(replicate[link], collapse = ", "), " is not part of ", tail(unlist(spatial$array.order), 1), " (available stations: ", paste(all.stations, collapse = ", "), ").", sep = ""))      
  }
  original <- all.stations[!all.stations %in% replicate]
  efficiency <- as.data.frame(matrix(ncol = 2, nrow = length(detections.list)))
  colnames(efficiency) <- c("original","replicate")
  rownames(efficiency) <- names(detections.list)
  for (i in 1:length(detections.list)) {
    efficiency[i, "original"] <- any(!is.na(match(original,detections.list[[i]]$Standard.Name)))
    efficiency[i, "replicate"] <- any(!is.na(match(replicate,detections.list[[i]]$Standard.Name)))
  }
  appendTo("debug", "Terminating lastMatrix.")
  return(efficiency)
}

#' Get efficiency estimate for last array
#' 
#' @inheritParams loadDetections
#' @inheritParams groupMovements
#' @inheritParams actel
#' 
#' @return the modified CJS model for the last array
#' 
#' @keywords internal
#' 
getEstimate <- function(spatial, detections.list, replicate){
  if (!is.null(replicate)) {
    get.estimate = TRUE
    last.array <- tryCatch(lastMatrix(spatial = spatial, detections.list = detections.list, replicate = replicate),
      error = function(e) {cat("Error in lastMatrix(): "); message(e); cat("\nRunning CJS without last array efficiency estimate.\n"); get.estimate <<- FALSE})
    if (get.estimate) {
      last.array.efficiency <- dualArrayCJS(input = last.array, silent = FALSE)
      last.array.results <- list(results = last.array.efficiency, matrix = last.array)
    } else {
      last.array.results <- "Replicates could not be used for last array estimation."
    }
  } else {
    last.array.results <- "No last array replicates were indicated."
  }
  return(last.array.results)
}

#' Calculate CJS for each group.release combination
#' 
#' @param the.matrices A list of detection matrices
#' @inheritParams simpleCJS
#' 
#' @return A list of CJS results
#' 
#' @keywords internal
#' 
getSplitCJS <- function(the.matrices, fixed.efficiency = NULL){
  output <- list()
  for(i in 1:length(the.matrices)){
    if(is.null(fixed.efficiency))
      efficiency.subset <- NULL
    else
      efficiency.subset <- fixed.efficiency[match(colnames(the.matrices[[i]]), names(fixed.efficiency))]
    output[[i]] <- simpleCJS(the.matrices[[i]], fixed.efficiency = efficiency.subset, silent = TRUE)
  }
  names(output) <- names(the.matrices)
  return(output)
}

#' Calculate CJS for each group
#' 
#' @inheritParams getSplitCJS
#' @inheritParams simplifyMovements
#' @inheritParams simpleCJS
#' 
#' @return A list of CJS results
#' 
#' @keywords internal
#' 
getGroupCJS <- function(the.matrices, status.df, fixed.efficiency = NULL) {
  output <- list()
  for (i in 1:length(unique(status.df$Group))) {
    link <- grepl(paste0("^", unique(status.df$Group)[i]), names(the.matrices))
    if(sum(link) == 1)
      output[[i]] <- simpleCJS(the.matrices[[which(link)]], fixed.efficiency = fixed.efficiency, silent = TRUE)
    else
      output[[i]] <- combineCJS(the.matrices[link], fixed.efficiency = fixed.efficiency, silent = TRUE)
  } 
  names(output) <- unique(status.df$Group)
  return(output)
}

#' Print progression graphic
#'
#' Prints progression graphics per fish group and release site.
#' 
#' @inheritParams actel
#' @inheritParams simplifyMovements
#' @param overall.CJS a single CJS with all the groups and release sites merged
#' @param split.CJS a list of CJS's for each group.release combination
#' @param group.CJS a list of CJS's for each group, with release sites merged
#' 
#' @keywords internal
#' 
printProgression <- function(status.df, overall.CJS, split.CJS, group.CJS) {
  # NOTE: The NULL variables below are actually column names used by ggplot.
  # This definition is just to prevent the package check from issuing a note due unknown variables.
  Group <- NULL

  appendTo("debug", "Starting printProgression_test.")
  ## Absolutes per array per group per release site.
  detailed.absolutes <- lapply(split.CJS, function(x) x$absolutes)
  maxcols <- max(unlist(lapply(detailed.absolutes, ncol)))
  for(i in 1:length(detailed.absolutes)) {
    if (ncol(detailed.absolutes[[i]]) < maxcols)
      detailed.absolutes[[i]] <- detailed.absolutes[[i]][, -1]
  }
  ## Progression dataframe
  the.levels <- c()
  recipient <- compileProgressionDataFrame(status.df = status.df, group.CJS = group.CJS, 
    detailed.absolutes = detailed.absolutes, i = 1, the.levels = the.levels)
  progression <- recipient[[1]]
  the.levels <- recipient[[2]]
  if (length(group.CJS) > 1) { 
    for (i in 2:length(group.CJS)) {
      recipient <- compileProgressionDataFrame(status.df = status.df, group.CJS = group.CJS, 
        detailed.absolutes = detailed.absolutes, i = i, the.levels = the.levels)
      progression <- rbind(progression, recipient[[1]])
      the.levels <- recipient[[2]]
    }
  }
  the.levels <- c(the.levels, "Estimated")
  progression$Fill <- factor(progression$Fill, levels = rev(unique(the.levels)))
  progression$Value[is.na(progression$Value)] <- 0
  progression$Array <- factor(progression$Array, levels = unique(progression$Array))
  progression$Group <- factor(progression$Group, levels = names(group.CJS))

  # prepareFunctions
  abort.additions <- FALSE
  temp <- prepareAdditions(group.CJS = group.CJS, progression = progression)
  if (is.null(temp)) {
    abort.additions <- TRUE
  } else {
    additions <- temp
  }
  totals <- prepareTotals(group.CJS = group.CJS)
  the.final.totals <- prepareFinalTotals (group.CJS = group.CJS, totals = totals, progression = progression)

  # Plot
  the.ceiling <- max(the.final.totals$n)
  cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")
  names(cbPalette) <- c("Orange", "Blue", "Green", "Yellow", "Darkblue", "Darkorange", "Pink", "Grey")
  p <- ggplot2::ggplot(data = progression, ggplot2::aes(x = progression$Array, y = progression$Value))
  # The bars
  p <- p + ggplot2::geom_bar(stat = "identity", ggplot2::aes(fill = progression$Fill))
  # The totals dashed line
  p <- p + ggplot2::geom_line(data = the.final.totals, ggplot2::aes(x = the.final.totals$nArray,
    y = the.final.totals$n, group = the.final.totals$Group), linetype = "dashed", colour = "red")
  # The additions annotation
  if (!abort.additions)
    p <- p + ggplot2::geom_text(data = additions, ggplot2::aes(x = (as.numeric(additions$Array) - 0.5), 
    label = paste0(" +", additions$n, " released")), y = additions$the.ceiling, hjust = 0, vjust = -0.5, colour = "red")
  p <- p + ggplot2::facet_grid(Group ~ .)
  p <- p + ggplot2::theme_bw()
  p <- p + ggplot2::theme(panel.grid.minor.x = ggplot2::element_blank(), 
    panel.grid.major.x = ggplot2::element_blank())
  if (length(levels(progression$Fill)) <= 8) {
    p <- p + ggplot2::scale_fill_manual(values = as.vector(cbPalette)[c(8, 1:length(levels(progression$Fill))-1)])
  }
  p <- p + ggplot2::theme(legend.title = ggplot2::element_blank())
  p <- p + ggplot2::scale_y_continuous(minor_breaks = seq(0, round(the.ceiling * 1.2, 0), 2), limits = c(0, the.ceiling * 1.2))
  p <- p + ggplot2::labs(y = "", x = "")
  ggplot2::ggsave("Report/progression.png", width = 7, height = 6)
  appendTo("debug", "Terminating printProgression_test.")
}


#' Check for unknown receivers in the raw detections
#' 
#' @inheritParams splitDetections
#' @inheritParams loadDetections
#' 
#' @keywords internal

checkEmptyReceivers <- function(spatial, detections){
  appendTo("debug", "Starting emptyReceiversCheck.")
  empty.receivers <- sum(is.na(match(spatial$receivers.serial, unique(detections$Receiver))))
  appendTo(c("Screen", "Report"), paste("M: Number of ALS: ", spatial$number.of.receivers, " (of which ", empty.receivers, " had no detections)", sep = ""))
  if (empty.receivers > 0) 
    appendTo(c("Screen", "Report", "Warning"), paste("W: No detections were found for receiver(s) ", paste(sort(spatial$receivers.serial[is.na(match(spatial$receivers.serial, unique(detections$Receiver)))]), 
      collapse = ", "), ".", sep = ""))
  rm(empty.receivers)
  appendTo("debug", "Terminating emptyReceiversCheck.")
}

#' Check for unknown receivers in the raw detections
#' 
#' @inheritParams splitDetections
#' @inheritParams loadDetections
#' 
#' @keywords internal
#' 
unknownReceiversCheckA <- function(detections, spatial){
  appendTo("debug", "Starting unknownReceiversCheckA.")  
  if (any(trigger <- is.na(match(unique(detections$Receiver), spatial$receivers.serial)))) {
    appendTo(c("Screen", "Report", "Warning"), paste0("W: Detections from receivers ", paste(unique(detections$Receiver)[trigger], collapse = ", "), " are present in the data, but these receivers are not part of the study's stations. Doublecheck potential errors."))
  }
  appendTo("debug", "Terminating unknownReceiversCheckA.")  
}

#' Check for target data in the unknown receivers
#' 
#' @inheritParams groupMovements
#' @inheritParams loadDetections
#' 
#' @return A list containing an updated spatial list and a TRUE/FALSE object indicating whether or not Standard station names must be reprocessed.
#' 
#' @keywords internal
#' 
unknownReceiversCheckB <- function(detections.list, spatial) {
  # NOTE: The NULL variables below are actually column names used by data.table.
  # This definition is just to prevent the package check from issuing a note due unknown variables.
  Receiver <- NULL
  
  appendTo("debug", "Starting unknownReceiversCheckB.")
  reset.names <- FALSE
  for (i in names(detections.list)) {
    if (any(is.na(match(detections.list[[i]][, Receiver], spatial$receivers.serial)))) {
      A <- detections.list[[i]][, Receiver]
      B <- spatial$receivers.serial
      unknown.receivers <- unique(detections.list[[i]][is.na(match(A, B)), Receiver])
      appendTo(c("Screen", "Report", "Warning"), paste("W: Fish ", i, " was detected in one or more receivers that are not listed in the study area (receiver(s): ", paste(unknown.receivers, collapse = ", "), ")!", sep = ""))
      cat("Possible options:\n   a) Stop and doublecheck the data (recommended)\n   b) Temporary include the hydrophone(s) to the stations list\n")
      check <- TRUE
      while (check) {
        decision <- commentCheck(line = "Which option should be followed?(a/b/comment) ", tag = i)
        if (decision == "a" | decision == "A" | decision == "b" | decision == "B") 
          check <- FALSE else cat("Option not recognized, please try again.\n")
        appendTo("UD", decision)
      }
      if (decision == "a" | decision == "A") {
        emergencyBreak()
        stop("Stopping analysis per user command.\n")
      }
      if (decision == "b" | decision == "B") {
        reset.names <- TRUE
        spatial <- deprecated_includeUnknownReceiver(spatial = spatial, unknown.receivers = unknown.receivers)
      }
    }
  }
  appendTo("debug", "Terminating unknownReceiversCheckB.")
  return(list(spatial = spatial, reset.names = reset.names))
}


#' Temporarily include missing receivers in the spatial object
#' 
#' @param unknown.receivers serial number of the receivers to be included
#' @inheritParams loadDetections
#' 
#' @keywords internal
#' 
deprecated_includeUnknownReceiver <- function(spatial, unknown.receivers){
  appendTo("debug", "Starting includeUnknownReceiver.")
  appendTo(c("Screen", "Report"), "M: Including missing receiver(s) in the stations list. Assigning to array 'Unknown'.")
    levels(spatial$stations$Receiver) <- c(levels(spatial$stations$Receiver), unknown.receivers)
  if (all(!grepl("Unknown", levels(spatial$stations$Station.Name)))) 
    levels(spatial$stations$Station.Name) <- c(levels(spatial$stations$Station.Name), "Unknown")
  if (all(!grepl("Unknown", levels(spatial$stations$Array)))) 
    levels(spatial$stations$Array) <- c(levels(spatial$stations$Array), "Unknown")
  for (j in seq_len(length(unknown.receivers))) {
    spatial$stations[nrow(spatial$stations) + 1, "Station.Name"] <- "Unknown"
    spatial$stations[nrow(spatial$stations), "Receiver"] <- unknown.receivers[j]
    spatial$stations[nrow(spatial$stations), "Array"] <- "Unknown"
    spatial$stations[nrow(spatial$stations), "Standard.Name"] <- paste("Ukn.", sum(spatial$stations$Station.Name == "Unknown"), sep = "")
  }
  spatial$receivers.serial <- as.matrix(spatial$stations[, spatial$receiver.columns])[!is.na(as.matrix(spatial$stations[, spatial$receiver.columns]))]
  appendTo("debug", "Terminating includeUnknownReceiver.")
  return(spatial)
}

#' Split detections by tag
#'
#' Splits the detections' table by tags and selects only detections from target tags
#' 
#' @inheritParams actel
#' @inheritParams loadDetections
#' @param bio A table with the tags and biometrics of the studied fish.
#' @param detections A dataframe with all the detections. Supplied by loadDetections.
#' @param silent logical: if TRUE, suppress messages.
#' 
#' @return A list of detections for each tag.
#' 
#' @keywords internal
#' 
deprecated_splitDetections <- function(detections, bio, spatial, exclude.tags = NULL, silent = FALSE) {
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

  for (i in names(trimmed.list)) {
    trimmed.list[[i]] <- assignArrays(input = trimmed.list[[i]], spatial = spatial)
  }

  appendTo("debug", "Terminating splitDetections.")
  return(list(trimmed.list = trimmed.list, bio = bio))
}

#' Load Spatial file and Check the structure
#' 
#' @inheritParams assembleSpatial
#' 
#' @return The spatial dataframe
#' 
#' @keywords internal
#' 
deprecated_loadSpatial <- function(file){
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
  if (!any(grepl("Station.Name", colnames(input)))) {
    appendTo(c("Screen", "Warning", "Report"), "W: No 'Station.Name' column found in the spatial file.\n   Creating a 'Station.Name' column to avoid function failure.")
    input$Station.Name <- paste0("St.", 1:nrow(input))
    input[input$Type == "Release", "Station.Name"] <- paste0("RS.", 1:sum(input$Type == "Release"))
  }
  appendTo("debug","Terminating loadSpatial.")
  return(input)
}

#' Process spatial elements
#' 
#' Creates a list containing multiple spatial elements required throughout the analyses
#' 
#' @param file an input file with spatial data.
#' @inheritParams splitDetections
#' @inheritParams actel
#' 
#' @return A list of 1) stations, 2) release sites, 3) ALS columns in the spatial file, 4) the Number of ASL, 5) The ALS serial numbers and 6) the array order.
#' 
#' @keywords internal
#' 
assembleSpatial <- function(file, bio, sections = NULL) {
  appendTo("debug", "Starting assembleSpatial.")
  input <- deprecated_loadSpatial(file = file)
  # Create standard names
  input <- setSpatialStandards(input = input)
  # Break the stations away
  appendTo("debug", "Creating 'stations'.")
  stations <- input[input$Type == "Hydrophone", -match("Type", colnames(input))]
  stations$Array <- factor(stations$Array, levels = unique(stations$Array))
  appendTo("debug", "Creating 'release.sites'.")
  # Extract serial numbers only
  receiver.columns <- grep("Receiver", colnames(stations))
  for (i in receiver.columns) {
    if (any(grepl("VR", stations[, i]))) {
      stations[, i] <- as.character(stations[, i])
      for (j in 1:nrow(stations)) stations[j, i] <- tail(unlist(strsplit(stations[j, i], "-")), 1)
      rm(j)
      stations[, i] <- as.factor(stations[, i])
    }
  }
  receivers.serial <- as.matrix(stations[, receiver.columns])[!is.na(as.matrix(stations[, receiver.columns]))]
  if (any(link <- table(receivers.serial) > 1)) {
    emergencyBreak()
    stop(ifelse(sum(link) > 1,"Receivers ","Receiver "), paste(names(table(receivers.serial))[link], collapse = ", "), ifelse(sum(link) > 1," are ", " is "), "duplicated in the spatial file.")
  }
  # If there is any release site in the spatial file
  if (sum(input$Type == "Release") > 0) {
    if (length(unique(bio$Release.site)) == 1 && unique(bio$Release.site) == "unspecified") {
      appendTo(c("Screen", "Report", "Warning"), "W: At least one release site has been indicated in the spatial file, but no release sites were specified in the biometrics file.\n   Discarding release site information to avoid script failure. Please doublecheck your data.")
      release.sites <- data.frame(Station.Name = "unspecified", 
                                  Longitude = NA, 
                                  Latitude = NA, 
                                  Array = stations$Array[1])
    } else {
      A <- input$Station.Name[input$Type == "Release"]
      B <- unique(bio$Release.site)
      if (any(is.na(match(B, A)))) {
        appendTo(c("Screen", "Report", "Warning"), "Error: There is a mismatch between the release sites reported and the release locations for the smolts.")
        cat("   Release sites listed in the spatial file:", paste(A, collapse = ", "), "\n")
        cat("   Sites listed in the biometrics file 'Release.site' column:", paste(B, collapse = ", ", "\n"))
        emergencyBreak()
        stop("The release names should be identical in the spatial objects file and in the biometrics file.\n")
      } else {
        from.row <- input$Type == "Release"
        from.col <- colnames(input)[!grepl("Receiver", colnames(input))]
        release.sites <- input[from.row, from.col]
        row.names(release.sites) <- 1:nrow(release.sites)
      }
      A <- unique(stations$Array)
      B <- unique(release.sites$Array)
      if (any(is.na(match(B, A)))) {
        appendTo(c("Screen", "Report", "Warning"), "Error: There is a mismatch between the expected first array of a release site and the list of arrays.")
        cat("   Arrays listed in the spatial file:", paste(A, collapse = ", "), "\n")
        cat("   Expected first arrays of the release sites:", paste(B, collapse = ", ", "\n"))
        emergencyBreak()
        stop("The expected first arrays should match the arrays where stations where deployed in the spatial file.\n")
      }
    }
  } else {
    if (length(unique(bio$Release.site)) > 1){
      appendTo("Screen", "M: Release sites were not specified in the spatial file but more than one release site is reported in the biometric data.\n   Assuming all released fish go through all receiver arrays.")
    }
    release.sites <- data.frame(Station.Name = unique(bio$Release.site), 
                                Longitude = NA,
                                Latitude = NA, 
                                Array = rep(stations$Array[1], length(unique(bio$Release.site))))
  }
  # Wrap up
  number.of.receivers <- sum(!is.na(stations[, receiver.columns]))
  if (!is.null(sections)) {
    array.order <- list()  # Used to determine if the fish's last detection was in the last array of a given section
    for (j in sections) {
      array.order[[j]] <- levels(stations$Array)[grepl(j, levels(stations$Array))]
    }
    if (any(trigger <- unlist(lapply(array.order,length)) == 0)) {
      appendTo(c("Screen", "Warning"), decision <- readline(paste("W: No arrays were found that match section(s) ",paste(names(array.order)[trigger], collapse = ", "), ". There could be a typing mistake!\n   Continue the analysis?(y/N) ", sep = "")))
      if (decision != "y" | decision != "Y" ){
        emergencyBreak()
        stop("Stopping analysis per user command.\n")
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
                 receiver.columns = receiver.columns, 
                 number.of.receivers = number.of.receivers, 
                 receivers.serial = receivers.serial, 
                 array.order = array.order)
  appendTo("debug", "Terminating assembleSpatial.")
  return(output)
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

