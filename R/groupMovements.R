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
groupMovements <- function(detections.list, bio, spatial, speed.method, maximum.time, 
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
        # recipient <- matrix(nrow = 1, ncol = 9)
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
        # recipient <- matrix(nrow = 1, ncol = 10)
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
      # recipient <- as.data.frame(recipient)
      # if (invalid.dist) {
        # colnames(recipient) <- c("Array", "Detections", "First.station", "Last.station", "First.time", "Last.time", "Time.travelling", "Time.on.array", "Valid")        
      # } else {
        # colnames(recipient) <- c("Array", "Detections", "First.station", "Last.station", "First.time", "Last.time", "Time.travelling", "Time.on.array", "Average.speed.m.s", "Valid")
      # }
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

      recipient <- upstreamCheck(i = i, recipient = recipient, bio = bio, spatial = spatial)
      
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
upstreamCheck <- function(i, recipient, bio, spatial) {
  appendTo("debug", paste("Starting upstreamCheck for fish ", i, ".", sep = ""))
  t1 <- bio[matchl(bio$Signal, tail(strsplit(i, "-")[[1]], 1)), "Release.site"]  # The release site from the target fish
  t2 <- spatial$release.sites[matchl(spatial$release.sites$Station.Name, t1), "Array"]  # The expected hydrophone array for that release site
  t3 <- match(t2, unlist(spatial$array.order))  # The position of the array relative to the study area
  if (is.na(t3)) {
    appendTo("Report", "Error: There is a mismatch between the expected first array of a release site and the list of arrays.\nPlease correct this before continuing.")
    emergencyBreak()
    stop("There is a mismatch between the expected first array of a release site and the list of arrays.\nPlease correct this before continuing.\n")
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
        stop("Script stopped by user command.")
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
            # rownames(recipient) <- 1:nrow(recipient)
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

#' Removes invalid events
#' 
#' Remove invalid movement events and recalculte times/speeds.
#'
#' @inheritParams actel
#' @inheritParams groupMovements
#' @param movements A list of movements for each target tag, created by groupMovements.
#' 
#' @keywords internal
#' 
#' @return The movement dataframe containing only valid events
#' 
simplifyMovements <- function(movements, bio, speed.method, dist.mat, invalid.dist) {
  simple.movements <- lapply(movements, function(x) x[(Valid), ])
  simple.movements <- simple.movements[unlist(lapply(simple.movements, nrow)) > 0]
  for(fish in names(simple.movements)){
    aux <- movementTimes(movements = simple.movements[[fish]], silent = FALSE)
    if (!invalid.dist)
        aux <- movementSpeeds(movements = aux, speed.method = speed.method, dist.mat = dist.mat, silent = FALSE)
    simple.movements[[fish]] <- speedReleaseToFirst(fish = fish, bio = bio, movements = aux,
     dist.mat = dist.mat, invalid.dist = invalid.dist, silent = FALSE)
  }
  return(simple.movements)
}
