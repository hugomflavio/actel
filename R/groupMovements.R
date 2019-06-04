#' Group movements
#'
#' Crawls trough the detections of each fish and groups them based on ALS arrays and time requirements.
#' 
#' @inheritParams actel
#' @inheritParams assembleEfficiency
#' @inheritParams assembleOutput
#' 
#' @return A list of movement events for each fish.
#' 
#' @keywords internal
#' 
groupMovements <- function(detections.list, bio, spatial, speed.method, maximum.time, dist.mat, invalid.dist) {
  appendTo("debug", "Starting groupMovements.")
  movements <- list()
  round.points <- roundDown(seq(from = length(detections.list)/10, to = length(detections.list), length.out = 10), to = 1)
  counter <- 1
  {
    pb <- txtProgressBar(min = 0, max = sum(unlist(lapply(detections.list, nrow))), style = 3, width = 60)
    for (i in names(detections.list)) {
      appendTo("debug", paste("Debug: (Movements) Analysing fish ", i, ".", sep = ""))
      if (invalid.dist) {
        recipient <- matrix(nrow = 1, ncol = 8)
      } else {
        recipient <- matrix(nrow = 1, ncol = 9)
      }
      recipient <- as.data.frame(recipient)
      if (invalid.dist) {
        colnames(recipient) <- c("Array", "Detections", "First station", "Last station", "First time", "Last time", "Time travelling", "Time on array")        
      } else {
        colnames(recipient) <- c("Array", "Detections", "First station", "Last station", "First time", "Last time", "Time travelling", "Time on array", "Average speed m.s")
      }
      j = 1
      z = 1
      time.limit <- TRUE
      brake <- nrow(detections.list[[i]])
      # While the row number is smaller or equal to the total number of rows
      while (j <= brake) {
        # For each station array
        for (k in levels(spatial$stations$Array)) {
          # if there are still rows to go and if the detection array matches the array being analysed
          if (j <= brake) 
          if (detections.list[[i]][j, "Array"] == k) {
            recipient[z, "Array"] = k
            recipient[z, "Detections"] = 0
            recipient[z, "First station"] = paste(detections.list[[i]][j, "Standard.Name"])
            recipient[z, "First time"] = paste(detections.list[[i]][j, 1])
            # while 1) the array remains the same 2) the row number is smaller or equal to the total number of rows 3) the time limit is not broken (default is 1 hour)
            while (detections.list[[i]][j, "Array"] == k & j <= brake & time.limit) {
              recipient[z, "Detections"] = recipient[z, "Detections"] + 1
              # If the row number is smaller than the total number of rows
              if (j < brake) 
                time.limit <- difftime(detections.list[[i]][j + 1, 1], detections.list[[i]][j, 1], units = "mins") < maximum.time
              j = j + 1
              counter = counter + 1
            }
            time.limit <- TRUE
            recipient[z, "Last station"] = paste(detections.list[[i]][j - 1, "Standard.Name"])
            recipient[z, "Last time"] = paste(detections.list[[i]][j - 1, 1])
            z = z + 1
            if (i == tail(names(detections.list), 1)) 
              counter <- sum(unlist(lapply(detections.list, nrow)))
            setTxtProgressBar(pb, counter)
            flush.console()
          }
        }
      }
      
      upstreamCheck(i = i, recipient = recipient, bio = bio, spatial = spatial)
      
      if (nrow(recipient) >= 1) {
        if (!invalid.dist)
          movements[[length(movements) + 1]] <- movementSpeeds(movements = recipient, 
            speed.method = speed.method, dist.mat = dist.mat, silent = FALSE)
        else
          movements[[length(movements) + 1]] <- movementTimes(movements = recipient,
            silent = FALSE)
        names(movements)[length(movements)] <- i
      }
      rm(recipient)
    }
    close(pb)
    rm(pb)
    rm(i, counter, round.points)
  }
  appendTo("debug", "Done.")
  return(movements)
}

#' Check for movements upstream fo the release site.
#'
#' @inheritParams assembleEfficiency
#' @inheritParams assembleOutput
#' @param i The tag being analysed.
#' @param recipient The movement events table for the tag.
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
    print(recipient)
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
        while (check) {
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
              recipient <- recipient[-row.line, ]
              if (nrow(recipient) > 1){
                decision <- readline("Do you want to remove more movement events?(y/N) ")
              } else {
                appendTo(c("Screen","Report"), paste("M: All movement events for fish ",i," were deleted by user command. Excluding it from further analysis.", sep = ""))
                decision <- "N"
              }
              if (decision == "Y" | decision == "y") {
                check = TRUE
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
  appendTo("debug", "Done.")
}

#' Removes discarded events
#' 
#' Refine the movement events based on the actel results. 
#'
#' @param fish The transmitter to be refined
#' @inheritParams actel
#' @inheritParams assembleOutput
#' 
#' @return The movement dataframe containing only valid events
#' 
#' @export
#' 
refineMovements <- function(fish, sections, status.df, movements){
  the.row <- match(fish,status.df$Transmitter)
  for (i in sections) {
    section.start <- status.df[the.row, paste("Arrived", i, sep = ".")]
    section.end <- status.df[the.row, paste("Left", i, sep = ".")]
    move.rows <- grep(i,movements$Array)
    if (is.na(section.start)){
      movements <- movements[-move.rows,]
    } else {
      if (any(check <- movements[move.rows,"First time"] < section.start | movements[move.rows, "Last time"] > section.end)) {
        movements <- movements[-move.rows[check],]
      }
    }
  }
  return(movements)
}