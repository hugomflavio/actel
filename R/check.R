#' Check report compatibility
#' 
#' Creates a "Report" folder if necessary and silently activates ggplot2 and reshape2 to avoid startup messages
#'
#' @return A TRUE/FALSE decision
#' 
#' @keywords internal
#' 
checkReport <- function(report){
  if (report) {
    appendTo("Report", "M: 'report' option has been activated.")
    if (length(setdiff(c("ggplot2", "reshape2"), rownames(installed.packages()))) > 0) {
      appendTo(c("Screen", "Report", "Warning"), "W: 'report' can only be activated if 'ggplot2' and 'reshape2' are installed. Please install these. Deactivating 'Report' for the current job.")
      report <- FALSE
    } 
    if (!rmarkdown::pandoc_available()) {
      appendTo(c("Screen", "Report", "Warning"), "W: 'report' can only be activated if pandoc is installed. You can install pandoc at: https://pandoc.org/installing.html\n   You can also check if pandoc is available to R by running rmarkdown::pandoc_available()")
      cat("Would you like to:\n\n  a) Continue with 'report' set to FALSE\n  b) Stop the analysis and install pandoc.\n\n")
      check <- TRUE
      while (check) {
        decision <- readline("Decision:(a/b) ")
        appendTo("UD", decision)
        if (decision == "a" | decision == "A") {
          appendTo(c("Screen", "Report", "Warning"), "W: Deactivating 'report' to prevent function failure.")
          report <- FALSE
          check <- FALSE
        }
        if (decision == "b" | decision == "B") {
          emergencyBreak()
          stop("Analysis stopped per user command.\n", call. = FALSE)        
        }
        if (check)
          appendTo("Screen", "Option not recognised; please input either 'a' or 'b'.\n")
      }
    }
    if (report) {
      if (!dir.exists("Report")) {
        appendTo("Screen", "M: Creating 'Report' subdirectory to store report files.")
        dir.create("Report")
      } else {
        appendTo("Screen", "W: 'Report' directory already present. Overwriting files already present.")
      }
    }
  }
  return(report)
}

#' Check path validity
#' 
#' Confirms that the target directory exists.
#' 
#' @inheritParams actel
#' 
#' @keywords internal
#' 
checkPath <- function(my.home, path) {
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
          stop("Analysis stopped per user command.\n", call. = FALSE)
        }
        if (unknown.input) {
          cat("Option not recognised, please input either 'a' or 'b'.\n")
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


#' Check for movements upstream of the release site.
#'
#' @inheritParams splitDetections
#' @inheritParams loadDetections
#' 
#' @return The updated movements
#' 
#' @keywords internal
#' 
checkUpstream <- function(movements, bio, spatial, arrays) {
  # NOTE: The NULL variables below are actually column names used by data.table.
  # This definition is just to prevent the package check from issuing a note due unknown variables.
  Array <- NULL
  for (fish in names(movements)) {
    appendTo("debug", paste("Starting checkUpstream for fish ", fish, ".", sep = ""))
    release.site <- as.character(bio$Release.site[na.as.false(bio$Transmitter == fish)])
    release.array <- as.character(with(spatial, release.sites[release.sites$Standard.Name == release.site, "Array"]))
    downstream.arrays <- c(release.array, arrays[[release.array]]$downstream)
    if (any(is.na(match(movements[[fish]][Array != "Unknown"]$Array, downstream.arrays)))) {
      cat("\n")
      appendTo(c("Screen", "Report", "Warning"), paste0("W: Fish ", fish, " was detected behind its release site! Opening relevant data for inspection."))
      appendTo("Screen", paste("   Release site:", release.site))
      appendTo("Screen", paste("   Expected first array:", release.array))
      cat(paste("\n   Movement table for fish ", fish, ":\n", sep =""))
      print(movements[[fish]])
      cat("\n")
      appendTo("Screen", "You may either:\n  a) Stop the analysis if the expected first array is wrong;\n  b) Continue as is (does not impact the results);\n  c) Render a movement event invalid, if you are confident it is a false detection.")
      cat("\n")
      unknown.input = TRUE
      while (unknown.input) {
        decision <- commentCheck(line = "Decision:(a/b/c/comment) ", tag = fish)
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
          appendTo("Screen", "Note: You can select multiple events at once by separating them with a space.")
          while (check) {
            the.string <- commentCheck(line = "Events to be rendered invalid: ", tag = fish)
            the.rows <- suppressWarnings(as.integer(unlist(strsplit(the.string, "\ "))))
            appendTo("UD", the.string)
            if (all(is.na(the.rows))) {
              decision <- readline("W: The input could not be recognised as row numbers, would you like to abort the process?(y/N) ")
              appendTo("UD", decision)
              if (decision == "y" | decision == "Y") {
                appendTo("Screen", "Aborting.")                 
                check <- FALSE
              } else {
                check <- TRUE
              }
            } else {
              if (any(is.na(the.rows))) {
                appendTo("Screen", "W: Part of the input could not be recognised as a row number.")
                the.rows <- the.rows[!is.na(the.rows)]
              }
              if (all(the.rows > 0 & the.rows <= nrow(movements[[fish]]))) {
                decision <- readline(paste0("Confirm: Would you like to render event(s) ", paste(the.rows, collapse = ", "), " invalid?(y/N) "))
                appendTo("UD", decision)
                if (decision == "y" | decision == "Y") {
                  movements[[fish]]$Valid[the.rows] <- FALSE
                  attributes(movements[[fish]])$p.type <- "Manual"
                  appendTo(c("Screen", "Report"), paste0("M: Movement event(s) ", paste(the.rows, collapse = ", "), " from fish ", fish," rendered invalid per user command."))
                  decision <- readline("Would you like to render any more movements invalid?(y/N) ")
                  appendTo("UD", decision)
                  if (decision == "y" | decision == "Y") {
                    check <- TRUE
                    appendTo("Screen", paste0("M: Updated movement table of fish ", fish, ":"))
                    print(movements[[fish]])
                    appendTo("Screen", "Note: You can select multiple events at once by separating them with a space.")
                  } else {
                    check <- FALSE
                  }
                }
              } else {
                appendTo("Screen", paste0("Please select only events within the row limits (1-", nrow(movements[[fish]]),")."))
                check <- TRUE
              }
            }
          } # end while
        }
        if (unknown.input) {
          appendTo("Screen", "Option not recognized, please input either 'a', 'b', 'c' or 'comment'.")
        }
      }
    }
  }
  appendTo("Debug", "Terminating checkUpstream.")
  return(movements)
}

#' Check if fish are jumping over arrays
#' 
#' @inheritParams simplifyMovements
#' @inheritParams splitDetections
#' @inheritParams dotPaths
#' @inheritParams explore
#' @inheritParams createStandards
#' 
#' @return a checked movements list
#' 
#' @keywords internal
#' 
checkJumpDistance <- function(movements, bio, spatial, dotmat, jump.warning = 2, jump.error = 3) {
  # NOTE: The NULL variables below are actually column names used by data.table.
  # This definition is just to prevent the package check from issuing a note due unknown variables.
  Valid <- NULL
  for (fish in names(movements)) {
    # Check release-to-first
    release <- as.character(bio$Release.site[na.as.false(bio$Transmitter == fish)])
    release.array <- with(spatial, release.sites[release.sites$Standard.Name == release, "Array"])
    release.jump <- dotmat[as.character(release.array), as.character(movements[[fish]]$Array[1])] + 1
    if (release.jump > jump.warning) {
      # Trigger warning
      appendTo(c("Report", "Warning", "Screen"), 
        paste0("W: Fish ", fish, " jumped through ", release.jump - 1, 
          ifelse(release.jump > 2, " arrays ", " array "), 
          "from release to first event (Release -> ", movements[[fish]]$Array[1], ")."))
    }
    if (release.jump > jump.error)
      trigger.error <- TRUE
    else
      trigger.error <- FALSE
    # Check event-to-event
    if (nrow(movements[[fish]]) > 1) {
      A <- movements[[fish]]$Array[-nrow(movements[[fish]])]
      B <- movements[[fish]]$Array[-1]
      aux <- cbind(A, B)
      jumps <- apply(aux, 1, function(x) dotmat[x[1], x[2]])
      names(jumps) <- paste(A, "->", B)
      if (any(link <- which(jumps > jump.warning))) {
        for (i in 1:length(link)) {
          # Trigger warning
          appendTo(c("Report", "Warning", "Screen"), 
            paste0("W: Fish ", fish, " jumped through ", jumps[link[i]] - 1, 
              ifelse(jumps[link[i]] > 2, " arrays ", " array "), 
              "in events ", link[i], " -> ", link[i] + 1, " (", names(jumps)[link[i]], ")."))
        }
      }
      if (any(jumps[link] > jump.error))
        trigger.error <- TRUE
    }
    # Trigger user interaction
    if (trigger.error) {
      appendTo("Screen", paste0("M: Opening movement table of fish ", fish, " for inspection:"))
      print(movements[[fish]])
      decision <- commentCheck(line = "Would you like to render any movement event invalid?(y/N/comment) ", tag = fish)
      appendTo("UD", decision)
      if (decision == "y" | decision == "Y") {
        appendTo("Screen", "Note: You can select multiple events at once by separating them with a space.")
        check <- TRUE
        while (check) {
          the.string <- commentCheck(line = "Events to be rendered invalid: ", tag = fish)
          the.rows <- suppressWarnings(as.integer(unlist(strsplit(the.string, "\ "))))
          appendTo("UD", the.string)
          if (all(is.na(the.rows))) {
            decision <- readline("W: The input could not be recognised as row numbers, would you like to abort the process?(y/N) ")
            appendTo("UD", decision)
            if (decision == "y" | decision == "Y") {
              appendTo("Screen", "Aborting.")                 
              check <- FALSE
            } else {
              check <- TRUE
            }
          } else {
            if (any(is.na(the.rows))) {
              appendTo("Screen", "W: Part of the input could not be recognised as a row number.")
              the.rows <- the.rows[!is.na(the.rows)]
            }
            if (all(the.rows > 0 & the.rows <= nrow(movements[[fish]]))) {
              decision <- readline(paste0("Confirm: Would you like to render event(s) ", paste(the.rows, collapse = ", "), " invalid?(y/N) "))
              appendTo("UD", decision)
              if (decision == "y" | decision == "Y") {
                movements[[fish]]$Valid[the.rows] <- FALSE
                attributes(movements[[fish]])$p.type <- "Manual"
                appendTo(c("Screen", "Report"), paste0("M: Movement event(s) ", paste(the.rows, collapse = ", "), " from fish ", fish," rendered invalid per user command."))
                decision <- readline("Would you like to render any more movements invalid?(y/N) ")
                appendTo("UD", decision)
                if (decision == "y" | decision == "Y") {
                  check <- TRUE
                  appendTo("Screen", paste0("M: Updated movement table of fish ", fish, ":"))
                  print(movements[[fish]])
                  appendTo("Screen", "Note: You can select multiple events at once by separating them with a space.")
                } else {
                  check <- FALSE
                }
              }
            } else {
              appendTo("Screen", paste0("Please select only events within the row limits (1-", nrow(movements[[fish]]),")."))
              check <- TRUE
            }
          }
        } # end while
      }
    } # end trigger error
  }
  return(movements)
}

#' Confirm that receivers were not re-deployed before being retrieved
#' 
#' @param input the table of deployments
#' 
#' @keywords internal
#' 
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
        stop("Fatal exception found. Read lines above for more details.\n", call. = FALSE)      
      }
    }
  }
  appendTo("debug","Terminating checkDeploymentTimes")
}

#' Confirm that the station names in the deployments table match those listed in the spatial file
#' 
#' @inheritParams checkDeploymentTimes
#' @param spatial The spatial data frame, as loaded by loadSpatial
#' 
#' @keywords internal
#' 
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
    stop("Fatal exception found. Read lines above for more details.\n", call. = FALSE)      
  }
  appendTo("debug","Terminating checkDeploymentStations")
  return(input)
}

#' Find detections from unknown receivers
#' 
#' @param input The detections data frame
#' 
#' @keywords internal
#' 
checkUnknownReceivers <- function(input) {
  appendTo("debug","Terminating unknownReceivers")
  unknown <- is.na(input$Standard.Name)
  if (any(unknown)) {
    appendTo(c("Screen", "Report", "Warning"), paste0("W: Detections from receivers ", paste(unique(input$Receiver[unknown]), collapse = ", "), " are present in the data, but these receivers are not part of the study's stations. Doublecheck potential errors."))
  }
  appendTo("debug","Terminating unknownReceivers")
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
checkTagsInUnknownReceivers <- function(detections.list, deployments, spatial) {
  # NOTE: The NULL variables below are actually column names used by data.table.
  # This definition is just to prevent the package check from issuing a note due unknown variables.
  Receiver <- NULL

  appendTo("debug", "Starting tagsInUnknownReceivers")
  for (i in names(detections.list)) {
    if (any(is.na(detections.list[[i]]$Standard.Name))) {
      A <- detections.list[[i]]$Receiver
      B <- names(deployments)
      unknown.receivers <- unique(detections.list[[i]][is.na(match(A, B)), Receiver])
      appendTo(c("Screen", "Report", "Warning"), paste("W: Fish ", i, " was detected in one or more receivers that are not listed in the study area (receiver(s): ", paste(unknown.receivers, collapse = ", "), ")!", sep = ""))
      cat("Possible options:\n   a) Stop and doublecheck the data (recommended)\n   b) Temporary include the hydrophone(s) to the stations list\n")
      check <- TRUE
      while (check) {
        decision <- commentCheck(line = "Which option should be followed?(a/b/comment) ", tag = i)
        if (decision == "a" | decision == "A" | decision == "b" | decision == "B") 
          check <- FALSE 
        else 
          cat("Option not recognized, please try again.\n")
        appendTo("UD", decision)
      }
      if (decision == "a" | decision == "A") {
        emergencyBreak()
        stop("Stopping analysis per user command.\n", call. = FALSE)
      }
      if (decision == "b" | decision == "B") {
        recipient <- includeUnknownReceiver(spatial = spatial, deployments = deployments, unknown.receivers = unknown.receivers)
        spatial <- recipient[[1]]
        deployments <- recipient[[2]]
      }
    }
  }
  appendTo("debug", "Terminating unknownReceiversCheckB.")
  return(list(spatial = spatial, deployments = deployments))
}

#' Check if there are detections for the target tags before release.
#'
#' @param input list of detections
#' @inheritParams splitDetections
#' 
#' @keywords internal
#' 
#' @return The detections list without invalid detections.
#' 
checkDetectionsBeforeRelease <- function(input, bio){
  appendTo("debug", "Starting detectionBeforeReleaseCheck.")  
  remove.tag <- NULL
  tag.list <- stripCodeSpaces(names(input))
  link <- match(bio$Signal, tag.list)
  for(i in seq_len(length(link))) {
    if (!is.na(link[i])) {
      if (any(to.remove <- !(input[[link[i]]]$Timestamp > bio$Release.date[i]))) {
        appendTo(c("Screen", "Warning", "Report"), paste0("Error: Fish ", names(input)[link[i]], " was detected before being released!"))
        appendTo("Screen", paste0("  Release time: ", bio$Release.date[i]))
        appendTo("Screen", paste0("  First detection time: ", input[[link[i]]]$Timestamp[1]))
        appendTo("Screen", paste0("  Number of detections before release: ", sum(to.remove)))
        cat("\n")
        appendTo("Screen", "You may either:\n  a) Stop the analysis and check the data;\n  b) Discard the before-release detections and continue.")
        cat("\n")
        unknown.input = TRUE
        while (unknown.input) {
          decision <- commentCheck(line = "Decision:(a/b/comment) ", tag = bio$Transmitter[i])
          if (decision == "a" | decision == "A") {
            unknown.input = FALSE
            emergencyBreak()
            stop("Script stopped by user command.", call. = FALSE)
          }
          if (decision == "b" | decision == "B")
            unknown.input = FALSE
          if (unknown.input)
            cat("Option not recognised, please input either 'a' or 'b'.\n")
        }
        appendTo("UD", decision)
        if (decision == "b" | decision == "B") {
          if (all(to.remove)) {
            appendTo(c("Screen", "Warning", "Report"), paste0("W: ALL detections from Fish ", names(input)[link[i]], " were removed per user command."))
            remove.tag <- c(remove.tag, link[i])
          } else {
            input[[link[i]]] <- input[[link[i]]][!to.remove, ]
            appendTo(c("Screen", "Warning", "Report"), paste0("M: ", sum(to.remove), " detections from Fish ", names(input)[link[i]], " were removed per user command."))
          }
        }
      }
    }
  }
  if (!is.null(remove.tag)) {
    input <- input[-remove.tag]
  }
  appendTo("debug", "Terminating detectionBeforeReleaseCheck.")  
  return(input)
}

#' Check if there are detections matching the target tags.
#'
#' @param input list of detections
#' @inheritParams splitDetections
#' 
#' @keywords internal
#' 
checkNoDetections <- function(input, bio){
  appendTo("debug", "Starting noDetectionsCheck.")  
  tag.list <- stripCodeSpaces(names(input))
  link <- match(bio$Signal, tag.list)
  if (all(is.na(link))) {
    appendTo(c("Screen", "Report"), "M: No detections were found in the input data which matched the target signals.")
    emergencyBreak()
    stop("Stopping analysis due to absence of valid detections.\n", call. = FALSE)
  }
  appendTo("debug", "Terminating noDetectionsCheck.")  
  return(list(list = tag.list,link = link))
}

#' Check if there are duplicated signals in the detected tags.
#'
#' @param input list of detections
#' @inheritParams splitDetections
#' @param tag.list list of the target signals
#' 
#' @keywords internal
#' 
checkDupSignals <- function(input, bio, tag.list){
  appendTo("debug", "Starting dupSignalsCheck.")  
  failsafe <- match(tag.list, bio$Signal)
  if (any(table(failsafe) > 1)) {
    appendTo(c("Screen", "Report"), "Error: One or more signals match more than one tag in the detections! Showing relevant signals/tags.")
    t1 <- cbind(names(input), bio$Signal[failsafe])
    t2 <- t1[complete.cases(t1), ]
    t3 <- table(t2[, 1], t2[, 2])
    rm(t1, t2)
    dupsig <- data.frame(Signal = colnames(t3)[apply(t3, 2, sum) > 1], Tags = NA, stringsAsFactors = FALSE)
    for (i in seq_len(nrow(dupsig))) dupsig$Tags[i] <- paste(row.names(t3)[t3[, dupsig$Signal[i]] == 1], collapse = ", ")
    rm(t3)
    for (i in seq_len(nrow(dupsig))) appendTo(c("Screen", "Report"), paste("   Signal ", dupsig$Signal[i], " was found on tags ", dupsig$Tags[i], ".", sep = ""))
    emergencyBreak()
    stop("Fatal exception found. Stopping analysis.\n", call. = FALSE)
  }
  appendTo("debug", "Terminating dupSignalsCheck.")  
}
