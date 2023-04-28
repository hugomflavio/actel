#' user_interaction.R arguments
#'
#' @param tag The tag being analysed.
#' @param detections The detections data.frame for a specific tag.
#' @param force Logical: If TRUE, the user is moved directly to indicating which movements should be invalidated.
#' @param trigger The message/warning that triggered the interaction
#' @param moves,all.moves The complete movements table for a specific tag.
#' @param displayed.moves The valid movements table for a specific tag.
#' @param event The event selected for expansion.
#' @param GUI One of "needed", "always" or "never". If "needed", a new window is
#'  opened to inspect the movements only if the movements table is too big to be
#'  displayed in R's console. If "always", a graphical interface is always created
#'  when the possibility to invalidate events emerges. If "never", a graphical
#'  interface is never invoked. In this case, if the table to be displayed does
#'  not fit in R's console, a temporary file will be saved and the user will be
#'  prompted to open and examine that file. Defaults to "needed".
#' @param save.tables.locally Logical: If a table must be temporarily stored into a file
#'  for user inspection, should it be saved in the current working directory, or
#'  in R's temporary folder?
#' 
#' @name user_interaction_args
#' @keywords internal
#'
NULL

#' Wrap frequently used code to handle user input
#' 
#' @param question The question to be asked
#' @param choices The accepted inputs. Leave empty for any input
#' @param tag the tag code (for comments only)
#' @param hash A string to attach to the decision in the UD. Ignored if input already has a hash string
#' 
#' @keywords internal
#' 
userInput <- function(question, choices, tag, hash) {
  appendTo("debug", "Running userInput.")
  if (interactive()) { # nocov start
    try.again <- TRUE
    
    while (try.again) {
      decision <- readline(question)
      aux <- strsplit(as.character(decision), "[ ]*#")[[1]]
      if (length(aux) == 0)
        output <- ""
      else
        output <- tolower(aux[1])
      
      if (!missing(choices) && is.na(match(output, choices))) {
        appendTo("Screen", paste0("Option not recognized, please choose one of: '", paste0(choices, collapse = "', '"), "'."))
        output <- NULL
      }
      
      if (!is.null(output)) {
        if (output == "comment") {
          if (missing(tag)) {
            warning("A comment was requested but that option is not available here. Please try again.", immediate. = TRUE, call. = FALSE)
          } else {
            appendTo("UD", paste("comment # on", tag))
            appendTo(c("UD", "Comment"), readline(paste0("New comment on tag ", tag, ": ")), tag)
            appendTo("Screen", "M: Comment successfully stored, returning to the previous interaction.")
          }
        } else {
          try.again <- FALSE
        }
      }
    } 
    
    if (length(aux) == 1 & !missing(hash))
      appendTo("UD", paste(decision, hash))
    else
      appendTo("UD", paste(decision))

  } else { # nocov end
    if (any(choices == "n"))
      output <- "n"
    if (any(choices == "b"))
      output <- "b"
  }
  return(output)
}


#' Handler for table interaction events
#'
#' @inheritParams user_interaction_args
#'
#' @return A data frame with the updated movements table
#'
#' @keywords internal
#'
tableInteraction <- function(moves, detections, tag, trigger, GUI, force = FALSE, save.tables.locally) { # nocov start
  Valid <- NULL
  appendTo("debug", "Running tableInteraction.")
  if (GUI == "never")
    popup <- FALSE
  if (GUI == "needed") {
    if ((sum(moves$Valid) * ncol(moves)) > min((800 * ncol(moves)), (getOption("max.print") - 2)))
      popup <- TRUE
    else
      popup <- FALSE
  }
  if (GUI == "always")
    popup <- TRUE

  if (popup) {
    output <- graphicalInvalidate(moves = moves, detections = detections, tag = tag, trigger = trigger)
    decision <- userInput(paste0("Would you like to leave a comment for tag ", tag, "?(y/n) "), 
                          choices = c("y", "n"),
                          hash = paste0("# comment ", tag, "?"))
    if (decision == "y") {
      appendTo("UD", paste("comment # on", tag))
      appendTo(c("UD", "Comment"), readline(paste0("New comment on tag ", tag, ": ")), tag)
      appendTo("Screen", "M: Comment successfully stored. Continuing analysis.")
    }
  } else {
    # check if table can be printed on console
    if (colnames(moves)[1] == "Section")
      outside.console <- sum(moves$Valid) > min(800, (floor(getOption("max.print") / (ncol(moves) - 2)) - 1))
    else
      outside.console <- sum(moves$Valid) > min(800, (floor(getOption("max.print") / ncol(moves)) - 1))
    # avoid issue #43
    if (R.Version()$major < 4) {
      if (colnames(moves)[1] == "Section")
        outside.console <- outside.console | nchar(paste0(capture.output(print(moves[moves$Valid, -c(5, 7)], topn = sum(moves$Valid))), collapse = "\n")) > 8000
      else
        outside.console <- outside.console | nchar(paste0(capture.output(print(moves[(Valid)], topn = sum(moves$Valid))), collapse = "\n")) > 8000
    }
    # ---
    # make decision
    if (outside.console) {
      if (save.tables.locally)
        target.file <- "actel_inspect_movements.csv"
      else
        target.file <- paste0(tempdir(), '/actel_inspect_movements.csv')
      
      # save file
      to.display <- data.table::as.data.table(cbind(data.frame(Event = 1:sum(moves$Valid)), moves[(Valid)]))
      data.table::fwrite(to.display, target.file, dateTimeAs = "write.csv", showProgress = FALSE)
      # display instructions
      message("M: The movements table for tag '", tag, "' is too large to display on the console and GUI is set to 'never'.\n   Temporarily saving the table to '", target.file, "'.\n   Please inspect this file and decide if any events should be considered invalid.")
      if (save.tables.locally)
        message("   This file will be automatically deleted once it has served its purpose.")
      else
        message("   Can't find the file? Check out the save.tables.locally argument.")
      message("Please use the 'Event' column as a reference for the event number. ", sum(!moves$Valid), " invalid event(s) omitted.")
      flush.console()
      # start interaction
      if (force) {
        output <- invalidateEvents(displayed.moves = moves[(Valid)], # beware: table interaction blindly calls to the first column of "from".
                                   all.moves = moves, 
                                   detections = detections, 
                                   tag = tag,
                                   GUI = GUI)
      } else {
        if (colnames(moves)[1] == "Section")
          text.to.display <- "Would you like to render any movement event invalid?(y/n/comment) "
        else
          text.to.display <- "Would you like to render any movement event invalid, or expand an event?(y/n/comment) "
        
        decision <- userInput(text.to.display,
                              choices = c("y", "n", "comment"), 
                              tag = tag, 
                              hash = paste0("# invalidate/expand moves in ", tag, "?"))
        if (decision == "y") {
          output <- invalidateEvents(displayed.moves = moves[(Valid)], # beware: table interaction blindly calls to the first column of "from".
                                    all.moves = moves, 
                                    detections = detections, 
                                    tag = tag,
                                    GUI = GUI,
                                    save.tables.locally = save.tables.locally)
        } else {
          output <- moves
        }
      }
      first.try <- TRUE
      while (file.exists(target.file)) {
        if (!suppressWarnings(file.remove(target.file))) {
          if (first.try) {
            warning("Please close the currently open 'actel_inspect_movements.csv' file so the analysis can continue", immediate. = TRUE, call. = FALSE); flush.console()
            first.try <- FALSE
          }
        }
      }
    } else {
      if (colnames(moves)[1] == "Section") {
        appendTo("Screen", paste0("Opening ", tag, "'s section movement events for inspection (", sum(!moves$Valid), " invalid event(s) omitted):"))
        to.display <- moves[, -c(5, 7)]
      } else {
        appendTo("Screen", paste0("Opening ", tag, "'s array movement events for inspection (", sum(!moves$Valid), " invalid event(s) omitted):"))
        to.display <- moves[(Valid)]
      }
      message(paste0(capture.output(print(to.display, topn = nrow(to.display))), collapse = "\n"))
      if (nrow(moves) >= 100)
        message("\nM: Long table detected, repeating warning(s) that triggered the interaction:\n-----\n", trigger, "\n-----")
      if (nrow(moves) < 100 & nrow(moves) >= 10)
        message("\nM: Please find the exception which triggered this interaction at the top of the table.")
      message("")
      if (force) {
        output <- invalidateEvents(displayed.moves = to.display, 
                                  all.moves = moves, 
                                  detections = detections, 
                                  tag = tag,
                                  GUI = GUI,
                                  save.tables.locally = save.tables.locally)
      } else {
        if (colnames(moves)[1] == "Section")
          text.to.display <- "Would you like to render any movement event invalid?(y/n/comment) "
        else
          text.to.display <- "Would you like to render any movement event invalid, or expand an event?(y/n/comment) "

        decision <- userInput(text.to.display,
                              choices = c("y", "n", "comment"), 
                              tag = tag, 
                              hash = paste0("# invalidate/expand moves in ", tag, "?"))
        if (decision == "y") {
          output <- invalidateEvents(displayed.moves = to.display, 
                                    all.moves = moves, 
                                    detections = detections, 
                                    tag = tag,
                                    GUI = GUI,
                                    save.tables.locally = save.tables.locally)
        } else {
          output <- moves
        }
      }
    }
  }
  if (any(output$Array == "Unknown" & output$Valid)) {
    appendTo(c("Screen", "Report", "Warning"), "Unknown events cannot be valid. Setting validity of these events back to FALSE.")
    output$Valid[output$Array == "Unknown"] <- FALSE
  }
  return(output)
} # nocov end


#' Allow the user to determine a given movement event invalid
#'
#' @inheritParams user_interaction_args
#'
#' @return A data frame with the movement events for the target tag and an updated 'Valid' column.
#'
#' @keywords internal
#'
invalidateEvents <- function(displayed.moves, all.moves, detections, tag, GUI, save.tables.locally) { # nocov start
  Valid <- NULL
  appendTo("debug", "Running invalidateEvents.")
  appendTo("Screen", "Note: You can select event ranges by separating them with a ':' and/or multiple events at once by separating them with a space or a comma.")
  
  check <- TRUE
  while (check) {

    if (colnames(displayed.moves)[1] == "Section") {
      the.string <- userInput("Events to be rendered invalid: ", tag = tag)
    } else {
      the.string <- userInput("Events to be rendered invalid (type 'expand' to inspect the detections of a given event): ", tag = tag)      
      if (the.string == "expand") {
        all.moves <- expandEvent(displayed.moves = displayed.moves, 
                                 all.moves = all.moves, 
                                 detections = detections, 
                                 tag = tag,
                                 GUI = GUI,
                                 save.tables.locally = save.tables.locally)
        displayed.moves <- all.moves[(Valid)]
        message("")
        appendTo("Screen", paste0("M: Updated movement table of tag ", tag, ":"))
        message(paste0(capture.output(print(displayed.moves, topn = nrow(displayed.moves))), collapse = "\n"))
        message("")
        if (colnames(displayed.moves)[1] == "Section")
          text.to.display <- "Would you like to render any movement event invalid?(y/n/comment) "
        else
          text.to.display <- "Would you like to render any movement event invalid, or expand an event?(y/n/comment) "
        
        decision <- userInput(text.to.display,
                              choices = c("y", "n", "comment"), 
                              tag = tag, 
                              hash = paste0("# invalidate/expand moves in ", tag, "?"))
        if (decision == "y") {
          appendTo("Screen", "Note: You can select event ranges by separating them with a ':' and/or multiple events at once by separating them with a space or a comma.")
        } else {
          check <- FALSE
        }
        next()
      }
    }
    the.inputs <- unlist(strsplit(the.string, "\ |,"))
    the.rows <- the.inputs[grepl("^[0-9]*$", the.inputs)]
    n.rows <- length(the.rows)
    if (length(the.rows) > 0)
      the.rows <- as.integer(the.rows)
    else
      the.rows <- NULL
    the.ranges <- the.inputs[grepl("^[0-9]*[0-9]:[0-9][0-9]*$", the.inputs)]
    n.ranges <- length(the.ranges)
    if (length(the.ranges) > 0) {
      the.ranges <- strsplit(the.ranges, ":")
      the.ranges <- unlist(lapply(the.ranges, function(x) {
        r <- as.integer(x)
        r[1]:r[2]
      }))
    } else {
      the.ranges <- NULL
    }
    the.rows <- sort(unique(c(the.rows, the.ranges)))
    if (is.null(the.rows)) {
      decision <- userInput("The input could not be recognised as row numbers, would you like to abort invalidation the process?(y/n/comment) ",
                            choices = c("y", "n", "comment"), tag = tag, hash = "# abort invalidation process?")
      if (decision == "y") {
        appendTo("Screen", "Aborting.")
        check <- FALSE
      } else {
        check <- TRUE
      }
    } else {
      if (sum(n.rows, n.ranges) < length(the.inputs))
        appendTo("Screen", "Part of the input could not be recognised as a row number.")

      if (all(the.rows > 0 & the.rows <= nrow(displayed.moves))) {
        
        if (length(the.rows) <= 10)
          decision <- userInput(paste0("Confirm: Would you like to render event(s) ", paste(the.rows, collapse = ", "), " invalid?(y/n/comment) "),
                                choices = c("y", "n", "comment"), tag = tag, hash = "# confirm?")
        else
          decision <- userInput(paste0("Confirm: Would you like to render ", length(the.rows), " events invalid?(y/n/comment) "),
                                choices = c("y", "n", "comment"), tag = tag, hash = "# confirm?")
        
        if (decision == "y") {
          displayed.moves$Valid[the.rows] <- FALSE
          attributes(displayed.moves)$p.type <- "Manual"
          
          # transfer movement validity
          all.moves <- transferValidity(from = displayed.moves, to = all.moves)

          if (any(all.moves$Valid)) {
            if (length(the.rows) <= 10)
              appendTo(c("Screen", "Report"), paste0("M: Movement event(s) ", paste(the.rows, collapse = ", "), " from tag ", tag," were rendered invalid per user command."))
            else
              appendTo(c("Screen", "Report"), paste0("M: ", length(the.rows), " movement event(s) from tag ", tag," were rendered invalid per user command."))
            
            if (colnames(all.moves)[1] == "Section")
              text.to.display <- "Would you like to render any more movement events invalid?(y/n/comment) "
            else
              text.to.display <- "Would you like to render any more movement events invalid, or expand an event?(y/n/comment) "

            decision <- userInput(text.to.display,
                                  choices = c("y", "n", "comment"), tag = tag, hash = "# invalidate more?")
            
            if (decision == "y") {
              if (colnames(all.moves)[1] == "Section")
                to.display <- all.moves[(Valid), -c(5, 7)]
              else
                to.display <- all.moves[(Valid)]
              check <- TRUE
              appendTo("Screen", paste0("M: Updated movement table of tag ", tag, "(", sum(!all.moves$Valid), " invalid event(s) omitted):"))
              message(paste0(capture.output(print(to.display, topn = nrow(to.display))), collapse = "\n"))
              appendTo("Screen", "Note: You can select event ranges by separating them with a ':' and/or multiple events at once by separating them with a space or a comma.")
            } else {
              check <- FALSE
            }

          } else {
            appendTo(c("Screen", "Report"), paste0("M: ALL movement events from tag ", tag," were rendered invalid per user command."))
            check <- FALSE
          }
        }
      } else {
        appendTo("Screen", paste0("Please select only events within the row limits (1-", nrow(all.moves),")."))
        check <- TRUE
      }
    }
  } # end while
  return(all.moves)
} # nocov end

#' Opens a new window that allows the user to determine movement event invalidity
#'
#' @inheritParams user_interaction_args
#'
#' @return A data frame with the movement events for the target tag and an updated 'Valid' column.
#'
#' @keywords internal
#'
graphicalInvalidate <- function(detections, moves, tag, trigger) { # nocov start
  appendTo("debug", "Running graphicalInvalidate.")

  type <- colnames(moves)[1]

  restart <- TRUE
  first.time <- TRUE
  while (restart) {
    restart <- FALSE

    displayed.moves <- cbind(data.frame(Event = 1:nrow(moves)), moves)
    displayed.moves$First.time <- as.character(displayed.moves$First.time)
    displayed.moves$Last.time <- as.character(displayed.moves$Last.time)
    displayed.moves <- displayed.moves[displayed.moves$Valid, ]

    if (nrow(displayed.moves) > 1200)
      tabbed <- TRUE
    else
      tabbed <- FALSE

    if (tabbed) {
      recipient <- eventsTabbedWidget(tag = tag,
                                      displayed.moves = displayed.moves,
                                      all.moves = moves,
                                      detections = detections,
                                      trigger = trigger,
                                      first.time = first.time,
                                      type = type)
      moves <- recipient$all.moves
      graphical_valid <- recipient$graphical_valid
      restart <- recipient$restart
      rm(recipient)
    } else {
      recipient <- eventsSingleWidget(tag = tag,
                                      displayed.moves = displayed.moves,
                                      all.moves = moves,
                                      detections = detections,
                                      trigger = trigger,
                                      first.time = first.time,
                                      type = type)
      moves <- recipient$all.moves
      graphical_valid <- recipient$graphical_valid
      restart <- recipient$restart
      rm(recipient)
    }
    
    first.time <- FALSE
    
    moves$Valid <- graphical_valid

    if (restart)
      message("M: Saving detection-level changes and refreshing event table."); flush.console()
  }

  if (any(!graphical_valid)) {
    link <- createEventRanges(which(!graphical_valid))
    appendTo("UD", paste0("# From the GUI, these events are invalid: ", paste(link, collapse = " ")))
  } else {
    appendTo("UD", "# From the GUI, no events were considered invalid")
  }
  return(moves)
} # nocov end

#' Handler for event expansion
#' 
#' @inheritParams user_interaction_args
#' 
#' @return An updated movements table
#' 
#' @keywords internal
#' 
expandEvent <- function(displayed.moves, all.moves, detections, tag, GUI, save.tables.locally) { # nocov start
  check <- TRUE
  abort <- FALSE
  while(check) {
    event <- userInput("Which event would you like to expand? ", 
                       tag = tag,
                       hash = "# Expand this event")
    event <- suppressWarnings(as.numeric(event))
    if (is.na(event)) {
      decision <- userInput("Could not recognise input as a number. Would you like to abort event expansion?(y/n) ",
                            choices = c("y", "n"),
                            tag = tag,
                            hash = "# abort?")
      if (decision == "y") {
        appendTo("Screen", "M: Aborting...")
        abort <- TRUE
        break()
      }
    } else {
      if (event < 1 | event > nrow(displayed.moves))
        appendTo("Screen", paste0("Please select only events within the row limits (1-", nrow(displayed.moves),")."))
      else
        check <- FALSE
    }
  }

  if (abort)
    return(all.moves)

  link <- detections$Timestamp >= displayed.moves$First.time[event] & detections$Timestamp <= displayed.moves$Last.time[event]
  sub.det <- detections[link, ]

  if (GUI == "never")
    popup <- FALSE
  if (GUI == "needed") {
    if (prod(dim(sub.det)) > min((800 * ncol(sub.det)), (getOption("max.print") - 2)))
      popup <- TRUE
    else
      popup <- FALSE
  }
  if (GUI == "always")
    popup <- TRUE

  if (popup) {
    output <- graphicalInvalidateDetections(detections = sub.det,
                                            displayed.moves = displayed.moves, 
                                            all.moves = all.moves, 
                                            event = event, 
                                            tag = tag,
                                            silent = FALSE)
  } else {
    # check if table can be printed on console
    outside.console <- nrow(sub.det) > min(800, (floor(getOption("max.print") / ncol(sub.det))))
    # avoid issue #43
    if (R.Version()$major < 4)
      outside.console <- outside.console | nchar(paste0(capture.output(print(sub.det, topn = nrow(sub.det))), collapse = "\n")) > 8000
    # ---
    # make decision
    if (outside.console) {
      if (save.tables.locally)
        target.file <- "actel_inspect_detections.csv"
      else
        target.file <- paste0(tempdir(), '/actel_inspect_detections.csv')
      
      # save file
      to.display <- cbind(data.frame(Index = 1:nrow(sub.det)), sub.det)
      write.csv(to.display, target.file, row.names = FALSE)
      # display instructions
      message("M: The detections table from event ", event, " of tag '", tag, "' is too large to display on the console and GUI is set to 'never'.\n   Temporarily saving the table to '", target.file, "'.\n   Please inspect this file and decide if any detections should be considered invalid.")
      if (save.tables.locally)
        message("   This file will be automatically deleted once it has served its purpose.")
      else
        message("   Can't find the file? Check out the save.tables.locally argument.")
      message("Please use the 'Index' column as a reference for the row number. ")
      flush.console()
      # start interaction
      decision <- userInput("Would you like to render any detections invalid?(y/n/comment) ",
                            choices = c("y", "n", "comment"), 
                            tag = tag, 
                            hash = paste0("# invalidate detections in event ", event, " of ", tag, "?"))
      if (decision == "y") {
        output <- invalidateDetections(displayed.moves = displayed.moves, 
                                       all.moves = all.moves, 
                                       detections = sub.det, 
                                       tag = tag,
                                       event = event)
      } else {
        output <- all.moves
      }
      first.try <- TRUE
      while (file.exists(paste0(tempdir(), "/actel_inspect_detections.csv"))) {
        if (!suppressWarnings(file.remove(paste0(tempdir(), "/actel_inspect_detections.csv")))) {
          if (first.try) {
            warning("Please close the currently open 'actel_inspect_detections.csv' file so the analysis can continue", immediate. = TRUE, call. = FALSE); flush.console()
            first.try <- FALSE
          }
        }
      }
    } else {
      appendTo("Screen", paste0("Opening detections from event ", event, " of tag ", tag, " for inspection:"))
      message(paste0(capture.output(print(sub.det, topn = nrow(sub.det))), collapse = "\n"))
      message("")
      decision <- userInput("Would you like to render any detections invalid?(y/n/comment) ",
                            choices = c("y", "n", "comment"), 
                            tag = tag, 
                            hash = paste0("# invalidate detections in event ", event, " of ", tag, "?"))
      if (decision == "y") {
        output <- invalidateDetections(displayed.moves = displayed.moves, 
                                       all.moves = all.moves, 
                                       detections = sub.det, 
                                       tag = tag,
                                       event = event)
      } else {
        output <- all.moves
      }
    }
  }
  return(output)
} # nocov end

#' Allow the user to determine a given set of detections invalid
#'
#' @inheritParams user_interaction_args
#'
#' @return A data frame with the movement events for the target tag and an updated 'Valid' column.
#'
#' @keywords internal
#'
invalidateDetections <- function(displayed.moves, all.moves, detections, tag, event) { # nocov start
  appendTo("debug", "Running invalidateDetections")
  appendTo("Screen", "Note: You can select row ranges by separating them with a ':' and/or multiple rows at once by separating them with a space or a comma.")

  check <- TRUE
  while (check) {
    the.string <- userInput("Detections to be rendered invalid: ", tag = tag)
    the.inputs <- unlist(strsplit(the.string, "\ |,"))
    the.rows <- the.inputs[grepl("^[0-9]*$", the.inputs)]
    n.rows <- length(the.rows)
    if (length(the.rows) > 0)
      the.rows <- as.integer(the.rows)
    else
      the.rows <- NULL
    the.ranges <- the.inputs[grepl("^[0-9]*[0-9]:[0-9][0-9]*$", the.inputs)]
    n.ranges <- length(the.ranges)
    if (length(the.ranges) > 0) {
      the.ranges <- strsplit(the.ranges, ":")
      the.ranges <- unlist(lapply(the.ranges, function(x) {
        r <- as.integer(x)
        r[1]:r[2]
      }))
    } else {
      the.ranges <- NULL
    }
    the.rows <- sort(unique(c(the.rows, the.ranges)))
    if (is.null(the.rows)) {
      decision <- userInput("The input could not be recognised as row numbers, would you like to abort invalidation the process?(y/n/comment) ",
                            choices = c("y", "n", "comment"), tag = tag, hash = "# abort detection invalidation process?")
      if (decision == "y") {
        appendTo("Screen", "Aborting.")
        check <- FALSE
      } else {
        check <- TRUE
      }
    } else {
      if (sum(n.rows, n.ranges) < length(the.inputs))
        appendTo("Screen", "Part of the input could not be recognised as a row number.")

      if (all(the.rows > 0 & the.rows <= nrow(detections))) {
        
        if (length(the.rows) <= 10)
          decision <- userInput(paste0("Confirm: Would you like to render detection(s) ", paste(the.rows, collapse = ", "), " invalid?(y/n/comment) "),
                                choices = c("y", "n", "comment"), tag = tag, hash = "# confirm?")
        else
          decision <- userInput(paste0("Confirm: Would you like to render ", length(the.rows), " detections invalid?(y/n/comment) "),
                                choices = c("y", "n", "comment"), tag = tag, hash = "# confirm?")
        
        if (decision == "y") {
          detections$Valid[the.rows] <- FALSE          
          if (length(the.rows) == 1)
            appendTo(c("Screen", "Report"), paste0("M: ", length(the.rows), " detection from valid event ", event, " of tag ", tag," was rendered invalid per user command."))
          else
            appendTo(c("Screen", "Report"), paste0("M: ", length(the.rows), " detections from valid event ", event, " of tag ", tag," were rendered invalid per user command."))
            
          decision <- userInput("Would you like to render any more detections invalid?(y/n/comment) ",
                                choices = c("y", "n", "comment"), tag = tag, hash = "# invalidate more?")
          
          if (decision == "y") {
            check <- TRUE
            appendTo("Screen", paste0("M: Updated detections table from valid event ", event, " of tag ", tag, ":"))
            message(paste0(capture.output(print(detections, topn = nrow(detections))), collapse = "\n"))
            appendTo("Screen", "Note: You can select event ranges by separating them with a ':' and/or multiple events at once by separating them with a space or a comma.")
          } else {
            check <- FALSE
          }
        }
        all.moves <- createNewEvents(displayed.moves = displayed.moves, 
                                     all.moves = all.moves, 
                                     detections = detections, 
                                     event = event)
      } else {
        appendTo("Screen", paste0("Please select only events within the row limits (1-", nrow(detections),")."))
        check <- TRUE
      }
    }
  } # end while
  return(all.moves)
} # nocov end

#' Opens a new window that allows the user to determine detection invalidity
#'
#' @inheritParams user_interaction_args
#'
#' @return A data frame with the movement events for the target tag and an updated 'Valid' column.
#'
#' @keywords internal
#'
graphicalInvalidateDetections <- function(detections, displayed.moves, all.moves, event, tag, silent = FALSE) { # nocov start
  appendTo("debug", "Running graphicalInvalidateDetections.")

  to.print <- cbind(data.frame(Index = 1:nrow(detections)), detections)
  to.print$Timestamp <- as.character(to.print$Timestamp)

  if (all(is.na(to.print$Sensor.Value)))
    to.print$Sensor.Value <- "NA"
  if (all(is.na(to.print$Sensor.Unit)))
    to.print$Sensor.Unit <- "NA"
  to.print$Valid <- TRUE

  if (nrow(to.print) > 11000) {
    tabbed <- TRUE
    to.print <- splitN(to.print, 10000)
  } else {
    tabbed <- FALSE
  }


  if (tabbed) {
    graphical_valid <- detectionsTabbedWidget(event = event,
                                              tag = tag,
                                              to.print = to.print,
                                              silent = silent)
  } else {
    graphical_valid <- detectionsSingleWidget(event = event,
                                              tag = tag,
                                              to.print = to.print,
                                              silent = silent)
  }
  
  detections$Valid <- graphical_valid

  all.moves <- createNewEvents(displayed.moves = displayed.moves, 
                               all.moves = all.moves, 
                               detections = detections, 
                               event = event)

  if (any(!graphical_valid)) {
    link <- createEventRanges(which(!graphical_valid))
    appendTo("UD", paste0("# From the GUI, these events are invalid: ", paste(link, collapse = " ")))
  } else {
    appendTo("UD", "# From the GUI, no events were considered invalid")
  }
  return(all.moves)
} # nocov end


#' Transfer validity updates from valid movements to all movements
#'
#' @param from The valid movements table with newly invalidated moves
#' @param to The target movements table
#'
#' @return A data frame with the movement events for the target tag and an updated 'Valid' column.
#'
#' @keywords internal
#'
transferValidity <- function(from, to) { # nocov start
  appendTo("debug", "Running transferValidity.")
  Valid <- NULL
  if (any(!from$Valid)) {
    aux <- from[!(Valid)]
    link <- apply(aux, 1, function(x) which(to[, 1] == x[1] & grepl(x["First.time"], to$First.time, fixed = TRUE)))
    to$Valid[link] <- FALSE
    attributes(to)$p.type <- "Manual"
  }
  return(to)
} # nocov end


#' Skips all validity checks for a tag and allows the user to freely invalidate events
#'
#' @inheritParams user_interaction_args
#' @param n A string indicating the overall progress.
#'
#' @return A data frame containing the movements for the target tag
#'
#' @keywords internal
#'
overrideValidityChecks <- function(moves, detections, tag, GUI, save.tables.locally, n) { # nocov start
  appendTo("debug", "Starting overrideValidityChecks.")
  message("----------------------------")
  appendTo(c("Screen", "Report"), trigger <- paste0("M: Override has been triggered for tag ", tag, " ", n, ". Entering full manual mode."))
  moves <- tableInteraction(moves = moves, detections = detections, tag = tag, 
                            trigger = trigger, GUI = GUI, save.tables.locally = save.tables.locally)
  attributes(moves)$p.type <- "Overridden"
  message("Terminating full manual mode\n----------------------------")
  return(moves) # nocov end
}

#' Upon invalidating detections, recombines the remaining valid detections 
#' into new events, and merges them with the remaining events.
#' 
#' @inheritParams user_interaction_args
#' 
#' @return A data frame containing the movements for the target tag
#'
#' @keywords internal
#'
createNewEvents <- function(displayed.moves, all.moves, detections, event) { # nocov start
  appendTo("debug", "Running createNewEvents.")
  # compile new events and combine with movements
  aux <- rle(detections$Valid)
  aux <- data.frame(Value = aux[[2]], n = aux[[1]])
  aux$stop <- cumsum(aux$n)
  aux$start <- c(1, aux$stop[-1] - (aux$n[-1] - 1))
  
  # the event needs to be converted to the original row number in the movements
  ori.event <- which(all.moves[[1]] == displayed.moves[[event, "Array"]] &
                     grepl(displayed.moves[[event, "First.time"]], all.moves$First.time, fixed = TRUE))

  # create new events
  to.add <- all.moves[rep(ori.event, nrow(aux)), ]
  to.add$Detections <- aux$n
  to.add$First.station <- detections$Standard.name[aux$start]
  to.add$First.time <- detections$Timestamp[aux$start]
  to.add$Last.station <- detections$Standard.name[aux$stop]
  to.add$Last.time <- detections$Timestamp[aux$stop]
  to.add <- movementTimes(movements = to.add, type = "array")
  if (matchl("Average.speed.m.s", colnames(to.add)))
    to.add$Average.speed.m.s[2:nrow(to.add)] <- NA
  to.add$Valid <- aux$Value

  done <- FALSE

  if (ori.event == 1) {
    all.moves <- rbind(to.add, all.moves[-1, ])
    done <- TRUE
  }

  if (!done && ori.event == nrow(all.moves)) {
    all.moves <- rbind(all.moves[-ori.event, ], to.add)
    done <- TRUE
  }

  if (!done && ori.event != 1 & ori.event != nrow(all.moves))
    all.moves <- rbind(all.moves[1:(ori.event - 1), ], to.add, all.moves[(ori.event + 1):nrow(all.moves), ])

  attributes(all.moves)$p.type <- "Manual"
  return(all.moves)
} # nocov end
