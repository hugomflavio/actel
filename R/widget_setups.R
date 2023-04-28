#' widget_setups.R arguments
#'
#' @param displayed.moves The valid movements table for a specific tag.
#' @param all.moves The complete movements table for a specific tag.
#' @param detections The detections data.frame for a specific tag.
#' @param trigger The message/warning that triggered the interaction
#' @param first.time Logical: Is this the first time this widget is running for this tag?
#' @param type The type of events (Array or Section)
#' @param tag The tag being analysed.
#' @param event The event selected for expansion.
#' @param to.print The subset of detections to be displayed.
#' 
#' @name widget_args
#' @keywords internal
#'
NULL

#' Event Widget (Tabbed version)
#' 
#' @inheritParams widget_args
#' 
#' @return The movements list, a vector of event validities, and a note on whether or not the widget should be restarted.
#' 
#' @keywords internal
#' 
eventsTabbedWidget <- function(tag, displayed.moves, all.moves, detections, trigger, first.time, type) { # nocov start
  appendTo("debug", "Running eventsTabbedWidget.")
  # initiate button variables
  cp <- NULL
  complain <- NULL
  confirm <- NULL
  restart <- FALSE
  graphical_valid <- NULL
  graphical_valid_indexes <- which(all.moves$Valid)

  placeholder <- gWidgets2::gwindow("Please wait", width = 300, height = 20)
  on.exit({if(gWidgets2::isExtant(placeholder)) gWidgets2::dispose(placeholder)}, add = TRUE)
  placeholder_layout <- gWidgets2::ggroup(horizontal = FALSE, container = placeholder)
  gWidgets2::glabel("The GUI window is loading...", container = placeholder_layout)

  message("M: Please wait while the GUI loads."); flush.console()

  moves.window <- gWidgets2::gwindow(paste0("Valid events for tag  ", tag, " (", sum(!all.moves$Valid), " invalid event(s) omitted)"),
                          width = 900, height = 500, visible = FALSE)

  on.exit({if(gWidgets2::isExtant(moves.window)) gWidgets2::dispose(moves.window)}, add = TRUE)

  g <- gWidgets2::ggroup(horizontal = FALSE, container = moves.window)
  hdr <- gWidgets2::glayout(container = g)
  hdr[1, 1] <- gWidgets2::glabel("Warning message:", container = hdr)
  hdr[1, 2, expand = TRUE] <- ""
  hdr[2, 1:2, expand = TRUE] <- gWidgets2::gtext(trigger, handler = NULL, container = hdr)
  hdr[3, 1:2, expand = TRUE] <- gWidgets2::glabel("Usage notes:\n   - Edit event validity by selecting rows and choosing the desired action below.\n   - Loading large tables can take some time. Please wait until the interaction buttons show up at the bottom of this window.", container = hdr)
  hdr[2, 1, expand = TRUE] <- gWidgets2::glabel("This table is very long!\n   - Please allow some time for the action buttons to complete their tasks (particularly those that span multiple pages).\n   - <b>Please wait</b> until the buttons appear at the bottom of the page before performing any action!", container = hdr)
  
  tbl <- list()
  nb <- gWidgets2::gnotebook(tab.pos = 3, expand = TRUE, container = g)
  # add handler that keeps track of current tab
  gWidgets2::addHandlerChanged(nb, handler = function(h, ...) {
    cp <<- h$page.no
  })

  tabbed.moves <- splitN(displayed.moves, 1000)

  for (i in 1:length(tabbed.moves)) {
    tbl[[i]] <- gWidgets2::gtable(tabbed.moves[[i]], multiple = TRUE, expand = TRUE, container = nb, label = names(tabbed.moves)[i])
  }

  btns <- gWidgets2::glayout(container = g)

  invalid_selected_function <- function(h, ...) {
    tbl[[cp]][match(tbl[[cp]]$get_value(), tbl[[cp]][, "Event"]), "Valid"] <- rep(FALSE, length(tbl[[cp]]$get_value()))
  }
  btns[1, 1] <- gWidgets2::gbutton(text = "Invalidate selected", handler = invalid_selected_function, action = NULL, container = btns)

  reset_selected_function <- function(h, ...) {
    tbl[[cp]][match(tbl[[cp]]$get_value(), tbl[[cp]][, "Event"]), "Valid"] <- rep(TRUE, length(tbl[[cp]]$get_value()))
  }
  btns[2, 1] <- gWidgets2::gbutton(text = "Revalidate selected", handler = reset_selected_function, action = NULL, container = btns)

  invalid_page_function <- function(h, ...) {
    tbl[[cp]][, "Valid"] <- rep(FALSE, nrow(tbl[[cp]]))
  }
  btns[1, 2] <- gWidgets2::gbutton(text = "Invalidate page", handler = invalid_page_function, action = NULL, container = btns)

  reset_page_function <- function(h, ...) {
    tbl2[[cp]][, "Valid"] <- rep(TRUE, nrow(tbl[[cp]]))
  }
  btns[2, 2] <- gWidgets2::gbutton(text = "Revalidate page", handler = reset_page_function, action = NULL, container = btns)

  invalid_all_function <- function(h, ...) {
    processing <- gWidgets2::gwindow("Processing...", width = 300, height = 30)
    for (i in 1:length(tbl)) {
      capture <- gWidgets2::glabel(paste("Processing page", i, "of", length(tbl)), container = processing)
      tbl[[i]][, "Valid"] <- rep(FALSE, nrow(tbl[[i]]))
    }
    gWidgets2::dispose(processing)
  }
  btns[1, 3] <- gWidgets2::gbutton(text = "Invalidate all", handler = invalid_all_function, action = NULL, container = btns)

  reset_all_function <- function(h, ...) {
    processing <- gWidgets2::gwindow("Processing...", width = 300, height = 30)
    for (i in 1:length(tbl)) {
      capture <- gWidgets2::glabel(paste("Processing page", i, "of", length(tbl)), container = processing)
      tbl[[i]][, "Valid"] <- rep(TRUE, nrow(tbl[[i]]))
    }
    gWidgets2::dispose(processing)
  }
  btns[2, 3] <- gWidgets2::gbutton(text = "Revalidate all", handler = reset_all_function, action = NULL, container = btns)

  invert_page_function <- function(h, ...) {
    tbl[[cp]][, "Valid"] <- !tbl[[cp]][, "Valid"]
  }
  btns[1, 4] <- gWidgets2::gbutton(text = "Invert page validities", handler = invert_page_function, action = NULL, container = btns)

  invert_all_function <- function(h, ...) {
    processing <- gWidgets2::gwindow("Processing...", width = 300, height = 30)
    for (i in 1:length(tbl)) {
      capture <- gWidgets2::glabel(paste("Processing page", i, "of", length(tbl)), container = processing)
      tbl[[i]][, "Valid"] <- !tbl[[i]][, "Valid"]
    }
    gWidgets2::dispose(processing)
  }
  btns[2, 4] <- gWidgets2::gbutton(text = "Invert all validities", handler = invert_all_function, action = NULL, container = btns)

  btns[1, 5, expand = TRUE] <- ""
  btns[2, 5, expand = TRUE] <- ""

  if (type == "Array") {
    expand_event_function <- function(h, ...) {
      event <- match(tbl[[cp]]$get_value(), displayed.moves$Event)
      if (length(event) < 1) {
        if (exists("complain") && gWidgets2::isExtant(complain))
          gWidgets2::dispose(complain)

        complain <<- gWidgets2::gwindow("Warning", width = 300, height = 20)
        complain_layout <- gWidgets2::ggroup(horizontal = FALSE, container = complain)
        gWidgets2::glabel("No event was selected to expand.", container = complain_layout)
        
        complain_function <- function(h, ...) {
          gWidgets2::dispose(complain)
        }
        complain_btn <- gWidgets2::gbutton(text = "Close", handler = complain_function, action = NULL, 
          expand = TRUE, container = complain_layout)
      }
      if (length(event) > 1) {
        if (exists("complain") && gWidgets2::isExtant(complain))
          gWidgets2::dispose(complain)

        complain <<- gWidgets2::gwindow("Warning", width = 300, height = 20)
        complain_layout <- gWidgets2::ggroup(horizontal = FALSE, container = complain)
        gWidgets2::glabel("Select only one event to expand.", container = complain_layout)
        
        complain_function <- function(h, ...) {
          gWidgets2::dispose(complain)
        }
        complain_btn <- gWidgets2::gbutton(text = "Close", handler = complain_function, action = NULL, 
          expand = TRUE, container = complain_layout)
      }
      if (length(event) == 1) {
        link <- detections$Timestamp >= displayed.moves$First.time[event] & 
                detections$Timestamp <= displayed.moves$Last.time[event]

        from <- match(displayed.moves$First.time[event], as.character(detections$Timestamp))
        to   <- match(displayed.moves$Last.time[event], as.character(detections$Timestamp))
        sub.det <- detections[from:to, ]

        gWidgets2::visible(moves.window) <- FALSE
        all.moves <<- graphicalInvalidateDetections(detections = sub.det, 
                                                    displayed.moves = displayed.moves, 
                                                    all.moves = all.moves, 
                                                    event = event, 
                                                    tag = tag,
                                                    silent = TRUE)

        graphical_valid <<- all.moves$Valid
        restart <<- TRUE
        gWidgets2::dispose(moves.window)
      }
    }
    btns[1, 6] <- gWidgets2::gbutton(text = "Expand event", handler = expand_event_function, action = NULL, container = btns)
  } else {
    btns[1, 6] <- ""
  }

  close_function <- function(h, ...) {
    aux <- lapply(tbl, function(x) as.data.frame(x[, c("Event", "Valid")]))
    x <- data.table::rbindlist(aux)
    graphical_valid <<- rep(FALSE, nrow(all.moves))
    graphical_valid[graphical_valid_indexes] <<- x$Valid[order(x$Event)]

    aux <- rle(graphical_valid)
    aux <- data.frame(Value = aux[[2]], n = aux[[1]])
    aux$stop <- cumsum(aux$n)
    aux$start <- c(1, aux$stop[-1] - (aux$n[-1] - 1))
    aux$combine <- aux$start != aux$stop
    aux$final <- aux$start
    aux$final[aux$combine] <- paste(aux$start[aux$combine], aux$stop[aux$combine], sep = ":")

    valid.summary <- aux[, c("final", "Value")]
    colnames(valid.summary) <- c("Detections", "Validity")

    if (exists("confirm") && gWidgets2::isExtant(confirm))
      gWidgets2::dispose(confirm)

    confirm <<- gWidgets2::gwindow("Confirm", width = 300, height = 300)
    confirm_layout <- gWidgets2::ggroup(horizontal = FALSE, container = confirm)
    gWidgets2::glabel("Confirm the following validity ranges.", container = confirm_layout)
    gWidgets2::gtable(valid.summary, multiple = TRUE, expand = TRUE, container = confirm_layout)
    confirm_btns <- gWidgets2::glayout(container = confirm_layout)

    confirm_function <- function(h, ...) {
      gWidgets2::dispose(confirm)
      gWidgets2::dispose(moves.window)
    }
    confirm_btns[1, 1, expand = TRUE] <- gWidgets2::gbutton(text = "Confirm", handler = confirm_function, action = NULL, container = confirm_btns)

    abort_function <- function(h, ...) {
      gWidgets2::dispose(confirm)
    }
    confirm_btns[1, 2, expand = TRUE] <- gWidgets2::gbutton(text = "Return", handler = abort_function, action = NULL, container = confirm_btns)      
  }
  btns[2, 6] <- gWidgets2::gbutton(text = "Submit and close", handler = close_function, action = NULL, container = btns)

  gWidgets2::dispose(placeholder)
  gWidgets2::visible(moves.window) <- TRUE

  if (first.time)
    message("M: Make any necessary edits in the external visualization window and submit the result to continue the analysis.\nNote: You can use Ctrl and Shift to select multiple events, and Ctrl+A to select all events at once."); flush.console()

  while (gWidgets2::isExtant(moves.window)) {}

  if (is.null(graphical_valid)) {
    appendTo(c("Screen", "Warning", "Report"), "External visualization window was closed before result submission. Assuming no changes are to be made.")
    graphical_valid <- all.moves$Valid
  }

  return(list(all.moves = all.moves, graphical_valid = graphical_valid, restart = restart))
} # nocov end

#' Event Widget (Single table version)
#' 
#' @inheritParams widget_args
#' 
#' @return The movements list, a vector of event validities, and a note on whether or not the widget should be restarted.
#' 
#' @keywords internal
#' 
eventsSingleWidget <- function(tag, displayed.moves, all.moves, detections, trigger, first.time, type) { # nocov start
  appendTo("debug", "Running eventsSingleWidget.")

  # initiate button variables
  complain <- NULL
  confirm <- NULL
  restart <- FALSE
  graphical_valid <- NULL
  graphical_valid_indexes <- which(all.moves$Valid)

  placeholder <- gWidgets2::gwindow("Please wait", width = 300, height = 20)
  on.exit({if(gWidgets2::isExtant(placeholder)) gWidgets2::dispose(placeholder)}, add = TRUE)
  placeholder_layout <- gWidgets2::ggroup(horizontal = FALSE, container = placeholder)
  gWidgets2::glabel("The GUI window is loading...", container = placeholder_layout)

  message("M: Please wait while the GUI loads."); flush.console()

  moves.window <- gWidgets2::gwindow(paste0("Valid events for tag  ", tag, " (", sum(!all.moves$Valid), " invalid event(s) omitted)"),
                          width = 900, height = 500, visible = TRUE)

  on.exit({if(gWidgets2::isExtant(moves.window)) gWidgets2::dispose(moves.window)}, add = TRUE)

  g <- gWidgets2::ggroup(horizontal = FALSE, container = moves.window)
  hdr <- gWidgets2::glayout(container = g)
  hdr[1, 1] <- gWidgets2::glabel("Warning message:", container = hdr)
  hdr[1, 2, expand = TRUE] <- ""
  hdr[2, 1:2, expand = TRUE] <- gWidgets2::glabel(trigger, handler = NULL, container = hdr)
  hdr[3, 1:2, expand = TRUE] <- gWidgets2::glabel("Usage notes:\n   - Edit event validity by selecting rows and choosing the desired action below.\n   - Loading large tables can take some time. Please wait until the interaction buttons show up at the bottom of this window.", container = hdr)

  tbl <- gWidgets2::gtable(displayed.moves, multiple = TRUE, expand = TRUE, container = g)

  btns <- gWidgets2::glayout(container = g)

  invalid_selected_function <- function(h, ...) {
    tbl[match(tbl$get_value(), tbl[, "Event"]), "Valid"] <- rep(FALSE, length(tbl$get_value()))
  }
  btns[1, 1] <- gWidgets2::gbutton(text = "Invalidate selected", handler = invalid_selected_function, action = NULL, container = btns)

  reset_selected_function <- function(h, ...) {
    tbl[match(tbl$get_value(), tbl[, "Event"]), "Valid"] <- rep(TRUE, length(tbl$get_value()))
  }
  btns[2, 1] <- gWidgets2::gbutton(text = "Revalidate selected", handler = reset_selected_function, action = NULL, container = btns)

  invalid_all_function <- function(h, ...) {
    tbl[, "Valid"] <- rep(FALSE, nrow(tbl))
  }
  btns[1, 2] <- gWidgets2::gbutton(text = "Invalidate all", handler = invalid_all_function, action = NULL, container = btns)

  reset_all_function <- function(h, ...) {
    tbl[, "Valid"] <- rep(TRUE, nrow(tbl))
  }
  btns[2, 2] <- gWidgets2::gbutton(text = "Revalidate all", handler = reset_all_function, action = NULL, container = btns)

  invert_all_function <- function(h, ...) {
      tbl[, "Valid"] <- !tbl[, "Valid"]
  }
  btns[1, 3] <- gWidgets2::gbutton(text = "Invert all validities", handler = invert_all_function, action = NULL, container = btns)

  btns[1, 4, expand = TRUE] <- ""
  btns[2, 4, expand = TRUE] <- ""

  if (type == "Array") {
    expand_event_function <- function(h, ...) {
      event <- match(tbl$get_value(), displayed.moves$Event)
      if (length(event) < 1) {
        if (exists("complain") && gWidgets2::isExtant(complain))
          gWidgets2::dispose(complain)

        complain <<- gWidgets2::gwindow("Warning", width = 300, height = 20)
        complain_layout <- gWidgets2::ggroup(horizontal = FALSE, container = complain)
        gWidgets2::glabel("No event was selected to expand.", container = complain_layout)
        
        complain_function <- function(h, ...) {
          gWidgets2::dispose(complain)
        }
        complain_btn <- gWidgets2::gbutton(text = "Close", handler = complain_function, action = NULL, 
          expand = TRUE, container = complain_layout)
      }
      if (length(event) > 1) {
        if (exists("complain") && gWidgets2::isExtant(complain))
          gWidgets2::dispose(complain)

        complain <<- gWidgets2::gwindow("Warning", width = 300, height = 20)
        complain_layout <- gWidgets2::ggroup(horizontal = FALSE, container = complain)
        gWidgets2::glabel("Select only one event to expand.", container = complain_layout)
        
        complain_function <- function(h, ...) {
          gWidgets2::dispose(complain)
        }
        complain_btn <- gWidgets2::gbutton(text = "Close", handler = complain_function, action = NULL, 
          expand = TRUE, container = complain_layout)
      }
      if (length(event) == 1) {
        link <- detections$Timestamp >= displayed.moves$First.time[displayed.moves$Valid][event] &
                detections$Timestamp <= displayed.moves$Last.time[displayed.moves$Valid][event]
 
        from <- match(displayed.moves$First.time[event], as.character(detections$Timestamp))
        to   <- match(displayed.moves$Last.time[event], as.character(detections$Timestamp))
        sub.det <- detections[from:to, ]
 
        gWidgets2::visible(moves.window) <- FALSE
        all.moves <<- graphicalInvalidateDetections(detections = sub.det, 
                                                    displayed.moves = displayed.moves, 
                                                    all.moves = all.moves, 
                                                    event = event, 
                                                    tag = tag,
                                                    silent = TRUE)

        graphical_valid <<- all.moves$Valid
        restart <<- TRUE
        gWidgets2::dispose(moves.window)
      }
    }
    btns[1, 5] <- gWidgets2::gbutton(text = "Expand event", handler = expand_event_function, container = btns)
  } else {
    btns[1, 5] <- ""
  }
  close_function <- function(h, ...) {
    x <- as.data.frame(tbl[, c("Event", "Valid")])

    graphical_valid <<- rep(FALSE, nrow(all.moves))
    graphical_valid[graphical_valid_indexes] <<- x$Valid[order(x$Event)]

    aux <- rle(graphical_valid)
    aux <- data.frame(Value = aux[[2]], n = aux[[1]])
    aux$stop <- cumsum(aux$n)
    aux$start <- c(1, aux$stop[-1] - (aux$n[-1] - 1))
    aux$combine <- aux$start != aux$stop
    aux$final <- aux$start
    aux$final[aux$combine] <- paste(aux$start[aux$combine], aux$stop[aux$combine], sep = ":")

    valid.summary <- aux[, c("final", "Value")]
    colnames(valid.summary) <- c("Events", "Validity")

    if (exists("confirm") && gWidgets2::isExtant(confirm))
      gWidgets2::dispose(confirm)

    confirm <<- gWidgets2::gwindow("Confirm", width = 300, height = 300)
    confirm_layout <- gWidgets2::ggroup(horizontal = FALSE, container = confirm)
    gWidgets2::glabel("Confirm the following validity ranges.", container = confirm_layout)
    gWidgets2::gtable(valid.summary, multiple = TRUE, expand = TRUE, container = confirm_layout)
    confirm_btns <- gWidgets2::glayout(container = confirm_layout)

    confirm_function <- function(h, ...) {
      gWidgets2::dispose(confirm)
      gWidgets2::dispose(moves.window)
    }
    confirm_btns[1, 1, expand = TRUE] <- gWidgets2::gbutton(text = "Confirm", handler = confirm_function, action = NULL, container = confirm_btns)

    abort_function <- function(h, ...) {
      gWidgets2::dispose(confirm)
    }
    confirm_btns[1, 2, expand = TRUE] <- gWidgets2::gbutton(text = "Return", handler = abort_function, action = NULL, container = confirm_btns)      
  }
  btns[2, 5] <- gWidgets2::gbutton(text = "Submit and close", handler = close_function, action = NULL, container = btns)

  gWidgets2::dispose(placeholder)
  gWidgets2::visible(moves.window) <- TRUE

  if (first.time)
    message("M: Make any necessary edits in the external visualization window and submit the result to continue the analysis.\nNote: You can use Ctrl and Shift to select multiple events, and Ctrl+A to select all events at once."); flush.console()

  while (gWidgets2::isExtant(moves.window)) {}

  if (is.null(graphical_valid)) {
    appendTo(c("Screen", "Warning", "Report"), "External visualization window was closed before result submission. Assuming no changes are to be made.")
    graphical_valid <- all.moves$Valid
  }

  return(list(all.moves = all.moves, graphical_valid = graphical_valid, restart = restart))
} # nocov end

#' Detections Widget (Tabbed version)
#' 
#' @inheritParams widget_args
#' 
#' @return A vector of detection validities.
#' 
#' @keywords internal
#' 
detectionsTabbedWidget <- function(event, tag, to.print, silent) { # nocov start
  appendTo("debug", "Running detectionsTabbedWidget.")

  # initiate button variables
  cp <- NULL
  confirm <- NULL
  graphical_valid <- NULL

  placeholder <- gWidgets2::gwindow("Please wait", width = 300, height = 20)
  on.exit({if(gWidgets2::isExtant(placeholder)) gWidgets2::dispose(placeholder)}, add = TRUE)
  placeholder_layout <- gWidgets2::ggroup(horizontal = FALSE, container = placeholder)
  gWidgets2::glabel("The GUI window is loading...", container = placeholder_layout)

  message("M: Please wait while the GUI loads."); flush.console()

  w2 <- gWidgets2::gwindow(paste0("Detections for valid event ", event, " from tag ", tag ,"."), 
                           width = 900, height = 500, visible = FALSE)

  on.exit({if(gWidgets2::isExtant(w2)) gWidgets2::dispose(w2)}, add = TRUE)
  
  g2 <- gWidgets2::ggroup(horizontal = FALSE, container = w2)
  hdr2 <- gWidgets2::glayout(container = g2)
  hdr2[1, 1, expand = TRUE] <- gWidgets2::glabel("Usage notes:\n   - Edit detection validity by selecting rows and choosing the desired action below.\n   - Loading large tables can take some time. Please wait until the interaction buttons show up at the bottom of this window.", container = hdr2)
  hdr2[2, 1, expand = TRUE] <- gWidgets2::glabel("This table is very long!\n   - Please allow some time for the action buttons to complete their tasks (particularly those that span multiple pages).\n   - _Please wait_ until the buttons appear at the bottom of the page before performing any action!", container = hdr2)
  
  tbl2 <- list()
  nb <- gWidgets2::gnotebook(tab.pos = 3, expand = TRUE, container = g2)
  # add handler that keeps track of current tab
  gWidgets2::addHandlerChanged(nb, handler = function(h, ...) {
    cp <<- h$page.no
  })

  for (i in 1:length(to.print)) {
      tbl2[[i]] <- gWidgets2::gtable(to.print[[i]], multiple = TRUE, expand = TRUE, container = nb, label = names(to.print)[i])
  }

  btns2 <- gWidgets2::glayout(container = g2)

  invalid_selected_function <- function(h, ...) {
    tbl2[[cp]][match(tbl2[[cp]]$get_value(), tbl2[[cp]][, "Index"]), "Valid"] <- rep(FALSE, length(tbl2[[cp]]$get_value()))
  }
  btns2[1, 1] <- gWidgets2::gbutton(text = "Invalidate selected", handler = invalid_selected_function, action = NULL, container = btns2)

  reset_selected_function <- function(h, ...) {
    tbl2[[cp]][match(tbl2[[cp]]$get_value(), tbl2[[cp]][, "Index"]), "Valid"] <- rep(TRUE, length(tbl2[[cp]]$get_value()))
  }
  btns2[2, 1] <- gWidgets2::gbutton(text = "Revalidate selected", handler = reset_selected_function, action = NULL, container = btns2)

  invalid_page_function <- function(h, ...) {
    tbl2[[cp]][, "Valid"] <- rep(FALSE, nrow(tbl2[[cp]]))
  }
  btns2[1, 2] <- gWidgets2::gbutton(text = "Invalidate page", handler = invalid_page_function, action = NULL, container = btns2)

  reset_page_function <- function(h, ...) {
    tbl2[[cp]][, "Valid"] <- rep(TRUE, nrow(tbl2[[cp]]))
  }
  btns2[2, 2] <- gWidgets2::gbutton(text = "Revalidate page", handler = reset_page_function, action = NULL, container = btns2)

  invalid_all_function <- function(h, ...) {
    processing <- gWidgets2::gwindow("Processing...", width = 300, height = 30)
    for (i in 1:length(tbl2)) {
      capture <- gWidgets2::glabel(paste("Processing page", i, "of", length(tbl2)), container = processing)
      tbl2[[i]][, "Valid"] <- rep(FALSE, nrow(tbl2[[i]]))
    }
    gWidgets2::dispose(processing)
  }
  btns2[1, 3] <- gWidgets2::gbutton(text = "Invalidate all", handler = invalid_all_function, action = NULL, container = btns2)

  reset_all_function <- function(h, ...) {
    processing <- gWidgets2::gwindow("Processing...", width = 300, height = 30)
    for (i in 1:length(tbl2)) {
      capture <- gWidgets2::glabel(paste("Processing page", i, "of", length(tbl2)), container = processing)
      tbl2[[i]][, "Valid"] <- rep(TRUE, nrow(tbl2[[i]]))
    }
    gWidgets2::dispose(processing)
  }
  btns2[2, 3] <- gWidgets2::gbutton(text = "Revalidate all", handler = reset_all_function, action = NULL, container = btns2)

  invert_page_function <- function(h, ...) {
    tbl2[[cp]][, "Valid"] <- !tbl2[[cp]][, "Valid"]
  }
  btns2[1, 4] <- gWidgets2::gbutton(text = "Invert page validities", handler = invert_page_function, action = NULL, container = btns2)

  invert_all_function <- function(h, ...) {
    processing <- gWidgets2::gwindow("Processing...", width = 300, height = 30)
    for (i in 1:length(tbl2)) {
      capture <- gWidgets2::glabel(paste("Processing page", i, "of", length(tbl2)), container = processing)
      tbl2[[i]][, "Valid"] <- !tbl2[[i]][, "Valid"]
    }
    gWidgets2::dispose(processing)
  }
  btns2[2, 4] <- gWidgets2::gbutton(text = "Invert all validities", handler = invert_all_function, action = NULL, container = btns2)

  btns2[2, 5, expand = TRUE] <- ""

  close_function <- function(h, ...) {
    aux <- lapply(tbl2, function(x) as.data.frame(x[, c("Index", "Valid")]))
    x <- data.table::rbindlist(aux)
    graphical_valid <<- x$Valid[order(x$Index)]

    aux <- rle(graphical_valid)
    aux <- data.frame(Value = aux[[2]], n = aux[[1]])
    aux$stop <- cumsum(aux$n)
    aux$start <- c(1, aux$stop[-1] - (aux$n[-1] - 1))
    aux$combine <- aux$start != aux$stop
    aux$final <- aux$start
    aux$final[aux$combine] <- paste(aux$start[aux$combine], aux$stop[aux$combine], sep = ":")

    valid.summary <- aux[, c("final", "Value")]
    colnames(valid.summary) <- c("Detections", "Validity")

    if (exists("confirm") && gWidgets2::isExtant(confirm))
      gWidgets2::dispose(confirm)

    confirm <<- gWidgets2::gwindow(paste0("Confirm"), width = 300, height = 300)
    confirm_layout <- gWidgets2::ggroup(horizontal = FALSE, container = confirm)
    gWidgets2::glabel("Confirm the following validity ranges.", container = confirm_layout)
    gWidgets2::gtable(valid.summary, multiple = TRUE, expand = TRUE, container = confirm_layout)
    confirm_btns <- gWidgets2::glayout(container = confirm_layout)

    confirm_function <- function(h, ...) {
      gWidgets2::dispose(confirm)
      gWidgets2::dispose(w2)
    }
    confirm_btns[1, 1, expand = TRUE] <- gWidgets2::gbutton(text = "Confirm", handler = confirm_function, action = NULL, container = confirm_btns)

    abort_function <- function(h, ...) {
      gWidgets2::dispose(confirm)
    }
    confirm_btns[1, 2, expand = TRUE] <- gWidgets2::gbutton(text = "Return", handler = abort_function, action = NULL, container = confirm_btns)      
  }
  btns2[2, 6] <- gWidgets2::gbutton(text = "Submit and close", handler = close_function, action = NULL, container = btns2)

  gWidgets2::dispose(placeholder)
  gWidgets2::visible(w2) <- TRUE

  if (!silent)
    message("M: Make any necessary edits in the external visualization window and submit the result to continue the analysis.\nNote: You can use Ctrl and Shift to select multiple detections, and Ctrl+A to select all events at once."); flush.console()

  while (gWidgets2::isExtant(w2)) {}

  if (is.null(graphical_valid)) {
    appendTo(c("Screen", "Warning", "Report"), "External visualization window was closed before result submission. Assuming no changes are to be made.")
    graphical_valid <- to.print$Valid
  }

  return(graphical_valid)
}  # nocov end

#' Detections Widget (Single table version)
#' 
#' @inheritParams widget_args
#' 
#' @return A vector of detection validities.
#' 
#' @keywords internal
#' 
detectionsSingleWidget <- function(event, tag, to.print, silent) { # nocov start
  appendTo("debug", "Running detectionsSingleWidget.")

  # initiate button variables
  confirm <- NULL
  graphical_valid <- NULL

  placeholder <- gWidgets2::gwindow("Please wait", width = 300, height = 20)
  on.exit({if(gWidgets2::isExtant(placeholder)) gWidgets2::dispose(placeholder)}, add = TRUE)
  placeholder_layout <- gWidgets2::ggroup(horizontal = FALSE, container = placeholder)
  gWidgets2::glabel("The GUI window is loading...", container = placeholder_layout)

  message("M: Please wait while the GUI loads."); flush.console()

  w2 <- gWidgets2::gwindow(paste0("Detections for valid event ", event, " from tag ", tag ,"."),
                           width = 900, height = 500, visible = FALSE)

  on.exit({if(gWidgets2::isExtant(w2)) gWidgets2::dispose(w2)}, add = TRUE)
  
  g2 <- gWidgets2::ggroup(horizontal = FALSE, container = w2)
  hdr2 <- gWidgets2::glayout(container = g2)
  hdr2[1, 1, expand = TRUE] <- gWidgets2::glabel("Usage notes:\n   - Edit detection validity by selecting rows and choosing the desired action below.\n   - Loading large tables can take some time. Please wait until the interaction buttons show up at the bottom of this window.", container = hdr2)

  tbl2 <- gWidgets2::gtable(to.print, multiple = TRUE, expand = TRUE, container = g2)

  btns2 <- gWidgets2::glayout(container = g2)

  invalid_selected_function <- function(h, ...) {
    tbl2[match(tbl2$get_value(), tbl2[, "Index"]), "Valid"] <- rep(FALSE, length(tbl2$get_value()))
  }
  btns2[1, 1] <- gWidgets2::gbutton(text = "Invalidate selected", handler = invalid_selected_function, action = NULL, container = btns2)

  reset_selected_function <- function(h, ...) {
    tbl2[match(tbl2$get_value(), tbl2[, "Index"]), "Valid"] <- rep(TRUE, length(tbl2$get_value()))
  }
  btns2[2, 1] <- gWidgets2::gbutton(text = "Revalidate selected", handler = reset_selected_function, action = NULL, container = btns2)

  invalid_all_function <- function(h, ...) {
    tbl2[, "Valid"] <- rep(FALSE, nrow(tbl2))
  }
  btns2[1, 2] <- gWidgets2::gbutton(text = "Invalidate all", handler = invalid_all_function, action = NULL, container = btns2)

  reset_all_function <- function(h, ...) {
    tbl2[, "Valid"] <- rep(TRUE, nrow(tbl2))
  }
  btns2[2, 2] <- gWidgets2::gbutton(text = "Revalidate all", handler = reset_all_function, action = NULL, container = btns2)

  invert_all_function <- function(h, ...) {
    tbl2[, "Valid"] <- !tbl2[, "Valid"]
  }
  btns2[1, 3] <- gWidgets2::gbutton(text = "Invert all validities", handler = invert_all_function, action = NULL, container = btns2)

  btns2[2, 4, expand = TRUE] <- ""

  close_function <- function(h, ...) {
    x <- as.data.frame(tbl2[, c("Index", "Valid")])
    graphical_valid <<- x$Valid[order(x$Index)]

    aux <- rle(graphical_valid)
    aux <- data.frame(Value = aux[[2]], n = aux[[1]])
    aux$stop <- cumsum(aux$n)
    aux$start <- c(1, aux$stop[-1] - (aux$n[-1] - 1))
    aux$combine <- aux$start != aux$stop
    aux$final <- aux$start
    aux$final[aux$combine] <- paste(aux$start[aux$combine], aux$stop[aux$combine], sep = ":")

    valid.summary <- aux[, c("final", "Value")]
    colnames(valid.summary) <- c("Detections", "Validity")

    if (exists("confirm") && gWidgets2::isExtant(confirm))
      gWidgets2::dispose(confirm)

    confirm <<- gWidgets2::gwindow(paste0("Confirm"), width = 300, height = 300)
    confirm_layout <- gWidgets2::ggroup(horizontal = FALSE, container = confirm)
    gWidgets2::glabel("Confirm the following validity ranges.", container = confirm_layout)
    gWidgets2::gtable(valid.summary, multiple = TRUE, expand = TRUE, container = confirm_layout)
    confirm_btns <- gWidgets2::glayout(container = confirm_layout)

    confirm_function <- function(h, ...) {
      gWidgets2::dispose(confirm)
      gWidgets2::dispose(w2)
    }
    confirm_btns[1, 1, expand = TRUE] <- gWidgets2::gbutton(text = "Confirm", handler = confirm_function, action = NULL, container = confirm_btns)

    abort_function <- function(h, ...) {
      gWidgets2::dispose(confirm)
    }
    confirm_btns[1, 2, expand = TRUE] <- gWidgets2::gbutton(text = "Return", handler = abort_function, action = NULL, container = confirm_btns)      
  }
  btns2[2, 5] <- gWidgets2::gbutton(text = "Submit and close", handler = close_function, action = NULL, container = btns2)

  gWidgets2::dispose(placeholder)
  gWidgets2::visible(w2) <- TRUE

  if (!silent)
    message("M: Make any necessary edits in the external visualization window and submit the result to continue the analysis.\nNote: You can use Ctrl and Shift to select multiple detections, and Ctrl+A to select all events at once."); flush.console()

  while (gWidgets2::isExtant(w2)) {}

  if (is.null(graphical_valid)) {
    appendTo(c("Screen", "Warning", "Report"), "External visualization window was closed before result submission. Assuming no changes are to be made.")
    graphical_valid <- to.print$Valid
  }

  return(graphical_valid)
} # nocov end
