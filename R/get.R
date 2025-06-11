#' Extract speeds from the analysis results.
#'
#' @inheritParams getTimes
#' @param direct Logical: Extract only speeds between arrays that are directly
#'        connected (i.e. neighbouring arrays)?
#' @param type The type of movements to record. One of "all", "forward", or
#'        "backward". In the two last options, only the forward or backwards
#'        (relatively to the study area structure) movement speeds are returned.
#'
#' @examples
#' # using the example results loaded with actel
#' getSpeeds(example.results)
#'
#' # You can specify which direction of movement to extract with 'type'
#' getSpeeds(example.results, type = "forward")
#' # or
#' getSpeeds(example.results, type = "backward")
#'
#' # and also how many events per tag (this won't change the output
#' # with the example.results, only because these results are minimal).
#' getSpeeds(example.results, n.events = "first")
#' # or
#' getSpeeds(example.results, n.events = "all")
#' # or
#' getSpeeds(example.results, n.events = "last")
#'
#' @return A data frame with the following columns:
#' \itemize{
#'  \item Tag: The tag of the animal who performed the recorded speed
#'  \item Event: The valid event where the speed was recorded
#'  \item From.array: The array from which the tags left
#'  \item From.station: The station from which the tags left
#'  \item To.array: The array to which the tags arrived
#'  \item To.station: The station to which the tags arrived
#'  \item Speed: The speed recorded in the described movement
#' }
#'
#' @export
#'
getSpeeds <- function(input, type = c("all", "forward", "backward"),
                      direct = FALSE, n.events = c("first", "all", "last")){
  event(type = "debug", "Running getSpeeds.")
  if (!inherits(input, "list")) {
    stop("Could not recognise the input as an actel results object.",
         call. = FALSE)
  }

  check_moves <- is.null(input$valid.movements)
  check_spatial <- is.null(input$spatial)
  check_rsp <- is.null(input$rsp.info)
  if (check_moves | check_spatial | check_rsp) {
    stop("Could not recognise the input as an actel results object.",
         call. = FALSE)
  }
  if (!is.null(input$dist.mat) && is.null(attributes(input$dist.mat)$valid)) {
    stop("The input object was not compiled using actel 1.1.0 or higher.",
         " Please re-run the analysis with the current version of actel.",
         call. = FALSE)
  }

  if (is.null(input$dist.mat) || !attributes(input$dist.mat)$valid) {
    stop("These results do not contain a valid distances matrix.",
         call. = FALSE)
  }

  type <- match.arg(type)
  n.events <- match.arg(n.events)

  output.list <- lapply(names(input$valid.movements), function(tag) {
    # cat(tag, "\n")
    # treat movements as data frame to avoid data.table shenanigans
    moves <- as.data.frame(input$valid.movements[[tag]])
    # find events with speeds
    rows_to_extract <- which(!is.na(moves$Average.speed.m.s))
    # find the release site
    release_link <- which(input$rsp.info$bio$Transmitter == tag)
    the_release <- input$rsp.info$bio$Release.site[release_link]
    # find expected first array
    array_link <- input$spatial$release.sites$Standard.name == the_release
    exp_first_array <- input$spatial$release.sites$Array[array_link]

    if (direct) {
      # if direct, check the neighbours
      rows_to_extract <- getSpeedsChecker(rows = rows_to_extract, moves = moves,
                                          first = exp_first_array,
                                          arrays = input$arrays,
                                          side = "neighbours")
    }

    if (length(rows_to_extract) > 0 & type == "forward") {
      # if forward, check all.before
      expected_arrays <- c(exp_first_array,
                           input$arrays[[exp_first_array]]$all.after)
      rows_to_extract <- getSpeedsChecker(rows = rows_to_extract, moves = moves,
                                          first = expected_arrays,
                                          arrays = input$arrays,
                                          side = "all.before")
    }

    if (length(rows_to_extract) > 0 & type == "backward") {
      # if backward, check all.after
      expected_arrays <- input$arrays[[exp_first_array]]$all.before
      rows_to_extract <- getSpeedsChecker(rows = rows_to_extract, moves = moves,
                                          first = expected_arrays,
                                          arrays = input$arrays,
                                          side = "all.after")
    }

    # Now we know which rows to keep. Compile the output table.
    speed.method <- attributes(input$dist.mat)$speed.method
    output <- getSpeedsCompiler(rows = rows_to_extract, moves = moves,
                                tag = tag, release = the_release,
                                speed.method = speed.method,
                                n.events = n.events)


    return(output)
  })

  # combine the output for every tag into a single table for simplicity
  output <- data.table::rbindlist(output.list)

  return(output)
}

#' Helper function of getSpeeds
#' 
#' Checks if each row should be considered for extraction or not.
#' 
#' @param rows a vector of rows to check.
#' @param moves the respective movements table.
#' @param first a list of valid arrays for the first movement.
#' @param arrays the list of arrays in the study area.
#' @param side which pool of array-relatives to check against.
#' 
#' @return an updated vector of rows to consider for extraction.
#' 
#' @keywords internal
#' 
getSpeedsChecker <- function(rows, moves, first, arrays,
                             side = c("neighbours", "all.before", 
                                      "all.after")) {
  # check that first event is connected to release
  # if not, we won't be extracting that row.
  if (rows[1] == 1 & !(moves$Array[1] %in% first)) {
    rows <- rows[-1]
  }

  # Then, for every other row, check that the move happened at an array that
  # neighbours the previous array where the tag had been detected.
  # (i.e. no skipped arrays)
  if (length(rows) > 0) {
    # Don't check row 1 again.
    if (rows[1] == 1) {
      check <- rows[-1]
      keep <- TRUE
    } else {
      check <- rows
      keep <- NULL
    }
    # If there's anything to check
    if (length(check) > 0) {
      aux <- sapply(check, function(i) {
        the_array <- moves$Array[i]
        return(moves$Array[i - 1] %in% arrays[[the_array]][[side]])
      })
      # this step keeps the first row if it wasn't invalidated earlier
      keep <- c(keep, aux)
    }
    rows <- rows[keep]
  }
}

#' Helper function of getSpeeds
#' 
#' Compiles the final output table for each tag
#' 
#' @inheritParams getSpeedsChecker
#' @param tag the transmitter code being analysed.
#' @param release the release site of the tag.
#' @inheritParams getSpeeds
#' 
#' @return a data.frame with the speeds for the target tag
#' 
#' @keywords internal
#' 
getSpeedsCompiler <- function(rows, moves, tag, release,
                              speed.method, n.events) {
    if (grepl("^first", speed.method)) {
      from_col <- "First.station"
    } else {
      from_col <- "Last.station"
    }

    if (grepl("first$", speed.method)) {
      to_col <- "First.station"
    } else {
      to_col <- "Last.station"
    }

    # compile the release line separately
    if (length(rows) > 0) {
      if (rows[1] == 1) {
        first_line <- data.frame(
          Tag = tag,
          Event = 1,
          From.array = "Release",
          From.station = release,
          To.array = moves$Array[1],
          To.station = moves[1, to_col],
          Speed = moves$Average.speed.m.s[1])
        rows <- rows[-1]
      } else {
        first_line <- NULL
      }
    } else {
      first_line <- NULL
    }

    # then prepare the other lines
    if (length(rows) > 0) {
      other_lines <- data.frame(
        Tag = rep(tag, length(rows)),
        Event = rows,
        From.array = moves$Array[rows - 1],
        From.station = moves[rows - 1, from_col],
        To.array = moves$Array[rows],
        To.station = moves[rows, to_col],
        Speed = moves$Average.speed.m.s[rows])
    } else {
      other_lines <- NULL
    }

    # and merge both fragments
    output <- rbind(first_line, other_lines)

    # and then discard any rows we don't want.
    if (!is.null(output) & n.events != "all") {
      output$temp_column <- with(output, paste0(From.array, "_", To.array))
      aux <- split(output, output$temp_column)

      if (n.events == "first") {
        aux <- lapply(aux, function(x) x[1, , drop = FALSE])
      }

      if (n.events == "last") {
        aux <- lapply(aux, function(x) x[nrow(x), , drop = FALSE])
      }

      output <- as.data.frame(data.table::rbindlist(aux))
      output <- output[, -match("temp_column", colnames(output))]
    }

    return(output)
}

#' Extract timestamps from the analysis results.
#'
#' @param input An actel results object generated by \code{\link{explore}},
#'        \code{\link{migration}} or \code{\link{residency}}.
#' @param locations The names of the arrays or sections to be included.
#'        If left NULL, information for all arrays/sections is extracted.
#' @param move.type The type of events to record: one of "array" or "section".
#' @param event.type The point to be recorded: one of "arrival" or "departure".
#' @param n.events The events to record. One of "first", "all", or "last".
#'
#' @examples
#' # using the example results loaded with actel
#' getTimes(example.results)
#'
#' # You can specify which events to extract with 'event.type'
#' getTimes(example.results, event.type = "arrival")
#' # or
#' getTimes(example.results, event.type = "departure")
#'
#' # and also how many events per tag.
#' getTimes(example.results, n.events = "first")
#' # or
#' getTimes(example.results, n.events = "all")
#' # or
#' getTimes(example.results, n.events = "last")
#'
#' @return A data frame with the timestamps for each tag (rows) and
#'         array (columns)
#'
#' @export
#'
getTimes <- function(input, locations = NULL, move.type = c("array", "section"),
                     event.type = c("arrival", "departure"),
                     n.events = c("first", "all", "last")){
  event(type = "debug", "Running getTimes.")
  if (!inherits(input, "list")) {
    stop("Could not recognise the input as an actel results object.",
         call. = FALSE)
  }

  check_moves <- is.null(input$valid.movements)
  check_spatial <- is.null(input$spatial)
  check_rsp <- is.null(input$rsp.info)
  if (check_moves | check_spatial | check_rsp) {
    stop("Could not recognise the input as an actel results object.",
         call. = FALSE)
  }

  move.type <- match.arg(move.type)
  event.type <- match.arg(event.type)
  n.events <- match.arg(n.events)

  if (input$rsp.info$analysis.type == "explore" & move.type == "section") {
    stop("Section times are not calculated for analyses of type 'explore'.",
         call. = FALSE)
  }

  if (move.type == "array") {
    moves <- input$valid.movements
  } else {
    moves <- input$section.movements
  }
  spatial <- input$spatial
  bio <- input$rsp.info$bio

  if (move.type == "array") {
    link <- is.na(match(locations, unique(spatial$stations$Array)))
    if (any(link)) {
      stop(ifelse(sum(link) > 1, "Arrays '", "Array '"),
        paste0(locations[link], collapse = "', '"),
        ifelse(sum(link) > 1, "' are", "' is"),
        " not part of this study's arrays.", call. = FALSE)
    }
  }

  if (move.type == "section") {
    link <- is.na(match(locations, names(spatial$array.order)))
    if (any(link)) {
    stop(ifelse(sum(link) > 1, "Sections '", "Section '"),
      paste0(locations[link], collapse = "', '"),
      ifelse(sum(link) > 1, "' are", "' is"),
      " not part of this study's sections.", call. = FALSE)
    }
  }

  if (event.type == "arrival") {
    the_col <- "First.time"
  } else {
    the_col <- "Last.time"
  }

  the_times <- list()

  # extract arrivals or departures
  capture <- lapply(moves, function(x) {
    # cat(".\n")
    aux <- x[[the_col]] # data.table syntax
    names(aux) <- x[[1]] # data.table syntax
    the_times[[length(the_times) + 1]] <<- aux
    return(NULL)
  })
  names(the_times) <- names(moves)

  # allow array/section movement flexibility
  if (move.type == "array") {
    col_order <- unlist(spatial$array.order)
  } else {
    col_order <- names(spatial$array.order)
  }


  # find maximum order of magnitude in number of events for each array/section
  # for each tag. This is used to pad the event names.
  mag <- sapply(the_times, function(x) nchar(max(table(names(x)))))

  # shuffle data into the right format
  list_output <- lapply(col_order, function(loc) {
    tag_output <- lapply(names(the_times), function(tag) {
      # cat(tag, "\n")
      link <- names(the_times[[tag]]) == loc
      if (any(link)) {
        # pad event numbers so they all align nicely
        padded_event_n <- stringr::str_pad(1:sum(link), mag[tag], pad = "0")
        # start output with one col then add the second.
        output <- data.frame(Event = paste0(tag, "_", padded_event_n))
        output[, loc] <- the_times[[tag]][link]
        # now trim out unneeded rows. Not the most efficient way to go about
        # this but it works for now, so leaving it be.
        if (n.events == "first") {
          first.row <- output[1, , drop = FALSE]
          # we can drop the event number since there's only one event per tag
          first.row$Event <- tag
          return(first.row)
        }
        if (n.events == "last") {
          last.row <- output[nrow(output), , drop = FALSE]
          # same as above
          last.row$Event <- tag
          return(last.row)
        }
        if (n.events == "all")
          return(output)
      } else {
        return(NULL)
      }
    })
    names(tag_output) <- names(the_times)
    loc_output <- as.data.frame(data.table::rbindlist(tag_output))
    rownames(loc_output) <- loc_output$Event
    loc_output <- loc_output[, -1, drop = FALSE]
    return(loc_output)
  })
  # Ensure all data frames contain the same rows, by the same order
  the_rows <- sort(unique(unlist(lapply(list_output, row.names))))
  list_output <- lapply(list_output, function(x) {
    rows_to_add <- the_rows[!the_rows %in% row.names(x)]
    x[rows_to_add, ] <- NA
    x <- x[the_rows, , drop = FALSE]
    return(x)
  })
  # so that we can bind it all together.
  output <- do.call(cbind, list_output)

  # trim only to the desired arrays/sections, if specificied
  if (!is.null(locations)) {
    output <- output[, locations, drop = FALSE]
    completely_empty <- apply(output, 1, function(r) all(is.na(r)))
    output <- output[!completely_empty, ,drop = FALSE]
  }

  output$Transmitter <- gsub("_[0-9]*$", "", rownames(output))
  output$Group <- bio$Group[match(output$Transmitter, bio$Transmitter)]
  # move new columns around
  output <- output[, c(ncol(output)-1, ncol(output), 1:(ncol(output) - 2))]
  rownames(output) <- 1:nrow(output)

  return(output)
}
