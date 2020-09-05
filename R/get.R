#' Extract speeds from the analysis results.
#' 
#' @inheritParams getTimes
#' @param direct Logical: Extract only speeds between arrays that are directly connected (i.e. neighbouring arrays)?
#' @param type The type of movements to record. One of "all", "forward", or "backward". In the two last options,
#'  only the forward or backwards (relatively to the study area structure) movement speeds are returned.
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
getSpeeds <- function(input, type = c("all", "forward", "backward"), direct = FALSE, n.events = c("first", "all", "last")){
  if (!inherits(input, "list"))
    stop("Could not recognise the input as an actel results object.", call. = FALSE)

  if (is.null(input$valid.movements) | is.null(input$spatial) | is.null(input$rsp.info))
    stop("Could not recognise the input as an actel results object.", call. = FALSE)

  if (!is.null(input$dist.mat) && is.null(attributes(input$dist.mat)$valid))
  	stop("The input object was not compiled using actel 1.1.0 or higher. Please re-run the analysis with the current version of actel.", call. = FALSE)

  if (is.null(input$dist.mat) || !attributes(input$dist.mat)$valid)
  	stop("These results do not contain a valid distances matrix.", call. = FALSE)

	type <- match.arg(type)
	n.events <- match.arg(n.events)
	speed.method <- attributes(input$dist.mat)$speed.method
	to.station.col <- ifelse(speed.method == "last to first", "First.station", "Last.station")
	
	output.list <- lapply(names(input$valid.movements), function(tag) {
		# cat(tag, "\n")
		# treat movements as data frame to avoid data.table shenanigans
		aux <- as.data.frame(input$valid.movements[[tag]])
		# find events with speeds
		to.extract <- which(!is.na(aux$Average.speed.m.s))

		if (direct) {
			if (to.extract[1] == 1) {
				# check that first event is connected to release
				the.release <- input$rsp.info$bio$Release.site[which(input$rsp.info$bio$Transmitter == tag)]
				the.first.array <- input$spatial$release.sites$Array[input$spatial$release.sites$Standard.name == the.release]
				# if not, exclude first event
				if (aux$Array[1] != the.first.array)
					to.extract <- to.extract[-1]
			}

			# check that remaining events are direct
			if (length(to.extract) > 0) {
				# If first event is still present, avoid it
				if (to.extract[1] == 1) {
					if (length(to.extract) > 1) {
						keep <- sapply(to.extract[-1], function(i) {
							the.array <- aux$Array[i]
							return(aux$Array[i - 1] %in% input$arrays[[the.array]]$neighbours)
						})
						to.extract <- to.extract[c(TRUE, keep)]
					}
				# else just work through everything
				} else {
					keep <- sapply(to.extract, function(i) {
						the.array <- aux$Array[i]
						return(aux$Array[i - 1] %in% input$arrays[[the.array]]$neighbours)
					})
					to.extract <- to.extract[keep]
				}
			}
		}

		if (length(to.extract) > 0 & type == "forward") {
			# if first event was selected
			if (to.extract[1] == 1) {
				# check that first event is at expected first array or at some array coming after it
				the.release <- input$rsp.info$bio$Release.site[which(input$rsp.info$bio$Transmitter == tag)]
				the.first.array <- input$spatial$release.sites$Array[input$spatial$release.sites$Standard.name == the.release]
				if (!(aux$Array[1] %in% c(the.first.array, input$arrays[[the.first.array]]$all.after))) #first array must be expected first array or after it
					to.extract <- to.extract[-1]
			}

			# check that remaining events are forward
			if (length(to.extract) > 0) {
				# If first event is still present, avoid it
				if (to.extract[1] == 1) {
					if (length(to.extract) > 1) {
						keep <- sapply(to.extract[-1], function(i) {
							the.array <- aux$Array[i]
							return(aux$Array[i - 1] %in% input$arrays[[the.array]]$all.before) # previous array must be before current array
						})
						to.extract <- to.extract[c(TRUE, keep)]
					}
				# else just work through everything
				} else {
					keep <- sapply(to.extract, function(i) {
						the.array <- aux$Array[i]
						return(aux$Array[i - 1] %in% input$arrays[[the.array]]$all.before) # previous array must be before current array
					})
					to.extract <- to.extract[keep]
				}
			}
		}

		if (length(to.extract) > 0 & type == "backward") {
			# if first event was selected
			if (to.extract[1] == 1) {
				# check that first event is at expected first array or at some array coming after it
				the.release <- input$rsp.info$bio$Release.site[which(input$rsp.info$bio$Transmitter == tag)]
				the.first.array <- input$spatial$release.sites$Array[input$spatial$release.sites$Standard.name == the.release]
				if (!(aux$Array[1] %in% input$arrays[[the.first.array]]$all.before)) # first array must be before expected first array
					to.extract <- to.extract[-1]
			}

			# check that remaining events are forward
			if (length(to.extract) > 0) {
				# If first event is still present, avoid it
				if (to.extract[1] == 1) {
					if (length(to.extract) > 1) {
						keep <- sapply(to.extract[-1], function(i) {
							the.array <- aux$Array[i]
							return(aux$Array[i - 1] %in% input$arrays[[the.array]]$all.after) # previous array must be after current array
						})
						to.extract <- to.extract[c(TRUE, keep)]
					}
				# else just work through everything
				} else {
					keep <- sapply(to.extract, function(i) {
						the.array <- aux$Array[i]
						return(aux$Array[i - 1] %in% input$arrays[[the.array]]$all.after) # previous array must be after current array
					})
					to.extract <- to.extract[keep]
				}
			}
		}

		if (length(to.extract) > 0) {
			if (to.extract[1] == 1) {
				first.line <- data.frame(
					Tag = tag,
					Event = 1,
					From.array = "Release",
					From.station = input$rsp.info$bio$Release.site[which(input$rsp.info$bio$Transmitter == tag)],
					To.array = aux$Array[1],
					To.station = aux[1, to.station.col],
					Speed = aux$Average.speed.m.s[1])
				to.extract <- to.extract[-1]
			} else {
				first.line <- NULL
			}
		} else {
			first.line <- NULL		
		} 

		if (length(to.extract) > 0) {
			other.lines <- data.frame(
				Tag = rep(tag, length(to.extract)),
				Event = to.extract,
				From.array = aux$Array[to.extract - 1],
				From.station = aux$Last.station[to.extract - 1],
				To.array = aux$Array[to.extract],
				To.station = aux[to.extract, to.station.col],
				Speed = aux$Average.speed.m.s[to.extract])
		} else {
			other.lines <- NULL
		}

		output <- rbind(first.line, other.lines)

		if (!is.null(output) & n.events != "all") {
			output$temp_column <- with(output, paste0(From.array, "_", To.array))
			aux <- split(output, output$temp_column)
		
			if (n.events == "first")
				aux <- lapply(aux, function(x) x[1, , drop = FALSE])
			
			if (n.events == "last") 
				aux <- lapply(aux, function(x) x[nrow(x), , drop = FALSE])

			output <- as.data.frame(data.table::rbindlist(aux))
			output <- output[, -match("temp_column", colnames(output))]
		}

		return(output)
	})

	output <- data.table::rbindlist(output.list)

	return(output)
}

#' Extract timestamps from the analysis results.
#'
#' @param input An actel results object generated by \code{\link{explore}}, \code{\link{migration}} or \code{\link{residency}}.
#' @param locations The names of the arrays or sections to be included. If left NULL, information for all arrays/sections is extracted.
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
#' @return A data frame with the timestamps for each tag (rows) and array (columns)
#'
#' @export
#'
getTimes <- function(input, locations = NULL, move.type = c("array", "section"), event.type = c("arrival", "departure"), n.events = c("first", "all", "last")){
  if (!inherits(input, "list"))
    stop("Could not recognise the input as an actel results object.", call. = FALSE)

  if (is.null(input$valid.movements) | is.null(input$spatial) | is.null(input$rsp.info))
    stop("Could not recognise the input as an actel results object.", call. = FALSE)

  move.type <- match.arg(move.type)
  event.type <- match.arg(event.type)
  n.events <- match.arg(n.events)

  if (input$rsp.info$analysis.type == "explore" & move.type == "section")
    stop("Section times are not calculated for analyses of type 'explore'.", call. = FALSE)

  if (move.type == "array")
    movements <- input$valid.movements
  else
    movements <- input$section.movements
  spatial <- input$spatial
  bio <- input$rsp.info$bio

  if (move.type == "array" & any(link <- is.na(match(locations, unique(spatial$stations$Array)))))
    stop(ifelse(sum(link) > 1, "Arrays '", "Array '"),
      paste0(locations[link], collapse = "', '"),
      ifelse(sum(link) > 1, "' are", "' is"), " not part of this study's arrays.", call. = FALSE)

  if (move.type == "section" & any(link <- is.na(match(locations, names(spatial$array.order)))))
    stop(ifelse(sum(link) > 1, "Sections '", "Section '"),
      paste0(locations[link], collapse = "', '"),
      ifelse(sum(link) > 1, "' are", "' is"), " not part of this study's sections.", call. = FALSE)

  if (event.type == "arrival")
    the.column <- "First.time"
  else
    the.column <- "Last.time"

  the.times <- list()

  # extract arrivals or departures
  capture.output <- lapply(movements, function(x) {
    # cat(".\n")
    aux <- x[[the.column]] # data.table syntax
    names(aux) <- x[[1]] # data.table syntax
    the.times[[length(the.times) + 1]] <<- aux
    return(NULL)
  })
  names(the.times) <- names(movements)

  # allow array/section movement flexibility
  if (move.type == "array")
    col.order <- unlist(spatial$array.order)
  else
    col.order <- names(spatial$array.order)

  # find maximum nchar for each tag
  max.char <- sapply(the.times, function(x) nchar(max(table(names(x)))))

  # shuffle data into the right format
  aux <- lapply(col.order, function(i) {
    aux <- lapply(names(the.times), function(j) {
      # cat(j, "\n")
      if (any(link <- names(the.times[[j]]) == i)) {
        output <- data.frame(
          Event = paste(j, stringr::str_pad(1:sum(link), max.char[j], pad = "0"), sep = "_"),
          V1 = the.times[[j]][link]
        )
        colnames(output)[2] <- i
        if (n.events == "first") {
          first.row <- output[1, , drop = FALSE]
          first.row$Event <- j
          return(first.row)
        }
        if (n.events == "last") {
          last.row <- output[nrow(output), , drop = FALSE]
          last.row$Event <- j
          return(last.row)
        }
        if (n.events == "all")
          return(output)
      } else {
        return(NULL)
      }
    })
    names(aux) <- names(the.times)
    output <- as.data.frame(data.table::rbindlist(aux))
    rownames(output) <- output$Event
    output <- output[, -1, drop = FALSE]
    return(output)
  })
  # Ensure all data frames contain the same rows, by the same order
  the.rows <- sort(unique(unlist(lapply(aux, row.names))))
  aux <- lapply(aux, function(x) {
    rows.to.add <- the.rows[!the.rows %in% row.names(x)]
    x[rows.to.add, ] <- NA
    x <- x[the.rows, , drop = FALSE]
    return(x)
  })
  output <- do.call(cbind, aux)

  if (!is.null(locations)) {
    output <- output[, locations, drop = FALSE]
    completely.empty <- apply(output, 1, function(r) all(is.na(r)))
    output <- output[!completely.empty, ,drop = FALSE]
  }

  output$Transmitter <- gsub("_[0-9]*$", "", rownames(output))
  output$Group <- bio$Group[match(output$Transmitter, bio$Transmitter)]
  output <- output[, c(ncol(output)-1, ncol(output), 1:(ncol(output) - 2))]
  rownames(output) <- 1:nrow(output)
  return(output)
}
