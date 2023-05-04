#' movements.R arguments
#' @param arrmoves the array movements list
#' @param secmoves the section movements list
#' @param bio The biometrics table.
#' @param detections.list A list of the detections split by each target tag.
#' @param dist.mat A matrix of the distances between the deployed ALS.
#' @param spatial The spatial list.
#' @param arrays A list containing information for each array.
#' @param movements the movement dataframe for the tag being analysed.
#' @param tag The tag ID of the animal currently being analysed
#' @param type The type of movements being analysed. One of "array" or "section".
#' @param valid.dist Logical: Is a valid distances matrix being used?
#' @name move_args
#' @keywords internal
NULL

#' Group movements
#'
#' Crawls trough the detections of each tag and groups them based on ALS arrays and time requirements.
#' 
#' @inheritParams move_args
#' @inheritParams explore
#'
#' @return A list containing the movement events for each tag.
#'
#' @keywords internal
#'
groupMovements <- function(detections.list, bio, spatial, speed.method, max.interval, tz, dist.mat) {
  appendTo("debug", "Running groupMovements.")
  trigger.unknown <- FALSE
  round.points <- floor(seq(from = length(detections.list)/10, to = length(detections.list), length.out = 10))
  counter <- 1
  {
    if (interactive())
      pb <- txtProgressBar(min = 0, max = sum(sapply(detections.list, function(x) sum(x$Valid))), style = 3, width = 60)
    movements <- lapply(names(detections.list), function(i) {
      appendTo("debug", paste0("Debug: (Movements) Analysing tag ", i, "."))
      x <- detections.list[[i]]
      # discount invalid detections
      x <- x[x$Valid, ]

      # only start if there are valid detections
      if (nrow(x) > 0) {
        if (attributes(dist.mat)$valid) {
          recipient <- data.frame(
            Array = NA_character_,
            Section = NA_character_,
            Detections = NA_integer_,
            First.station = NA_character_,
            Last.station = NA_character_,
            First.time = as.POSIXct(NA_character_, tz = tz, format = "%y-%m-%d %H:%M:%S"),
            Last.time = as.POSIXct(NA_character_, tz = tz, format = "%y-%m-%d %H:%M:%S"),
            Time.travelling = NA_character_,
            Time.in.array = NA_character_,
            Average.speed.m.s = NA_real_,
            Valid = NA,
            stringsAsFactors = FALSE
            )
        } else {
          recipient <- data.frame(
            Array = NA_character_,
            Section = NA_character_,
            Detections = NA_integer_,
            First.station = NA_character_,
            Last.station = NA_character_,
            First.time = as.POSIXct(NA_character_, tz = tz, format = "%y-%m-%d %H:%M:%S"),
            Last.time = as.POSIXct(NA_character_, tz = tz, format = "%y-%m-%d %H:%M:%S"),
            Time.travelling = NA_character_,
            Time.in.array = NA_character_,
            Valid = NA,
            stringsAsFactors = FALSE
            )
        }
        z <- 1
        array.shifts <- c(which(x$Array[-1] != x$Array[-length(x$Array)]), nrow(x))
        time.shifts <- which(difftime(x$Timestamp[-1], x$Timestamp[-length(x$Timestamp)], units = "mins") > max.interval)
        all.shifts <- sort(unique(c(array.shifts, time.shifts)))
        capture <- lapply(seq_len(length(all.shifts)), function(j) {
          if (j == 1)
            start <- 1
          else
            start <- all.shifts[j - 1] + 1
          stop <- all.shifts[j]
          recipient[z, "Array"] <<- paste(x$Array[start])
          recipient[z, "Section"] <<- paste(x$Section[start])
          recipient[z, "Detections"] <<- stop - start + 1
          recipient[z, "First.station"] <<- paste(x$Standard.name[start])
          recipient[z, "First.time"] <<- x$Timestamp[start]
          recipient[z, "Last.station"] <<- paste(x$Standard.name[stop])
          recipient[z, "Last.time"] <<- x$Timestamp[stop]
          z <<- z + 1
          counter <<- counter + stop - start + 1
          if (i == tail(names(detections.list), 1))
            counter <<- sum(unlist(lapply(detections.list, nrow)))
          if (interactive())
            setTxtProgressBar(pb, counter)
          flush.console()
        })
        counter <<- counter

        recipient$Valid <- TRUE
        if (any(link <- recipient$Array == "Unknown")) {
          recipient$Valid[recipient$Array == "Unknown"] <- FALSE
          trigger.unknown <<- TRUE
        }

        recipient <- data.table::as.data.table(recipient)
        recipient <- movementTimes(movements = recipient, type = "array")
        if (attributes(dist.mat)$valid)
          recipient <- movementSpeeds(movements = recipient,
            speed.method = speed.method, dist.mat = dist.mat)

        attributes(recipient)$p.type <- "Auto"
      } else {
        recipient <- NULL
      }
      return(recipient)
    })
    names(movements) <- names(detections.list)

    # remove potentially empty movement sets
    movements <- movements[sapply(movements, function(x) !is.null(x))]

    if (interactive())
      close(pb)
  }

  if (trigger.unknown)
    warning("Movement events at 'Unknown' locations have been rendered invalid.", immediate. = TRUE, call. = FALSE)

  appendTo("debug", "Done.")
  return(movements)
}

#' Removes invalid events
#'
#' Remove invalid movement events and recalculate times/speeds.
#'
#' @inheritParams move_args
#' @inheritParams explore
#'
#' @return The movement data frame containing only valid events for the target tag.
#'
#' @keywords internal
#'
simplifyMovements <- function(movements, discard.first, tag, bio, speed.method, dist.mat) {
  # NOTE: The NULL variables below are actually column names used by data.table.
  # This definition is just to prevent the package check from issuing a note due unknown variables.
  Valid <- NULL

  if (any(movements$Valid)) {
    simple.movements <- movements[(Valid), ]
    aux <- movementTimes(movements = simple.movements, type = "array")
    if (attributes(dist.mat)$valid)
        aux <- movementSpeeds(movements = aux, speed.method = speed.method, dist.mat = dist.mat)

    if (is.null(discard.first)) {
      output <- speedReleaseToFirst(tag = tag, bio = bio, movements = aux,
        dist.mat = dist.mat, speed.method = speed.method)
    } else {
      output <- aux
    }

    return(output)
  } else {
    return(NULL)
  }
}


#' Calculate time and speed
#'
#' Triggers movementTimes and also calculates the speed between events.
#'
#' @inheritParams move_args
#' @inheritParams explore
#'
#' @return The movement data frame with speed calculations for the target tag.
#'
#' @keywords internal
#'
movementSpeeds <- function(movements, speed.method, dist.mat) {
  appendTo("debug", "Running movementSpeeds.")
  movements$Average.speed.m.s[1] <- NA
  if (nrow(movements) > 1) {
    capture <- lapply(2:nrow(movements), function(i) {
      if (movements$Array[i] != movements$Array[i - 1] & all(!grep("^Unknown$", movements$Array[(i - 1):i]))) {
        if (speed.method == "last to first"){
          a.sec <- as.vector(difftime(movements$First.time[i], movements$Last.time[i - 1], units = "secs"))
          my.dist <- dist.mat[movements$First.station[i], gsub(" ", ".", movements$Last.station[i - 1])]
        }
        if (speed.method == "last to last"){
          a.sec <- as.vector(difftime(movements$Last.time[i], movements$Last.time[i - 1], units = "secs"))
          my.dist <- dist.mat[movements$Last.station[i], gsub(" ", ".", movements$Last.station[i - 1])]
        }
        movements$Average.speed.m.s[i] <<- round(my.dist/a.sec, 6)
        rm(a.sec, my.dist)
      } else {
        movements$Average.speed.m.s[i] <<- NA_real_
      }
    })
  }
  return(movements)
}

#' Calculate movement times
#'
#' Determines the duration of an event and the time from an event to the next.
#'
#' @inheritParams move_args
#'
#' @return The movement data frame with time calculations for the target tag.
#'
#' @keywords internal
#'
movementTimes <- function(movements, type = c("array", "section")){
  type = match.arg(type)
  time.in <- paste0("Time.in.", type)
  appendTo("debug", "Running movementTimes.")
  # Time travelling
  if (nrow(movements) > 1) {
    aux <- data.frame(
      First.time = movements$First.time[-1],
      Last.time = movements$Last.time[-nrow(movements)]
      )
    recipient <- apply(aux, 1, function(x) {
      s <- as.vector(difftime(x[1], x[2], units = "secs"))
      h <- floor(s / 3600)
      s <- s - (3600 * h)
      m <- floor(s / 60)
      s <- round(s - (60 * m), 0)
      if (m < 10)
        m <- paste0("0", m)
      if (s < 10)
        s <- paste0("0", s)
      return(paste(h, m, s, sep = ":"))
    })
    movements$Time.travelling[2:nrow(movements)] <- recipient
  }
  # Time on array
  movements[, time.in] <- apply(movements, 1, function(x) {
    if (x["Detections"] == 1) {
      return("0:00:00")
    } else {
      s <- as.vector(difftime(x["Last.time"], x["First.time"], units = "secs"))
      h <- floor(s / 3600)
      s <- s - (3600 * h)
      m <- floor(s / 60)
      s <- round(s - (60 * m), 0)
      if (m < 10)
        m <- paste0("0", m)
      if (s < 10)
        s <- paste0("0", s)
      return(paste(h, m, s, sep = ":"))
    }
  })
  return(movements)
}

#' Calculate time and speed since release.
#'
#' @inheritParams move_args
#' @inheritParams explore
#'
#' @return The movement data frame containing time and speed from release to first event.
#'
#' @keywords internal
#'
speedReleaseToFirst <- function(tag, bio, movements, dist.mat, speed.method){
  appendTo("debug", "Running speedReleaseToFirst.")
  the.row <- match(tag, bio$Transmitter)
  if (is.na(the.row) | length(the.row) != 1)
    stopAndReport('This error should never happen. Contact the developer. [tag not in bio$Transmitter, or match is not unique]')
  origin.time <- bio[the.row, "Release.date"]
  origin.place <- as.character(bio[the.row, "Release.site"])
  if (origin.time <= movements$First.time[1]) {
    s <- as.vector(difftime(movements$First.time[1], origin.time, units = "secs"))
    h <- floor(s / 3600)
    s <- s - (3600 * h)
    m <- floor(s / 60)
    s <- round(s - (60 * m), 0)
    if (m < 10)
      m <- paste0("0", m)
    if (s < 10)
      s <- paste0("0", s)
    movements$Time.travelling[1] <- paste(h, m, s, sep = ":")
    if (attributes(dist.mat)$valid & movements$Array[1] != "Unknown") {
      if (speed.method == "last to first") {
        a.sec <- as.vector(difftime(movements$First.time[1], origin.time, units = "secs"))
        my.dist <- dist.mat[movements$First.station[1], origin.place]
        movements$Average.speed.m.s[1] <- round(my.dist/a.sec, 6)
      }
      if (speed.method == "last to last") {
        a.sec <- as.vector(difftime(movements$Last.time[1], origin.time, units = "secs"))
        my.dist <- dist.mat[movements$Last.station[1], origin.place]
        movements$Average.speed.m.s[1] <- round(my.dist/a.sec, 6)
      }
    }
  } else {
    movements$Time.travelling[1] <- NA
    movements$Average.speed.m.s[1] <- NA
  }
  return(movements)
}

#' Compress array-movements into section-movements
#'
#' @inheritParams move_args
#'
#' @return A data frame containing the section movements for the target tag.
#'
#' @keywords internal
#'
sectionMovements <- function(movements, spatial, valid.dist) {
  Valid <- NULL

  if (any(movements$Valid))
    vm <- movements[(Valid)]
  else
    return(NULL)

  # determine section of each array movement
  aux <- lapply(seq_along(spatial$array.order), function(i) {
    arrays <- spatial$array.order[[i]]
    x <- rep(NA_character_, nrow(vm))
    x[matchl(vm$Array, arrays)] <- names(spatial$array.order)[i]
    return(x)
  })

  # combine object above into single vector
  event.index <- combine(aux)
  
  # determine in which array movements the tag changed section
  aux <- rle(event.index)
  last.events <- cumsum(aux$lengths)
  first.events <- c(1, last.events[-length(last.events)] + 1)

  if (valid.dist) {
    the.speeds <- unlist(lapply(seq_along(aux$values), function(i) {
        if (last.events[i] == first.events[i]) {
          return(NA_real_)
        } else {
          # cut the first event as it displays the speed from the previous section to this one
          return(mean(vm$Average.speed.m.s[(first.events[i] + 1):last.events[i]], na.rm = TRUE))
        }
      }))
    recipient <- data.frame(
      Section = aux$values,
      Events = aux$lengths,
      Detections = unlist(lapply(seq_along(aux$values), function(i) sum(vm$Detections[first.events[i]:last.events[i]]))),
      First.array = vm$Array[first.events],
      First.station = vm$First.station[first.events],
      Last.array = vm$Array[last.events],
      Last.station = vm$Last.station[last.events],
      First.time = vm$First.time[first.events],
      Last.time = vm$Last.time[last.events],
      Time.travelling = c(vm$Time.travelling[1], rep(NA_character_, length(aux$values) - 1)),
      Time.in.section = rep(NA_character_, length(aux$values)),
      Speed.in.section.m.s = the.speeds,
      Valid = rep(TRUE, length(aux$values)),
      stringsAsFactors = FALSE
      )
  } else {
    recipient <- data.frame(
      Section = aux$values,
      Events = aux$lengths,
      Detections = unlist(lapply(seq_along(aux$values), function(i) sum(vm$Detections[first.events[i]:last.events[i]]))),
      First.array = vm$Array[first.events],
      First.station = vm$First.station[first.events],
      Last.array = vm$Array[last.events],
      Last.station = vm$Last.station[last.events],
      First.time = vm$First.time[first.events],
      Last.time = vm$Last.time[last.events],
      Time.travelling = c(vm$Time.travelling[1], rep(NA_character_, length(aux$values) - 1)),
      Time.in.section = rep(NA_character_, length(aux$values)),
      Valid = rep(TRUE, length(aux$values)),
      stringsAsFactors = FALSE
      )
  }
  output <- as.data.table(movementTimes(movements = recipient, type = "section"))
  attributes(output)$p.type <- attributes(movements)$p.type
  return(output)
}

#' update array-movement validity based on the section-movements
#'
#' @return A list with the updated array movements.
#'
#' @keywords internal
#'
updateValidity <- function(arrmoves, secmoves) {
  Valid <- NULL
  output <- lapply(names(arrmoves),
    function(i) {
      if (!is.null(secmoves[[i]]) && any(!secmoves[[i]]$Valid)) {
        aux <- secmoves[[i]][(!Valid)]
        to.change <- unlist(lapply(1:nrow(aux),
          function(j) {
            A <- which(arrmoves[[i]]$First.time == aux$First.time[j])
            B <- A + (aux$Events[j] - 1)
            return(A:B)
          }))
        appendTo(c("Screen", "Report"), paste0("M: Rendering ", length(to.change), " array movement(s) invalid for tag ", i ," as the respective section movements were discarded by the user."))
        arrmoves[[i]]$Valid[to.change] <- FALSE
        if (attributes(arrmoves[[i]])$p.type == "Auto")
          attributes(arrmoves[[i]])$p.type <- "Manual"
      }
      return(arrmoves[[i]])
    })
  names(output) <- names(arrmoves)
  return(output)
}

#' Wrapper for simplifyMovements
#' 
#' @inheritParams move_args
#' @inheritParams explore
#' @param movements A list of movements for each tag.
#' 
#' @return A list of valid movements
#' 
#' @keywords internal
#' 
assembleValidMoves <- function(movements, bio, discard.first, speed.method, dist.mat) {
  appendTo("debug", "Running assembleValidMoves.")
  counter <- 0
  if (interactive())
    pb <- txtProgressBar(min = 0, max = sum(sapply(movements, nrow)), style = 3, width = 60)

  valid.movements <- lapply(seq_along(movements), function(i) {
    output <- simplifyMovements(movements = movements[[i]], tag = names(movements)[i], bio = bio, 
                                discard.first = discard.first, speed.method = speed.method, dist.mat = dist.mat)
    counter <<- counter + nrow(movements[[i]])
    if (interactive())
      setTxtProgressBar(pb, counter)    
    return(output)
   })

  if (interactive())
    close(pb)
  rm(counter)
  
  names(valid.movements) <- names(movements)
  valid.movements <- valid.movements[!unlist(lapply(valid.movements, is.null))]
  
  return(valid.movements)  
}
  
#' Wrapper for sectionMovements
#' 
#' @inheritParams move_args
#' @inheritParams explore
#' @param valid.moves A list of movements for each tag.
#' 
#' @return A list of valid movements
#' 
#' @keywords internal
#' 
assembleValidSecMoves <- function(valid.moves, spatial, valid.dist) {
  appendTo("debug", "Running assembleValidSecMoves.")

  counter <- 0
  if (interactive())
    pb <- txtProgressBar(min = 0, max = sum(sapply(valid.moves, nrow)), style = 3, width = 60)

  secmoves <- lapply(seq_along(valid.moves), function(i) {
    appendTo("debug", paste0("debug: Compiling valid section movements for tag ", names(valid.moves)[i],"."))

    output <- sectionMovements(movements = valid.moves[[i]], spatial = spatial, 
                               valid.dist = valid.dist)

    counter <<- counter + nrow(valid.moves[[i]])
    if (interactive())
      setTxtProgressBar(pb, counter)    

    return(output)
  })

  if (interactive())
    close(pb)
  rm(counter)
  
  names(secmoves) <- names(valid.moves)
  
  return(secmoves)  
}

