#' update array-movement validity based on the section-movements
#' 
#' @param arrmoves the array movements
#' @param secmoves the section movements
#' 
#' @return the updated array movements
#' 
#' @keywords internal
#' 
updateAMValidity <- function(arrmoves, secmoves) {
  Valid <- NULL
  output <- lapply(names(secmoves), 
    function(i) {
      if (any(!secmoves[[i]]$Valid)) {
        aux <- secmoves[[i]][(!Valid)]
        to.change <- unlist(lapply(1:nrow(aux),
          function(j) {
            A <- which(arrmoves[[i]]$First.time == aux$First.time)
            B <- A + (aux$Events - 1)
            return(A:B)
          }))
        appendTo(c("Screen", "Report"), paste0("M: Rendering ", length(to.change), " array movements invalid for fish ", i ," as the respective section movements were discarded by the user."))
        arrmoves[[i]]$Valid[to.change] <- FALSE
      }
      return(arrmoves[[i]])
    })
  names(output) <- names(secmoves)
  return(output)
}

#' Compress array-movements into section-movements
#' 
#' @inheritParams simplifyMovements
#' @inheritParams migration
#' 
#' @return the section movements
#' 
#' @keywords internal
#' 
sectionMovements <- function(movements, sections) {
	Valid <- NULL
	output <- list()
	for (fish in names(movements)) {
		valid.movements <- movements[[fish]][(Valid)]
		if (nrow(valid.movements) > 0) {
			aux <- lapply(seq_along(sections), function(i) {
				x <- rep(NA_character_, nrow(valid.movements))
				x[grepl(sections[i], valid.movements$Array)] <- sections[i]
				return(x)
				})

			event.index <- combine(aux)
			aux <- rle(event.index)
			last.events <- cumsum(aux$lengths)
			first.events <- c(1, last.events[-length(last.events)] + 1)
			
			recipient <- data.frame(
				Section = aux$values,
				Events = aux$lengths,
				Detections = unlist(lapply(seq_along(aux$values), function(i) sum(valid.movements$Detections[first.events[i]:last.events[i]]))),
				First.array = valid.movements$Array[first.events],
				Last.array = valid.movements$Array[last.events],
				First.time = valid.movements$First.time[first.events],
				Last.time = valid.movements$Last.time[last.events],
				Time.travelling = c(valid.movements$Time.travelling[1], rep(NA_character_, length(aux$values) - 1)),
				Time.in.section = rep(NA_character_, length(aux$values)),
				Speed.in.section.m.s = unlist(lapply(seq_along(aux$values), function(i) mean(valid.movements$Average.speed.m.s[first.events[i]:last.events[i]], na.rm = TRUE))),
				Valid = rep(TRUE, length(aux$values)),
				stringsAsFactors = FALSE
				)
			output[[length(output) + 1]] <- as.data.table(movementTimes(movements = recipient, type = "section"))
			names(output)[length(output)] <- fish
			attributes(output[[length(output)]])$p.type <- attributes(movements[[fish]])$p.type
		}
	}
	return(output)
}

#' Collect summary information for the residency analysis
#' 
#' @param secmoves the section-movements
#' @param movements the array-movements
#' @inheritParams migration
#' 
#' @return A residency summary table
#' 
#' @keywords internal
#' 
assembleResidency <- function(secmoves, movements, sections) {
  Last.array <- NULL
  Last.time <- NULL
  Valid <- NULL
  Section <- NULL
  
  recipient <- vector()
  for (i in seq_along(sections)) {
    recipient <- c(recipient, paste(c("Total.time", "Times.entered", "Average.entry", "Average.time", "Average.departure"), sections[i], sep = "."))
  }
  recipient <- c(recipient, "Very.last.array", "Very.last.time", "Status", "Valid.detections", "Invalid.detections", "Valid.events", "Invalid.events", "P.type")

  res.df <- matrix(nrow = length(secmoves), ncol = length(recipient))
  res.df <- as.data.frame(res.df, stringsAsFactors = FALSE)
  
  colnames(res.df) <- recipient
  rm(recipient)
  rownames(res.df) <- names(secmoves)
	  
  for (fish in names(secmoves)) {
  	aux <- split(secmoves[[fish]], secmoves[[fish]]$Section)
  	recipient <- lapply(seq_along(aux), function(i) {
  		recipient <- rep(NA, ncol(res.df))
  		names(recipient) <- colnames(res.df)
  		recipient <- t(as.data.frame(recipient))
  		total.time <- apply(aux[[i]][, c("First.time", "Last.time")], 1, function(x) difftime(x[2], x[1], units = "secs"))
  		recipient[1, paste0("Total.time.", names(aux)[i])] <- sum(total.time)
  		recipient[1, paste0("Times.entered.", names(aux)[i])] <- nrow(aux[[i]])
  		entry.time <- mean(circular::circular(decimalTime(substr(aux[[i]]$First.time, start = 12, stop = 20)), units = "hours", template = "clock24"))
  		if (entry.time < 0)
  			entry.time <- 24 + entry.time
  		recipient[1, paste0("Average.entry.", names(aux)[i])] <- minuteTime(entry.time, format = "h", seconds = FALSE)
  		leave.time <- mean(circular::circular(decimalTime(substr(aux[[i]]$Last.time, start = 12, stop = 20)), units = "hours", template = "clock24"))
  		recipient[1, paste0("Average.time.", names(aux)[i])] <- mean(total.time)
  		if (leave.time < 0)
  			leave.time <- 24 + leave.time
  		recipient[1, paste0("Average.departure.", names(aux)[i])] <- minuteTime(leave.time, format = "h", seconds = FALSE)
  		return(recipient)
  	})
  	recipient <- as.data.frame(combine(recipient), stringsAsFactors = FALSE)
  	recipient$Very.last.array <- secmoves[[fish]][.N, Last.array]
  	recipient$Very.last.time <- as.character(secmoves[[fish]][.N, Last.time])
  	recipient$Status <- paste0("Disap. in ", secmoves[[fish]][.N, Section])
  	recipient$Valid.detections <- sum(secmoves[[fish]]$Detections)
  	recipient$Valid.events <- sum(movements[[fish]]$Valid)
  	if (any(!movements[[fish]]$Valid)) { 
  		recipient$Invalid.detections <- sum(movements[[fish]][!(Valid)]$Detections)
  		recipient$Invalid.events <- sum(!movements[[fish]]$Valid)
  	} else {
  		recipient$Invalid.detections <- 0
  		recipient$Invalid.events <- 0
  	}
  	recipient$P.type <- attributes(secmoves[[fish]])$p.type
  	res.df[fish, ] <- recipient
  	rm(recipient)
  }
	# Convert time data
	for (section in sections) {
		for (the.col in c("Average.time.", "Total.time.")) {
  		res.df[, paste0(the.col, section)] <- as.numeric(res.df[, paste0(the.col, section)])
  		units(res.df[, paste0(the.col, section)]) <- "secs"
  		aux <- mean(res.df[, paste0(the.col, section)], na.rm = TRUE)
  		if (as.numeric(aux) > 86400)
  			units(res.df[, paste0(the.col, section)]) <- "days"
  		if (as.numeric(aux) <= 86400 & as.numeric(aux) > 3600)
  			units(res.df[, paste0(the.col, section)]) <- "hours"
  		if (as.numeric(aux) <= 3600)
  			units(res.df[, paste0(the.col, section)]) <- "minutes"
  		res.df[, paste0(the.col, section)] <- round(res.df[, paste0(the.col, section)], 2)
  	}
	}
  res.df$Transmitter <- row.names(res.df)
  return(res.df)
}

#' Create status.df
#'
#' Combines the timetable and the original biometrics.
#' 
#' @inheritParams actel
#' @inheritParams deployValues
#' @inheritParams splitDetections
#' @inheritParams simplifyMovements
#' @inheritParams loadDetections
#' @inheritParams groupMovements
#' 
#' @return A data frame containing all the final data for each fish.
#' 
#' @keywords internal
#' 
res_assembleOutput <- function(res.df, bio, spatial, sections, tz.study.area) {
  appendTo("debug", "Merging 'bio' and 'res.df'.")
  status.df <- merge(bio, res.df, by = "Transmitter", all = TRUE)
  
  appendTo("debug", "Completing entries for fish that were never detected.")
  status.df$Status[is.na(status.df$Status)] <- "Disap. at Release"
  status.df$Status <- factor(status.df$Status, levels = c(paste("Disap. in", sections), "Disap. at Release"))
  status.df$Very.last.array[is.na(status.df$Very.last.array)] <- "Release"
  status.df$Very.last.array <- factor(status.df$Very.last.array, levels = c("Release", levels(spatial$stations$Array)))
  status.df$P.type[is.na(status.df$P.type)] <- "Skipped"
	status.df$Valid.detections[is.na(status.df$Valid.detections)] <- 0
	status.df$Invalid.detections[is.na(status.df$Invalid.detections)] <- 0
	status.df$Valid.events[is.na(status.df$Valid.events)] <- 0
	status.df$Invalid.events[is.na(status.df$Invalid.events)] <- 0

	# Convert time stamps
  status.df$Release.date <- as.POSIXct(status.df$Release.date, tz = tz.study.area)
  status.df$Very.last.time <- as.POSIXct(status.df$Very.last.time, tz = tz.study.area)
  
  if (file.exists("temp_comments.txt")) {
    temp <- read.table("temp_comments.txt", header = FALSE, sep = "\t")
    status.df[, "Script.comments"] <- NA_character_
    for (i in seq_len(nrow(temp))) {
      link <- match(temp[i, 1], status.df$Transmitter)
      if (is.na(status.df$Script.comments[link])) {
        status.df$Script.comments[link] <- paste(temp[i, 2])
      } else {
        status.df$Script.comments[link] <- paste(status.df$Script.comments[link], temp[i, 2], sep = "// ")
      }
    }
  }
  appendTo("debug", "Done.")
  return(status.df)
}
