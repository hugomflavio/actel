#' Create status.df
#'
#' Combines the timetable and the original biometrics.
#' 
#' @inheritParams actel
#' @inheritParams assembleEfficiency
#' @param timetable A table of the entering and leaving points for each section per target tag, created by assembleTimetable.
#' @param bio A table with the tags and biometrics of the studied fish.
#' @param dist.mat A matrix of the distances between the deployed ALS.
#' @param invalid.dist Wether or not the distance matrix suplied is valid for the study area.
#' 
#' @return A dataframe containing all the final data for each fish.
#' 
#' @keywords internal
#' 
assembleOutput <- function(timetable, bio, movements, spatial, sections, dist.mat, invalid.dist, tz.study.area) {
  appendTo("debug", "Merging 'bio' and 'timetable'.")
  status.df <- merge(bio, timetable, by = "Transmitter", all = T)
  
  appendTo("debug", "Completing entries for fish that were never detected.")
  status.df$Status[is.na(status.df$Status)] <- paste("Disap. in", sections[1])
  status.df$Status <- factor(status.df$Status, levels = c(paste("Disap. in", sections), "Succeeded"))
  status.df$Very.last.array[is.na(status.df$Very.last.array)] <- "Release"
  status.df$Very.last.array <- factor(status.df$Very.last.array, levels = c("Release", levels(spatial$stations$Array)))
  status.df$P.type[is.na(status.df$P.type)] <- "Skipped"
  status.df$Detections[is.na(status.df$Detections)] <- 0

  the.cols <- grepl("Release.date", colnames(status.df)) | grepl("Arrived",colnames(status.df)) | grepl("Left",colnames(status.df))
  for (i in colnames(status.df)[the.cols]) {
    status.df[, i] <- as.POSIXct(status.df[,i], tz = tz.study.area)
  }
  rm(the.cols)
  
  appendTo("debug", "Calculating time from release to first detection.")
  for (i in 1:nrow(status.df)) {
    appendTo("debug", paste("(status.df) Analysing fish ", status.df$Signal[i], " (", i, ").", sep = ""))
    arriving.points <- status.df[i, paste("Arrived", sections, sep = ".")]
    if (any(!is.na(arriving.points))) {
      first.section <- sections[head(which(!is.na(arriving.points)), 1)]
      pointA <- as.POSIXct(status.df[i, paste("Arrived", first.section, sep = ".")], tz = tz.study.area)
      pointB <- as.POSIXct(status.df[i, "Release.date"], tz = tz.study.area)
      AtoB <- as.vector(difftime(pointA, pointB, units = "secs"))
      if (AtoB >= 0) {
        status.df[i, paste("Time.until", first.section, sep = ".")] <- AtoB
        if (!invalid.dist) {
          dist.row <- status.df[i, paste("First.station", first.section, sep = ".")]
          dist.col <- as.character(status.df[i, "Release.site"])
          df.to.col <- paste("Speed.to", first.section, sep = ".")
          df.from.col <- paste("Time.until", first.section, sep = ".")
          status.df[i, df.to.col] <- dist.mat[dist.row, dist.col]/status.df[i, df.from.col]
        }
      } else {
        appendTo(c("Screen", "Report"), paste("Error: Fish ", status.df$Transmitter[i], " was detected before being released!", sep = ""))
        appendTo("Screen", paste("  Release time: ", status.df[i, "Release.date"], sep = ""))
        appendTo("Screen", paste("  First valid detection: ", status.df[i, paste("Arrived", first.section, sep = ".")], " (", first.section, ").", sep = ""))
        cat("  Movement table for fish", status.df$Transmitter[i], "\n")
        print(movements[[paste(status.df$Transmitter[i])]])
        cat("\n")
        appendTo("Screen", "You may either:\n  a) Stop the analysis and check your data;\n  b) Continue anyway (time and speed from release to first valid detection will not be calculated).")
        cat("\n")
        unknown.input = TRUE
        while (unknown.input) {
          decision <- commentCheck(line = "Decision:(a/b) ", tag = status.df$Transmitter[i])
          if (decision == "a" | decision == "A") {
          unknown.input = FALSE
          emergencyBreak()
          stop("Script stopped by user command.")
          }
          if (decision == "b" | decision == "B") {
          unknown.input = FALSE
          }
          if (unknown.input) {
          cat("Option not recognised, please input either 'a' or 'b'.\n")
          }
        }
        appendTo("UD", decision)
        # }
        rm(decision)
      }
      rm(AtoB)
    }
  }
  rm(i)
  
  if (file.exists("temp_comments.txt")) {
    temp <- read.table("temp_comments.txt", header = F, sep = "\t")
    status.df[, "Script.comments"] <- NA
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
