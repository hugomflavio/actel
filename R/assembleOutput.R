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
      status.df[i, paste("Time.until", first.section, sep = ".")] <- AtoB
      if (!invalid.dist) {
        dist.row <- status.df[i, paste("First.station", first.section, sep = ".")]
        dist.col <- as.character(status.df[i, "Release.site"])
        df.to.col <- paste("Speed.to", first.section, sep = ".")
        df.from.col <- paste("Time.until", first.section, sep = ".")
        status.df[i, df.to.col] <- dist.mat[dist.row, dist.col]/status.df[i, df.from.col]
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
