#' Calculate time and speed
#' 
#' Triggers movementTimes and also calculates the speed between events.
#'
#' @inheritParams actel
#' @inheritParams simplifyMovements
#' @inheritParams groupMovements
#' @param silent logical: If TRUE, debug messages are issued (only works within actel)
#' 
#' @return The movement dataframe with time and speed calculations
#' 
#' @export
#' 
movementSpeeds <- function(movements, speed.method, dist.mat, silent = TRUE) {
  if (!silent) 
    appendTo("debug", "Starting movementSpeeds.")
  if (nrow(movements) > 1) {
    movements$Average.speed.m.s[1] <- NA
    for (l in 2:nrow(movements)) {
      if (movements$Array[l] != movements$Array[l - 1]) {
        if (speed.method == "last to first"){
          a.sec <- as.vector(difftime(movements$First.time[l], movements$Last.time[l - 1], units = "secs"))
          my.dist <- dist.mat[movements$First.station[l], gsub(" ", ".", movements$Last.station[l - 1])]
        }
        if (speed.method == "first to first"){
          a.sec <- as.vector(difftime(movements$First.time[l], movements$First.time[l - 1], units = "secs"))
          my.dist <- dist.mat[movements$First.station[l], gsub(" ", ".", movements$First.station[l - 1])]
        }
        movements[l, 9] <- round(my.dist/a.sec, 6)
        rm(a.sec, my.dist)
      }
    }
  }
  if (!silent) 
    appendTo("debug", "Terminating movementSpeeds.")
  return(movements)
}

#' Calculate movement times
#' 
#' Determines the duration of an event and the time from an event to the next. 
#'
#' @inheritParams simplifyMovements
#' @inheritParams movementSpeeds
#' 
#' @return The movement dataframe with time and speed calculations
#' 
#' @export
#' 
movementTimes <- function(movements, silent = TRUE){
  if (!silent) 
    appendTo("debug", "Starting movementTimes.")
  # Time travelling
  if (nrow(movements) > 1) {
    for (l in 2:nrow(movements)) {
      a <- as.vector(difftime(movements$First.time[l], movements$Last.time[l - 1], units = "hours"))
      h <- a%/%1
      m <- ((a%%1) * 60)%/%1
      if (m < 10) 
        m <- paste("0", m, sep = "")
      movements[l, 7] <- paste(h, m, sep = ":")
    }
    rm(l)
  }
  # Time on array
  for (l in 1:nrow(movements)) {
    if (movements[l, 2] == 1) {
      movements[l, 8] <- "0:00"
    } else {
      a <- as.vector(difftime(movements$Last.time[l], movements$First.time[l], units = "hours"))
      h <- a%/%1
      m <- ((a%%1) * 60)%/%1
      if (m < 10) 
        m <- paste("0", m, sep = "")
      movements[l, 8] <- paste(h, m, sep = ":")
      rm(a, h, m)
    }
  }
  rm(l)
  if (!silent) 
    appendTo("debug", "Terminating movementTimes.")
  return(movements)
}

#' Calculate time and speed since release.
#' 
#' @inheritParams simplifyMovements
#' @inheritParams actel
#' @inheritParams groupMovements
#' @inheritParams movementSpeeds
#' 
#' @return The movement dataframe containing the missing information.
#' 
#' @export
#' 
speedReleaseToFirst <- function(fish, bio, movements, dist.mat, invalid.dist = FALSE, silent = TRUE){
  if (!silent) 
    appendTo("debug", "Starting speedReleaseToFirst.")
  the.row <- match(fish,bio$Transmitter)
  origin.time <- bio[the.row,"Release.date"]
  origin.place <- as.character(bio[the.row,"Release.site"])
  if (origin.time <= movements$First.time[1]) {
    a <- as.vector(difftime(movements$First.time[1], origin.time, units = "hours"))
    h <- a%/%1
    m <- ((a%%1) * 60)%/%1
    if (m < 10) 
      m <- paste("0", m, sep = "")
    movements$Time.travelling[1] <- paste(h, m, sep = ":")
    if (!invalid.dist) {
      a.sec <- as.vector(difftime(movements$First.time[1], origin.time, units = "secs"))
      my.dist <- dist.mat[movements$First.station[1], origin.place]
      movements$Average.speed.m.s[1] <- round(my.dist/a.sec, 6)
    }
  } else {
    movements$Time.travelling[1] <- NA
    movements$Average.speed.m.s[1] <- NA
  }
  if (!silent)
    appendTo("debug", "Done.")
  return(movements)
} 
