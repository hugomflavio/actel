#' Calculate time and speed
#' 
#' Triggers movementTimes and also calculates the speed between events.
#'
#' @inheritParams actel
#' @inheritParams assembleOutput
#' @param silent logical: If TRUE, debug messages are issued (only works within actel)
#' 
#' @return The movement dataframe with time and speed calculations
#' 
#' @export
#' 
movementSpeeds <- function(movements, speed.method, dist.mat, silent = TRUE) {
  if (!silent) 
    appendTo("debug", "Starting movementSpeeds.")
  movements <- movementTimes(movements = movements, silent = silent)
  if (nrow(movements) > 1) {
    movements[1, 9] <- NA
    for (l in 2:nrow(movements)) {
      if (movements[l, 1] != movements[l - 1, 1]) {
        if (speed.method == "last to first"){
          a.sec <- as.vector(difftime(movements[l, 5], movements[l - 1, 6], units = "secs"))
          my.dist <- dist.mat[movements[l, 3], gsub(" ", ".", movements[l - 1, 4])]
        }
        if (speed.method == "first to first"){
          a.sec <- as.vector(difftime(movements[l, 5], movements[l - 1, 5], units = "secs"))
          my.dist <- dist.mat[movements[l, 3], gsub(" ", ".", movements[l - 1, 3])]
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
#' @inheritParams assembleOutput
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
      a <- as.vector(difftime(movements[l, 5], movements[l - 1, 6], units = "hours"))
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
      a <- as.vector(difftime(movements[l, 6], movements[l, 5], units = "hours"))
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
#' @inheritParams assembleOutput
#' 
#' @return The movement dataframe containing the missing information.
#' 
#' @export
#' 
speedReleaseToFirst <- function(fish, status.df, movements, dist.mat, invalid.dist = FALSE, silent = TRUE){
  if (!silent) 
    appendTo("debug", "Starting speedReleaseToFirst.")
  the.row <- match(fish,status.df$Transmitter)
  origin.time <- status.df[the.row,"Release.date"]
  origin.place <- as.character(status.df[the.row,"Release.site"])
  if (origin.time <= movements[1, 5]) {
    a <- as.vector(difftime(movements[1, 5], origin.time, units = "hours"))
    h <- a%/%1
    m <- ((a%%1) * 60)%/%1
    if (m < 10) 
      m <- paste("0", m, sep = "")
    movements[1, 7] <- paste(h, m, sep = ":")
    if (!invalid.dist) {
      a.sec <- as.vector(difftime(movements[1, 5], origin.time, units = "secs"))
      my.dist <- dist.mat[movements[1, 3], origin.place]
      movements[1, 9] <- round(my.dist/a.sec, 6)
    }
  } else {
    movements[1, c(7,9)] <- NA
  }
  if (!silent)
    appendTo("debug", "Done.")
  return(movements)
} 
