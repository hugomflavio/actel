#' Process spatial elements
#' 
#' Creates a list containing multiple spatial elements required throughout the analyses
#' 
#' @inheritParams assembleOutput
#' @inheritParams actel
#' 
#' @return A list of 1) stations, 2) release sites, 3) ALS columns in the spatial file, 4) the Number of ASL, 5) The ALS serial numbers and 6) the array order.
#' 
#' @keywords internal
#' 
assembleSpatial <- function(bio, sections) {
  appendTo("debug", "Starting assembleSpatial.")
  input <- loadSpatial()
  # Create standard names
  input <- setSpatialStandards(input = input)
  # Break the stations away
  appendTo("debug", "Creating 'stations'.")
  stations <- input[input$Type == "Hydrophone", -match("Type", colnames(input))]
  stations$Array <- factor(stations$Array, levels = unique(stations$Array))
  appendTo("debug", "Creating 'release.sites'.")
  # Extract serial numbers only
  receiver.columns <- grep("Receiver", colnames(stations))
  for (i in receiver.columns) {
    if (any(grepl("VR", stations[, i]))) {
      stations[, i] <- as.character(stations[, i])
      for (j in 1:nrow(stations)) stations[j, i] <- unlist(strsplit(stations[j, i], "-"))[2]
      rm(j)
      stations[, i] <- as.factor(stations[, i])
    }
  }
  receivers.serial <- as.matrix(stations[, receiver.columns])[!is.na(as.matrix(stations[, receiver.columns]))]
  if (any(link <- table(receivers.serial) > 1)) {
    emergencyBreak()
    stop(ifelse(sum(link) > 1,"Receivers ","Receiver "), paste(names(table(receivers.serial))[link], collapse = ", "), ifelse(sum(link) > 1," are ", " is "), "duplicated in the spatial file.")
  }
  # If there is any release site in the spatial file
  if (sum(input$Type == "Release") > 0) {
    if (length(unique(bio$Release.site)) == 1 && unique(bio$Release.site) == "unspecified") {
      appendTo(c("Screen", "Report", "Warning"), "W: At least one release site has been indicated in the spatial file, but no release sites were specified in the biometrics file.\n   Discarding release site information to avoid script failure. Please doublecheck your data.")
      release.sites <- data.frame(Station.Name = "unspecified", 
                                  Longitude = NA, 
                                  Latitude = NA, 
                                  Array = stations$Array[1])
    } else {
      A <- input[input$Type == "Release", "Station.Name"]
      B <- unique(bio$Release.site)
      if (any(is.na(match(A, B)))) {
        appendTo(c("Screen", "Report", "Warning"), ": There is a mismatch between the release sites reported and the release locations for the smolts.")
        emergencyBreak()
        stop("The release names should be identical in the spatial objects file and in the biometrics file.\n")
      } else {
        from.row <- input$Type == "Release"
        from.col <- colnames(input)[!grepl("Receiver",colnames(input))]
        release.sites <- input[from.row, from.col]
        row.names(release.sites) <- 1:nrow(release.sites)
      }
    }
  } else {
    if (length(unique(bio$Release.site)) > 1){
      appendTo("Screen", "M: Release sites were not specified in the spatial file but more than one release site is reported in the biometric data.\n   Assuming all released fish go through all receiver arrays.")
    }
    release.sites <- data.frame(Station.Name = unique(bio$Release.site), 
                                Longitude = NA,
                                Latitude = NA, 
                                Array = rep(stations$Array[1], length(unique(bio$Release.site))))
  }
  # Wrap up
  number.of.receivers <- sum(!is.na(stations[, receiver.columns]))
  array.order <- list()  # Used to determine if the fish's last detection was in the last array of a given section
  for (j in sections) {
    array.order[[j]] <- levels(stations$Array)[grepl(j, levels(stations$Array))]
  }
  if (any(trigger <- unlist(lapply(array.order,length)) == 0)) {
    appendTo(c("Screen","Warnings"), decision <- readline(paste("W: No arrays were found that match section(s) ",paste(names(array.order)[trigger], collapse = ", "), ". There could be a typing mistake!\n   Continue the analysis?(y/N) ", sep = "")))
    if (decision != "y" | decision != "Y" ){
      emergencyBreak()
      stop("Stopping analysis per user command.\n")
    }
  }
  output <- list(stations = stations, 
                 release.sites = release.sites, 
                 receiver.columns = receiver.columns, 
                 number.of.receivers = number.of.receivers, 
                 receivers.serial = receivers.serial, 
                 array.order = array.order)
  appendTo("debug", "Done.")
  return(output)
}

#' Create Standard Names for spatial elements
#' 
#' Includes standard names and also reprints 'spatial.csv' 
#' 
#' @param input A dataframe with spatial information.
#'  
#' @return A dataframe with the same information as the input plus the Standard names.
#' 
#' @keywords internal
#' 
setSpatialStandards <- function(input){
  input$Standard.Name <- as.character(input$Station.Name)
  link <- input$Type == "Hydrophone"
  input$Standard.Name[link] <- paste("St.", seq_len(sum(input$Type == "Hydrophone")), sep = "")
  write.csv(input, "spatial.csv", row.names = F)
  return(input)
}
