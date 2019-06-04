#' Efficiency calculation wrapper
#'
#' Collects the input from interSection, inter-array and intra-array into a list.
#' 
#' @inheritParams actel
#' @param detections.list A list of the detections split by each target tag, created by splitDetections.
#' @param spatial A list of the spatial objects, created by assembleSpatial.
#' @param movements A list of movements for each target tag, created by groupMovements.
#' 
#' @return A list containing 1) the intersection efficiency, 2) the inter-array efficiency and 3) the the intra-array efficiency.
#' 
#' @keywords internal
#' 
assembleEfficiency <- function(sections, spatial, detections.list, movements, minimum.detections) {
  appendTo("debug", "Calculating efficiency.")
  if( length(sections) > 1) {
    intersection <- interSection(sections = sections, movements = movements, minimum.detections = minimum.detections)
  } else {
    appendTo("Screen","M: Skipping inter-section efficiency calculations as there are is only one section.")
    intersection <- "Skipped"    
  }
  if (length(unique(spatial$stations$Array)) > 1) {
    interarray <- interArray(spatial = spatial, movements = movements, minimum.detections = minimum.detections)
  } else {
    appendTo("Screen","M: Skipping inter-array efficiency calculations as there are is only one array.")
    interarray <- "Skipped"    
  }
  if (any(table(spatial$stations$Array) > 1)) {
    intraarray <- intraArray(spatial = spatial, detections.list = detections.list)
  } else {
    appendTo("Screen","M: Skipping intra-array efficiency calculations as there are no arrays with more than one receiver.")
    intraarray <- "Skipped"
  }
  appendTo("debug", "Done")
  return(list(Section = intersection, Inter.Array = interarray, Intra.Array = intraarray))
}

#' Calculate intra-array efficiency
#'
#' @inheritParams assembleEfficiency
#' 
#' @return a table of intra-array efficiency estimates.
#' 
#' @keywords internal
#' 
intraArray <- function(spatial, detections.list) {
  appendTo("debug", "Calculating intraArray efficiency.")
  recipientA <- matrix(ncol = length(spatial$stations$Standard.Name), nrow = length(detections.list))
  colnames(recipientA) <- spatial$stations$Standard.Name
  rownames(recipientA) <- names(detections.list)
  recipientA <- recipientA[, !grepl("Ukn", colnames(recipientA))]
  for (i in names(detections.list)) {
    recipientA[i, ] <- matchl(colnames(recipientA), detections.list[[i]]$Standard.Name)
  }
  rm(i)
  
  efficiency <- list()
  for (i in unique(spatial$stations$Array[!grepl("Unknown", spatial$stations$Array)])) {
    array.stations <- spatial$stations$Standard.Name[spatial$stations$Array == i]
    recipientB <- as.data.frame(matrix(ncol = length(array.stations), nrow = 3))
    colnames(recipientB) <- array.stations
    rownames(recipientB) <- c("detected", "missed", "efficiency")
    for (j in colnames(recipientB)) {
      recipientB[1, j] <- sum(recipientA[, j])
    }
    rm(j)
    recipientB[2, ] <- 0
    for (j in rownames(recipientA)) {
      for (k in array.stations) {
        if (!recipientA[j, k] & any(recipientA[j, array.stations])) 
          recipientB[2, k] <- recipientB[2, k] + 1
      }
    }
    rm(j, k, array.stations)
    for (j in seq_len(ncol(recipientB))) {
      if (recipientB[1, j] == 0 & recipientB[2, j] == 0) {
        recipientB[3, j] <- "-"
      } else {
        recipientB[3, j] <- paste(round(recipientB[1, j]/sum(recipientB[1:2, j]) * 100, 1), "%", sep = "")
      }
    }
    rm(j)
    efficiency[[i]] <- recipientB
    rm(recipientB)
  }
  appendTo("debug", "Done.")
  return(efficiency)
}

#' Calculate inter-array efficiency
#'
#' @inheritParams actel
#' @inheritParams assembleEfficiency
#' 
#' @return a table of inter-array efficiency estimates.
#' 
#' @keywords internal
#' 
interArray <- function(spatial, movements, minimum.detections) {
  appendTo("debug", "Calculating interarray efficiency.")
  efficiency <- as.data.frame(matrix(ncol = length(unlist(spatial$array.order)), nrow = 3))
  colnames(efficiency) <- unlist(spatial$array.order)
  rownames(efficiency) <- c("events", "missed", "efficiency")
  efficiency[1:2, ] = 0
  for (i in names(movements)) {
    submoves <- movements[[i]][!grepl("Unknown", movements[[i]][, "Array"]), ]
    if (nrow(submoves) >= 1) {
      # appendTo('debug',i)
      already.checked <- rep(FALSE, length(unlist(spatial$array.order)) - 1)
      if ((nrow(submoves) == 1 & submoves[1, "Detections"] >= minimum.detections) | nrow(submoves) > 1) {
        destination <- match(submoves[1, "Array"], unlist(spatial$array.order))
        # appendTo('debug',paste('R ->',destination))
        if (destination != 1) {
          cols.to.edit <- seq(from = 1, to = destination - 1)
          for (j in cols.to.edit) {
          if (!already.checked[j]) {
            already.checked[j] = TRUE
            efficiency[1, j] <- efficiency[1, j] + 1
            efficiency[2, j] <- efficiency[2, j] + 1
            # appendTo('debug',paste('+1E +1M for',paste(cols.to.edit,collapse=', ')))
          }
          }
          rm(j, cols.to.edit)
        }
      }
      if (nrow(submoves) > 1) {
        for (j in 1:(nrow(submoves) - 1)) {
          # appendTo('debug',j)
          starting.point <- match(submoves[j, "Array"], unlist(spatial$array.order))
          destination <- match(submoves[j + 1, "Array"], unlist(spatial$array.order))
          # appendTo('debug',paste(starting.point,'->',destination))
          if (starting.point < destination) {
          if (!already.checked[starting.point]) {
            already.checked[starting.point] = TRUE
            efficiency[1, starting.point] <- efficiency[1, starting.point] + 1
            # appendTo('debug',paste('+1E for',starting.point))
          }
          if (destination - starting.point != 1) {
            cols.to.edit <- seq(from = starting.point + 1, to = destination - 1)
            for (j in cols.to.edit) {
            if (!already.checked[j]) {
              already.checked[j] = TRUE
              efficiency[1, j] <- efficiency[1, j] + 1
              efficiency[2, j] <- efficiency[2, j] + 1
              # appendTo('debug',paste('+1E +1M for',paste(cols.to.edit,collapse=', ')))
            }
            }
            rm(cols.to.edit)
          }
          }
          rm(starting.point, destination)
          if (all(already.checked)) {
          # appendTo('debug','All arrays checked, skipping to next fish.')
          (break)()
          }
        }
      }
    }
  }
  for (i in seq_len(ncol(efficiency) - 1)) {
    if (efficiency[1, i] == 0 & efficiency[2, i] == 0) {
      efficiency[3, i] <- "-"
    } else {
      efficiency[3, i] <- paste(round((efficiency[1, i] - efficiency[2, i])/efficiency[1, i] * 100, 1), "%", sep = "")
    }
  }
  efficiency[is.na(efficiency)] <- "-"
  appendTo("debug", "Done.")
  return(efficiency)
}

#' Calculate inter-section efficiency
#'
#' @inheritParams actel
#' @inheritParams assembleEfficiency
#' 
#' @return a table of inter-section efficiency estimates.
#' 
#' @keywords internal
#' 
interSection <- function(sections, movements, minimum.detections) {
  appendTo("debug", "Calculating inter-section efficiency.")
  efficiency <- as.data.frame(matrix(ncol = length(sections), nrow = 3))
  colnames(efficiency) <- sections
  rownames(efficiency) <- c("events", "missed", "efficiency")
  efficiency[1:2, ] = 0
  for (i in names(movements)) {
    submoves <- movements[[i]][!grepl("Unknown", movements[[i]][, "Array"]), ]
    if (nrow(submoves) >= 1) {
      # appendTo('debug',i)
      already.checked <- rep(FALSE, length(sections) - 1)
      if ((nrow(submoves) == 1 & submoves[1, "Detections"] >= minimum.detections) | nrow(submoves) > 1) {
        destination <- which(pmatch(sections, submoves[1, "Array"]) == 1)
        # appendTo('debug',paste('R ->',destination))
        if (destination != 1) {
          cols.to.edit <- seq(from = 1, to = destination - 1)
          for (j in cols.to.edit) {
          if (!already.checked[j]) {
            already.checked[j] = TRUE
            efficiency[1, j] <- efficiency[1, j] + 1
            efficiency[2, j] <- efficiency[2, j] + 1
            # appendTo('debug',paste('+1E +1M for',paste(cols.to.edit,collapse=', ')))
          }
          }
          rm(j, cols.to.edit)
        }
      }
      if (nrow(submoves) > 1) {
        for (j in 1:(nrow(submoves) - 1)) {
          # appendTo('debug',j)
          starting.point <- which(pmatch(sections, submoves[j, "Array"]) == 1)
          destination <- which(pmatch(sections, submoves[j + 1, "Array"]) == 1)
          # appendTo('debug',paste(starting.point,'->',destination))
          if (starting.point < destination) {
          if (!already.checked[starting.point]) {
            already.checked[starting.point] = TRUE
            efficiency[1, starting.point] <- efficiency[1, starting.point] + 1
            # appendTo('debug',paste('+1E for',starting.point))
          }
          if (destination - starting.point != 1) {
            cols.to.edit <- seq(from = starting.point + 1, to = destination - 1)
            for (j in cols.to.edit) {
            if (!already.checked[j]) {
              already.checked[j] = TRUE
              efficiency[1, j] <- efficiency[1, j] + 1
              efficiency[2, j] <- efficiency[2, j] + 1
              # appendTo('debug',paste('+1E +1M for',paste(cols.to.edit,collapse=', ')))
            }
            }
            rm(cols.to.edit)
          }
          }
          rm(starting.point, destination)
          if (all(already.checked)) {
          # appendTo('debug','All arrays checked, skipping to next fish.')
          (break)()
          }
        }
      }
    }
  }
  for (i in seq_len(ncol(efficiency) - 1)) {
    if (efficiency[1, i] == 0 & efficiency[2, i] == 0) {
      efficiency[3, i] <- "-"
    } else {
      efficiency[3, i] <- paste(round((efficiency[1, i] - efficiency[2, i])/efficiency[1, i] * 100, 1), "%", sep = "")
    }
  }
  efficiency[is.na(efficiency)] <- "-"
  appendTo("debug", "Done.")
  return(efficiency)
}
