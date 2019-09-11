#' Create section.overview
#'
#' Produces a table with the survival per group of fish present in the biometrics.
#' 
#' @inheritParams actel
#' @inheritParams simplifyMovements
#' 
#' @return A dataframe containing the survival per group of fish present in the biometrics.
#' 
#' @keywords internal
#' 
assembleSectionOverview <- function(status.df, sections) {
  appendTo("debug", "Starting assembleSectionOverview.")
  section.overview <- as.data.frame.matrix(with(status.df, table(Group, Status)))
  section.overview$Total <- as.vector(with(status.df, table(Group)))
  colnames(section.overview) <- gsub(" ", ".", colnames(section.overview))
  if (length(sections) >= 2) {
    to.col <- paste("Migrated.to", sections[2], sep = ".")
    from.col <- paste("Disap..in", sections[1], sep = ".")
    section.overview[, to.col] <- section.overview$Total - section.overview[, from.col]
    recipient <- vector()
    for (i in 2:length(sections)) {
      recipient <- c(recipient, paste(c("Migrated.to", "Disap..in"), sections[i], sep = "."))
    }
  } else {
    recipient <- NULL
  }
  if (length(sections) > 2) {
    for (i in 3:length(sections)) {
      to.col <- paste("Migrated.to", sections[i], sep = ".")
      from.colA <- paste("Migrated.to", sections[i - 1], sep = ".")
      from.colB <- paste("Disap..in", sections[i - 1], sep = ".")
      section.overview[, to.col] <- section.overview[, from.colA] - section.overview[, from.colB]
    }
  }
  recipient <- c("Total", paste("Disap..in", sections[1], sep = "."), recipient, "Succeeded")
  appendTo("debug", "Terminating assembleSectionOverview.")
  return(section.overview[, recipient])
}


#' Create array.overview
#'
#' @return A dataframe containing the progression per group of fish present in the biometrics.
#' 
#' @keywords internal
#' 
assembleArrayOverview <- function(group.CJS) {
  appendTo("debug", "Starting assembleArrayOverview.")
  recipient <- lapply(group.CJS, function(x) x$absolutes)

  for (i in 1:length(recipient)) {
    recipient[[i]][1, ] <- apply(recipient[[i]][c(1,3), ], 2, sum, na.rm = TRUE)
    recipient[[i]][2, ] <- recipient[[i]][4, ]
    recipient[[i]][3, ] <- recipient[[i]][2, ] - recipient[[i]][1, ]
    recipient[[i]] <- recipient[[i]][1:3, ]
    rownames(recipient[[i]]) <- c("Known", "Estimated", "Difference")
  }
  appendTo("debug", "Terminating assembleArrayOverview.")  
  return(recipient)
}
