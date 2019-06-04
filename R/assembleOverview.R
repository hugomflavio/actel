#' Create group.overview
#'
#' Produces a table with the survival per group of fish present in the biometrics.
#' 
#' @inheritParams actel
#' @inheritParams assembleOutput
#' @param status.df A dataframe with all the final data for each fish, created by assembleOutput.
#' 
#' @return A dataframe containing the survival per group of fish present in the biometrics.
#' 
#' @keywords internal
#' 
assembleOverview <- function(status.df, sections, dist.mat, invalid.dist) {
  appendTo("debug", "Starting assembleOverview.")
  group.overview <- as.data.frame.matrix(with(status.df, table(Group, Status)))
  group.overview$Total <- as.vector(with(status.df, table(Group)))
  colnames(group.overview) <- gsub(" ", ".", colnames(group.overview))
  if (length(sections) >= 2) {
    to.col <- paste("Migrated.to", sections[2], sep = ".")
    from.col <- paste("Disap..in", sections[1], sep = ".")
    group.overview[, to.col] <- group.overview$Total - group.overview[, from.col]
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
      group.overview[, to.col] <- group.overview[, from.colA] - group.overview[, from.colB]
    }
  }
  recipient <- c("Total", paste("Disap..in", sections[1], sep = "."), recipient, "Succeeded")
  appendTo("debug", "Done.")
  return(group.overview[, recipient])
}
