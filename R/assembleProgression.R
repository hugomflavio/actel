#' Calculate the progression
#'
#' Produces a table with the number of fish that crossed each ALS array.
#' 
#' @inheritParams assembleEfficiency
#' @inheritParams assembleOverview
#' 
#' @return A dataframe containing the number of fish that crossed each ALS array.
#' 
#' @keywords internal
#' 
assembleProgression <- function(status.df, spatial) {
  appendTo("debug", "Starting assembleProgression.")
  status.df$Combined <- paste(status.df$Group, status.df$Release.site, sep = "-")
  if (any(levels(status.df$Very.last.array) == "Unknown")) {
    link <- levels(status.df$Very.last.array) != "Unknown"
    new.levels <- levels(status.df$Very.last.array)[link]
    status.df$Very.last.array <- factor(status.df$Very.last.array, levels = new.levels)
  }
  progression <- as.data.frame.matrix(with(status.df, table(Combined, Very.last.array)))
  progression$Total <- as.vector(with(status.df, table(Combined)))
  progression <- progression[, c(ncol(progression), 1:(ncol(progression) - 1))]
  original.row.number <- nrow(progression)
  original.row.names <- rownames(progression)
  rownames(progression) <- paste(original.row.names, "last.array", sep = ".")
  for (i in seq_len(nrow(progression))) {
    to.row <- paste(original.row.names[i], "remaining", sep = ".")
    progression[to.row, 1] <- progression[i, 1]
    for (j in 2:ncol(progression)) {
      to.row <- paste(original.row.names[i], "remaining", sep = ".")
      progression[to.row, j] <- progression[i, 1] - sum(progression[i, 2:j])
    }
    rm(j)
  }
  rm(i)
  new.row.order <- vector()
  for (i in seq_len(original.row.number)) {
    new.row.order <- c(new.row.order, i, (original.row.number + i))
  }
  rm(i)
  progression <- progression[new.row.order, ]
  for (i in seq_len(nrow(spatial$release.sites))) {
    if (grep(spatial$release.sites$Array[i], colnames(progression)) > 3) {
      # to avoid errors if the release site does not contain all groups
      to.row <- grepl(spatial$release.sites$Station.Name[i], rownames(progression)) & grepl("remaining", rownames(progression))
      to.col <- 3:(grep(spatial$release.sites$Array[i], colnames(progression)) - 1)
      progression[to.row, to.col] <- 0
    }
  }
  progression$Total[grepl("last.array",rownames(progression))] <- 0
  appendTo("debug", "Done.")
  return(progression)
}
