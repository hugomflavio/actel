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
  appendTo("debug", "Starting assembleOverview.")
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
  appendTo("debug", "Done.")
  return(section.overview[, recipient])
}


#' Create array.overview
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
  
  return(recipient)
}

# #' Create section.overview
# #' 
# #' FAULTY - FISH CAN MOVE BACKUP UPWARDS
# #' which means that if "if.last.skip.section" is TRUE, the estimates may be wrong.
# #' 
# assembleSectionOverview <- function(array.overview, sections, if.last.skip.section, success.arrays) {
#   section.overview <- array.overview
#   success.arrays <- c("Sea1", "Sea2", "Sea3")
#   # success.arrays <- c("Sea2", "Sea3")
#   for(g in 1:length(array.overview)) {
#     recipient <- matrix(nrow = 3, ncol = sum(1, length(sections) * 2))
#     recipient[, 1] <- array.overview[[g]][, 1]
#     the.cols <- c("Release")
#     for(s in 1:length(sections)) {
#       if(s == 1)
#         the.cols <- c(the.cols, paste0("Disap.in.", sections[s]))
#       else
#         the.cols <- c(the.cols, paste0(c("Reached.", "Disap.in."), sections[s]))
#     }
#     the.cols <- c(the.cols, "Succeeded")
#     colnames(recipient) <- the.cols
#     rownames(recipient) <- rownames(array.overview[[g]])
#     recipient
#     for(s in sections) {
#       link <- grepl(s, colnames(array.overview[[g]]))
#       the.cols <- colnames(array.overview[[g]])[link]
#       target <- which(grepl(paste0("Disap.in.", s), colnames(recipient)))
#       last <- length(the.cols)
#       if (s != tail(sections, 1)) {
#         if (if.last.skip.section) {
#           recipient[, target + 1] <- array.overview[[g]][, the.cols[last]]
#           recipient[, target] <- recipient[, target - 1] - array.overview[[g]][, the.cols[last]]
#         } else {
#           recipient[, target + 1] <- array.overview[[g]][, the.cols[last]]
#           recipient[, target] <- recipient[, target - 1] - array.overview[[g]][, the.cols[last]]        
#         }
#       } else {
#         if (all(!is.na(match(the.cols, success.arrays)))) {
#           recipient[, target + 1] <- array.overview[[g]][, the.cols[1]]
#           recipient[, target] <- c(0, 0, 0)
#         } else {
#           the.succeed <- the.cols[min(match(success.arrays, the.cols), na.rm = TRUE)]
#           the.disap <- the.cols[is.na(match(the.cols, success.arrays))]
#           recipient[, target + 1] <- array.overview[[g]][, the.succeed]
#           recipient[, target] <- array.overview[[g]][, the.succeed] - array.overview[[g]][, the.disap]
#         }
#       }
#     }
#     section.overview[[g]] <- recipient
#   }
# }
