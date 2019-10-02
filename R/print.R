#' Collect summary information on the tags detected but that are not part of the study.
#'
#' @param input list of detections for the tags to be excluded.
#' @param restart logical: if TRUE, remove file 'temp_strays.csv' from the working directory.
#' 
#' @keywords internal
#' 
collectStrays <- function(input, restart = FALSE){
  appendTo("debug", "Starting collectStrays.")
  if (restart && file.exists("temp_strays.csv")) {
    file.remove("temp_strays.csv")
  }
  if (length(input) > 0) {
    recipient <- data.frame(Transmitter = names(input), 
      N.detections = unlist(lapply(input,nrow)), 
      First.detection = unlist(lapply(input, function(x) as.character(head(x$Timestamp,1)))),
      Last.detection = unlist(lapply(input, function(x) as.character(tail(x$Timestamp,1)))),
      Receivers = unlist(lapply(input, function(x) paste(unique(x$Receiver), collapse = ", ")))
      )
    write.table(recipient, file = "temp_strays.csv", sep = ",", append = file.exists("temp_strays.csv"), row.names = FALSE, col.names = !file.exists("temp_strays.csv"))
  }
  appendTo("debug", "Terminating collectStrays.")
}

#' Store summary information on the stray tags detected in a permanent file.
#'
#' @keywords internal
#'
storeStrays <- function(){
  appendTo("debug", "Starting storeStrays.")
  if (file.exists("temp_strays.csv")) {
    if (file.exists(newname <- "stray_tags.csv")) {
      continue <- TRUE
      index <- 1
      while (continue) {
        if (file.exists(newname <- paste("stray_tags", index, "csv", sep = "."))) {
          index <- index + 1
        } else {
          continue <- FALSE
        }
      }
    }
    file.rename("temp_strays.csv", newname)
  }
  appendTo("debug", "Terminating storeStrays.")
}

#' Print biometric graphics 
#'
#' Searches for columns containing biometric data and prints graphics per fish group
#' 
#' @inheritParams splitDetections
#' 
#' @return string to be included in printRmd
#'  
#' @keywords internal
#' 
printBiometrics <- function(bio) {
  appendTo("debug", "Starting printBiometrics.")
  biometric.fragment <- ""
  if (any(C <- grepl("Length", colnames(bio)) | grepl("Weight", colnames(bio)) | grepl("Mass", colnames(bio)) | grepl("Size", colnames(bio)))) {
    if (sum(C) > 1) {
      graphic.width <- paste(90 / sum(C), "%", sep = "")
    } else {
      graphic.width <- 0.45
      graphic.width <- paste(45, "%", sep = "")
    }
    counter <- 1
    for (i in colnames(bio)[C]) {
      appendTo("debug", paste("Debug: Creating graphic '", gsub("[.]", "_", i), "_boxplot.png'.\n", sep = ""))
      p <- ggplot2::ggplot(bio, ggplot2::aes(x = as.factor(Group), y = bio[, i]))
      p <- p + ggplot2::stat_boxplot(geom = "errorbar", na.rm = T)
      p <- p + ggplot2::geom_boxplot(na.rm = T)
      p <- p + ggplot2::theme_bw()
      p <- p + ggplot2::theme(panel.grid.minor.x = ggplot2::element_blank(), panel.grid.major.x = ggplot2::element_blank())
      p <- p + ggplot2::labs(x = "", y = i)
      ggplot2::ggsave(paste("Report/", gsub("[.]", "_", i), "_boxplot.png", sep = ""), width = 3, height = 4)
      rm(p)
      if (counter %% 2 == 0)
        biometric.fragment <- paste(biometric.fragment, "![](", gsub("[.]", "_", i), "_boxplot.png){ width=", graphic.width, " }\n", sep = "")
      else
        biometric.fragment <- paste(biometric.fragment, "![](", gsub("[.]", "_", i), "_boxplot.png){ width=", graphic.width, " }", sep = "")
    }
  }
  rm(C, graphic.width)
  appendTo("debug", "Terminating printBiometrics.")
  return(biometric.fragment)
}

#' Print dotplots
#'
#' Prints dotplots of multiple variables.
#' 
#' @inheritParams simplifyMovements
#' @inheritParams groupMovements
#' 
#' @keywords internal
#' 
printDotplots <- function(status.df, invalid.dist) {
  appendTo("debug", "Starting printDotplots.")
  t1 <- status.df[status.df$Detections > 0, c("Transmitter", "Detections", colnames(status.df)[grepl("Time.until", colnames(status.df)) | grepl("Speed.to", colnames(status.df)) | grepl("Time.in", 
    colnames(status.df))])]
  t1$Transmitter <- factor(t1$Transmitter, levels = rev(t1$Transmitter))
  largest <- c(-1, -1, 
    unlist(
      apply(t1[, c(-1, -2)], 2, 
        function(x) 
        if (!all(is.na(x))) {
          max(x, na.rm = T)
        } else {
          NA
        }
      )
    )
  )
  largest[grepl("Speed.to", colnames(t1))] <- -1
  keep.seconds <- largest <= 600 & largest >= 0
  to.minutes <- largest <= 36000 & largest > 600
  to.hours <- largest <= 172800 & largest > 36000
  to.days <- largest > 172800
  t1[, to.minutes] <- t1[, to.minutes]/60
  t1[, to.hours] <- t1[, to.hours]/3600
  t1[, to.days] <- t1[, to.days]/86400
  colnames(t1)[keep.seconds] <- paste(colnames(t1)[keep.seconds], ".\n(Secs)", sep = "")
  colnames(t1)[to.minutes] <- paste(colnames(t1)[to.minutes], ".\n(Mins)", sep = "")
  colnames(t1)[to.hours] <- paste(colnames(t1)[to.hours], ".\n(Hours)", sep = "")
  colnames(t1)[to.days] <- paste(colnames(t1)[to.days], ".\n(Days)", sep = "")
  colnames(t1)[grepl("Speed.to", colnames(t1))] <- paste(colnames(t1)[grepl("Speed.to", colnames(t1))], ".\n(m/s)", sep = "")
  colnames(t1)[2] <- "Detections\n(n)"
  rm(keep.seconds, to.minutes, to.hours, to.days, largest)
  if (!invalid.dist) {
    t2 <- t1[, !grepl("Time.until", colnames(t1))]
  } else {
    t2 <- t1[, !grepl("Speed.to", colnames(t1))]
  }
  colnames(t2) <- sub("Time.i", "I", colnames(t2))
  colnames(t2) <- sub("Time.u", "U", colnames(t2))
  colnames(t2) <- sub("Speed.t", "T", colnames(t2))
  PlotData <- suppressMessages(reshape2::melt(t2))
  PlotData$Colour <- "Orange"
  for (j in colnames(t2)[-1]) {
    limits <- quantile(t2[, j], probs = c(0.1, 0.9), na.rm = T)
    toadd <- vector()
    for (i in 1:nrow(t2)) {
      if (!is.na(t2[i, j]) && t2[i, j] <= limits[1]) {
        toadd[i] <- "Orange"
      } else {
        if (!is.na(t2[i, j]) && t2[i, j] >= limits[2]) {
          toadd[i] <- "Red"
        } else {
          toadd[i] <- "Black"
        }
      }
    }
    rm(i)
    PlotData$Colour[PlotData$variable == j] <- toadd
    rm(toadd)
  }
  rm(j, limits)
  levels(PlotData$variable) <- gsub("[.]", " ", levels(PlotData$variable))
  p <- ggplot2::ggplot(PlotData, ggplot2::aes(x = value, y = Transmitter))
  p <- p + ggplot2::geom_point(colour = PlotData$Colour, na.rm = TRUE)
  p <- p + ggplot2::theme_bw()
  p <- p + ggplot2::theme(panel.grid.minor.x = ggplot2::element_blank(), panel.grid.major.x = ggplot2::element_blank())
  p <- p + ggplot2::facet_grid(. ~ variable, scales = "free_x")
  p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1))
  # p <- p + ggplot2::theme(strip.text.x = ggplot2::element_text(size = 0.5))
  p <- p + ggplot2::labs(x = "", y = "")
  # ggplot2::ggsave(paste("Report/dotplots.pdf", sep = ""), width = 6, height = 10)
  ggplot2::ggsave(paste("Report/dotplots.png", sep = ""), width = 6, height = (1.3 + 0.115 * (nrow(t2) - 1)))
  appendTo("debug", "Terminating printDotplots.")
}

#' Print survival graphic
#'
#' Prints survival graphics per fish group.
#' 
#' @param section.overview A data frame containing the survival per group of fish present in the biometrics. Supplied by assembleOverview.
#' 
#' @keywords internal
#' 
printSurvivalGraphic <- function(section.overview) {
  appendTo("debug", "Starting printSurvivalGraphic.")
  cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")
  names(cbPalette) <- c("Orange", "Blue", "Green", "Yellow", "Darkblue", "Darkorange", "Pink", "Grey")
  survival <- data.frame(group = character(), section = character(), value = vector(), stringsAsFactors = FALSE)
  i = 2
  while (i <= ncol(section.overview)) {
    for (j in 1:nrow(section.overview)) {
      active.row <- nrow(survival) + 1
      survival[active.row, ] <- NA
      survival$group[active.row] <- rownames(section.overview)[j]
      survival$section[active.row] <- tail(strsplit(colnames(section.overview)[i], "[.]")[[1]], 1)
      if (section.overview[j, i - 1] > 0) {
        survival$value[active.row] <- 1 - section.overview[j, i]/section.overview[j, i - 1]
      } else {
        survival$value[active.row] <- 0
      }
    }
    rm(active.row, j)
    i = i + 2
  }
  for (j in 1:nrow(section.overview)) {
    active.row <- nrow(survival) + 1
    survival[active.row, ] <- NA
    survival$group[active.row] <- rownames(section.overview)[j]
    survival$section[active.row] <- "Overall"
    survival$value[active.row] <- section.overview[j, ncol(section.overview)]/section.overview[j, 1]
  }
  survival$section <- factor(survival$section, levels = unique(survival$section))
  p <- ggplot2::ggplot(survival, ggplot2::aes(x = section, y = value))
  p <- p + ggplot2::geom_bar(stat = "identity", fill = cbPalette[[2]], colour = cbPalette[[2]])
  p <- p + ggplot2::facet_grid(. ~ group)
  p <- p + ggplot2::theme_bw()
  p <- p + ggplot2::ylim(0, 1)
  p <- p + ggplot2::labs(x = "", y = "Survival")
  # ggplot2::ggsave(paste("Report/survival.pdf", sep = ""), width = nrow(section.overview) * 2, height = 4)
  ggplot2::ggsave(paste("Report/survival.png", sep = ""), width = nrow(section.overview) * 2, height = 4)
  appendTo("debug", "Terminating printSurvivalGraphic.")
}

#' Print progression graphic
#'
#' Prints progression graphics per fish group and release site.
#' 
#' @inheritParams actel
#' @inheritParams simplifyMovements
#' @param overall.CJS a single CJS with all the groups and release sites merged
#' @param split.CJS a list of CJS's for each group.release combination
#' @param group.CJS a list of CJS's for each group, with release sites merged
#' 
#' @keywords internal
#' 
printProgression <- function(status.df, overall.CJS, split.CJS, group.CJS) {
  appendTo("debug", "Starting printProgression_test.")
  ## Absolutes per array per group per release site.
  detailed.absolutes <- lapply(split.CJS, function(x) x$absolutes)
  maxcols <- max(unlist(lapply(detailed.absolutes, ncol)))
  for(i in 1:length(detailed.absolutes)) {
    if (ncol(detailed.absolutes[[i]]) < maxcols)
      detailed.absolutes[[i]] <- detailed.absolutes[[i]][, -1]
  }
  ## Progression dataframe
  the.levels <- c()
  recipient <- compileProgressionDataFrame(status.df = status.df, group.CJS = group.CJS, 
    detailed.absolutes = detailed.absolutes, i = 1, the.levels = the.levels)
  progression <- recipient[[1]]
  the.levels <- recipient[[2]]
  if (length(group.CJS) > 1) { 
    for (i in 2:length(group.CJS)) {
      recipient <- compileProgressionDataFrame(status.df = status.df, group.CJS = group.CJS, 
        detailed.absolutes = detailed.absolutes, i = i, the.levels = the.levels)
      progression <- rbind(progression, recipient[[1]])
      the.levels <- recipient[[2]]
    }
  }
  the.levels <- c(the.levels, "Estimated")
  progression$Fill <- factor(progression$Fill, levels = rev(unique(the.levels)))
  progression$Value[is.na(progression$Value)] <- 0
  progression$Array <- factor(progression$Array, levels = unique(progression$Array))
  progression$Group <- factor(progression$Group, levels = names(group.CJS))

  # prepareFunctions
  abort.additions <- FALSE
  temp <- prepareAdditions(group.CJS = group.CJS, progression = progression)
  if (is.null(temp)) {
    abort.additions <- TRUE
  } else {
    additions <- temp
  }
  totals <- prepareTotals(group.CJS = group.CJS)
  the.final.totals <- prepareFinalTotals (group.CJS = group.CJS, totals = totals, progression = progression)

  # Plot
  the.ceiling <- max(the.final.totals$n)
  cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")
  names(cbPalette) <- c("Orange", "Blue", "Green", "Yellow", "Darkblue", "Darkorange", "Pink", "Grey")
  p <- ggplot2::ggplot(data = progression, ggplot2::aes(x = Array, y = Value))
  # The bars
  p <- p + ggplot2::geom_bar(stat = "identity", ggplot2::aes(fill = Fill))
  # The totals dashed line
  p <- p + ggplot2::geom_line(data = the.final.totals, ggplot2::aes(x = nArray,
    y = n, group = Group), linetype = "dashed", colour = "red")
  # The additions annotation
  if (!abort.additions)
    p <- p + ggplot2::geom_text(data = additions, ggplot2::aes(x = (as.numeric(Array) - 0.5), 
    label = paste0(" +", n, " released")), y = the.ceiling, hjust = 0, vjust = -0.5, colour = "red")
  p <- p + ggplot2::facet_grid(Group ~ .)
  p <- p + ggplot2::theme_bw()
  p <- p + ggplot2::theme(panel.grid.minor.x = ggplot2::element_blank(), 
    panel.grid.major.x = ggplot2::element_blank())
  if (length(levels(progression$Fill)) <= 8) {
    p <- p + ggplot2::scale_fill_manual(values = as.vector(cbPalette)[c(8, 1:length(levels(progression$Fill))-1)])
  }
  p <- p + ggplot2::theme(legend.title = ggplot2::element_blank())
  p <- p + ggplot2::scale_y_continuous(minor_breaks = seq(0, round(the.ceiling * 1.2, 0), 2), limits = c(0, the.ceiling * 1.2))
  p <- p + ggplot2::labs(y = "", x = "")
  ggplot2::ggsave("Report/progression.png", width = 7, height = 6)
  appendTo("debug", "Terminating printProgression_test.")
}

#' Compile Progression in a ggplot-friendly fashion
#' 
#' Used within printProgression
#' 
#' @inheritParams printProgression
#' @inheritParams simplifyMovements
#' @param detailed.absolutes a list of absolutes extracted from split.CJS
#' @param the.levels a vector containing levels for later use in ggplot
#' @param i the current iteration number
#' 
#' @keywords internal
#' 
#' @return An updated progression data frame and an updated the.levels
#' 
compileProgressionDataFrame <- function(status.df, group.CJS, detailed.absolutes, i, the.levels){
  known.overall <- apply(group.CJS[[i]]$absolutes[c(1,3),], 2, sum, na.rm = TRUE)
  link <- grepl(paste0("^", unique(status.df$Group)[i]), names(detailed.absolutes))
  originals <- names(detailed.absolutes)[link]
  new <- sub(paste0("^", unique(status.df$Group)[i], "."), "Known-", originals)
  the.levels <- c(the.levels, new)

  the.array <- c()
  the.value <- c()
  the.group <- c()
  the.fill  <- c()
  for (j in 1:length(originals)) {
    the.array <- c(the.array, colnames(detailed.absolutes[[originals[j]]]))
    the.value <- c(the.value, apply(detailed.absolutes[[originals[j]]][c(1,3), ], 2, sum, na.rm = TRUE))
    the.group <- c(the.group, rep(names(group.CJS)[i], ncol(detailed.absolutes[[originals[j]]])))
    the.fill  <- c(the.fill, rep(new[j], ncol(detailed.absolutes[[originals[j]]])))
  }
  rm(new)

  output <- data.frame(
    Array = c(the.array, colnames(group.CJS[[i]]$absolutes)),
    Value = c(the.value, group.CJS[[i]]$absolutes[4, ] - known.overall),
    Group = c(the.group, rep(names(group.CJS)[i], ncol(group.CJS[[i]]$absolutes))),
    Fill  = c(the.fill, rep("Estimated", ncol(group.CJS[[i]]$absolutes))),
    stringsAsFactors = FALSE
    )
  return(list(output, the.levels))
}

#' Summarise places were more fish were released
#' 
#' The "additions" will give the labels
#' 
#' @inheritParams printProgression
#' @param progression the progression data frame, provided by compileProgressionDataFrame
#' 
#' @keywords internal
#' 
#' @return The additions data frame, or NULL if additions should be aborted.
#' 
prepareAdditions <- function(group.CJS, progression) {
  if (sum(unlist(lapply(group.CJS, function(x) length(x$additions)))) > 0) {
    additions <- as.data.frame(matrix(ncol = 2, nrow = sum(unlist(lapply(group.CJS, function(x) length(x$additions))))))
    colnames(additions) <- c("Array","n")
    rownames(additions) <- names(group.CJS)
    for (i in 1:length(group.CJS)) {
      if(any(names(group.CJS[[i]]) == "additions")) {
        additions$Array[i] <- names(group.CJS[[i]]$additions)
        additions$n[i] <- group.CJS[[i]]$additions
      }
    }
    additions <- additions[!is.na(additions$Array), ]
    additions$Array <- factor(additions$Array, levels = unique(progression$Array))
    additions$Group <- rownames(additions)
  } else {
    additions <- NULL
  }
  return(additions)
}

#' Compile the totals at each entry point per group
#' 
#' @inheritParams printProgression
#' 
#' @return the totals data frame
#' 
#' @keywords internal
#' 
prepareTotals <- function(group.CJS) {
  totals <- as.data.frame(matrix(ncol = 2, nrow = 0))
  colnames(totals) <- c("Array","n")
  for (i in 1:length(group.CJS)) {
    totals[nrow(totals) + 1, "Array"] <- "Release"
    totals[nrow(totals), "n"] <- group.CJS[[i]]$absolutes[1, 1]
    totals[nrow(totals), "Group"] <- names(group.CJS)[i]
    if(any(names(group.CJS[[i]]) == "additions")) {
      for (j in length(group.CJS[[i]]$additions)) {
        totals[nrow(totals) + 1, "Array"] <- names(group.CJS[[i]]$additions)
        totals[nrow(totals), "n"] <- sum(totals$n[totals$Group == names(group.CJS[i])], group.CJS[[i]]$additions, na.rm = TRUE)
        totals[nrow(totals), "Group"] <- names(group.CJS)[i]
      }
    }
  }
  return(totals)
}

#' Prepare final totals in ggplot-friendly style
#' 
#' the "the.final.totals" will provide the data for the red dashed line
#' 
#' @inheritParams printProgression
#' @inheritParams prepareAdditions
#' @param totals the totals data frame, provided by prepareTotals
#' 
#' @keywords internal
#' 
#' @return the final total data frame
#' 
prepareFinalTotals <- function(group.CJS, totals, progression){
  for(i in names(group.CJS)){
    temp <- totals[totals$Group == i, ]
    recipient <- temp
    for(j in 1:nrow(temp)) {
      recipient[j * 2 - 1, ] <- temp[j, ]
      recipient[j * 2, ] <- temp[j, ]
      if (j < nrow(temp))
        recipient[j * 2, "Array"] <- temp[j + 1, "Array"]
      else 
        recipient[j * 2, "Array"] <- paste(tail(progression$Array, 1))
    }
    recipient$Array <- factor(recipient$Array, levels = unique(progression$Array))
    recipient$nArray <- NA
    recipient$nArray[1:(nrow(recipient) - 1 )] <- as.numeric(recipient$Array[1:(nrow(recipient) - 1)]) - 0.5
    recipient$nArray[nrow(recipient)] <- as.numeric(recipient$Array[nrow(recipient)]) + 0.5

    if(exists("the.final.totals")) {
      the.final.totals <- rbind(the.final.totals, recipient)
    } else {
      the.final.totals <- recipient
    }
  }
  return(the.final.totals)
}

#' print Rmd fragment for inclusion in the report
#' 
#' @param array.overview a list of absolute detection numbers for each group
#' 
#' @return Rmd fragment
#' 
#' @keywords internal
#' 
printArrayOverview <- function(array.overview) {
  options(knitr.kable.NA = '-')
  array.overview.fragment <- c("")
  for(i in 1:length(array.overview)) {
    array.overview.fragment <- paste0(array.overview.fragment, '

**Group: ', names(array.overview)[i],'**

', paste(knitr::kable(array.overview[[i]]), collapse = "\n"), '\n')
  }
  return(array.overview.fragment)
}

#' Print efficiency fragment
#'
#' Prints the ALS inter-array efficiency for inclusion in printRmd.
#' 
#' @inheritParams printProgression
#' @param last.array.results The output of the getEstimate calculations.
#' 
#' @keywords internal
#' 
printEfficiency <- function(overall.CJS, last.array.results){
    efficiency.fragment <- paste('
**Individuals detected and estimated**

```{r efficiency1, echo = FALSE}
knitr::kable(overall.CJS$absolutes)
```

**Array efficiency**

```{r efficiency2, echo = FALSE}
to.print <- t(paste(round(overall.CJS$efficiency * 100, 1), "%", sep = ""))
if (grepl("NA", to.print[, ncol(to.print)]))
 to.print[, ncol(to.print)] = "-"
colnames(to.print) <- names(overall.CJS$efficiency)
rownames(to.print) <- "efficiency"
knitr::kable(to.print)
```

**Lambda:** `r I(overall.CJS$lambda)`

')
  if (is.list(last.array.results)){
    efficiency.fragment <- paste(efficiency.fragment, '
#### Last array efficiency estimate

```{r efficiency3, echo = FALSE,  comment = NA}
knitr::kable(last.array.results$results$absolutes)
```

**Last array estimated efficiency: **', round(last.array.results$results$combined.efficiency * 100, 2), "%", sep = "")
  } else {
    efficiency.fragment <- paste(efficiency.fragment, '
#### Last array efficiency estimate

```{r efficiency3, echo = FALSE,  comment = NA}
cat(last.array.results)
```', sep = "")
  }
  return(efficiency.fragment)
}

#' Print individual graphics
#'
#' Prints the individual detections for each fish, overlaying the points in time considered crucial during the analysis.
#' 
#' @inheritParams actel
#' @inheritParams groupMovements
#' @inheritParams simplifyMovements
#' @inheritParams assembleMatrices
#' @param extension the format of the generated graphics
#' 
#' @return String to be included in printRmd
#' 
#' @keywords internal
#' 
printIndividuals <- function(redraw, detections.list, bio, status.df = NULL, tz.study.area, 
  movements, simple.movements = NULL, extension = "png") {
  cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")
  names(cbPalette) <- c("Orange", "Blue", "Green", "Yellow", "Darkblue", "Darkorange", "Pink", "Grey")
  appendTo(c("Screen", "Report"), "M: Drawing individual graphics for the report.")
  if (exists("redraw") && redraw == FALSE) {
    appendTo(c("Screen", "Report"), "M: 'redraw' is set to FALSE, only drawing new graphics.")
  }
  pb <- txtProgressBar(min = 0, max = length(detections.list), style = 3, width = 60)
  individual.plots <- ""
  for (i in 1:length(detections.list)) {
    fish <- names(detections.list)[i]
    PlotData <- detections.list[[fish]]
    if (!(exists("redraw") && redraw == FALSE && file.exists(paste("Report/", fish, ".png", sep = "")))) {
      all.moves.line <- data.frame(
        Station = as.vector(t(movements[[fish]][, c("First.station", "Last.station")])),
        Timestamp = as.vector(t(movements[[fish]][, c("First.time", "Last.time")]))
      )
      all.moves.line$Station <- factor(all.moves.line$Station, levels = levels(PlotData$Standard.Name))
      all.moves.line$Timestamp <- as.POSIXct(all.moves.line$Timestamp, tz = tz.study.area)
      add.simple.movements <- FALSE
      if (!is.null(simple.movements[[fish]])) {
        add.simple.movements <- TRUE
        simple.moves.line <- data.frame(
          Station = as.vector(t(simple.movements[[fish]][, c("First.station", "Last.station")])),
          Timestamp = as.vector(t(simple.movements[[fish]][, c("First.time", "Last.time")]))
          )
        simple.moves.line$Station <- factor(simple.moves.line$Station, levels = levels(PlotData$Standard.Name))
        simple.moves.line$Timestamp <- as.POSIXct(simple.moves.line$Timestamp, tz = tz.study.area)
      }
      appendTo("debug", paste("Debug: Printing graphic for fish", fish, ".", sep = ""))
      colnames(PlotData)[1] <- "Timestamp"
      the.row <- which(bio$Transmitter == fish)
      start.line <- as.POSIXct(bio$Release.date[the.row], tz = tz.study.area)
      first.time <- min(c(as.POSIXct(head(PlotData$Timestamp, 1), tz = tz.study.area), start.line))
      attributes(first.time)$tzone <- tz.study.area
      last.time <- as.POSIXct(tail(PlotData$Timestamp, 1), tz = tz.study.area)
      if (!is.null(status.df)) {
        status.row <- which(status.df$Transmitter == fish)
        relevant.line <- status.df[status.row, (grepl("Arrived", colnames(status.df)) | grepl("Left", colnames(status.df)))]
      }
      # Start plot
      p <- ggplot2::ggplot(PlotData, ggplot2::aes(x = Timestamp, y = Standard.Name, colour = Array))
      # Choose background
      default.cols <- TRUE
      if (!is.null(status.df) && status.df$P.type[status.row] == "Overridden") {
        p <- p + ggplot2::theme(
          panel.background = ggplot2::element_rect(fill = "white"),
          panel.border = ggplot2::element_rect(fill = NA, colour = "#ef3b32" , size = 2),
          panel.grid.major = ggplot2::element_line(size = 0.5, linetype = 'solid', colour = "#ffd8d6"), 
          panel.grid.minor = ggplot2::element_line(size = 0.25, linetype = 'solid', colour = "#ffd8d6"),
          legend.key = ggplot2::element_rect(fill = "white", colour = "white"),
          )
        default.cols <- FALSE
      } 
      if (!is.null(status.df) && status.df$P.type[status.row] == "Manual") {
         p <- p + ggplot2::theme(
          panel.background = ggplot2::element_rect(fill = "white"),
          panel.border = ggplot2::element_rect(fill = NA, colour = "#ffd016" , size = 2),
          panel.grid.major = ggplot2::element_line(size = 0.5, linetype = 'solid', colour = "#f2e4b8"), 
          panel.grid.minor = ggplot2::element_line(size = 0.25, linetype = 'solid', colour = "#f2e4b8"),
          legend.key = ggplot2::element_rect(fill = "white", colour = "white"),
          )
        default.cols <- FALSE
      } 
      if (default.cols) {
        p <- p + ggplot2::theme_bw()
      }
      # Plot starting line
      p <- p + ggplot2::geom_vline(xintercept = start.line, linetype = "dashed")
      # Plot entry/exit lines
      if (!is.null(status.df)) {
        for (l in 1:length(relevant.line)) {
          if (!is.na(relevant.line[l])) {
            p <- p + ggplot2::geom_vline(xintercept = as.POSIXct(relevant.line[[l]], tz = tz.study.area), linetype = "dashed", color = "grey")
          }
        }
        rm(l, relevant.line)
      }
      # Plot movements
      p <- p + ggplot2::geom_path(data = all.moves.line, ggplot2::aes(x = Timestamp, y = Station, group = 1), col = "grey40", linetype = "dashed")
      if (add.simple.movements) {
        p <- p + ggplot2::geom_path(data = simple.moves.line, ggplot2::aes(x = Timestamp, y = Station, group = 1), col = "grey40")
      }
      # Trim graphic
      p <- p + ggplot2::xlim(first.time, last.time)
      # Paint
      if (length(levels(PlotData$Array)) <= 8) {
        p <- p + ggplot2::scale_color_manual(values = as.vector(cbPalette)[1:length(levels(PlotData$Array))], drop = FALSE)
      } else {
        p <- p + ggplot2::scale_color_discrete(drop = FALSE)
      }
      # Plot points
      p <- p + ggplot2::geom_point()
      # Fixate Y axis
      p <- p + ggplot2::scale_y_discrete(drop = FALSE)
      # Caption and title
      p <- p + ggplot2::guides(colour = ggplot2::guide_legend(reverse = T))
      if (!is.null(status.df))
        p <- p + ggplot2::labs(title = paste(fish, " (", status.df[status.df$Transmitter == fish, "Status"], ")", sep = ""), x = paste("tz: ", tz.study.area, sep = ""), y = "Station Standard Name")
      else
        p <- p + ggplot2::labs(title = paste(fish, " (", nrow(PlotData), " detections)", sep = ""), x = paste("tz: ", tz.study.area, sep = ""), y = "Station Standard Name")
      # Save
      if (length(levels(PlotData$Standard.Name)) <= 30)
        the.height <- 4
      else
        the.height <- 4 + (length(levels(PlotData$Standard.Name)) - 30) * 0.1
      ggplot2::ggsave(paste0("Report/", fish, ".", extension), width = 5, height = the.height)  # better to save in png to avoid point overlapping issues
      rm(PlotData, start.line, last.time, first.time)
    }
    if (i%%2 == 0) {
      individual.plots <- paste0(individual.plots, "![](", fish, ".", extension, "){ width=50% }\n")
    } else {
      individual.plots <- paste0(individual.plots, "![](", fish, ".", extension, "){ width=50% }")
    }
    setTxtProgressBar(pb, i)
  }
  close(pb)
  return(individual.plots)
}

#' Print circular graphics for each array
#' 
#' Prints the time of first entry point on each of the arrays. Functions adapted from the circular R package.
#' 
#' @param times a (list of) circular object(s)
#' 
#' @keywords internal
#' 
#' @return A rmd string to be attached to the report.
#' 
printCircular <- function(times, bio){
  cbPalette <- c("#56B4E9", "#c0ff3e", "#E69F00", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")
  circular.plots <- ""
  colours <- paste(cbPalette[c(1:length(unique(bio$Group)))], 80, sep = "")
  for (i in 1:length(times)) {
    if (length(unique(bio$Group)) > 1) {
      link <- match(names(times[[i]]), bio$Transmitter)
      groups <- factor(bio$Group[link], levels = sort(unique(bio$Group)))
      trim.times <- split(times[[i]], groups)
      ylegend <- -0.97 + (0.1 * (length(unique(groups)) - 2))
    } else {
      trim.times <- times[i]
      names(trim.times) <- unique(bio$Group)
      ylegend <- -0.97
    }
    prop <- roundDown(1 / max(unlist(lapply(trim.times, function(x) table(roundUp(x, to = 1)) / sum(!is.na(x))))), to = 1)
    {svg(paste0("Report/times_", names(times)[i], ".svg"), height = 5, width = 5, bg = "transparent")
    par(mar = c(1, 2, 2, 1))
    circular:::CirclePlotRad(main = names(times)[i], shrink = 1.05)
    # circularSection(from = sunset, 
    #   to = sunrize, units = "hours", template = "clock24", 
    #   limits = c(1, 0), fill = scales::alpha("grey", 0.3), border = "transparent")
    params <- myRoseDiag(trim.times, bins = 24, radii.scale = "linear",
      prop = prop, tcl.text = -0.1, tol = 0.05, col = colours)
    roseMean(trim.times, col = params$col, mean.length = c(0.07, -0.07), mean.lwd = 6,
      box.range = "std.error", fill = "white", border = "black",
      box.size = c(1.015, 0.985), edge.length = c(0.025, -0.025),
      edge.lwd = 2)
    ringsRel(plot.params = params, border = "black", ring.text = TRUE, 
      ring.text.pos = 0.07, rings.lty = "f5", ring.text.cex = 0.8)
      legend(x = -1.2, y = ylegend,
      legend = paste(names(trim.times), " (", unlist(lapply(trim.times, function(x) sum(!is.na(x)))), ")", sep =""),
      fill = params$col, bty = "n", x.intersp = 0.3, cex = 0.8)
    dev.off()}
    if (i %% 2 == 0)
      circular.plots <- paste0(circular.plots, "![](times_", names(times)[i], ".svg){ width=50% }\n")
    else
      circular.plots <- paste0(circular.plots, "![](times_", names(times)[i], ".svg){ width=50% }")
  }
  return(circular.plots)
}

#' Draw a section on the outside of the circle
#' 
#' @param from value where the section should start
#' @param to value where the section should end
#' @param units units of the from and to variables, defaults to "hours"
#' @param template variable to feed into the circular package base functions
#' @param limits two values controlling the vertical start and end points of the section
#' @param fill The colour of the section
#' @param border The colour of the section's border
#' 
#' @keywords internal
#' 
circularSection <- function(from, to, units = "hours", template = "clock24", limits = c(1, 0), fill = "white", border = "black"){
  if( inherits(from,"character") ){
    hour.from <- circular(decimalTime(from), units = units, template = template)
    hour.to <- circular(decimalTime(to), units = units, template = template)
  } else {
    hour.from <- circular(from, units = units, template = template)
    hour.to <- circular(to, units = units, template = template)
  }
  if(hour.to < hour.from) hour.to <- hour.to + 24
  zero <- attr(hour.from,"circularp")$zero # extracted from the circular data
  xmin <- as.numeric(conversion.circular(hour.from, units = "radians")) * -1
  xmax <- as.numeric(conversion.circular(hour.to, units = "radians")) * -1
  xx <- c(limits[1] * cos(seq(xmin, xmax, length = 1000) + zero), rev(limits[2] * cos(seq(xmin, xmax, length = 1000) + zero)))
  yy <- c(limits[1] * sin(seq(xmin, xmax, length = 1000) + zero), rev(limits[2] * sin(seq(xmin, xmax, length = 1000) + zero)))
  polygon(xx, yy, col = fill, border = border)
}

#' Edited rose diagram function
#' 
#' Adapted from the rose.diag function of the circular package
#' 
#' @param x a vector, matrix or data.frame. The object is coerced to class circular.
#' @param pch point character to use. See help on par.
#' @param cex point character size. See help on par.
#' @param axes logical: if TRUE axes are plotted according to properties of x.
#' @param shrink parameter that controls the size of the plotted circle. Default is 1. Larger values shrink the circle, while smaller values enlarge the circle.
#' @param bins number of arcs to partition the circle with.
#' @param upper logical: if TRUE, the rose diagram cells are "upper"-closed intervals.
#' @param ticks logical: if TRUE ticks are plotted according to the value of bins.
#' @param tcl length of the ticks.
#' @param tcl.text the position of the axis labels.
#' @param radii.scale make possible to choose sector radius form: square-root of relative frequency (sqrt, default) or conventional linear scale (linear).
#' @param border the colour to draw the border. The default, NULL, means to use par("fg"). Use border = NA to omit borders.
#' @param col the colour for filling the rose diagram. The default, NULL, is to leave rose diagram unfilled. The values are recycled if needed.
#' @param tol proportion of white space at the margins of plot.
#' @param uin desired values for the units per inch parameter. If of length 1, the desired units per inch on the x axis.
#' @param xlim,ylim the ranges to be encompassed by the x and y axes. Useful for centering the plot
#' @param prop numerical constant determining the radii of the sectors. By default, prop = 1. 
#' @param digits number of digits used to print axis values.
#' @param plot.info an object from plot.circular that contains information on the zero, the rotation and next.points.
#' @param units the units used in the plot. If NULL the units of the first component of 'x' is used.
#' @param template the template of the plot. Ignored if plot.info is provided.
#' @param zero the zero of the plot. Ignored if plot.info or template are provided.
#' @param rotation the rotation of the plot. Ignored if plot.info or template are provided.
#' @param main,sub,xlab,ylab title, subtitle, x label and y label of the plot.
#' @param add add the rose diag to an existing plot.
#' @param control.circle parameters passed to plot.default in order to draw the circle. The function circle.control is used to set the parameters.
#' @param rings logical: if TRUE, inner rings are displayed for visual reference
#' @param rings.lty line type of the rings, See help on par.
#' @param ring.text logical: if notes should be displayed.
#' @param ring.text.pos The position of the rings' text. Ignored if ring.text is set to FALSE.
#' @param ring.text.cex The size of the ring's text. Ignored if ring.text is set to FALSE.
#' 
#' @return A list with the zero, rotation and next.points values, to be parsed to an overlaying graphic.
#' 
myRoseDiag <- function (x, pch = 16, cex = 1, axes = TRUE, shrink = 1, bins = 24, 
  upper = TRUE, ticks = TRUE, tcl = 0.025, tcl.text = 0.125, 
  radii.scale = c("sqrt", "linear"), border = NULL, col = c("lightblue", "#c0ff3e80", "#ffc0cb80", "#F0E4424D", "#0072B24D", "#D55E004D"), 
  tol = 0.04, uin = NULL, xlim = c(-1, 1), ylim = c(-1, 1), 
  prop = 1, digits = 2, plot.info = NULL, units = NULL, template = NULL, 
  zero = NULL, rotation = NULL, main = NULL, sub = NULL, xlab = "", 
  ylab = "", add = TRUE, control.circle = circular::circle.control(), rings = c("none", "absolute", "relative"), 
  rings.lty = 2, ring.text = FALSE, ring.text.pos = -0.04, ring.text.cex = 1) {
  rings <- match.arg(rings)
  radii.scale <- match.arg(radii.scale)
  
  if (is.list(x)) {
    max.length <- max(unlist(lapply(x, length)))
    for (i in 1:length(x)) {
      if (length(x[[i]]) < max.length)
        x[[i]] <- c(x[[i]], circular::circular(rep(NA, max.length - length(x[[i]]))))
    }
  }
  
  xx <- as.data.frame(x)
  nseries <- ncol(xx)
  
  if (length(col) != nseries)
    col <- rep(col, length.out = nseries)
  xcircularp <- attr(circular::as.circular(xx[, 1]), "circularp")
  modulo <- xcircularp$modulo
  if (is.null(units)) 
    units <- xcircularp$units
  if (is.null(plot.info)) {
    if (is.null(template)) 
      template <- xcircularp$template
    if (template == "geographics" | template == "clock24") {
      zero <- pi / 2
      rotation <- "clock"
    } else {
      if (template == "clock12") {
        zero <- pi / 2
        rotation <- "clock"
        modulo <- "pi"
      }
    }
    if (is.null(zero)) 
      zero <- xcircularp$zero
    if (is.null(rotation)) 
      rotation <- xcircularp$rotation
    next.points <- 0
  } else {
    zero <- plot.info$zero
    rotation <- plot.info$rotation
    next.points <- plot.info$next.points
  }
  if (!add) {
    circular:::CirclePlotRad(xlim = xlim, ylim = ylim, uin = uin, shrink = shrink, 
      tol = tol, main = main, sub = sub, xlab = xlab, ylab = ylab, 
      control.circle = control.circle)
  }
  if (is.null(bins)) {
    bins <- NROW(x)
  } else {
    bins <- round(bins)
    if (bins <= 0) 
      stop("bins must be non negative")
  }
  if (is.null(border)) {
    border <- seq(nseries)
  } else {
    if (length(border) != nseries) {
      border <- rep(border, length.out = nseries)
    }
  }
  pch <- rep(pch, nseries, length.out = nseries)
  if (axes) {
    circular::axis.circular(units = units, template = template, zero = zero, 
      rotation = rotation, digits = digits, cex = cex, 
      tcl = tcl, tcl.text = tcl.text)
  }
  if (!is.logical(ticks)) 
    stop("ticks must be logical")
  if (ticks) {
    at <- circular::circular((0:bins) / bins * 2 * pi, zero = zero, rotation = rotation)
    circular::ticks.circular(at, tcl = tcl)
  }
  for (iseries in 1:nseries) {
    x <- xx[, iseries]
    x <- c(na.omit(x))
    n <- length(x)
    if (n) {
      x <- circular::conversion.circular(x, units = "radians", modulo = modulo)
      attr(x, "circularp") <- attr(x, "class") <- NULL
      if (template == "clock12") 
        x <- 2 * x
      x <- x %% (2 * pi)
      circular:::RosediagRad(x, zero = zero, rotation, bins, upper, 
        radii.scale, prop, border, col = col[iseries])
    }
  }
  return(invisible(list(zero = zero, rotation = rotation, next.points = 0, x = x, bins = bins,
    radii.scale = radii.scale, prop = prop, col = col)))
}

#' Draw rings at absolute points
#' 
#' Adapted from RosediagRad to draw rings inside the circular plot. Called if rings = TRUE in myRoseDiag
#' 
#' @inheritParams myRoseDiag
#' 
#' @keywords internal
#'  
ringsAbs <- function(plot.params, border, rings.lty, 
  ring.text, ring.text.pos, ring.text.cex){
  n <- length(plot.params$x)
  freq <- rep(plot.params$x, plot.params$bins)
  arc <- (2 * pi)/plot.params$bins
  breaks <- seq(0, 2 * pi, length.out = (plot.params$bins + 1))
  freq <- hist.default(plot.params$x, breaks = breaks, plot = FALSE)$counts   
  my.max <- range(freq)[2]
  my.breaks <- my.max / divisors(my.max)
    while(length(my.breaks) < 3){
      my.max <- my.max + 1
      my.breaks <- divisors(my.max)
    } 
  clean.breaks <- my.breaks[-c(1, length(my.breaks))]
  nlines <- my.max / clean.breaks
  breaker <- clean.breaks[which.min(abs(nlines - 4))]
  line.values <- seq(from = breaker, to = my.max, by = breaker)
  rel.values <- line.values / n
  if (plot.params$radii.scale == "sqrt") {
     radius <- sqrt(rel.values)*plot.params$prop
  } else {
     radius <- rel.values*plot.params$prop
  }
  for (i in 1:length(radius)) {
    xx <- c(radius[i] * cos(seq(0, 2 * pi, length = 1000) + plot.params$zero))
    yy <- c(radius[i] * sin(seq(0, 2 * pi, length = 1000) + plot.params$zero))
    polygon(xx, yy, border = border, lty = rings.lty)
  }
  if(ring.text){
    text(x = rep(0, length(radius)), y = (radius * -1) + ring.text.pos, line.values, cex = ring.text.cex)
  }
}

#' Draw rings at relative points
#' 
#' Adapted from RosediagRad to draw rings inside the circular plot. Called if rings = TRUE in myRoseDiag
#' 
#' @inheritParams myRoseDiag
#' 
#' @keywords internal
#'  
ringsRel <- function(plot.params, border, rings.lty, 
  ring.text, ring.text.pos, ring.text.cex){
  range <- seq(from = 0, to = 1 / plot.params$prop, length.out = 5)
  radius <- c(0.25, 0.50, 0.75, 1)
  line.values <- paste(round(range * 100, 2), "%", sep = "")
  if (plot.params$radii.scale == "sqrt")
    radius <- sqrt(radius)
  for (i in 1:3) {
    xx <- c(radius[i] * cos(seq(0, 2 * pi, length = 1000) + plot.params$zero))
    yy <- c(radius[i] * sin(seq(0, 2 * pi, length = 1000) + plot.params$zero))
    polygon(xx, yy, border = border, lty = rings.lty)
  }

  if(ring.text)
    text(x = rep(0, 4), y = (radius * -1) + ring.text.pos, line.values[-1], cex = ring.text.cex)
}

#' Draw mean value in the axis margin
#' 
#' Computes and draws the mean value for a given dataset, may also plot standard error of the mean
#' or standard deviation ranges.
#' 
#' @param input the input dataset
#' @param mean.col colour of the mean dash.
#' @param mean.length vertical length of the mean dash.
#' @param mean.lwd width of the mean dash.
#' @param box.range One of "none", "std.error" or "sd", controls the statistic used to draw the range boxes.
#' @param fill Fill colour for the range box.
#' @param border Border colour for the range box.
#' @param box.size Vertical size of the range box.
#' @param edge.length Vertical size of the edge whiskers in the range box.
#' @param edge.lwd Width of the edge whiskers in the range box.
#' 
roseMean <- function(input, col = c("cornflowerblue", "chartreuse3", "deeppink"),
  mean.length = c(0.0125, -0.0125), mean.lwd = 4,
  box.range = c("none", "std.error", "sd"), fill = "white", border = "black",
  box.size = c(1.015, 0.985), edge.length = c(0.025, -0.025), edge.lwd = 2){
  box.range <- match.arg(box.range)
  col <- scales::alpha(col, 1)
  if(is.matrix(input) | is.data.frame(input)){
    plotdata <- list()
    for(i in 1:ncol(input)) plotdata[[i]] <- input[,i]
    names(plotdata) <- colnames(input)
  } else {
    if (is.list(input)){
      if (any(!unlist(lapply(input, function(x) inherits(x, "circular")))))
        stop("Input is a list but not all elements in the list are circular objects.\n")
      plotdata <- input
    } else {
      plotdata <- list(input)
    }
  }
  if (!exists("plotdata"))
    stop("Input must be a list of circular objects, a data.frame, a matrix or a vector.\n")
  for (i in 1:length(plotdata)) {
  b <- circular::mean.circular(plotdata[[i]], na.rm = T)
  if(box.range != "none"){
    if(box.range == "std.error")
      c <- std.error(plotdata[[i]], silent = TRUE)
    if(box.range == "sd")
      c <- sd(plotdata[[i]])
    zero <- attr(plotdata[[i]], "circularp")$zero # extracted from the circular data
    left <- as.numeric(circular::conversion.circular((b - c), units = "radians")) * -1
    right <- as.numeric(circular::conversion.circular((b + c), units = "radians")) * -1
      xx <- c(box.size[1] * cos(seq(left, right, length = 1000) + zero), rev(box.size[2] * cos(seq(left, right, length = 1000) + zero)))
      yy <- c(box.size[1] * sin(seq(left, right, length = 1000) + zero), rev(box.size[2] * sin(seq(left, right, length = 1000) + zero)))
      polygon(xx, yy, col = fill, border = border)
    circular::lines.circular(c(b + c, b + c), edge.length, lwd = edge.lwd, col = border)
    circular::lines.circular(c(b - c, b - c), edge.length, lwd = edge.lwd, col = border)
  }
  circular::lines.circular(c(b, b), mean.length, lwd = mean.lwd, col = col[i], lend = 1)
  }
}

