#' Check report compatibility
#' 
#' Creates a "Report" folder if necessary and silently activates ggplot2 and reshape2 to avoid startup messages
#'
#' @return A TRUE/FALSE decision
#' 
#' @keywords internal
#' 
folderCheck <- function(report, redraw){
  if (report) {
    appendTo("Report", "M: 'report' option has been activated.")
    if (length(setdiff(c("ggplot2", "reshape2"), rownames(installed.packages()))) > 0) {
      appendTo(c("Screen", "Report", "Warning"), "W: 'report' option can only be activated if 'ggplot2' and 'reshape2' are installed. Please install these. Deactivating 'Report' for the current job.")
      report <- FALSE
    } else {
      suppressPackageStartupMessages(library(ggplot2))
      suppressPackageStartupMessages(library(reshape2))
      if (!dir.exists("Report")) {
        appendTo("Screen", "M: Creating 'Report' subdirectory to store report files.")
        dir.create("Report")
      } else {
        if (redraw) {
        appendTo("Screen", "W: 'Report' directory already present. Overwriting files already present.")
        } else {
        appendTo("Screen", "W: 'Report' directory already present. Skipping files already present.")
        }
      }
    }
  }
  return(report)
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
  largest <- c(-1, -1, unlist(apply(t1[, c(-1, -2)], 2, function(x) if (!all(is.na(x))) {
    max(x, na.rm = T)
  } else {
    NA
  })))
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
  ggplot2::ggsave(paste("Report/dotplots.png", sep = ""), width = 6, height = 10)
  appendTo("debug", "Terminating printDotplots.")
}

#' Print survival graphic
#'
#' Prints survival graphics per fish group.
#' 
#' @param section.overview A dataframe containing the survival per group of fish present in the biometrics. Supplied by assembleOverview.
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
#' @return An updated progression dataframe and an updated the.levels
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
#' @param progression the progression dataframe, provided by compileProgressionDataFrame
#' 
#' @keywords internal
#' 
#' @return The additions dataframe, or NULL if additions should be aborted.
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
#' @return the totals dataframe
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
#' @param totals the totals dataframe, provided by prepareTotals
#' 
#' @keywords internal
#' 
#' @return the final total dataframe
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
#' @param array.overview a list of abolute detection numbers for each group
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
#' @param extention the format of the generated graphics
#' 
#' @return String to be included in printRmd
#' 
#' @keywords internal
#' 
printIndividuals <- function(redraw, detections.list, status.df, tz.study.area, movements, simple.movements, extention = "png") {
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
          Station = as.vector(t(movements[[fish]][, c("First station", "Last station")])),
          Timestamp = as.vector(t(movements[[fish]][, c("First time", "Last time")]))
        )
      all.moves.line$Station <- factor(all.moves.line$Station, levels = levels(PlotData$Standard.Name))
      all.moves.line$Timestamp <- as.POSIXct(all.moves.line$Timestamp, tz = tz.study.area)
      add.simple.movements <- FALSE
      if (!is.null(simple.movements[[fish]])) {
        add.simple.movements <- TRUE
        simple.moves.line <- data.frame(
          Station = as.vector(t(simple.movements[[fish]][, c("First station", "Last station")])),
          Timestamp = as.vector(t(simple.movements[[fish]][, c("First time", "Last time")]))
          )
        simple.moves.line$Station <- factor(simple.moves.line$Station, levels = levels(PlotData$Standard.Name))
        simple.moves.line$Timestamp <- as.POSIXct(simple.moves.line$Timestamp, tz = tz.study.area)
      }
      appendTo("debug", paste("Debug: Printing graphic for fish", fish, ".", sep = ""))
      colnames(PlotData)[1] <- "Timestamp"
      the.row <- status.df$Signal == tail(strsplit(fish, "-")[[1]], 1)
      start.line <- as.POSIXct(status.df$Release.date[the.row], tz = tz.study.area)
      first.time <- min(c(as.POSIXct(head(PlotData$Timestamp, 1), tz = tz.study.area), start.line))
      attributes(first.time)$tzone <- tz.study.area
      last.time <- as.POSIXct(tail(PlotData$Timestamp, 1), tz = tz.study.area)
      relevant.line <- status.df[the.row, (grepl("Arrived", colnames(status.df)) | grepl("Left", colnames(status.df)))]
      # Start plot
      p <- ggplot2::ggplot(PlotData, ggplot2::aes(x = Timestamp, y = Standard.Name, colour = Array))
      # Choose background
      default.cols <- TRUE
      if (status.df$P.type[the.row] == "Overridden") {
        p <- p + ggplot2::theme(
          panel.background = ggplot2::element_rect(fill = "white"),
          panel.border = ggplot2::element_rect(fill = NA, colour = "#ef3b32" , size = 2),
          panel.grid.major = ggplot2::element_line(size = 0.5, linetype = 'solid', colour = "#ffd8d6"), 
          panel.grid.minor = ggplot2::element_line(size = 0.25, linetype = 'solid', colour = "#ffd8d6"),
          legend.key = ggplot2::element_rect(fill = "white", colour = "white"),
          )
        default.cols <- FALSE
      } 
      if (status.df$P.type[the.row] == "Manual") {
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
      for (l in 1:length(relevant.line)) {
        if (!is.na(relevant.line[l])) {
          p <- p + ggplot2::geom_vline(xintercept = as.POSIXct(relevant.line[[l]], tz = tz.study.area), linetype = "dashed", color = "grey")
        }
      }
      rm(l)
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
      p <- p + ggplot2::labs(title = paste(fish, " (", status.df[status.df$Transmitter == fish, "Status"], ")", sep = ""), x = paste("tz: ", tz.study.area, sep = ""), y = "Station Standard Name")
      ggplot2::ggsave(paste0("Report/", fish, ".", extention), width = 5, height = 4)  # better to save in png to avoid point overlapping issues
      rm(PlotData, start.line, last.time, relevant.line, first.time)
    }
    if (i%%2 == 0) {
      individual.plots <- paste0(individual.plots, "![](", fish, ".", extention, "){ width=50% }\n")
    } else {
      individual.plots <- paste0(individual.plots, "![](", fish, ".", extention, "){ width=50% }")
    }
    setTxtProgressBar(pb, i)
  }
  close(pb)
  return(individual.plots)
}

#' Print Rmd report
#'
#' Creates a Rmd report and converts it to hmtl.
#' 
#' @param name.fragment Rmarkdown string specifying the type of report for the title.
#' @param header.fragment Rmarkdown string specifying the type of report for the header.
#' @param biometric.fragment Rmarkdown string specifying the biometric graphics drawn.
#' @param efficiency.fragment Rmarkdown string specifying the efficiency results.
#' @param array.overview.fragment Rmarkdown string specifying the array overview results.
#' @param survival.graph.size Rmarkdown string specifying the type size of the survival graphics.
#' @param individual.plots Rmarkdown string specifying the name of the individual plots.
#' @inheritParams loadDetections
#' 
#' @keywords internal
#' 
printRmd <- function(name.fragment, header.fragment, biometric.fragment, efficiency.fragment, array.overview.fragment,
  survival.graph.size, individual.plots, spatial){
  appendTo("Screen", "M: Producing final report.")
  if (file.exists(reportname <- paste("Report/actel_report", name.fragment, ".Rmd", sep = ""))) {
    continue <- TRUE
    index <- 1
    while (continue) {
      if(file.exists(reportname <- paste("Report/actel_report", name.fragment, ".", index, ".Rmd", sep = ""))) {
        index <- index + 1
      } else {
        continue <- FALSE
      }
    }
    appendTo("Screen",paste("M: An actel report is already present in the present directory, saving new report as 'actel_report", name.fragment, ".", index, ".html'.", sep = ""))
    rm(continue,index)
  } else {
    appendTo("Screen",paste("M: Saving actel report as 'actel_report", name.fragment, ".html'.", sep = ""))
  }
  if (any(grepl("Ukn.", spatial$stations$Standard.Name))) {
    unknown.fragment <- paste('<span style="color:red"> Number of relevant unknown receivers: **', sum(grepl("Ukn.", spatial$stations$Standard.Name)), '**</span>\n', sep = "")
  } else {
    unknown.fragment <- ""
  } 
  report <- readr::read_file("temp_log.txt")
  folder.fragment <- 
  sink(reportname)
  cat(paste(
'---
title: "Acoustic telemetry analysis report"
author: "Actel package"
output: 
  html_document:
    includes:
      after_body: toc_menu.html
---

### Summary

Selected folder: ', stringr::str_extract(pattern = '(?<=M: Selected folder: )[^\r]*', string = report), '

Timestamp: **', stringr::str_extract(pattern = '(?<=Timestamp:)[^\r]*', string = report), '** 

Number of target tags: **`r I(nrow(status.df))`**

', header.fragment,' 

Number of listed receivers: **`r I(spatial$number.of.receivers)`** (of which **', stringr::str_extract(pattern = '(?<=of which )[0-9]*', string = report), '** had no detections)

', unknown.fragment,'

Data time range: ', stringr::str_extract(pattern = '(?<=Data time range: )[^\r]*', string = report), '


### List of Stations

```{r stations, echo = FALSE}
knitr::kable(spatial$stations)
```


### List of Release sites

```{r releases, echo = FALSE}
knitr::kable(spatial$release.sites)
```

### Array forward efficiency

', efficiency.fragment,'

### Warning messages

```{r warnings, echo = FALSE, comment = NA}
if(file.exists("../temp_warnings.txt")) cat(gsub("\\r", "", readr::read_file("../temp_warnings.txt"))) else cat("No warnings were raised during the analysis.")
```


### User comments

```{r comments, echo = FALSE, comment = NA}
 if(file.exists("../temp_comments.txt")) cat(gsub("\\r", "", readr::read_file("../temp_comments.txt"))) else cat("No comments were included during the analysis.")
```


### Biometric graphics

<center>
', biometric.fragment,'
</center>


### Survival

```{r survival, echo = FALSE}
knitr::kable(section.overview)
```

<center>
![](survival.png){ ',survival.graph.size ,' }
</center>


### Progression

Note:
  : The progression calculations do not account for intra-section backwards movements. This implies that the total number of fish to have been **last seen** at a given array may be lower than the displayed below. Please refer to the [section survival overview](#survival) to find out where each of your fish was considered to have disappeared.
  
<center>
![](progression.png){ width=75% }
</center>

', array.overview.fragment, '

### Dotplots

<center>
![](dotplots.png){ width=95% }
</center>


### Full log

```{r log, echo = FALSE, comment = NA}
cat(gsub("\\r", "", readr::read_file("../temp_log.txt")))
```


### Individual plots

Note:
  : Manually **edited** fish are highlighted with **yellow** graphic borders.
  : Manually **overridden** fish are highlighted with **red** graphic borders.

<center>
', individual.plots,'
</center>


', sep = ""), fill = TRUE)
sink()

if(file.exists("Report/toc_menu.html"))
  file.remove("Report/toc_menu.html")
sink("Report/toc_menu.html")
cat(
'<style>
h3 {
  padding-top: 25px;
  padding-bottom: 15px;
}

h4 {
  padding-top: 25px;
  padding-bottom: 15px;
}

/* The sidebar menu */
.sidenav {
  height: 100%; 
  width: 110px; 
  position: fixed; 
  z-index: 1; 
  top: 0; 
  left: 0;
  background-color: #fcfcfc;
  overflow-x: hidden; 
  padding-top: 20px;
}

/* The navigation menu links */
.sidenav a {
  padding: 6px 8px 6px 16px;
  text-decoration: none;
  /*font-size: 25px;*/
  color: #818181;
  display: block;
}

.sidenav p {
  padding: 6px 8px 6px 16px;
  text-decoration: none;
  font-size: 25px;
  color: #818181;
  display: block;
}

.sidenav a:hover {
  background-color: #52a548;
  color: #f1f1f1;
}

.fluid-row {
  margin-left: 110px; /* Same as the width of the sidebar */
  padding: 0px 10px;
}

.section {
  margin-left: 110px; /* Same as the width of the sidebar */
  padding: 0px 10px;
}

.level4 {
  margin-left: 0px; /* Same as the width of the sidebar */
  padding: 0px 0px;
}

/* On smaller screens, where height is less than 450px, change the style of the sidebar (less padding and a smaller font size) */
@media screen and (max-height: 450px) {
  .sidenav {padding-top: 15px;}
  .sidenav a {font-size: 18px;}
}
</style>
  
<div class="sidenav">
  <p>Index:</p>
  <a href="#summary">Summary</a>
  <a href="#list-of-stations">Stations</a>
  <a href="#list-of-release-sites">Releases</a>
  <a href="#array-forward-efficiency">Efficiency</a>
  <a href="#warning-messages">Warnings</a>
  <a href="#user-comments">Comments</a>
  <a href="#biometric-graphics">Biometrics</a>
  <a href="#survival">Survival</a>
  <a href="#progression">Progression</a>
  <a href="#dotplots">Dotplots</a>
  <a href="#full-log">Full log</a>
  <a href="#individual-plots">Individuals</a>
</div>
', fill = TRUE)
sink()
return(reportname)
}




