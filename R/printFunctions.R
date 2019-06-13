#' Check latex compatibility
#' 
#' Looks for the installed packages
#'
#' @return A TRUE/FALSE decision
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
#' @inheritParams actel
#' 
#' @return string to be included in printRmd
#'  
#' @keywords internal
#' 
printBiometrics <- function(bio) {
  if (any(C <- grepl("Length", colnames(bio)) | grepl("Weight", colnames(bio)) | grepl("Mass", colnames(bio)) | grepl("Size", colnames(bio)))) {
    biometric.fragment <- ""
    if (sum(C) > 1) {
      graphic.width <- paste(90 / sum(C), "%", sep = "")
    } else {
      graphic.width <- 0.45
      graphic.width <- paste(45, "%", sep = "")
    }
    counter <- 1
    for (i in colnames(bio)[C]) {
      if (exists("DEBUG")) 
        cat(paste("Debug: Creating graphic '", gsub("[.]", "_", i), "_boxplot.pdf'.\n", sep = ""))
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
  if (!exists("biometric.fragment"))
    biometric.fragment <- ""
  return(biometric.fragment)
}

#' Print dotplots
#'
#' Prints dotplots of multiple variables.
#' 
#' @inheritParams assembleOverview
#' 
#' @keywords internal
#' 
printDotplots <- function(status.df, invalid.dist) {
  if (exists("DEBUG")) 
    cat(paste("Debug: Creating graphic 'dotplots.pdf'.\n", sep = ""))
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
}

#' Print survival graphic
#'
#' Prints survival graphics per fish group.
#' 
#' @param group.overview A dataframe containing the survival per group of fish present in the biometrics. Supplied by assembleOverview.
#' 
#' @keywords internal
#' 
printSurvivalGraphic <- function(group.overview) {
  cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")
  names(cbPalette) <- c("Orange", "Blue", "Green", "Yellow", "Darkblue", "Darkorange", "Pink", "Grey")
  survival <- data.frame(group = character(), section = character(), value = vector(), stringsAsFactors = FALSE)
  i = 2
  while (i <= ncol(group.overview)) {
    for (j in 1:nrow(group.overview)) {
      active.row <- nrow(survival) + 1
      survival[active.row, ] <- NA
      survival$group[active.row] <- rownames(group.overview)[j]
      survival$section[active.row] <- tail(strsplit(colnames(group.overview)[i], "[.]")[[1]], 1)
      if (group.overview[j, i - 1] > 0) {
        survival$value[active.row] <- 1 - group.overview[j, i]/group.overview[j, i - 1]
      } else {
        survival$value[active.row] <- 0
      }
    }
    rm(active.row, j)
    i = i + 2
  }
  for (j in 1:nrow(group.overview)) {
    active.row <- nrow(survival) + 1
    survival[active.row, ] <- NA
    survival$group[active.row] <- rownames(group.overview)[j]
    survival$section[active.row] <- "Overall"
    survival$value[active.row] <- group.overview[j, ncol(group.overview)]/group.overview[j, 1]
  }
  survival$section <- factor(survival$section, levels = unique(survival$section))
  p <- ggplot2::ggplot(survival, ggplot2::aes(x = section, y = value))
  p <- p + ggplot2::geom_bar(stat = "identity", fill = cbPalette[[2]], colour = cbPalette[[2]])
  p <- p + ggplot2::facet_grid(. ~ group)
  p <- p + ggplot2::theme_bw()
  p <- p + ggplot2::ylim(0, 1)
  p <- p + ggplot2::labs(x = "", y = "Survival")
  # ggplot2::ggsave(paste("Report/survival.pdf", sep = ""), width = nrow(group.overview) * 2, height = 4)
  ggplot2::ggsave(paste("Report/survival.png", sep = ""), width = nrow(group.overview) * 2, height = 4)
}

#' Print progression graphic
#'
#' Prints progression graphics per fish group and release site.
#' 
#' @inheritParams actel
#' @inheritParams assembleEfficiency
#' @param progression A dataframe containing the number of fish that crossed each ALS array. Supplied by assembleProgression.
#' 
#' @keywords internal
#' 
printProgression <- function(progression, success.arrays, spatial) {
  cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")
  names(cbPalette) <- c("Orange", "Blue", "Green", "Yellow", "Darkblue", "Darkorange", "Pink", "Grey")
  appendTo("debug","Creating graphic 'progression.pdf'.")
  finish.line <- min(match(success.arrays, unlist(spatial$array.order))) + 0.5  # One would expect '-0.5' but the progression plot has one extra level (release)
  recipient <- c()
  for (i in seq_len(nrow(progression))) {
    recipient[i] <- strsplit(rownames(progression)[i], "[.]")[[1]][1]
  }
  rm(i)
  progression$group <- recipient
  rm(recipient)
  progression$stat <- rep(c("Last", "Remaining"), nrow(progression)/2)
  PlotData <- suppressMessages(reshape2::melt(progression[, -1]))
  # PlotData$value[is.na(PlotData$value)] == 0
  labels <- data.frame(group = character(), variable = character(), value = vector(), label = character(), stringsAsFactors = FALSE)
  i = 2
  while (i <= nrow(PlotData)) {
    active.row <- nrow(labels) + 1
    labels[active.row, ] <- NA
    labels$group[active.row] <- PlotData$group[i]
    labels$variable[active.row] <- as.character(PlotData$variable[i])
    if (!is.na(PlotData$value[i])) 
      labels$value[active.row] <- sum(PlotData$value[c(i - 1, i)]) + 2
    if (!is.na(PlotData$value[i])) 
      labels$label[active.row] <- paste(PlotData$value[c(i - 1, i)], collapse = ", ")
    i = i + 2
    rm(active.row)
  }
  rm(i)
  labels <- labels[complete.cases(labels), ]
  labels$variable <- factor(labels$variable)
  
  p <- ggplot2::ggplot(PlotData, ggplot2::aes(x = variable, y = value))
  p <- p + ggplot2::geom_bar(stat = "identity", ggplot2::aes(fill = stat), na.rm = TRUE)
  p <- p + ggplot2::geom_text(data = labels, ggplot2::aes(x = variable, y = value, label = label), na.rm = TRUE)
  p <- p + ggplot2::facet_grid(group ~ .)
  p <- p + ggplot2::theme_bw()
  p <- p + ggplot2::theme(panel.grid.minor.x = ggplot2::element_blank(), panel.grid.major.x = ggplot2::element_blank())
  p <- p + ggplot2::scale_fill_manual(values = c(Last = cbPalette[[2]], Remaining = cbPalette[[5]]), labels = c("Last detected here", "Detected downstream"))
  p <- p + ggplot2::geom_vline(xintercept = finish.line, linetype = "dashed")
  p <- p + ggplot2::theme(legend.title = ggplot2::element_blank(), legend.position = c(0.99, 0.99), legend.justification = c("right", "top"), legend.box.just = "right", legend.margin = ggplot2::margin(2, 
    6, 3, 2), legend.background = ggplot2::element_rect(fill = "white", linetype = "solid", colour = "black", size = 0.35))
  p <- p + ggplot2::ylim(0, max(labels$value) * 1.2)
  p <- p + ggplot2::labs(y = "", x = "")
  # ggplot2::ggsave(paste("Report/progression.pdf", sep = ""), width = 6, height = 6)
  ggplot2::ggsave(paste("Report/progression.png", sep = ""), width = 6, height = 6)
  rm(PlotData, labels, p, finish.line)
}

#' Print efficiency fragment
#'
#' Prints the ALS inter-array efficiency for inclusion in printRmd.
#' 
#' @param efficiency A list containing the efficiency. Supplied by assembleEfficiency.
#' 
#' @keywords internal
#' 
printEfficiency <- function(efficiency){
  efficiency.fragment <- ""
  n <- 10
  for (i in names(efficiency$inter.array)) {
    efficiency.fragment <- paste(efficiency.fragment, '
#### Fish released at ', i, '

**Individuals detected and estimated**

```{r efficiency', n + 1, ', echo = FALSE}
knitr::kable(efficiency$inter.array$', i, '$results$absolutes)
```

**Array efficiency**

```{r efficiency', n + 2, ', echo = FALSE}
to.print <- t(paste(round(efficiency$inter.array$', i, '$results$efficiency * 100, 1), "%", sep = ""))
to.print[, ncol(to.print)] = "-"
colnames(to.print) <- names(efficiency$inter.array$', i, '$results$efficiency)
rownames(to.print) <- "efficiency"
knitr::kable(to.print)
```

**Lambda:** `r I(efficiency$inter.array$', i, '$results$lambda)`

', sep = "")
    n = n + 10
  }

  if (length(efficiency$last.array$results) == 1){
    efficiency.fragment <- paste(efficiency.fragment, '
#### Last array efficiency estimate

```{r efficiency', n, ', echo = FALSE,  comment = NA}
cat(efficiency$last.array$results)
```', sep = "")
  } else {
    efficiency.fragment <- paste(efficiency.fragment, '
#### Last array efficiency estimate

```{r efficiency', n, ', echo = FALSE,  comment = NA}
knitr::kable(efficiency$last.array$results$absolutes)
```

**Last array estimated efficiency: **', round(efficiency$last.array$results$combined.efficiency * 100, 2), "%", sep = "")
  }
  return(efficiency.fragment)
}
#' Print individual graphics
#'
#' Prints the individual detections for each fish, overlaying the points in time considered crucial during the analysis.
#' 
#' @inheritParams actel
#' @inheritParams assembleEfficiency
#' @inheritParams assembleOverview
#' 
#' @return String to be included in printRmd
#' 
#' @keywords internal
#' 
printIndividuals <- function(redraw, detections.list, status.df, tz.study.area) {
  cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")
  names(cbPalette) <- c("Orange", "Blue", "Green", "Yellow", "Darkblue", "Darkorange", "Pink", "Grey")
  appendTo("Report", "M: Drawing individual graphics for the report.")
  cat("M: Drawing individual graphics for the report.\n")
  if (exists("redraw") && redraw == FALSE) {
    appendTo("Report", "M: 'redraw' is set to FALSE, only drawing new graphics.")
  }
  if (!exists("DEBUG")) 
    pb <- txtProgressBar(min = 0, max = length(detections.list), style = 3, width = 60)
  individual.plots <- paste("")
  for (i in 1:length(detections.list)) {
    fish <- names(detections.list)[i]
    if (!(exists("redraw") && redraw == FALSE && file.exists(paste("Report/", fish, ".png", sep = "")))) {
      appendTo("debug",paste("Debug: Printing graphic for fish", fish, ".", sep = ""))
      PlotData <- detections.list[[fish]]
      colnames(PlotData)[1] <- "Timestamp"
      the.row <- status.df$Signal == tail(strsplit(fish, "-")[[1]], 1)
      start.line <- as.POSIXct(status.df$Release.date[the.row], tz = tz.study.area)
      first.time <- min(c(as.POSIXct(head(PlotData$Timestamp, 1), tz = tz.study.area), start.line))
      attributes(first.time)$tzone <- tz.study.area
      last.time <- as.POSIXct(tail(PlotData$Timestamp, 1), tz = tz.study.area)
      relevant.line <- status.df[the.row, (grepl("Arrived", colnames(status.df)) | grepl("Left", colnames(status.df)))]
      p <- ggplot2::ggplot(PlotData, ggplot2::aes(x = Timestamp, y = Standard.Name, colour = Array))
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
      p <- p + ggplot2::geom_vline(xintercept = start.line, linetype = "dashed")
      for (l in 1:length(relevant.line)) {
        if (!is.na(relevant.line[l])) {
          p <- p + ggplot2::geom_vline(xintercept = as.POSIXct(relevant.line[[l]], tz = tz.study.area), linetype = "dashed", color = "grey")
        }
      }
      rm(l)
      p <- p + ggplot2::xlim(first.time, last.time)
      if (length(levels(PlotData$Array)) <= 8) {
        p <- p + ggplot2::scale_color_manual(values = as.vector(cbPalette)[1:length(levels(PlotData$Array))], drop = FALSE)
      } else {
        p <- p + ggplot2::scale_color_discrete(drop = FALSE)
      }
      p <- p + ggplot2::geom_point()
      p <- p + ggplot2::guides(colour = ggplot2::guide_legend(reverse = T))
      p <- p + ggplot2::labs(title = paste(fish, " (", status.df[status.df$Transmitter == fish, "Status"], ")", sep = ""), x = paste("tz: ", tz.study.area, sep = ""), y = "Station Standard Name")
      ggplot2::ggsave(paste("Report/", fish, ".png", sep = ""), width = 5, height = 4)  # better to save in png to avoid point overlapping issues
      rm(PlotData, start.line, last.time, relevant.line, first.time)
    }
    if (i%%2 == 0) {
      individual.plots <- paste(individual.plots, "![](", fish, ".png){ width=50% }\n", sep = "")
    } else {
      individual.plots <- paste(individual.plots, "![](", fish, ".png){ width=50% }", sep = "")
    }
    if (!exists("DEBUG")) 
      setTxtProgressBar(pb, i)
    flush.console()
  }
  rm(i, fish)
  if (!exists("DEBUG")) {
    close(pb)
    rm(pb)
  }
  return(individual.plots)
}

#' Print Rmd report
#'
#' Creates a Rmd report and converts it to hmtl.
#' 
#' @param name.fragment Rmarkdown string specifying the type of report for the title.
#' @param header.fragment Rmarkdown string specifying the type of report for the header.
#' @param biometric.fragment Rmarkdown string specifying the biometric graphics drawn.
#' @param efficiency.fragment Rmarkdown string specifying the biometric graphics drawn.
#' @param survival.graph.size Rmarkdown string specifying the type size of the survival graphics.
#' @param individual.plots Rmarkdown string specifying the name of the individual plots.
#' 
#' @keywords internal
#' 
printRmd <- function(name.fragment, header.fragment, biometric.fragment, efficiency.fragment,
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

Note:
  : Efficiency estimates were calculated separatly for different release sites.

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
knitr::kable(group.overview)
```

<center>
![](survival.png){ ',survival.graph.size ,' }
</center>


### Progression

<center>
![](progression.png){ width=65% }
</center>


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




