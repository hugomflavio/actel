#' Generate default ggplot colours
#'
#' Copied from: \url{https://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette}.
#'
#' @param n The number of colours to be generated
#'
#' @return a vector of colours with the same length as n
#'
#' @keywords internal
#'
gg_colour_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  grDevices::hcl(h = hues, l = 65, c = 100)[1:n]
}

#' Print progression diagram
#'
#' @inheritParams explore
#' @inheritParams migration
#' @inheritParams createStandards
#' @param dot a dot data frame
#' @param overall.CJS a single CJS with all the groups and release sites merged
#' @param status.df A data frame with the final migration results
#'
#' @return No return value, called to plot and save graphic.
#'
#' @keywords internal
#'
printProgression <- function(dot, overall.CJS, spatial, status.df, print.releases) {
  appendTo("debug", "Starting printProgression")

  cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")
  names(cbPalette) <- c("Orange", "Blue", "Green", "Yellow", "Darkblue", "Darkorange", "Pink", "Grey")

  sections <- names(spatial$array.order)

  # Prepare node data frame
  diagram_nodes <- data.frame(
  id = 1:length(unique(unlist(dot[, c(1, 3)]))),
  original.label = unique(unlist(dot[, c(1, 3)])),
  label = unique(unlist(dot[, c(1, 3)])),
  stringsAsFactors = FALSE)

  if (length(sections) > 1) {
    if (length(sections) <= length(cbPalette))
      to.fill <- cbPalette
    else
      to.fill <- gg_colour_hue(length(sections))

    diagram_nodes$fillcolor <- rep(NA_character_, nrow(diagram_nodes))
    
    for (i in 1:length(sections)) {
      arrays <- spatial$array.order[[i]]
      diagram_nodes$fillcolor[matchl(diagram_nodes$label, arrays)] <- to.fill[i]
    }
  } else {
    diagram_nodes$fillcolor <- rep("#56B4E9", nrow(diagram_nodes))
  }

  for (i in 1:nrow(diagram_nodes)) {
    link <- grep(paste0("^", diagram_nodes$label[i], "$"), names(overall.CJS$absolutes))
    diagram_nodes$label[i] <- paste0(diagram_nodes$label[i],
      "\\nEfficiency: ", ifelse(is.na(overall.CJS$efficiency[link]), "--", round(overall.CJS$efficiency[link] * 100, 0)),
      "%\\nDetected: ", overall.CJS$absolutes["detected", link],
      "\\nMax.Est.: ", ifelse(is.na(overall.CJS$absolutes["estimated", link]), "--",overall.CJS$absolutes["estimated", link]))
  }

 # Release nodes
  if (print.releases) {
    release_nodes <- spatial$release.sites[, c("Standard.name", "Array")]
    release_nodes$n <- 0
    n <- table(status.df$Release.site)
    link <- match(names(n), release_nodes$Standard.name)
    release_nodes$n[link] <- n
    release_nodes$label <- apply(release_nodes, 1, function(s) {
      paste0(s[1], "\\nn = ", s[3])
    })
    if (any(link <- grepl( "|", release_nodes$Array, fixed = TRUE))) {
      expanded <- NULL
      for(i in which(link)) {
        aux <- data.frame(Standard.name = release_nodes$Standard.name[i],
          Array = unlist(strsplit(release_nodes$Array[i], "|", fixed = TRUE)),
          n = release_nodes$n[i],
          label = release_nodes$label[i])
        expanded <- rbind(expanded, aux)
      }
      release_nodes <- rbind(release_nodes[-which(link), ], expanded)
      rm(expanded)
    }
    release_nodes$id <- nrow(diagram_nodes) + as.numeric(as.factor(release_nodes$label))
  } else {
    release_nodes <- NULL
  }

  # node string
  node_list <- split(diagram_nodes, diagram_nodes$fillcolor)
  node_aux <- paste0(unlist(lapply(node_list, function(n) {
    s <- paste0("node [fillcolor = '", n$fillcolor[1], "']\n")
    s <- paste0(s, paste0(n$id, " [label = '", n$label, "']", collapse = "\n"), "\n")
  })), collapse = "\n")

  if (print.releases) {
    node_fragment <- paste0(node_aux, "\n",
      paste0("node [fillcolor = '#e0f4ff'\nfontcolor = '#727475'\nstyle = 'rounded, filled'\nshape = box]\n",
      paste0(release_nodes$id, " [label = 'R.S.: ", release_nodes$label, "']", collapse = "\n"), "\n"))
  } else {
    node_fragment <- node_aux
  }

  # prepare edge data frame
  if (nrow(dot) == 1) {
    if (dot[1, 1] == dot[1, 3]) {
      diagram_edges <- NULL
      complete <- FALSE
    } else {
      diagram_edges <- as.data.frame(t(apply(dot[, c(1, 3), drop = FALSE], 2, function(x) match(x, diagram_nodes$original.label))))
      complete <- TRUE
    }
  } else  {
    diagram_edges <- as.data.frame(apply(dot[, c(1, 3), drop = FALSE], 2, function(x) match(x, diagram_nodes$original.label)))
    complete <- TRUE
  }
  if (complete) {
    diagram_edges$type <- NA_character_
    diagram_edges$type[dot$to == "--"] <- "[dir = none]"
    diagram_edges$type[dot$to == "->"] <- "[arrowtail = tee, arrowhead = normal, dir = both]"
    diagram_edges$type[dot$to == "<-"] <- "[arrowtail = normal, arrowhead = tee, dir = both]"
    colnames(diagram_edges) <- c("from", "to", "type")
  }
  # Release edges
  if (print.releases) {
    release_edges <- data.frame(from = release_nodes$id,
      to = diagram_nodes$id[match(release_nodes$Array, diagram_nodes$original.label)],
      type = rep("[arrowtype='normal']", nrow(release_nodes)))

    # edge string
    combined_edges <- rbind(diagram_edges, release_edges)
  } else {
    combined_edges <- diagram_edges
  }

  if (!is.null(combined_edges)) {
    edge_list <- split(combined_edges, combined_edges$type)

    edge_fragment <- paste0(unlist(lapply(edge_list, function(n) {
      s <- paste0("edge ", n$type[1], "\n")
      s <- paste0(s, paste0(apply(n, 1, function(x) paste0(x[1:2], collapse = "->")), collapse = "\n"), "\n")
    })), collapse = "\n")
  } else {
    edge_fragment <- ""
  }

  x <- paste0("digraph {
  rankdir = LR
  node [shape = circle,
        style = filled,
        fontname = helvetica,
        fontcolor = white,
        color = grey]

  ", node_fragment, "

  edge [color = grey]
  ", edge_fragment, "
  }")

  plot <- DiagrammeR::grViz(x)
  plot_string <- DiagrammeRsvg::export_svg(plot)
  plot_raw <- charToRaw(plot_string)
  rsvg::rsvg_svg(svg = plot_raw, file = paste0(tempdir(), "/actel_report_auxiliary_files/mb_efficiency.svg"))

  if (!file.exists(paste0(tempdir(), "/actel_report_auxiliary_files/mb_efficiency.svg"))) {
    {grDevices::png(paste0(tempdir(), "/actel_report_auxiliary_files/mb_efficiency.png"), width = 600, height = 100)
     par(mar = c(1, 1, 1, 1))
     plot(NA, xlim = 0:1, ylim = 0:1, xaxt = "n", yaxt = "n", ann = FALSE)
     text(x = 0.5, y = 0.5, "Could not save SVG graphic.\nPlease verify that the SVG engines are working.")
     grDevices::dev.off()
    }
  }
  appendTo("debug", "Finished printProgression")
}

#' Print DOT diagram
#'
#' @param dot a dot data frame
#' @inheritParams explore
#' @inheritParams migration
#' @inheritParams setSpatialStandards
#'
#' @return No return value, called to plot and save graphic.
#'
#' @keywords internal
#'
printDot <- function(dot, spatial, print.releases) {
# requires:
# DiagrammeR
# DiagrammeRsvg
# rsvg
  appendTo("debug", "Starting printProgression")

  cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")
  names(cbPalette) <- c("Orange", "Blue", "Green", "Yellow", "Darkblue", "Darkorange", "Pink", "Grey")

  sections <- names(spatial$array.order)
  # Prepare node data frame
  diagram_nodes <- data.frame(
    id = 1:length(unique(unlist(dot[, c(1, 3)]))),
    label = unique(unlist(dot[, c(1, 3)])),
    stringsAsFactors = FALSE)

  if (length(sections) > 1) {
    if (length(sections) <= length(cbPalette))
      to.fill <- cbPalette
    else
      to.fill <- gg_colour_hue(length(sections))

    diagram_nodes$fillcolor <- rep(NA_character_, nrow(diagram_nodes))
    
    for (i in 1:length(sections)) {
      arrays <- spatial$array.order[[i]]
      diagram_nodes$fillcolor[matchl(diagram_nodes$label, arrays)] <- to.fill[i]
    }
  } else {
    diagram_nodes$fillcolor <- rep("#56B4E9", nrow(diagram_nodes))
  }

 # Release nodes
  if (print.releases) {
    release_nodes <- spatial$release.sites[, c("Standard.name", "Array")]
    if (any(link <- grepl( "|", release_nodes$Array, fixed = TRUE))) {
      expanded <- NULL
      for(i in which(link)) {
        aux <- data.frame(Standard.name = release_nodes$Standard.name[i],
          Array = unlist(strsplit(release_nodes$Array[i], "|", fixed = TRUE)))
        expanded <- rbind(expanded, aux)
      }
      release_nodes <- rbind(release_nodes[-which(link), ], expanded)
      rm(expanded)
    }
    colnames(release_nodes)[1] <- "label"
    release_nodes$id <- nrow(diagram_nodes) + as.numeric(as.factor(release_nodes$label))
  } else {
    release_nodes <- NULL
  }

  # node string
  node_list <- split(diagram_nodes, diagram_nodes$fillcolor)
  node_aux <- paste0(unlist(lapply(node_list, function(n) {
    s <- paste0("node [fillcolor = '", n$fillcolor[1], "']\n")
    s <- paste0(s, paste0(n$id, " [label = '", n$label, "']", collapse = "\n"), "\n")
  })), collapse = "\n")

  if (print.releases) {
    node_fragment <- paste0(node_aux, "\n",
      paste0("node [fillcolor = '#e0f4ff'\nfontcolor = '#727475'\nstyle = 'rounded, filled'\nshape = box]\n",
      paste0(release_nodes$id, " [label = 'R.S.: ", release_nodes$label, "']", collapse = "\n"), "\n"))
  } else {
    node_fragment <- node_aux
  }

  # prepare edge data frame
  if (nrow(dot) == 1) {
    if (dot[1, 1] == dot[1, 3]) {
      diagram_edges <- NULL
      complete <- FALSE
    } else {
      diagram_edges <- as.data.frame(t(apply(dot[, c(1, 3), drop = FALSE], 2, function(x) match(x, diagram_nodes$label))))
      complete <- TRUE
    }
  } else {
    diagram_edges <- as.data.frame(apply(dot[, c(1, 3), drop = FALSE], 2, function(x) match(x, diagram_nodes$label)))
    complete <- TRUE
  }
  if (complete) {
    diagram_edges$type <- NA_character_
    diagram_edges$type[dot$to == "--"] <- "[dir = none]"
    diagram_edges$type[dot$to == "->"] <- "[arrowtail = tee, arrowhead = normal, dir = both]"
    diagram_edges$type[dot$to == "<-"] <- "[arrowtail = normal, arrowhead = tee, dir = both]"
    colnames(diagram_edges) <- c("from", "to", "type")
  }

  # Release edges
  if (print.releases) {
    release_edges <- data.frame(from = release_nodes$id,
      to = diagram_nodes$id[match(release_nodes$Array, diagram_nodes$label)],
      type = rep("[arrowtype='normal']", nrow(release_nodes)))

    # edge string
    combined_edges <- rbind(diagram_edges, release_edges)
  } else {
    combined_edges <- diagram_edges
  }

  if (!is.null(combined_edges)) {
    edge_list <- split(combined_edges, combined_edges$type)

    edge_fragment <- paste0(unlist(lapply(edge_list, function(n) {
      s <- paste0("edge ", n$type[1], "\n")
      s <- paste0(s, paste0(apply(n, 1, function(x) paste0(x[1:2], collapse = "->")), collapse = "\n"), "\n")
    })), collapse = "\n")
  } else {
    edge_fragment <- ""
  }

  x <- paste0("digraph {
  rankdir = LR
  node [shape = circle,
        style = filled,
        fontname = helvetica,
        fontcolor = white,
        color = grey]

  ", node_fragment, "

  edge [color = grey]
  ", edge_fragment, "
  }")

  plot <- DiagrammeR::grViz(x)
  plot_string <- DiagrammeRsvg::export_svg(plot)
  plot_raw <- charToRaw(plot_string)
  rsvg::rsvg_svg(svg = plot_raw, file = paste0(tempdir(), "/actel_report_auxiliary_files/mb_arrays.svg"))

  # failsafe in case SVG printing fails
  if (!file.exists(paste0(tempdir(), "/actel_report_auxiliary_files/mb_arrays.svg"))) {
    {grDevices::png(paste0(tempdir(), "/actel_report_auxiliary_files/mb_arrays.png"), width = 800, height = 100)
      par(mar = c(1, 1, 1, 1))
      plot(NA, xlim = 0:1, ylim = 0:1, xaxt = "n", yaxt = "n", ann = FALSE)
      text(x = 0.5, y = 0.5, "Could not save SVG graphic.\nPlease verify that the SVG engines are working.")
      grDevices::dev.off()
    }
  }
  appendTo("debug", "Finished printProgression")
}

#' Print biometric graphics
#'
#' Searches for columns containing biometric data and prints graphics per animal group
#'
#' @inheritParams splitDetections
#'
#' @return A string of file locations in rmd syntax, to be included in printRmd
#'
#' @keywords internal
#'
printBiometrics <- function(bio) {
  # NOTE: The NULL variables below are actually column names used by ggplot.
  # This definition is just to prevent the package check from issuing a note due unknown variables.
  Group <- NULL

  appendTo("debug", "Starting printBiometrics")
  biometric.fragment <- ""
  if (any(C <- grepl("Length", colnames(bio)) | grepl("Weight", colnames(bio)) | grepl("Mass", colnames(bio)) | grepl("Size", colnames(bio)))) {
    graphic.width <- 0.45
    graphic.width <- paste0(45, "%")
    counter <- 1
    for (i in colnames(bio)[C]) {
      appendTo("debug", paste0("Debug: Creating graphic '", gsub("[.]", "_", i), "_boxplot.png'."))
      p <- ggplot2::ggplot(bio, ggplot2::aes(x = as.factor(Group), y = bio[, i]))
      p <- p + ggplot2::stat_boxplot(geom = "errorbar", na.rm = TRUE)
      p <- p + ggplot2::geom_boxplot(na.rm = TRUE)
      p <- p + ggplot2::theme_bw()
      p <- p + ggplot2::theme(panel.grid.minor.x = ggplot2::element_blank(), panel.grid.major.x = ggplot2::element_blank())
      p <- p + ggplot2::labs(x = "", y = i)
      ggplot2::ggsave(paste0(tempdir(), "/actel_report_auxiliary_files/", gsub("[.]", "_", i), "_boxplot.png"), width = 3, height = 4)
      rm(p)
      if (counter %% 2 == 0)
        biometric.fragment <- paste0(biometric.fragment, "![](", tempdir(), "/actel_report_auxiliary_files/", gsub("[.]", "_", i), "_boxplot.png){ width=", graphic.width, " }\n")
      else
        biometric.fragment <- paste0(biometric.fragment, "![](", tempdir(), "/actel_report_auxiliary_files/", gsub("[.]", "_", i), "_boxplot.png){ width=", graphic.width, " }")
      counter <- counter + 1
    }
  }
  appendTo("debug", "Finished printBiometrics")
  return(biometric.fragment)
}

#' Print dotplots
#'
#' Prints dotplots of multiple variables.
#'
#' @inheritParams simplifyMovements
#' @inheritParams sectionMovements
#'
#' @return No return value, called to plot and save graphic.
#'
#' @keywords internal
#'
printDotplots <- function(status.df, valid.dist) {
  # NOTE: The NULL variables below are actually column names used by ggplot.
  # This definition is just to prevent the package check from issuing a note due unknown variables.
  value <- NULL
  Transmitter <- NULL

  appendTo("debug", "Starting printDotplots")
  t1 <- status.df[status.df$Valid.detections > 0, c("Transmitter", "Valid.detections", colnames(status.df)[grepl("Average.time.until", colnames(status.df)) | grepl("Average.speed.to", colnames(status.df)) | grepl("Total.time.in",
    colnames(status.df))])]
  t1 <- t1[, apply(t1, 2, function(x) !all(is.na(x)))]
  t1$Transmitter <- factor(t1$Transmitter, levels = rev(t1$Transmitter))
  link <- unlist(sapply(colnames(t1), function(x) attributes(t1[,x])$units))
  colnames(t1)[match(names(link), colnames(t1))] <- paste0(names(link), "\n(", link, ")")
  colnames(t1)[grepl("Average.speed.to", colnames(t1))] <- paste0(colnames(t1)[grepl("Average.speed.to", colnames(t1))], "\n(m/s)")
  colnames(t1)[2] <- "Detections\n(n)"
  if (valid.dist) {
    t2 <- t1[, !grepl("Average.time.until", colnames(t1))]
  } else {
    t2 <- t1[, !grepl("Average.speed.to", colnames(t1))]
  }
  colnames(t2) <- sub("Total.time.i", "I", colnames(t2))
  colnames(t2) <- sub("Average.time.until", "To", colnames(t2))
  colnames(t2) <- sub("Average.speed.t", "T", colnames(t2))
  PlotData <- suppressWarnings(suppressMessages(reshape2::melt(t2)))
  PlotData$Colour <- "#ba009e" # purple, for bugs
  for (j in colnames(t2)[-1]) {
    limits <- quantile(t2[, j], probs = c(0.1, 0.9), na.rm = T)
    toadd <- vector()
    for (i in 1:nrow(t2)) {
      if (!is.na(t2[i, j]) && t2[i, j] <= limits[1]) {
        toadd[i] <- "#56B4E9" # blue
      } else {
        if (!is.na(t2[i, j]) && t2[i, j] >= limits[2])
          toadd[i] <- "#f20000" # red
        else
          toadd[i] <- "#000000" # black
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
  p <- p + ggplot2::labs(x = "", y = "")
  ggplot2::ggsave(paste0(tempdir(), "/actel_report_auxiliary_files/dotplots.png"), width = 6, height = (1.3 + 0.115 * (nrow(t2) - 1)), limitsize = FALSE)
  appendTo("debug", "Finished printDotplots")
}

#' Print survival graphic
#'
#' Prints survival graphics per animal group.
#'
#' @param section.overview A data frame containing the survival per animal group present in the biometrics. Supplied by assembleOverview.
#'
#' @return No return value, called to plot and save graphic.
#'
#' @keywords internal
#'
printSurvivalGraphic <- function(section.overview) {
  appendTo("debug", "Starting printSurvivalGraphic")
  section <- NULL
  value <- NULL
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
  p <- p + ggplot2::scale_y_continuous(limits = c(0, 1), expand = c(0, 0, 0.05, 0))
  p <- p + ggplot2::labs(x = "", y = "Survival")
  the.width <- max(2, sum(grepl("Disap.", colnames(section.overview))) * nrow(section.overview))
  ggplot2::ggsave(paste0(tempdir(), "/actel_report_auxiliary_files/survival.png"), width = the.width, height = 4, limitsize = FALSE)
  appendTo("debug", "Finished printSurvivalGraphic")
}


#' print Rmd fragment for inclusion in the report
#'
#' @param array.overview a list of absolute detection numbers for each group
#'
#' @return An rmd string to be included in the report.
#'
#' @keywords internal
#'
printArrayOverview <- function(array.overview) {
  appendTo("debug", "Starting printArrayOverview")
  oldoptions <- options(knitr.kable.NA = "-")
  on.exit(options(oldoptions), add = TRUE)
  array.overview.fragment <- c("")
  for(i in 1:length(array.overview)) {
    array.overview.fragment <- paste0(array.overview.fragment, '

**Group: ', names(array.overview)[i],'**

', paste(knitr::kable(array.overview[[i]]), collapse = "\n"), '\n')
  }
  appendTo("debug", "Finished printArrayOverview")
  return(array.overview.fragment)
}

#' Print efficiency fragment
#'
#' Prints the ALS inter-array efficiency for inclusion in printRmd.
#'
#' @param CJS the overall CJS result from a migration analysis. Used to assess if efficiency should be printed.
#' @param efficiency the overall efficiency result from a residency analysis. Used to assess if efficiency should be printed.
#' @param intra.CJS The output of the getEstimate calculations.
#' @param type The type of analysis being run (migration or residency)
#'
#' @return A rmd string to be included in the report.
#'
#' @keywords internal
#'
printEfficiency <- function(CJS = NULL, efficiency = NULL, intra.CJS, type = c("migration", "residency")){
  appendTo("debug", "Starting printEfficiency")
  oldoptions <- options(knitr.kable.NA = "-")
  on.exit(options(oldoptions), add = TRUE)
  type <- match.arg(type)
  if (type == "migration") {
    if (is.null(CJS)) {
      efficiency.fragment <- "Inter-array efficiency could not be calculated. See full log for more details.\n"
    } else {
      to.print <- t(paste0(round(CJS$efficiency * 100, 1), "%"))
      to.print[grepl("NA", to.print)] <- "-"
      colnames(to.print) <- names(CJS$efficiency)
      rownames(to.print) <- "efficiency"

      efficiency.fragment <- paste0('
Note:
  : These efficiency calculations **do not account for** backwards movements. This implies that the total number of animals to have been **last seen** at a given array **may be lower** than the displayed below. Please refer to the [section survival overview](#section-survival) and [last seen arrays](#last-seen-arrays) to find out how many animals were considered to have disappeared per section.
  : The data used in the tables below is stored in the `overall.CJS` object. Auxiliary information can also be found in the `matrices` and `arrays` objects.
  : These efficiency values are estimated using the analytical equations provided in the paper "Using mark-recapture models to estimate survival from telemetry data" by [Perry et al. (2012)](<https://www.researchgate.net/publication/256443823_Using_mark-recapture_models_to_estimate_survival_from_telemetry_data>). In some situations, more advanced efficiency estimation methods may be required.
  : You can try running `advEfficiency(results$overall.CJS)` to obtain beta-drawn efficiency distributions (replace `results` with the name of the object where you saved the analysis).

**Individuals detected and estimated**

', paste(knitr::kable(CJS$absolutes), collapse = "\n"), '

**Array efficiency**

**Note:** These values already include any intra-array efficiency estimates that have been requested.

', paste(knitr::kable(to.print), collapse = "\n"), '

')
    }
  } else {
    if (is.null(efficiency)) {
      efficiency.fragment <- "Inter-array efficiency could not be calculated. See full log for more details.\n"
    } else {
      absolutes <- as.data.frame(apply(efficiency$absolutes, c(1, 2), function(x) ifelse(is.na(x), "-", x)))

      minmax <- t(paste0(round(efficiency$max.efficiency * 100, 1), "%"))
      minmax[grepl("NA", minmax)] <- "-"
      minmax <- as.data.frame(minmax, stringsAsFactors = FALSE)
      colnames(minmax) <- names(efficiency$max.efficiency)

      aux <- t(paste0(round(efficiency$min.efficiency * 100, 1), "%"))
      aux[grepl("NA", aux)] <- "-"
      minmax[2, ] <- aux

      rownames(minmax) <- c("Maximum efficiency", "Minimum efficiency")

      efficiency.fragment <- paste0('
Note:
  : More information on the differences between "Known missed events" and "Potentially missed events" can be found in the package vignettes.
  : The data used in this table is stored in the `efficiency` object.
  : These efficiency values are estimated using a simple step-by-step method (described in the package vignettes). In some situations, more advanced efficiency estimation methods may be required.
  : You can try running `advEfficiency(results$efficiency)` to obtain beta-drawn efficiency distributions (replace `results` with the name of the object where you saved the analysis).

**Events recorded and missed**

', paste(knitr::kable(absolutes), collapse = "\n"), '

**Array efficiency**

**Note:** These values already include any intra-array efficiency estimates that have been requested.

', paste(knitr::kable(minmax), collapse = "\n"), '

')
    }
  }
  if (!is.null(intra.CJS)){
    efficiency.fragment <- paste0(efficiency.fragment, '
#### Intra array efficiency estimates

Note:
  : The data used in the table(s) below is stored in the `intra.array.CJS` object. Auxiliary information can also be found in the `intra.array.matrices` object.
  : These efficiency values are estimated using the analytical equations provided in the paper "Using mark-recapture models to estimate survival from telemetry data" by [Perry et al. (2012)](<https://www.researchgate.net/publication/256443823_Using_mark-recapture_models_to_estimate_survival_from_telemetry_data>). In some situations, more advanced efficiency estimation methods may be required.
  : You can try running `advEfficiency(results$intra.array.CJS$', names(intra.CJS)[1], ')` to obtain beta-drawn efficiency distributions (replace `results` with the name of the object where you saved the analysis).
')
    for (i in 1:length(intra.CJS)) {
    efficiency.fragment <- paste0(efficiency.fragment, '
**Array: ', names(intra.CJS)[i], '**

```{r efficiency',i + 2,', echo = FALSE,  comment = NA}
knitr::kable(intra.array.CJS[[',i ,']]$absolutes)
```

*Estimated efficiency:* ', round(intra.CJS[[i]]$combined.efficiency * 100, 2), "%\n")
    }
  }
  appendTo("debug", "Finished printEfficiency")
  return(efficiency.fragment)
}

#' Print individual graphics
#'
#' Prints the individual detections for each tag, overlaying the points in time considered crucial during the analysis.
#'
#' @inheritParams explore
#' @inheritParams groupMovements
#' @inheritParams simplifyMovements
#' @inheritParams assembleMatrices
#' @inheritParams plotDetections
#' @param extension the format of the generated graphics
#'
#' @return A string of file locations in rmd syntax, to be included in printRmd
#'
#' @keywords internal
#'
printIndividuals <- function(detections.list, movements, valid.movements, spatial, 
  status.df = NULL, rsp.info, y.axis = c("auto", "stations", "arrays"), extension = "png") {

  appendTo("debug", "Starting printIndividuals")

  # NOTE: The NULL variables below are actually column names used by ggplot.
  # This definition is just to prevent the package check from issuing a note due unknown variables.
  Timestamp <- NULL
  Standard.name <- NULL
  Array <- NULL
  Station <- NULL
  y.axis <- match.arg(y.axis)
  
  fake.results <- list(detections = detections.list,
                movements = movements,
                valid.movements = valid.movements,
                spatial = spatial,
                status.df = status.df,
                rsp.info = rsp.info)

  appendTo(c("Screen", "Report"), "M: Drawing individual detection graphics.")

  if (interactive())
    pb <- txtProgressBar(min = 0, max = length(detections.list), style = 3, width = 60) # nocov
  counter <- 0
  individual.plots <- ""

  if (y.axis == "auto") {
    if (nrow(spatial$stations) > 40 | length(unique(spatial$stations$Array)) > 14)
      y.axis <- "arrays"
    else
      y.axis <- "stations"
  }

  capture <- lapply(names(detections.list), function(tag) {
    counter <<- counter + 1

    p <- plotDetections(input = fake.results, tag = tag, y.axis = y.axis)

    # decide height
    if (y.axis == "stations")
      to.check <- levels(detections.list[[tag]]$Standard.name)
    else
      to.check <- levels(detections.list[[tag]]$Array)

    if (length(to.check) <= 29)
      the.height <- 4
    else
      the.height <- 4 + ((length(to.check) - 30) * 0.1)

    # default width:
    the.width <- 5
    # Adjustments depending on number of legend valudes
    if (y.axis == "stations")
      to.check <- levels(detections.list[[tag]]$Array)
    else
      to.check <- names(spatial$array.order)

    if (length(to.check) > 14 & length(to.check) <= 29) {
      if (counter %% 2 == 0) {
        p <- p + ggplot2::guides(colour = ggplot2::guide_legend(ncol = 2))
        the.width <- 6
      } else {
        p <- p + ggplot2::theme(legend.position = "none")
        the.width <- 4
      }
    }
    if (length(to.check) > 29) {
      if (counter %% 2 == 0) {
        p <- p + ggplot2::guides(colour = ggplot2::guide_legend(ncol = 3))
        the.width <- 7.5
      } else {
        p <- p + ggplot2::theme(legend.position = "none")
        the.width <- 2.5
      }
    }
    # Save
    ggplot2::ggsave(paste0(tempdir(), "/actel_report_auxiliary_files/", tag, ".", extension), width = the.width, height = the.height, limitsize = FALSE)  # better to save in png to avoid point overlapping issues

    if (counter %% 2 == 0) {
      individual.plots <<- paste0(individual.plots, "![](", tempdir(), "/actel_report_auxiliary_files/", tag, ".", extension, "){ width=", the.width * 10, "% }\n")
    } else {
      individual.plots <<- paste0(individual.plots, "![](", tempdir(), "/actel_report_auxiliary_files/", tag, ".", extension, "){ width=", the.width * 10, "% }")
    }
    if (interactive())
      setTxtProgressBar(pb, counter) # nocov
  })
  if (interactive())
    close(pb) # nocov

  appendTo("debug", "Finished printIndividuals")

  return(individual.plots)
}

#' Print circular graphics for each array
#'
#' Prints the time of first entry point on each of the arrays. Contains functions adapted from the circular R package.
#'
#' For more details about the original function, visit the circular package homepage at \url{https://github.com/cran/circular}
#'
#' @param times a (list of) circular object(s)
#' @inheritParams splitDetections
#'
#' @keywords internal
#'
#' @return A string of file locations in rmd syntax, to be included in printRmd
#'
printCircular <- function(times, bio, suffix = NULL){
  appendTo("debug", "Starting printCircular")

  cbPalette <- c("#56B4E9", "#c0ff3e", "#E69F00", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")
  circular.plots <- ""

  circular.scale <- getOption("actel.circular.scale", default = "area")

  if (!(circular.scale %in% c("area", "linear"))) {
    appendTo(c("Screen", "Warning"), "Option actel.circular.scale was set but value is not recognized (accepted values: 'area', 'linear'). Defaulting back to 'area'.")
    radii.scale <- "sqrt"
  } else {
    if (circular.scale == "area") {
      radii.scale <- "sqrt"
    } else {
      radii.scale <- "linear"
    }
  }

  work.path <- paste0(tempdir(), "/actel_report_auxiliary_files/")

  # failsafe in case SVG printing fails
  {
    grDevices::png(paste0(work.path, "circular_svg_print_failure_bounce_back.png"), width = 500, height = 100)
    par(mar = c(1, 1, 1, 1))
    plot(NA, xlim = 0:1, ylim = 0:1, xaxt = "n", yaxt = "n", ann = FALSE)
    text(x = 0.5, y = 0.5, "Could not save SVG graphic.\nPlease verify that the SVG engines are working.")
    grDevices::dev.off()
  }

  if (length(unique(bio$Group)) < 8)
    colours <- paste0(cbPalette[c(1:length(unique(bio$Group)))], 80)
  else
    colours <- paste0(gg_colour_hue(length(unique(bio$Group))), 80)

  names(colours) <- sort(unique(bio$Group))

  if (length(unique(bio$Group)) >= 6)
    legend.pos <- "bottom"
  else
    legend.pos <- "corner"

  for (i in 1:length(times)) {
    if (length(unique(bio$Group)) > 1) {
      link <- match(names(times[[i]]), bio$Transmitter)
      groups <- factor(bio$Group[link], levels = sort(unique(bio$Group)))
      trim.times <- split(times[[i]], groups)
      trim.times <- trim.times[!unlist(lapply(trim.times, function(x) all(is.na(x))))]
      ylegend <- -0.97 + (0.1 * (length(unique(groups)) - 2))
    } else {
      trim.times <- times[i]
      names(trim.times) <- unique(bio$Group)
      ylegend <- -0.97
    }
   
    colours.to.use <- colours[names(trim.times)]
   
    if (legend.pos == "bottom") {
      ylegend <- -1.15
      xlegend <- 0
      xjust <- 0.5
      number.of.columns <- 1
      if (length(colours.to.use) > 2)
        number.of.columns <- 2
      if (length(colours.to.use) > 6)
        number.of.columns <- 3
      if (length(colours.to.use) > 9 & !any(nchar(names(colours.to.use)) > 9))
        number.of.columns <- 4
    } else {
      ylegend <- -0.97 + (0.08 * (length(colours.to.use) - 2))
      xlegend <- -1.3
      xjust <- 0
      number.of.columns <- 1
    }

    prop <- floor(1 / max(unlist(lapply(trim.times, function(x) table(ceiling(x)) / sum(!is.na(x))))))

    if (legend.pos == "corner")
      b <- 1
    else
      b <- (ceiling(length(colours.to.use) / number.of.columns))

    vertical.mar <- b + 2
   
    # The try call prevents the report from crashing down in the presence of unknown errors.
    try(
        {grDevices::svg(paste0(work.path, "times_", names(times)[i], suffix, ".svg"), 
                        height = 5, width = 5, bg = "transparent")
        
        par(mar = c(b, (b + 2) / 2, 2, (b + 2) / 2), xpd = TRUE) # bottom, left, top, right

        copyOfCirclePlotRad(main = names(times)[i], shrink = 1.05, xlab = "", ylab = "")

        params <- myRoseDiag(trim.times, bins = 24, radii.scale = radii.scale,
          prop = prop, tcl.text = -0.1, tol = 0.05, col = colours.to.use, border = "black")

        roseMean(trim.times, col = scales::alpha(params$col, 1), mean.length = c(0.07, -0.07), mean.lwd = 6,
          box.range = "std.error", fill = "white", horizontal.border = "black", 
          vertical.border = scales::alpha(sapply(params$col, darken), 1), box.size = c(1.015, 0.985), 
          edge.length = c(0.025, -0.025), edge.lwd = 2)

        ringsRel(plot.params = params, border = "black", ring.text = TRUE,
          ring.text.pos = 0.07, rings.lty = "f5", ring.text.cex = 0.8)

        legend(x = xlegend, y = ylegend, xjust = xjust, ncol = number.of.columns,
          legend = paste(names(trim.times), " (", unlist(lapply(trim.times, function(x) sum(!is.na(x)))), ")", sep =""),
          fill = params$col, bty = "n", x.intersp = 0.3, cex = 0.8)

        grDevices::dev.off()},
      silent = TRUE)

    if (i %% 2 == 0) {
      if (file.exists(paste0(work.path, "times_", names(times)[i], suffix, ".svg")))
        circular.plots <- paste0(circular.plots, "![](", work.path, "times_", names(times)[i], suffix, ".svg){ width=50% }\n")
      else
        circular.plots <- paste0(circular.plots, "![](", work.path, "circular_svg_print_failure_bounce_back.png){ width=50% }\n")
    } else {
      if (file.exists(paste0(work.path, "times_", names(times)[i], suffix, ".svg")))
        circular.plots <- paste0(circular.plots, "![", work.path, "circular_svg_print_failure_bounce_back.png](times_", names(times)[i], suffix, ".svg){ width=50% }")
      else
        circular.plots <- paste0(circular.plots, "![](", work.path, "circular_svg_print_failure_bounce_back.png){ width=50% }")
    }
  }

  appendTo("debug", "Finished printCircular")
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
#' @return No return value, adds to an existing plot.
#'
#' @keywords internal
#'
circularSection <- function(from, to, units = "hours", template = "clock24", limits = c(1, 0), fill = "white", border = "black"){
  if( inherits(from,"character") ){
    hour.from <- circular::circular(decimalTime(from), units = units, template = template)
    hour.to <- circular::circular(decimalTime(to), units = units, template = template)
  } else {
    hour.from <- circular::circular(from, units = units, template = template)
    hour.to <- circular::circular(to, units = units, template = template)
  }
  if(hour.to < hour.from) hour.to <- hour.to + 24
  zero <- attr(hour.from,"circularp")$zero # extracted from the circular data
  xmin <- as.numeric(circular::conversion.circular(hour.from, units = "radians")) * -1
  xmax <- as.numeric(circular::conversion.circular(hour.to, units = "radians")) * -1
  xx <- c(limits[1] * cos(seq(xmin, xmax, length = 1000) + zero), rev(limits[2] * cos(seq(xmin, xmax, length = 1000) + zero)))
  yy <- c(limits[1] * sin(seq(xmin, xmax, length = 1000) + zero), rev(limits[2] * sin(seq(xmin, xmax, length = 1000) + zero)))
  polygon(xx, yy, col = fill, border = border)
}

#' Edited rose diagram function
#'
#' Adapted from the \code{\link[circular]{rose.diag}} function of the circular package.
#'
#' For more details about the original function, visit the circular package homepage at \url{https://github.com/cran/circular}.
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
#' @param xlim,ylim the ranges to be encompassed by the x and y axes. Useful for centring the plot
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
#'
#' @return A list with the zero, rotation and next.points values, to be parsed to an overlaying graphic.
#'
#' @keywords internal
#'
myRoseDiag <- function (x, pch = 16, cex = 1, axes = TRUE, shrink = 1, bins = 24,
  upper = TRUE, ticks = TRUE, tcl = 0.025, tcl.text = 0.125,
  radii.scale = c("sqrt", "linear"), border = NULL, col = c("lightblue", "#c0ff3e80", "#ffc0cb80", "#F0E4424D", "#0072B24D", "#D55E004D"),
  tol = 0.04, uin = NULL, xlim = c(-1, 1), ylim = c(-1, 1),
  prop = 1, digits = 2, plot.info = NULL, units = NULL, template = NULL,
  zero = NULL, rotation = NULL, main = NULL, sub = NULL, xlab = "",
  ylab = "", add = TRUE, control.circle = circular::circle.control()) {
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
    copyOfCirclePlotRad(xlim = xlim, ylim = ylim, uin = uin, shrink = shrink,
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
      copyOfRosediagRad(x, zero = zero, rotation, bins, upper,
        radii.scale, prop, border, col = col[iseries])
    }
  }
  return(invisible(list(zero = zero, rotation = rotation, next.points = 0, x = x, bins = bins,
    radii.scale = radii.scale, prop = prop, col = col)))
}


#' Draw rings at relative points
#'
#' Adapted from RosediagRad() (from the circular package) to draw rings inside the circular plot. Called if rings = TRUE in \code{\link{myRoseDiag}}
#'
#' For more details about the circular package, visit its homepage at \url{https://github.com/cran/circular}
#'
#' @inheritParams myRoseDiag
#' @param rings.lty line type of the rings, See help on par.
#' @param ring.text logical: if notes should be displayed.
#' @param ring.text.pos The position of the rings' text. Ignored if ring.text is set to FALSE.
#' @param ring.text.cex The size of the ring's text. Ignored if ring.text is set to FALSE.
#'
#' @return No return value, adds to an existing plot.
#'
#' @keywords internal
#'
ringsRel <- function(plot.params, border, rings.lty,
  ring.text, ring.text.pos, ring.text.cex){
  range <- seq(from = 0, to = 1 / plot.params$prop, length.out = 5)
  radius <- c(0.25, 0.50, 0.75, 1)
  line.values <- paste0(round(range * 100, 2), "%")
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
#' @param col The colours to be used when drawing the means.
#' @param mean.length vertical length of the mean dash.
#' @param mean.lwd width of the mean dash.
#' @param box.range One of "none", "std.error" or "sd", controls the statistic used to draw the range boxes.
#' @param fill Fill colour for the range box.
#' @param horizontal.border Border colour for the horizontal edges of the range box.
#' @param vertical.border Border colour for the vertical edges of the range box.
#' @param box.size Vertical size of the range box.
#' @param edge.length Vertical size of the edge whiskers in the range box.
#' @param edge.lwd Width of the edge whiskers in the range box.
#'
#' @return No return value, adds to an existing plot.
#'
#' @keywords internal
#'
roseMean <- function(input, col = c("cornflowerblue", "chartreuse3", "deeppink"),
  mean.length = c(0.0125, -0.0125), mean.lwd = 4, box.range = c("none", "std.error", "sd"), 
  fill = "white", horizontal.border = "black", vertical.border = "black",
  box.size = c(1.015, 0.985), edge.length = c(0.025, -0.025), edge.lwd = 2){

  box.range <- match.arg(box.range)
  
  if(is.matrix(input) | is.data.frame(input)){
    # the different series in the input must be broken into individual
    # list elements to play nicely with the code below.
    plotdata <- list()
    for(i in 1:ncol(input)) {
      plotdata[[i]] <- input[,i]
    }
    names(plotdata) <- colnames(input)
  } else {
    if (is.list(input)){
      if (any(!unlist(lapply(input, function(x) inherits(x, "circular")))))
        stop("Input is a list but not all elements in the list are circular objects.\n")
      plotdata <- input
    } else {
      # this is the case where a vector was provided
      plotdata <- list(input)
    }
  }

  if (!exists("plotdata"))
    stop("Input must be a list of circular objects, a data.frame, a matrix or a vector.\n")
  
  # expand colour and fill to match the number of data series to be plotted
  col <- rep(col, length.out = length(plotdata))
  fill <- rep(fill, length.out = length(plotdata))
  # same as above but for borders
  horizontal.border <- rep(horizontal.border, length.out = length(plotdata))
  vertical.border <- rep(vertical.border, length.out = length(plotdata))

  # now start plogging, one at a time
  for (i in 1:length(plotdata)) {
    # calculate a circular mean first (needed for the if-statement below)
    m <- circular::mean.circular(plotdata[[i]], na.rm = TRUE)
   
    # start by plotting the desired ranges, if one was requested
    if(box.range != "none"){
      if(box.range == "std.error")
        range <- std.error.circular(plotdata[[i]], silent = TRUE)
      if(box.range == "sd")
        range <- circular::sd.circular(plotdata[[i]])

      # extract the location of the zero from the circular data
      zero <- attr(plotdata[[i]], "circularp")$zero

      # calculate the placement of the range borders
      left <- as.numeric(circular::conversion.circular((m - range), units = "radians")) * -1
      right <- as.numeric(circular::conversion.circular((m + range), units = "radians")) * -1

      # convert to x and y coordinates
      xx <- c(box.size[1] * cos(seq(left, right, length = 1000) + zero), rev(box.size[2] * cos(seq(left, right, length = 1000) + zero)))
      yy <- c(box.size[1] * sin(seq(left, right, length = 1000) + zero), rev(box.size[2] * sin(seq(left, right, length = 1000) + zero)))
      
      # plot the range box
      polygon(xx, yy, col = fill[i], border = horizontal.border[i])

      # plot the range borders
      circular::lines.circular(c(m + range, m + range), edge.length, lwd = edge.lwd, col = vertical.border[i])
      circular::lines.circular(c(m - range, m - range), edge.length, lwd = edge.lwd, col = vertical.border[i])
    }

    # plot the thick mean in the middle
    circular::lines.circular(c(m, m), mean.length, lwd = mean.lwd, col = col[i], lend = 1)
  }
}

#' circular:::circlePlotRad
#'
#' This function is an EXACT copy of the function circlePlotRad() in the circular package.
#' As the function is not exported by the original package, I have copied it here to resolve
#' the note thrown by devtools::check()
#'
#' For more details about the original function, visit the circular package homepage at \url{https://github.com/cran/circular}
#'
#' @param xlim,ylim the ranges to be encompassed by the x and y axes. Useful for centring the plot.
#' @param uin desired values for the units per inch parameter. If of length 1, the desired units per inch on the x axis.
#' @param shrink parameter that controls the size of the plotted circle. Default is 1. Larger values shrink the circle, while smaller values enlarge the circle.
#' @param tol proportion of white space at the margins of plot.
#' @param main,sub,xlab,ylab title, subtitle, x label and y label of the plot.
#' @param control.circle parameters passed to plot.default in order to draw the circle. The function circle.control is used to set the parameters.
#'
#' @return No return value, adds to an existing plot.
#'
#' @keywords internal
#'
copyOfCirclePlotRad <- function (xlim = c(-1, 1), ylim = c(-1, 1), uin = NULL, shrink = 1,
    tol = 0.04, main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
    control.circle = circular::circle.control())
{
    xlim <- shrink * xlim
    ylim <- shrink * ylim
    midx <- 0.5 * (xlim[2] + xlim[1])
    xlim <- midx + (1 + tol) * 0.5 * c(-1, 1) * (xlim[2] - xlim[1])
    midy <- 0.5 * (ylim[2] + ylim[1])
    ylim <- midy + (1 + tol) * 0.5 * c(-1, 1) * (ylim[2] - ylim[1])
    oldpin <- par("pin")
    xuin <- oxuin <- oldpin[1]/diff(xlim)
    yuin <- oyuin <- oldpin[2]/diff(ylim)
    if (is.null(uin)) {
        if (yuin > xuin)
            yuin <- xuin
        else xuin <- yuin
    }
    else {
        if (length(uin) == 1)
            uin <- uin * c(1, 1)
        if (any(c(xuin, yuin) < uin))
            stop("uin is too large to fit plot in")
        xuin <- uin[1]
        yuin <- uin[2]
    }
    xlim <- midx + oxuin/xuin * c(-1, 1) * diff(xlim) * 0.5
    ylim <- midy + oyuin/yuin * c(-1, 1) * diff(ylim) * 0.5
    n <- control.circle$n
    x <- cos(seq(0, 2 * pi, length = n))
    y <- sin(seq(0, 2 * pi, length = n))
    axes <- FALSE
    log <- ""
    xaxs <- "i"
    yaxs <- "i"
    ann <- par("ann")
    frame.plot <- axes
    panel.first <- NULL
    panel.last <- NULL
    asp <- NA
    plot.default(x = x, y = y, type = control.circle$type, xlim = xlim,
        ylim = ylim, log = "", main = main, sub = sub,
        xlab = xlab, ylab = ylab, ann = ann, axes = axes, frame.plot = frame.plot,
        panel.first = panel.first, panel.last = panel.last, asp = asp,
        col = control.circle$col, bg = control.circle$bg, pch = control.circle$pch,
        cex = control.circle$cex, lty = control.circle$lty, lwd = control.circle$lwd)
}

#' circular:::RosediagRad
#'
#' This function is an EXACT copy of the function RosediagRad() in the circular package.
#' As the function is not exported by the original package, I have copied it here to resolve
#' the note thrown by devtools::check()
#'
#' For more details about the original function, visit the circular package homepage at \url{https://github.com/cran/circular}
#'
#' @inheritParams myRoseDiag
#'
#' @return No return value, adds to an existing plot.
#'
#' @keywords internal
#'
copyOfRosediagRad <- function (x, zero, rotation, bins, upper, radii.scale, prop,
    border, col, ...)
{
    n <- length(x)
    freq <- rep(0, bins)
    arc <- (2 * pi)/bins
    if (!is.logical(upper))
        stop("upper must be logical")
    if (upper == TRUE)
        x[x == 0] <- 2 * pi
    x[x >= 2 * pi] <- 2 * pi - 4 * .Machine$double.eps
    breaks <- seq(0, 2 * pi, length.out = (bins + 1))
    freq <- hist.default(x, breaks = breaks, plot = FALSE, right = upper)$counts
    rel.freq <- freq/n
    if (rotation == "clock")
        rel.freq <- rev(rel.freq)
    if (radii.scale == "sqrt") {
        radius <- sqrt(rel.freq * prop)
    }
    else {
        radius <- rel.freq * prop
    }
    sector <- seq(0, 2 * pi - (2 * pi)/bins, length = bins)
    mids <- seq(arc/2, 2 * pi - pi/bins, length = bins)
    for (i in 1:bins) {
        if (rel.freq[i] != 0) {
            xx <- c(0, radius[i] * cos(seq(sector[i], sector[i] +
                (2 * pi)/bins, length = 1000/bins) + zero), 0)
            yy <- c(0, radius[i] * sin(seq(sector[i], sector[i] +
                (2 * pi)/bins, length = 1000/bins) + zero), 0)
            polygon(xx, yy, border = border, col = col, ...)
        }
    }
}

#' Print arrival and departure times per section
#'
#' @param section.times A list of entry and exit times for each section
#' @inheritParams splitDetections
#' @param detections A valid detections list
#'
#' @return No return value, called to plot and save graphic.
#'
#' @keywords internal
#'
printSectionTimes <- function(section.times, bio, detections) {
  appendTo("debug", "Starting printSectionTimes")

  Date <- Group <- NULL
  
  cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")
  names(cbPalette) <- c("Orange", "Blue", "Green", "Yellow", "Darkblue", "Darkorange", "Pink", "Grey")
  
  time.range <- c(min(bio$Release.date), max(do.call(c, lapply(detections, function(x) x$Timestamp))))
  dayrange <- as.Date(time.range)
  dayrange[1] <- dayrange[1] - 1
  dayrange[2] <- dayrange[2] + 1

  capture.output <- lapply(names(section.times), function(i) {
    # cat(i, '\n')
    plotdata <- suppressMessages(reshape2::melt(section.times[[i]]))
    plotdata <- plotdata[complete.cases(plotdata), ]
  
    link <- match(plotdata$Transmitter, bio$Transmitter)
    plotdata$Group <- bio$Group[link]
    plotdata$Group <- droplevels(plotdata$Group)
  
    plotdata$Date <- as.Date(substring(as.character(plotdata$value), 1, 10))
  
    p <- ggplot2::ggplot(data = plotdata, ggplot2::aes(x = Date, fill = Group))
    p <- p + ggplot2::geom_bar(width = 0.9)
    p <- p + ggplot2::theme_bw()
    p <- p + ggplot2::facet_wrap(variable ~ ., ncol = 1, scales = "free")
    p <- p + ggplot2::scale_y_continuous(expand = c(0, 0, 0.05, 0))
    p <- p + ggplot2::scale_x_date(limits = dayrange)
    p <- p + ggplot2::labs(x = "", y = "n")
  
    if (length(unique(plotdata$Group)) <= 8) {
      p <- p + ggplot2::scale_fill_manual(values = as.vector(cbPalette)[1:length(unique(plotdata$Group))], drop = FALSE)
    }
  
    ggplot2::ggsave(paste0(tempdir(), "/actel_report_auxiliary_files/", i,"_days.png"), width = 10, height = length(unique(plotdata$variable)) * 2, limitsize = FALSE)
  })

  appendTo("debug", "Finished printSectionTimes")
}

#' print the distribution of tags per location
#'
#' @param global.ratios the global ratios
#' @param group.ratios the global ratios
#' @param time.ratios the daily ratios
#' @inheritParams migration
#'
#' @return No return value, called to plot and save graphic.
#'
#' @keywords internal
#'
printGlobalRatios <- function(global.ratios, group.ratios, time.ratios, spatial, rsp.info) {
  appendTo("debug", "Starting printGlobalRatios")

  Timeslot <- NULL
  Location <- NULL
  n <- NULL
  cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")
  names(cbPalette) <- c("Orange", "Blue", "Green", "Yellow", "Darkblue", "Darkorange", "Pink", "Grey")

  fake.results <- list(valid.movements = "placeholder",
                spatial = spatial,
                rsp.info = rsp.info,
                time.ratios = time.ratios,
                group.ratios = group.ratios,
                global.ratios = global.ratios)

  p <- plotRatios(input = fake.results, type = "absolutes")
  tryCatch(ggplot2::ggsave(paste0(tempdir(), "/actel_report_auxiliary_files/global_ratios_absolutes.svg"), width = 10, height = 4),
    error = function(e) {ggplot2::ggsave(paste0(tempdir(), "/actel_report_auxiliary_files/global_ratios_absolutes.png"), width = 10, height = 4)})

  p <- plotRatios(input = fake.results, type = "percentages")
  tryCatch(ggplot2::ggsave(paste0(tempdir(), "/actel_report_auxiliary_files/global_ratios_percentages.svg"), width = 10, height = 4),
    error = function(e) {ggplot2::ggsave(paste0(tempdir(), "/actel_report_auxiliary_files/global_ratios_percentages.png"), width = 10, height = 4)})

  appendTo("debug", "Finished printGlobalRatios")
}

#' print the individual locations per day
#'
#' @param ratios the daily ratios
#' @inheritParams sectionMovements
#' 
#' @return A string of file locations in rmd syntax, to be included in printRmd
#'
#' @keywords internal
#'
printIndividualResidency <- function(ratios, global.ratios, spatial, rsp.info) {
  appendTo("debug", "Starting printIndividualResidency")

  counter <- 0
  individual.plots <- NULL

  fake.results <- list(valid.movements = "placeholder",
                spatial = spatial,
                rsp.info = rsp.info,
                time.ratios = ratios,
                global.ratios = global.ratios)

  if (interactive())
    pb <- txtProgressBar(min = 0, max = length(ratios), style = 3, width = 60)

  capture <- lapply(names(ratios), function(i) {
    counter <<- counter + 1
    
    p <- plotResidency(input = fake.results, tag = i)

    ggplot2::ggsave(paste0(tempdir(), "/actel_report_auxiliary_files/", i,"_residency.png"), width = 10, height = 1.5)
    
    individual.plots <<- paste0(individual.plots, "![](", tempdir(), "/actel_report_auxiliary_files/", i, "_residency.png){ width=95% }\n")
    
    if (interactive())
      setTxtProgressBar(pb, counter)
  })

  if (interactive())
    close(pb)

  appendTo("debug", "Finished printIndividualResidency")
  return(individual.plots)
}

#' Print a simple barplot with the number of tags last seen at each section
#'
#' @param input a table with the last seen data
#' @inheritParams sectionMovements
#'
#' @return No return value, called to plot and save graphic.
#'
#' @keywords internal
#'
printLastSection <- function(input, spatial) {
  appendTo("debug", "Starting printLastSection")
  Section <- NULL
  n <- NULL

  cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")
  names(cbPalette) <- c("Orange", "Blue", "Green", "Yellow", "Darkblue", "Darkorange", "Pink", "Grey")
  
  input$Group <- rownames(input)
  plotdata <- suppressMessages(reshape2::melt(input))
  colnames(plotdata) <- c("Group", "Section", "n")
  plotdata$Section <- factor(gsub("Disap. in |Disap. at ", "", plotdata$Section), 
                             levels = c(names(spatial$array.order), "Release"))

  if (length(levels(plotdata$Section)) < 5)
    number.of.columns <- 4

  if (length(levels(plotdata$Section)) >= 5)
    number.of.columns <- 3

  if (length(levels(plotdata$Section)) >= 10)
    number.of.columns <- 2

  if (length(levels(plotdata$Section)) >= 20)
    number.of.columns <- 1

  number.of.rows <- ceiling(length(unique(plotdata$Group)) / number.of.columns)

  # 900px per row (300px per inch on my computer)
  if (number.of.rows <= 2)
    the.height <- 900 * number.of.rows
  else
    the.height <- 900 * log(number.of.rows, base = 1.5)

  # 20 px per x-axis label character
  longest.section.name <- max(nchar(as.character(plotdata$Section)))
  the.height <- the.height + 20 * longest.section.name

  p <- ggplot2::ggplot(plotdata, ggplot2::aes(x = Section, y = n))
  p <- p + ggplot2::geom_bar(stat = "identity", fill = cbPalette[[2]], colour = "transparent")
  p <- p + ggplot2::theme_bw()
  p <- p + ggplot2::labs(x = "", y = "n")
  p <- p + ggplot2::scale_y_continuous(expand = c(0, 0, 0.05, 0))
  p <- p + ggplot2::facet_wrap(. ~ Group, ncol = number.of.columns)
  p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1))
  
  ggplot2::ggsave(paste0(tempdir(), "/actel_report_auxiliary_files/last_section.png"), units = "px", width = 1800, height = ceiling(the.height), limitsize = FALSE)

  appendTo("debug", "Finished printLastSection")
}

#' Print a simple barplot with the number of tags last seen at each section
#'
#' @return No return value, called to plot and save graphic.
#'
#' @keywords internal
#'
printLastArray <- function(status.df) {
  appendTo("debug", "Starting printLastArray")

  Very.last.array <- NULL
  Group <- NULL
  cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")
  names(cbPalette) <- c("Orange", "Blue", "Green", "Yellow", "Darkblue", "Darkorange", "Pink", "Grey")

  if (length(levels(status.df$Group)) <= 8)
      the.colours <- as.vector(cbPalette)[1:length(levels(status.df$Group))]
  else
      the.colours <- gg_colour_hue(length(levels(status.df$Group)))

  p <- ggplot2::ggplot(status.df, ggplot2::aes(x = Very.last.array))
  p <- p + ggplot2::geom_bar(ggplot2::aes(fill = Group), colour = "transparent", width = 0.5, position = ggplot2::position_dodge2(preserve = "single", padding = 0))
  p <- p + ggplot2::scale_fill_manual(values = the.colours, drop = FALSE)
  p <- p + ggplot2::scale_x_discrete(drop = FALSE)
  p <- p + ggplot2::theme_bw()
  p <- p + ggplot2::labs(x = "Last seen at...", y = "n")
  p <- p + ggplot2::coord_flip()
  p <- p + ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE))
  the.height <- max(2, ((length(levels(status.df$Very.last.array)) - 1) * 0.5))
  ggplot2::ggsave(paste0(tempdir(), "/actel_report_auxiliary_files/last_arrays.png"), width = 6, height = the.height, limitsize = FALSE)

  appendTo("debug", "Finished printLastArray")
}

#' Print sensor data for each individual tag
#'
#' @param detections A valid detections list
#' @param extension the format of the generated graphics
#'
#' @keywords internal
#'
#' @return A string of file locations in rmd syntax, to be included in printRmd
#'
printSensorData <- function(detections, spatial, rsp.info, colour.by = c("auto", "stations", "arrays"), extension = "png") {
  appendTo("debug", "Starting printSensorData")

  individual.plots <- NULL
  Timestamp <- NULL
  Sensor.Value <- NULL
  Sensor.Unit <- NULL
  colour.by <- match.arg(colour.by)

  appendTo(c("Screen", "Report"), "M: Printing sensor values for tags with sensor data.")

  fake.results <- list(valid.detections = detections,
                spatial = spatial,
                valid.movements = "valid.movements",
                rsp.info = rsp.info)

  if (interactive())
    pb <- txtProgressBar(min = 0, max = length(detections), style = 3, width = 60) # nocov

  counter <- 0
  n.plots <- 0
  individual.plots <- ""

  if (colour.by == "auto") {
    if (nrow(spatial$stations) > 40 | length(unique(spatial$stations$Array)) > 14)
      colour.by <- "arrays"
    else
      colour.by <- "stations"
  }

  capture <- lapply(names(detections), function(tag) {
    counter <<- counter + 1
    if (any(!is.na(detections[[tag]]$Sensor.Value))) {
      p <- plotSensors(input = fake.results, tag = tag, verbose = getOption("actel.debug", default = FALSE),
        colour.by = ifelse(colour.by == "stations", "array", "section"))

      if (length(unique(detections[[tag]]$Sensor.Unit)) > 2)
        the.height <- 4 + ((length(unique(detections[[tag]]$Sensor.Unit)) - 2) * 2)
      else
        the.height <- 4

      # default width:
      the.width <- 5
      # Adjustments depending on number of legend valudes
      if (colour.by == "stations")
        to.check <- levels(detections[[tag]]$Array)
      else
        to.check <- names(spatial$array.order)

      if (length(to.check) > 14 & length(to.check) <= 29) {
        if (counter %% 2 == 0) {
          p <- p + ggplot2::guides(colour = ggplot2::guide_legend(ncol = 2))
          the.width <- 5.8
        } else {
          p <- p + ggplot2::theme(legend.position = "none")
          the.width <- 4.2
        }
      }
      if (length(to.check) > 29) {
        if (counter %% 2 == 0) {
          p <- p + ggplot2::guides(colour = ggplot2::guide_legend(ncol = 3))
          the.width <- 7.5
        } else {
          p <- p + ggplot2::theme(legend.position = "none")
          the.width <- 2.5
        }
      }

      # Save
      ggplot2::ggsave(paste0(tempdir(), "/actel_report_auxiliary_files/", tag, "_sensors.", extension), width = the.width, height = the.height, limitsize = FALSE)  # better to save in png to avoid point overlapping issues

      n.plots <- n.plots + 1
      if (n.plots %% 2 == 0) {
        individual.plots <<- paste0(individual.plots, "![](", tempdir(), "/actel_report_auxiliary_files/", tag, "_sensors.", extension, "){ width=", the.width * 10, "% }\n")
      } else {
        individual.plots <<- paste0(individual.plots, "![](", tempdir(), "/actel_report_auxiliary_files/", tag, "_sensors.", extension, "){ width=", the.width * 10, "% }")
      }
    }
    if (interactive())
      setTxtProgressBar(pb, counter)
  })
  if (interactive())
    close(pb)

  appendTo("debug", "Finished printSensorData")

  return(individual.plots)
}
