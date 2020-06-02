#' Plot detections for a single tag
#' 
#' The output of plotMoves is a ggplot object, which means you can then use it in combination
#' with other ggplot functions, or even together with other packages such as patchwork.
#' 
#' @param input The results of an actel analysis (either explore, migration or residency).
#' @param tag The transmitter to be plotted.
#' @param title An optional title for the plot. If left emptry, a default title will be added.
#' @param xlab,ylab Optional axis names for the plot. If left emptry, default axis names will be added.
#' @param col An optional colour scheme for the detections. If left empty, default colours will be added.
#' @param array.alias A named vector of format c("old_array_name" = "new_array_name") to replace 
#'  default array names with user defined ones.
#' @param frame.warning Logical. By default, actel highlights manually changed or overriden tags in yellow
#'  and red plot frames, respectively. Set to FALSE to deactivate this behaviour.
#' 
#' @return A ggplot object.
#' 
#' @examples
#' # Using the example results that come with actel
#' plotMoves(example.results, 'R64K-4451')
#' 
#' # Because plotMoves returns a ggplot object, you can store
#' # it and edit it manually, e.g.:
#' library(ggplot2)
#' p <- plotMoves(example.results, 'R64K-4451')
#' p <- p + xlab("changed the x axis label a posteriori")
#' p
#' 
#' # You can also save the plot using ggsave!
#' 
#' @export
#' 
plotMoves <- function(input, tag, title, xlab, ylab, col, array.alias, frame.warning = TRUE) {
  # NOTE: The NULL variables below are actually column names used by ggplot.
  # This definition is just to prevent the package check from issuing a note due unknown variables.
  Timestamp <- NULL
  Standard.name <- NULL
  Array <- NULL
  Station <- NULL

  cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")
  names(cbPalette) <- c("Orange", "Blue", "Green", "Yellow", "Darkblue", "Darkorange", "Pink", "Grey")

  if (!inherits(input, "list"))
    stop("Could not recognise the input as an actel results object.", call. = FALSE)

  if (is.null(input$valid.movements) | is.null(input$spatial) | is.null(input$rsp.info))
    stop("Could not recognise the input as an actel results object.", call. = FALSE)

  if (is.na(match(tag, names(input$detections))))
    stop("Could not find tag ", tag, " in the input.", call. = FALSE)

  # start preparing inputs
  tz <- input$rsp.info$tz

  # relevant detection data
  detections <- input$detections[[tag]]
  movements <- input$movements[[tag]]
  valid.movements <- input$valid.movements[[tag]]

  # vertical start dash
  start.line <- as.POSIXct(input$rsp.info$bio$Release.date[which(input$rsp.info$bio$Transmitter == tag)], tz = tz)

  # grey dashes (for migration only)
  if (input$rsp.info$analysis.type == "migration") {
    like.migration <- TRUE
    status.df <- input$status.df
    relevant.line <- status.df[which(status.df$Transmitter == tag), (grepl("First.arrived", colnames(status.df)) | grepl("Last.left", colnames(status.df)))]
  } else {
    like.migration <- FALSE
  }

  # Y axis order
  spatial <- input$spatial
  link <- match(spatial$stations$Array, c(unlist(spatial$array.order), "Unknown"))
  names(link) <- 1:length(link)
  link <- sort(link)
  link <- as.numeric(names(link))
  y.order <- spatial$stations$Standard.name[link]
  detections$Standard.name <- factor(detections$Standard.name, levels = y.order)

  # array levels
  if (any(levels(detections$Array) == "Unknown"))
    levels(detections$Array)[levels(detections$Array) == "Unknown"] = "Invalid"
  else
    levels(detections$Array) <- c(levels(detections$Array), "Invalid")

  if (!missing(array.alias)) {
    link <- match(names(array.alias), levels(detections$Array))
    if (any(is.na(link)))
      warning("Could not find ", ifelse(sum(is.na(link) == 1), "array ", "arrays "), names(array.alias)[is.na(link)], " in the study's arrays.", call. = FALSE, immediate. = TRUE)
    levels(detections$Array)[link[!is.na(link)]] <- array.alias[!is.na(link)]
  }

  if (any(!detections$Valid))
    detections$Array[!detections$Valid] <- "Invalid"
  
  # detection colours
  if (missing(col)) {
    if (length(levels(detections$Array)) <= 8) {
      col <- as.vector(cbPalette)[c(1:(length(levels(detections$Array)) - 1), 8)]
    } else {
      col <- c(gg_colour_hue(length(levels(detections$Array)) - 1), "#999999")
    }
  } else {
    if (length(col) != (length(levels(detections$Array)) - 1)) {
      warning("Not enough colours supplied in 'col' (", length(col)," supplied and ", (length(levels(detections$Array)) - 1), " needed). Reusing colours.", immediate. = TRUE, call. = FALSE)
      col <- rep(col, length.out = (length(levels(detections$Array)) - 1))
    }
    col <- c(col, "#999999")
  }

  # movements lines
  all.moves.line <- data.frame(
    Station = as.vector(t(movements[, c("First.station", "Last.station")])),
    Timestamp = as.vector(t(movements[, c("First.time", "Last.time")]))
  )

  all.moves.line$Station <- factor(all.moves.line$Station, levels = levels(detections$Standard.name))
  all.moves.line$Timestamp <- as.POSIXct(all.moves.line$Timestamp, tz = tz)
  
  add.valid.movements <- FALSE
  if (!is.null(valid.movements)) {
    add.valid.movements <- TRUE
    simple.moves.line <- data.frame(
      Station = as.vector(t(valid.movements[, c("First.station", "Last.station")])),
      Timestamp = as.vector(t(valid.movements[, c("First.time", "Last.time")]))
      )
    simple.moves.line$Station <- factor(simple.moves.line$Station, levels = levels(detections$Standard.name))
    simple.moves.line$Timestamp <- as.POSIXct(simple.moves.line$Timestamp, tz = tz)
  }
  
  # plot ranges
  first.time <- min(c(as.POSIXct(head(detections$Timestamp, 1), tz = tz), start.line))
  attributes(first.time)$tzone <- tz
  last.time <- as.POSIXct(tail(detections$Timestamp, 1), tz = tz)

  # plot title
  if (missing(title)) {
    if (like.migration)
      title <- paste0(tag, " (", status.df[status.df$Transmitter == tag, "Status"], ")")
    else
      title <- paste0(tag, " (", nrow(detections), " detections)")
  }

  # Start plot
  p <- ggplot2::ggplot(detections, ggplot2::aes(x = Timestamp, y = Standard.name, colour = Array))

  # Choose background
  default.cols <- TRUE
  if (frame.warning & attributes(movements)$p.type == "Overridden") { # nocov start
    p <- p + ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "white"),
      panel.border = ggplot2::element_rect(fill = NA, colour = "#ef3b32" , size = 2),
      panel.grid.major = ggplot2::element_line(size = 0.5, linetype = 'solid', colour = "#ffd8d6"), 
      panel.grid.minor = ggplot2::element_line(size = 0.25, linetype = 'solid', colour = "#ffd8d6"),
      legend.key = ggplot2::element_rect(fill = "white", colour = "white"),
      )
    default.cols <- FALSE
  } # nocov end
  if (frame.warning & attributes(movements)$p.type == "Manual") {
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
  if (like.migration) {
    for (l in 1:length(relevant.line)) {
      if (!is.na(relevant.line[l])) {
        p <- p + ggplot2::geom_vline(xintercept = as.POSIXct(relevant.line[[l]], tz = tz), linetype = "dashed", color = "grey")
      }
    }
    rm(l, relevant.line)
  }
  # Plot movements
  p <- p + ggplot2::geom_path(data = all.moves.line, ggplot2::aes(x = Timestamp, y = Station, group = 1), col = "grey40", linetype = "dashed")
  if (add.valid.movements) {
    p <- p + ggplot2::geom_path(data = simple.moves.line, ggplot2::aes(x = Timestamp, y = Station, group = 1), col = "grey40")
  }
  # Trim graphic
  p <- p + ggplot2::xlim(first.time, last.time)
  # Paint
  p <- p + ggplot2::scale_color_manual(values = col, drop = FALSE)
  # Plot points
  p <- p + ggplot2::geom_point()
  # Fixate Y axis
  p <- p + ggplot2::scale_y_discrete(drop = FALSE)
  # Caption and title
  p <- p + ggplot2::guides(colour = ggplot2::guide_legend(reverse = TRUE))
  p <- p + ggplot2::labs(title = title, x = ifelse(missing(xlab), paste("tz:", tz), xlab), y = ifelse(missing(ylab), "Station Standard Name", ylab))

  return(p)
}

#' Print circular graphics for time series.
#' 
#' Wraps functions adapted from the circular R package.
#' 
#' For more details about the original functions, visit the circular package homepage at \url{https://github.com/cran/circular}
#' 
#' @param times A list of of time vectors (each vector will be plotted as a series).
#' @param night A vector of two times defining the start and stop of the night period (in HH:MM format).
#' @param col A vector of colour names to paint each time series (colours will be added transparency).
#' @param alpha A value between 0 and 1 for the opacity of each layer (defaults to 0.8).
#' @param title A title for the plot.
#' @param mean.dash Logical: Should the mean value be displayed on the plot's edge?
#' @param mean.range Logical: Should the SEM be displayed? (only relevant if mean.dash = TRUE)
#' @param rings Logical: Should inner plot rings be displayed?
#' @param file A file name to save the plot as an SVG. Leave NULL to plot on active graphics device.
#' 
#' @examples
#' # The output of timesToCircular can be used as an input to plotTimes.
#' x <- getTimes(example.results, location = "River1", n.events = "first", event.type = "arrival")
#' times <- timesToCircular(x)
#' 
#' # plot times
#' plotTimes(times)
#' 
#' # A night period can be added with 'night'
#' plotTimes(times, night = c("20:00", "06:00"))
#' 
#' @return A circular plot
#' 
#' @export
#' 
plotTimes <- function(times, night = NULL, col = NULL, alpha = 0.8, title = "", mean.dash = TRUE, 
  mean.range = TRUE, rings = TRUE, file = NULL){
  
  if (!inherits(times, "list"))
    stop("'times' must be a list.", call. = FALSE)

  if (!is.null(night) && length(night) != 2)
    stop("'night' must have two values.", call. = FALSE)

  if (!is.null(night) && is.character(night) && any(!grepl("[0-2][0-9]:[0-5][0-9]", night)))
    stop("'night' values must be either numeric (between 0 and 24) or in a HH:MM format.", call. = FALSE)

  if (!is.null(night) && is.numeric(night) && (any(night > 24) | any(night < 0)))
    stop("'night' values must be either numeric (between 0 and 24) or in a HH:MM format.", call. = FALSE)

  if (length(title) > 1)
    stop("Please provide only one 'title'.", call. = FALSE)

  if (!is.logical(mean.dash))
    stop("'mean.dash' must be either TRUE or FALSE.", call. = FALSE)

  if (!is.logical(mean.range))
    stop("'mean.range' must be either TRUE or FALSE.", call. = FALSE)

  if (!is.logical(rings))
    stop("'rings' must be either TRUE or FALSE.", call. = FALSE)

  if (!is.null(file) && length(file) > 1)
    stop("Please provide only one 'file' name.", call. = FALSE)

  if (length(alpha) > 1)
    stop("Please provide only one 'alpha' value.", call. = FALSE)

  if (!is.numeric(alpha) || (alpha < 0 | alpha > 1))
    stop("'alpha' must be numeric (between 0 and 1).", call. = FALSE)

  if (is.null(col)) {
    if (length(times) > 8)
      stop("To plot this many time series simultaneously, colours must be specified using 'col'.", call. = FALSE)
    cbPalette <- c("#56B4E9", "#c0ff3e", "#E69F00", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")
    colours <- scales::alpha(colour = cbPalette[c(1:length(times))], alpha = alpha)
  } else {
    if (length(col) != length(times))
      stop("'col' must be of the same length as 'times'.", call. = FALSE)
      colours <- scales::alpha(colour = col, alpha = alpha)
  }

  if (length(times) > 2)
    ylegend <- -0.97 + (0.1 * (length(times) - 2))
  else
    ylegend <- -0.97
  
  prop <- roundDown(1 / max(unlist(lapply(times, function(x) table(roundUp(x, to = 1)) / sum(!is.na(x))))), to = 1)
  
  if (!is.null(file)) {
    if (!grepl(".svg$", file))
      file <- paste0(file, ".svg")
    grDevices::svg(file, height = 5, width = 5, bg = "transparent")
  }
  
  oldpar <- par(mar = c(1, 2, 2, 1))
  on.exit(par(oldpar), add = TRUE)
  copyOfCirclePlotRad(main = title, shrink = 1.05)
  
  if (!is.null(night)) {
    circularSection(from = night[1], 
      to = night[2], units = "hours", template = "clock24", 
      limits = c(1, 0), fill = scales::alpha("grey", 0.3), border = "transparent")
  }

  params <- myRoseDiag(times, bins = 24, radii.scale = "linear",
    prop = prop, tcl.text = -0.1, tol = 0.05, col = colours, border = "black")
  
  if (mean.dash) {
    roseMean(times, col = params$col, mean.length = c(0.07, -0.07), mean.lwd = 6,
      box.range = ifelse(mean.range, "std.error", "none"), fill = "white", border = "black",
      box.size = c(1.015, 0.985), edge.length = c(0.025, -0.025),
      edge.lwd = 2)
  }

  if (rings) {
    ringsRel(plot.params = params, border = "black", ring.text = TRUE, 
      ring.text.pos = 0.07, rings.lty = "f5", ring.text.cex = 0.8)
  }

  legend(x = -1.2, y = ylegend,
    legend = paste(names(times), " (", unlist(lapply(times, function(x) sum(!is.na(x)))), ")", sep =""),
    fill = params$col, bty = "n", x.intersp = 0.3, cex = 0.8)

  if(!is.null(file)) {
    grDevices::dev.off()
    message("M: Plot saved to ", file)
  }
}
