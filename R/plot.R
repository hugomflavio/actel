#' Plot sensor data for a single tag
#'
#' The output of plotSensors is a ggplot object, which means you can then use it in combination
#' with other ggplot functions, or even together with other packages such as patchwork.
#'
#' @param input The results of an actel analysis (either explore, migration or residency).
#' @param tag The transmitter to be plotted.
#' @param sensor The sensors to be plotted. If left empty, all available sensors are plotted
#' @param title An optional title for the plot. If left empty, a default title will be added.
#' @param xlab,ylab Optional axis names for the plot. If left empty, default axis names will be added.
#' @param pcol The colour for the points. If unset, a default palette is used.
#' @param psize The size of the points. Defaults to 1.
#' @param lcol The colour for the line. Defaults to grey.
#' @param lsize The width of the line. Defaults to 0.5 (same as standard ggplots)
#' @param colour.by One of "arrays" or "sections", defines how the points should be coloured.
#' @param array.alias A named vector of format c("old_array_name" = "new_array_name") to replace
#'  default array names with user defined ones. Only relevant if colour.by = "arrays".
#' @param verbose Logical: Should warning messages be printed, if necessary?
#'
#' @return A ggplot object.
#'
#' @examples
#' # Using the example results that come with actel
#' plotSensors(example.results, 'R64K-4451')
#'
#' # Because plotSensors returns a ggplot object, you can store
#' # it and edit it manually, e.g.:
#' library(ggplot2)
#' p <- plotSensors(example.results, 'R64K-4451')
#' p <- p + xlab("changed the x axis label a posteriori")
#' p
#'
#' # You can also save the plot using ggsave!
#'
#' @export
#'
plotSensors <- function(input, tag, sensor, title = tag, xlab, ylab, pcol, psize = 1, lsize = 0.5, 
  colour.by = c("array", "section"), array.alias, lcol = "grey40", verbose = TRUE) {
  Timestamp <- NULL
  Sensor.Value <- NULL
  Sensor.Unit <- NULL
  Colour <- NULL
  colour.by <- match.arg(colour.by)

  cbPalette <- c("#56B4E9", "#E69F00", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")
  names(cbPalette) <- c("Blue", "Orange", "Green", "Yellow", "Darkblue", "Darkorange", "Pink", "Grey")

  if (!inherits(input, "list"))
    stop("Could not recognise the input as an actel results object.", call. = FALSE)

  if (is.null(input$valid.movements) | is.null(input$spatial) | is.null(input$rsp.info))
    stop("Could not recognise the input as an actel results object.", call. = FALSE)

  if (length(tag) > 1)
    stop("Please list only one tag", call. = FALSE)

  if (is.na(match(tag, names(input$valid.detections))))
    stop("Could not find tag '", tag, "' in the input.", call. = FALSE)

  detections <- input$valid.detections[[tag]]
  spatial <- input$spatial

  if (all(is.na(detections$Sensor.Value)))
    stop("No sensor data found for this tag.", call. = FALSE)

  if (any(is.na(detections$Sensor.Value))) {
    if (verbose)
      appendTo(c("Screen", "Warning"), paste0(sum(is.na(detections$Sensor.Value)), " rows in this tag's detections do not contain sensor values and will be discarded."))
    detections <- detections[!is.na(detections$Sensor.Value), ]
  }

  if (any(link <- is.na(detections$Sensor.Unit) | detections$Sensor.Unit == "")) {
    detections$Sensor.Unit[link] <- paste0("? (", detections$Signal[link], ")")
    if (verbose)
      appendTo(c("Screen", "Warning"), "Not all rows with sensor data contain a sensor unit! Plotting unknown data separately.")
  }

  if (missing(sensor)) {
    sensor <- unique(detections$Sensor.Unit)
  } else {
    if (any(link <- is.na(match(sensor, unique(detections$Sensor.Unit)))))
      stop("Could not find sensor unit(s) '", paste(sensor[link], collapse = "', '"), "' in the tag detections.", call. = FALSE)
  }

  # renaming arrays if relevant
  if (!missing(array.alias)) {
    if (colour.by == "section") {
      link <- match(names(array.alias), levels(detections$Array))
      if (any(is.na(link)))
        warning("Could not find ", ifelse(sum(is.na(link) == 1), "array ", "arrays "), names(array.alias)[is.na(link)], " in the study's arrays.", call. = FALSE, immediate. = TRUE)
      levels(detections$Array)[link[!is.na(link)]] <- array.alias[!is.na(link)]
    } else {
      warning("array.alias can only be used when colour.by = 'array'. Ignoring array.alias.", immediate. = TRUE, call. = FALSE)
    }
  }

  # detection colour column
  if (colour.by == "array") {
    detections$Colour <- detections$Array
  } else {
    aux <- lapply(seq_along(spatial$array.order), function(i) {
      x <- match(detections$Array, spatial$array.order[[i]])
      x[!is.na(x)] <- names(spatial$array.order)[i]
      return(x)
    })
    aux <- combine(aux)
    detections$Colour <- factor(aux, levels = names(spatial$array.order))
  }

  # choose colours
  if (missing(pcol)) {
    if (length(levels(detections$Colour)) <= 7) {
      pcol <- as.vector(cbPalette)[c(1:length(levels(detections$Colour)))]
    } else {
      pcol <- c(gg_colour_hue(length(levels(detections$Colour))))
    }
  } else {
    if (length(pcol) != length(levels(detections$Colour)))
      warning("Not enough colours supplied in 'pcol' (", length(col)," supplied and ", length(levels(detections$Colour)), " needed). Reusing colours.", immediate. = TRUE, call. = FALSE)
    pcol <- rep(pcol, length.out = length(levels(detections$Colour)))
    }

  detections <- detections[detections$Sensor.Unit %in% sensor, ]

  p <- ggplot2::ggplot(data = detections, ggplot2::aes(x = Timestamp, y = Sensor.Value, by = Sensor.Unit))
  p <- p + ggplot2::geom_line(col = lcol, size = lsize)
  p <- p + ggplot2::geom_point(size = psize, ggplot2::aes(colour = Colour))
  p <- p + ggplot2::scale_color_manual(values = pcol, drop = FALSE, name = ifelse(colour.by == "array", "Array", "Section"))
  p <- p + ggplot2::labs(title = title, x = ifelse(missing(xlab), paste("tz:", input$rsp.info$tz), xlab), y = ifelse(missing(ylab), "Sensor value", ylab))
  p <- p + ggplot2::theme_bw()
  p <- p + ggplot2::facet_grid(Sensor.Unit ~ ., scales = "free_y")
  return(p)
}

#' Plot simultaneous/cumulative presences at a give array or set of arrays
#' 
#' @param input The results of an actel analysis (either explore, migration or residency).
#' @param arrays One or more arrays to be analysed. If multiple arrays are provided, data will be grouped.
#' @param title An optional title for the plot.
#' @param xlab,ylab Optional axis names for the plot. If left empty, default axis names will be added.
#' @param lwd The line width, only relevant for line plots.
#' @param col The colour of the line or bars. Defaults to blue.
#' @param type The type of plot to be drawn. By default, a line is plotted if cumulative = TRUE, and bars are plotted otherwise.
#' @param timestep The time resolution for the grouping of the results. Defaults to "days", but can be set to "hours" and "mins" (at the expense of computing time).
#' @param cumulative Logical. If TRUE, a cumulative plot of arrivals is drawn, otherwise the number of tags simultaneously present at the array(s) is drawn.
#'
#' @return A ggplot object.
#'
#' @examples
#' # Using the example results that come with actel
#' plotArray(example.results, arrays = "A9")
#'
#' # Because plotArray returns a ggplot object, you can store
#' # it and edit it manually, e.g.:
#' library(ggplot2)
#' p <- plotArray(example.results, arrays = "A9")
#' p <- p + xlab("changed the x axis label a posteriori")
#' p
#'
#' # You can also save the plot using ggsave!
#' 
#' @export
#' 
plotArray <- function(input, arrays, title, xlab, ylab, lwd = 1, col = "#56B4E9", 
  type = c("default", "bars", "lines"), timestep = c("days", "hours", "mins"), cumulative = FALSE) {
  x <- NULL
  y <- NULL

  timestep <- match.arg(timestep)
  type <- match.arg(type)

  if (type == "default") {
    if (cumulative)
      type <- "lines"
    else
      type <- "bars"
  }

  if (!inherits(input, "list"))
    stop("Could not recognise the input as an actel results object.", call. = FALSE)

  if (is.null(input$valid.movements) | is.null(input$spatial) | is.null(input$rsp.info))
    stop("Could not recognise the input as an actel results object.", call. = FALSE)

  if (any(link <- is.na(match(arrays, names(input$arrays)))))
    stop("Could not find array(s) '", paste(arrays[link], collapse = "', '"), "' in the study area.", call. = FALSE)

  array.string <- paste0("^", arrays, "$", collapse = "|")
  # choose title
  if (missing(title)) {
    if (cumulative)
      title <- paste(paste(arrays, collapse = "|"), "- Cumulative arrivals")
    else
      title <- paste(paste(arrays, collapse = "|"), "- Simultaneous presence")
  }

  # extract information
  first.time <- as.POSIXct(NA)[-1]
  last.time <- as.POSIXct(NA)[-1]
  
  plot.list <- lapply(names(input$valid.movements), function(tag) {
    # cat(tag, "\n")
    moves <- as.data.frame(input$valid.movements[[tag]])
    output <- moves[grepl(array.string, moves$Array), ]

    if (nrow(output) > 0) {
      first.time <<- c(first.time, output$First.time[1])
      last.time <<- c(last.time, output$Last.time[nrow(output)])
      output$First.time <- round.POSIXt(output$First.time, units = timestep)
      output$Last.time <- round.POSIXt(output$Last.time, units = timestep)
      return(output)
    } else {
      return(NULL)
    }
  })

  attributes(first.time)$tzone <- input$rsp.info$tz
  attributes(last.time)$tzone <- input$rsp.info$tz

  if (is.null(first.time) | is.null(last.time))
    stop("Not enough valid data to draw a plot. Aborting.", call. = FALSE)

  plot.list <- plot.list[!sapply(plot.list, is.null)]

  if (cumulative)
    last.time <- round.POSIXt(max(first.time), units = timestep)
  else
    last.time <- round.POSIXt(max(last.time), units = timestep)

  first.time <- round.POSIXt(min(first.time), units = timestep)
  
  if (timestep == "days")
    seconds <- 3600 * 24

  if (timestep == "hours")
    seconds <- 3600

  if (timestep == "mins")
    seconds <- 60

  timerange <- seq(from = first.time, to = last.time, by = seconds)  

  plotdata <- data.frame(x = timerange, y = rep(0, length(timerange)))

  capture <- lapply(plot.list, function(x) {
    to.add <- rep(0, nrow(plotdata))
    if (cumulative) {
      arrived.here <- which(x$First.time[1] == timerange)
      to.add[arrived.here:length(to.add)] <- 1
    } else {
      arrived.here <- sapply(x$First.time, function(i) which(i == timerange))
      left.here <- sapply(x$Last.time, function(i) which(i == timerange))
      for(i in 1:length(arrived.here))
        to.add[arrived.here[i]:left.here[i]] <- 1
    }
    plotdata$y <<- plotdata$y + to.add
  })

  if (type == "lines") {
    aux1 <- plotdata
    aux1$x <- aux1$x - (seconds / 2)
    aux1$index <- seq(from = 1, to = nrow(aux1) * 2 - 1, by = 2)

    aux2 <- plotdata
    aux2$x <- aux2$x + (seconds / 2)
    aux2$index <- seq(from = 2, to = nrow(aux2) * 2, by = 2)

    plotdata <- rbind(aux1, aux2)
    plotdata <- plotdata[order(plotdata$index), ]
  }

  p <- ggplot2::ggplot(data = plotdata, ggplot2::aes(x = x, y = y))
  if (type == "bars") {
    p <- p + ggplot2::geom_bar(stat = "identity", fill = col)
    p <- p + ggplot2::scale_y_continuous(expand = c(0, 0, 0.05, 0))
  }
  if (type == "lines")
    p <- p + ggplot2::geom_path(size = lwd, col = col)
  p <- p + ggplot2::theme_bw()
  p <- p + ggplot2::labs(title = title, x = ifelse(missing(xlab), paste("tz:", input$rsp.info$tz), xlab), y = ifelse(missing(ylab), "n", ylab))
  return(p)
}

#' Plot moves for one ore more tags
#' 
#' The output of plotMoves is a ggplot object, which means you can then use it in combination
#' with other ggplot functions, or even together with other packages such as patchwork.
#'
#' @param input The results of an actel analysis (either explore, migration or residency).
#' @param tags The transmitters to be plotted (optional).
#' @param title An optional title for the plot.
#' @param xlab,ylab Optional axis names for the plot. If left empty, default axis names will be added.
#' @param col An optional colour scheme for the detections. If left empty, default colours will be added.
#' @param array.alias A named vector of format c("old_array_name" = "new_array_name") to replace
#'  default array names with user defined ones.
#' @param show.release Logical: Should the line from release to first detection be displayed?
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
plotMoves <- function(input, tags, title, xlab, ylab, col, array.alias, show.release = TRUE) {
  movements <- NULL
  tag <- NULL
  event <- NULL

  cbPalette <- c("#56B4E9", "#E69F00", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")
  names(cbPalette) <- c("Blue", "Orange", "Green", "Yellow", "Darkblue", "Darkorange", "Pink", "Grey")

  if (!inherits(input, "list"))
    stop("Could not recognise the input as an actel results object.", call. = FALSE)

  if (is.null(input$valid.movements) | is.null(input$spatial) | is.null(input$rsp.info))
    stop("Could not recognise the input as an actel results object.", call. = FALSE)

  if (!missing(array.alias)) {
    link <- match(names(array.alias), names(input$arrays))
    if (any(is.na(link)))
      warning("Could not find ", ifelse(sum(is.na(link) == 1), "array '", "arrays '"), paste0(names(array.alias)[is.na(link)], collapse = "', '"), "' in the study's arrays.", call. = FALSE, immediate. = TRUE)
  } else {
    array.alias <- NULL
  }

  if (missing(tags)) {
    message("M: Argument 'tags' not set. Plotting all detected tags.")
    tags <- names(input$valid.movements)
  } else {
    if (any(link <- is.na(match(tags, names(input$valid.movements)))))
      stop("There are no valid movements for tag(s) '", paste(tags[link], collapse = "', '"), "'.\n", call. = FALSE)
  }

  if (length(tags) > 5)
    warning("Plotting many tags at the same time will likely result in ugly output.", call. = FALSE, immediate. = TRUE)

  # choose colours
  if (missing(col)) {
    if (length(tags) <= 7) {
      col <- as.vector(cbPalette)[1:length(tags)]
    } else {
      col <- gg_colour_hue(length(tags))
    }
  } else {
    if (length(col) < length(tags)) {
      warning("Not enough colours supplied in 'col' (", length(col)," supplied and ", length(tags), " needed). Reusing colours.", immediate. = TRUE, call. = FALSE)
      col <- rep(col, length.out = length(tags))
    }
  }

  # choose title
  if (missing(title)) {
    if (length(tags) == 1)
      title <- tags
    else
      title <- NULL
  }

  # prepare Y axis levels
  grouping <- names(input$arrays)
  if (show.release && is.na(match("Release", grouping)))
    grouping <- c("Release", grouping)

  # extract information
  plot.list <- lapply(tags, function(tag) {
    # cat(tag, "\n")
    moves <- input$valid.movements[[tag]]
    release.date <- input$rsp.info$bio$Release.date[input$rsp.info$bio$Signal == extractSignals(tag)]

    if (show.release) {
      x1 <- data.frame(array = factor(c("Release", moves$Array), levels = grouping),
                       date = c(release.date, moves$First.time),
                       index = seq(from = 1, to = (nrow(moves) * 2) + 1, by = 2),
                       tag = rep(tag, nrow(moves) + 1),
                       event = 0:nrow(moves))
    } else {
      x1 <- data.frame(array = factor(moves$Array, levels = grouping),
                       date = moves$First.time,
                       index = seq(from = 1, to = (nrow(moves) * 2) - 1, by = 2),
                       tag = rep(tag, nrow(moves)),
                       event = 1:nrow(moves))
    }

    new.i <- seq(from = 2, to = nrow(x1) * 2, by = 2)
    if (show.release)
      new.i <- new.i[-length(new.i)]

    x2 <- data.frame(array = factor(moves$Array, levels = grouping),
                     date = moves$Last.time,
                     index = new.i,
                     tag = rep(tag, nrow(moves)),
                     event = 1:nrow(moves))

    output <- rbind(x1, x2)
    output <- output[order(output$index), ]

    if (!is.null(array.alias)) {
      link <- match(names(array.alias), levels(output$array))
      levels(output$array)[link[!is.na(link)]] <- array.alias[!is.na(link)]
    }

    return(output)
  })

  plotdata <- data.table::rbindlist(plot.list)

  aux <- lapply(plot.list, function(x) x[nrow(x), ])
  final.points <- data.table::rbindlist(aux)

  p <- ggplot2::ggplot(data = plotdata, ggplot2::aes(x = date, y = array, colour = tag))
  p <- p + ggplot2::geom_line(ggplot2::aes(group = tag))
  p <- p + ggplot2::geom_line(ggplot2::aes(group = interaction(tag, event)), size = 2)
  p <- p + ggplot2::geom_point(data = final.points)
  p <- p + ggplot2::theme_bw()
  p <- p + ggplot2::scale_y_discrete(drop = FALSE)   # Show all Y axis values
  p <- p + ggplot2::scale_color_manual(name = "", values = col, drop = FALSE)
  if (length(tags) == 1)
    p <- p + ggplot2::theme(legend.position = "none")
  p <- p + ggplot2::labs(title = title, x = ifelse(missing(xlab), paste("tz:", input$rsp.info$tz), xlab), y = ifelse(missing(ylab), "Array", ylab))
  return(p)
}


#' Plot detections for a single tag
#'
#' The output of plotDetections is a ggplot object, which means you can then use it in combination
#' with other ggplot functions, or even together with other packages such as patchwork.
#'
#' @param input The results of an actel analysis (either explore, migration or residency).
#' @param tag The transmitter to be plotted.
#' @param type The type of y axis desired. One of "stations" (default) or "arrays".
#' @param title An optional title for the plot. If left empty, a default title will be added.
#' @param xlab,ylab Optional axis names for the plot. If left empty, default axis names will be added.
#' @param col An optional colour scheme for the detections. If left empty, default colours will be added.
#' @param array.alias A named vector of format c("old_array_name" = "new_array_name") to replace
#'  default array names with user defined ones.
#' @param frame.warning Logical. By default, actel highlights manually changed or overridden tags in yellow
#'  and red plot frames, respectively. Set to FALSE to deactivate this behaviour.
#'
#' @return A ggplot object.
#'
#' @examples
#' # Using the example results that come with actel
#' plotDetections(example.results, 'R64K-4451')
#'
#' # Because plotDetections returns a ggplot object, you can store
#' # it and edit it manually, e.g.:
#' library(ggplot2)
#' p <- plotDetections(example.results, 'R64K-4451')
#' p <- p + xlab("changed the x axis label a posteriori")
#' p
#'
#' # You can also save the plot using ggsave!
#'
#' @export
#'
plotDetections <- function(input, tag, type = c("stations", "arrays"), title, xlab, ylab, col, array.alias, frame.warning = TRUE) {
  # NOTE: The NULL variables below are actually column names used by ggplot.
  # This definition is just to prevent the package check from issuing a note due unknown variables.
  Timestamp <- NULL
  Station <- NULL
  plot.y <- NULL
  Colour <- NULL

  type <- match.arg(type)

  cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")
  names(cbPalette) <- c("Orange", "Blue", "Green", "Yellow", "Darkblue", "Darkorange", "Pink", "Grey")

  if (!inherits(input, "list"))
    stop("Could not recognise the input as an actel results object.", call. = FALSE)

  if (is.null(input$valid.movements) | is.null(input$spatial) | is.null(input$rsp.info))
    stop("Could not recognise the input as an actel results object.", call. = FALSE)

  if (length(tag) > 1)
    stop("Please list only one tag", call. = FALSE)
 
 if (is.na(match(tag, names(input$detections))))
    stop("Could not find tag '", tag, "' in the input.", call. = FALSE)

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
  if (type == "stations") {
    link <- match(spatial$stations$Array, c(unlist(spatial$array.order), "Unknown"))
    names(link) <- 1:length(link)
    link <- sort(link)
    link <- as.numeric(names(link))
    y.order <- spatial$stations$Standard.name[link]
  } else {
    if (any(detections$Array == "Unknown"))
      y.order <- c(unlist(spatial$array.order), "Unknown")
    else
      y.order <- unlist(spatial$array.order)
  }

  # y values
  if (type == "stations")
    detections$plot.y <- factor(detections$Standard.name, levels = y.order)    
  else
    detections$plot.y <- factor(detections$Array, levels = y.order)

  # Expand array levels (for type == stations only)
  if (type == "stations") {
    if (any(levels(detections$Array) == "Unknown"))
      levels(detections$Array)[levels(detections$Array) == "Unknown"] = "Invalid"
    else
      levels(detections$Array) <- c(levels(detections$Array), "Invalid")
  }

  # renaming arrays if relevant
  if (!missing(array.alias)) {
    if (type == "stations") {
      link <- match(names(array.alias), levels(detections$Array))
      if (any(is.na(link)))
        warning("Could not find ", ifelse(sum(is.na(link) == 1), "array ", "arrays "), names(array.alias)[is.na(link)], " in the study's arrays.", call. = FALSE, immediate. = TRUE)
      levels(detections$Array)[link[!is.na(link)]] <- array.alias[!is.na(link)]
    } else {
      warning("array.alias can only be used when type = 'stations'. Ignoring array.alias.", immediate. = TRUE, call. = FALSE)
    }
  }

  # detection colour column
  if (type == "stations") {
    detections$Colour <- detections$Array
  } else {
    aux <- lapply(seq_along(spatial$array.order), function(i) {
      x <- match(detections$plot.y, spatial$array.order[[i]])
      x[!is.na(x)] <- names(spatial$array.order)[i]
      return(x)
    })
    aux <- combine(aux)
    aux[is.na(aux)] <- "Invalid"
    detections$Colour <- factor(aux, levels = c(names(spatial$array.order), "Invalid"))
  }

  # Rename colour for invalid data
  if (any(!detections$Valid))
    detections$Colour[!detections$Valid] <- "Invalid"

  # choose colours
  if (missing(col)) {
    if (length(levels(detections$Colour)) <= 8) {
      col <- as.vector(cbPalette)[c(1:(length(levels(detections$Colour)) - 1), 8)]
    } else {
      col <- c(gg_colour_hue(length(levels(detections$Colour)) - 1), "#999999")
    }
  } else {
    if (length(col) != (length(levels(detections$Colour)) - 1)) {
      warning("Not enough colours supplied in 'col' (", length(col)," supplied and ", (length(levels(detections$Colour)) - 1), " needed). Reusing colours.", immediate. = TRUE, call. = FALSE)
      col <- rep(col, length.out = (length(levels(detections$Colour)) - 1))
    }
    col <- c(col, "#999999")
  }

  # movements lines
  if (!is.null(movements)) {
    add.movements <- TRUE
    if (type == "stations") {
      all.moves.line <- data.frame(
        plot.y = as.vector(t(movements[, c("First.station", "Last.station")])),
        Timestamp = as.vector(t(movements[, c("First.time", "Last.time")])))
    } else {
      all.moves.line <- data.frame(
        plot.y = rep(movements$Array, each = 2),
        Timestamp = as.vector(t(movements[, c("First.time", "Last.time")])))
    }
    all.moves.line$plot.y <- factor(all.moves.line$plot.y, levels = levels(detections$plot.y))
    all.moves.line$Timestamp <- as.POSIXct(all.moves.line$Timestamp, tz = tz)
  } else {
    add.movements <- FALSE
  }

  add.valid.movements <- FALSE
  if (!is.null(valid.movements)) {
    add.valid.movements <- TRUE
    if (type == "stations") {
      simple.moves.line <- data.frame(
        plot.y = as.vector(t(valid.movements[, c("First.station", "Last.station")])),
        Timestamp = as.vector(t(valid.movements[, c("First.time", "Last.time")])))
    } else {
      simple.moves.line <- data.frame(
        plot.y = rep(valid.movements$Array, each = 2),
        Timestamp = as.vector(t(valid.movements[, c("First.time", "Last.time")])))
    }
    simple.moves.line$plot.y <- factor(simple.moves.line$plot.y, levels = levels(detections$plot.y))
    simple.moves.line$Timestamp <- as.POSIXct(simple.moves.line$Timestamp, tz = tz)
  }

  # plot ranges
  first.time <- min(c(as.POSIXct(head(detections$Timestamp, 1), tz = tz), start.line))
  attributes(first.time)$tzone <- tz
  last.time <- as.POSIXct(tail(detections$Timestamp, 1), tz = tz)

  # plot title
  if (missing(title)) {
    if (like.migration)
      title <- paste0(tag, " (", status.df[status.df$Transmitter == tag & !is.na(status.df$Transmitter), "Status"], ")")
    else
      title <- paste0(tag, " (", nrow(detections), " detections)")
  }

  # Start plot
  p <- ggplot2::ggplot(detections, ggplot2::aes(x = Timestamp, y = plot.y, colour = Colour))

  # Choose background
  default.cols <- TRUE
  if ((frame.warning & add.movements) && attributes(movements)$p.type == "Overridden") {
    p <- p + ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "white"),
      panel.border = ggplot2::element_rect(fill = NA, colour = "#ef3b32" , size = 2),
      panel.grid.major = ggplot2::element_line(size = 0.5, linetype = 'solid', colour = "#ffd8d6"),
      panel.grid.minor = ggplot2::element_line(size = 0.25, linetype = 'solid', colour = "#ffd8d6"),
      legend.key = ggplot2::element_rect(fill = "white", colour = "white"),
      )
    default.cols <- FALSE
  }
  if ((frame.warning & add.movements) && attributes(movements)$p.type == "Manual") {
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
  if (add.movements)
    p <- p + ggplot2::geom_path(data = all.moves.line, ggplot2::aes(x = Timestamp, group = 1), col = "grey40", linetype = "dashed")
  if (add.valid.movements)
    p <- p + ggplot2::geom_path(data = simple.moves.line, ggplot2::aes(x = Timestamp, group = 1), col = "grey40")
  # Trim graphic
  p <- p + ggplot2::xlim(first.time, last.time)
  # Paint
  p <- p + ggplot2::scale_color_manual(values = col, drop = FALSE, name = ifelse(type == "stations", "Array", "Section"))
  # Plot points
  p <- p + ggplot2::geom_point()
  # Show all Y axis values
  p <- p + ggplot2::scale_y_discrete(drop = FALSE)
  # Caption and title
  p <- p + ggplot2::guides(colour = ggplot2::guide_legend(reverse = TRUE))
  if (missing(ylab)) {
    if (type == "stations")
      ylab <- "Station Standard Name"
    else
      ylab <- "Array"
  }
  p <- p + ggplot2::labs(title = title, x = ifelse(missing(xlab), paste("tz:", tz), xlab), y = ylab)

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
#' @param mean.range.darken.factor A numeric factor to darken the mean range edges for each group. Values greater
#'  than 1 darken the colour, and values lower than 1 lighten the colour.
#' @param rings Logical: Should inner plot rings be displayed?
#' @param file A file name to save the plot to. Leave NULL to plot on active graphics device. Available file extensions: .svg, .pdf, .png and .tiff.
#' @param height,width The height and width of the output file. Use inches for .pdf and .svg files or pixels for .png and .tiff files.
#' @param bg The colour of the plot background. Defaults to "transparent".
#' @param ncol The number of columns in which to set the legend items. By default, actel decides the number
#'  of columns based on the number of data series to be plotted.
#' @param legend.pos Where should the legend be drawn? By default, actel decides whether to plot the legend
#'  in the corner of the plot at the bottom the plot depending on the number of data series to plot.
#'  Possible values: 'auto', 'corner', 'bottom'.
#' @param ylegend Adjustment to the vertical positioning of the legend. Only relevant if the legend is being drawn
#'  in the corner of the plot.
#' @param xlegend Adjustment to the horizontal positioning of the legend.
#' @param xjust How the legend is to be justified when the legend is drawn at the bottom a the plot. 
#'  One of 'auto' (i.e. let actel decide the best adjustment), 'left', 'centre', or 'right'.
#' @param cex A numerical vector giving the amount by which plotting characters and symbols should be scaled 
#'  relative to the default. When saving the plot in a vectorial form, it is recommended to change the height
#'  and width arguments rather than the cex.
#' @param expand Parameter that controls the size of the plotted circle. Defaults to 0.95. Larger values expand the circle, while smaller values shrink the circle.
#'
#' @examples
#' # The output of timesToCircular can be used as an input to plotTimes.
#' x <- getTimes(example.results, location = "A1", n.events = "first", event.type = "arrival")
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
plotTimes <- function(times, night = NULL, col, alpha = 0.8, title = "", mean.dash = TRUE,
  mean.range = TRUE, mean.range.darken.factor = 1.4, rings = TRUE, file, width, height, bg = "transparent", ncol, 
  legend.pos = c("auto", "corner", "bottom"), ylegend, xlegend, xjust = c("auto", "centre", "left", "right"), 
  expand = 0.95, cex = 1){

  legend.pos <- match.arg(legend.pos)
  xjust <- match.arg(xjust)

  if (!inherits(times, "list"))
    stop("'times' must be a list.", call. = FALSE)

  if (any(link <- !sapply(times, function(i) "circular" %in% class(i))))
    stop(ifelse(sum(link) > 1, "Element(s) ", "Element "),
         paste(which(link), collapse = " "), " in 'times' ",
         ifelse(sum(link) > 1, "are not ", "is not a "), "circular ",
         ifelse(sum(link) > 1, "objects.", "object."), call. = FALSE)

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

  if (!missing(file) && length(file) > 1)
    stop("Please provide only one 'file' name.", call. = FALSE)

  if (length(alpha) > 1)
    stop("Please provide only one 'alpha' value.", call. = FALSE)

  if (!is.numeric(alpha) || (alpha < 0 | alpha > 1))
    stop("'alpha' must be numeric (between 0 and 1).", call. = FALSE)

  if (missing(col)) {
    if (length(times) > 8) {
      aux <- gg_colour_hue(length(times))
    } else {
      aux <- c("#56B4E9", "#c0ff3e", "#E69F00", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")[1:length(times)]
    }
    colours <- scales::alpha(colour = aux, alpha = alpha)
  } else {
    if (length(col) < length(times))
      stop("'col' must be of the same length as 'times' (", length(col), " != ", length(times), ").", call. = FALSE)
    colours <- scales::alpha(colour = col, alpha = alpha)
  }

  if (!missing(file)) {
    if (grepl(".svg$", file) | grepl(".pdf$", file)) {
      if (cex != 1)
        message("M: When saving vectorial plots, it is recommended to refine the 'width' and 'height', rather than the 'cex'.")
      if (missing(height))
        height <- 5
      if (missing(width))
        width <- 5
    }

    if (grepl(".png$", file) | grepl(".tiff$", file)) {
      if (missing(height))
        height <- 500
      if (missing(width))
        width <- 500
    }

    unk.ext <- TRUE
    if (unk.ext && grepl(".svg$", file)) {
      grDevices::svg(file, height = height, width = width, bg = bg)
      unk.ext <- FALSE
    }
    if (unk.ext && grepl(".pdf$", file)) {
      grDevices::pdf(file, height = height, width = width, bg = bg)
      unk.ext <- FALSE
    }
    if (unk.ext && grepl(".png$", file)) {
      grDevices::png(file, height = height, width = width,  bg = bg)
      unk.ext <- FALSE
    }
    if (unk.ext && grepl(".tiff$", file)) {
      grDevices::tiff(file, height = height, width = width, bg = bg)
      unk.ext <- FALSE
    }
    if (unk.ext) {
      stop("Could not recognise 'file' extension (recognised extensions: .svg, .pdf, .png, .tiff).", call. = FALSE)
    }
    on.exit(grDevices::dev.off())
  }

  if (legend.pos == "auto") {
    if (length(times) >= 6)
      legend.pos <- "bottom"
    else
      legend.pos <- "corner"
  }

  if (missing(ncol)) {
    ncol <- 1
    if (legend.pos == "bottom") {
      if (length(times) > 2)
        ncol <- 2
      if (length(times) > 6)
        ncol <- 3
      if (length(times) > 9)
        ncol <- 4
    }
  }

  if (legend.pos == "corner") {
    if (missing(ylegend)) {
      if (ncol > 1)
        warning("Plotting the legend in the corner but ncol > 1. This will likely lead to bad results.", immediate. = TRUE, call. = FALSE)
      if (roundUp(length(times)/ ncol, 1) > 2) {
        if (!missing(file) && (grepl(".png$", file) | grepl(".tiff", file)))
          ylegend <- -0.97 + (0.06 * (roundUp(length(times) / ncol, 1) - 2))
        if (missing(file) || (grepl(".svg$", file) | grepl(".pdf", file)))
          ylegend <- -0.97 + (0.08 * (roundUp(length(times) / ncol, 1) - 2))
      } else {
        ylegend <- -0.97      
      }
    }
    if (xjust != "auto")
      warning("'xjust' was set but legend is being plotted in the corner. Ignoring 'xjust'.", immediate. = TRUE, call. = FALSE)  
    xjust <- "left"
  } else {
    if (missing(ylegend))
      ylegend <- -1.15
    if (xjust == "auto") {
      if (ncol > 1)
        xjust <- "centre"
      else
        xjust <- "left"
    }
  }

  if (xjust == "left") {
    xjust <- 0
    if (missing(xlegend))
      xlegend <- -1.3
  }

  if (xjust == "centre") {
    xjust <- 0.5
    if (missing(xlegend))
      xlegend <- 0
  }
  
  if (xjust == "right") {
    xjust <- 1
    if (missing(xlegend))
      xlegend <- 1.3
  }

  prop <- roundDown(1 / max(unlist(lapply(times, function(x) table(roundUp(x, to = 1)) / sum(!is.na(x))))), to = 1)

  if (!missing(file))
    area.prop <- width/height
  else
    area.prop <- 1

  if (legend.pos == "corner")
    b <- 1
  else
    b <- (roundUp(length(times) / ncol, 1))

  vertical.mar <- b + 2
  horizontal.mar <- vertical.mar * area.prop
  oldpar <- par(mar = c(b, horizontal.mar / 2, 2, horizontal.mar / 2), cex = cex, xpd = TRUE) # bottom, left, top, right

  # resetting the par is only necessary if no file was specified, 
  # and thus dev.off() was not called at the end of the function.
  on.exit(if (missing(file)) par(oldpar), add = TRUE)

  copyOfCirclePlotRad(main = title, shrink = 1 - (expand - 1), xlab = "", ylab = "")

  if (!is.null(night)) {
    circularSection(from = night[1],
      to = night[2], units = "hours", template = "clock24",
      limits = c(1, 0), fill = scales::alpha("grey", 0.3), border = "transparent")
  }

  params <- myRoseDiag(times, bins = 24, radii.scale = "linear",
    prop = prop, tcl.text = -0.1, tol = 0.05, col = colours, border = "black")

  if (mean.dash) {
    roseMean(times, col = scales::alpha(params$col, 1), mean.length = c(0.07, -0.07), mean.lwd = 6,
      box.range = ifelse(mean.range, "std.error", "none"), fill = "white", horizontal.border = "black", 
      vertical.border = scales::alpha(sapply(params$col, function(i) darken(i, mean.range.darken.factor)), 1), box.size = c(1.015, 0.985), 
      edge.length = c(0.025, -0.025), edge.lwd = 2)
  }

  if (rings) {
    ringsRel(plot.params = params, border = "black", ring.text = TRUE,
      ring.text.pos = 0.07, rings.lty = "f5", ring.text.cex = 0.8)
  }

  legend(x = xlegend, y = ylegend, xjust = xjust,
    legend = paste(names(times), " (", unlist(lapply(times, function(x) sum(!is.na(x)))), ")", sep =""),
    fill = params$col, bty = "n", x.intersp = 0.3, cex = 0.8, ncol = ncol)

  if(!missing(file))
    message("M: Plot saved to ", file)
}

#' Calculate beta estimations for efficiency
#'
#' advEfficiency estimates efficiency ranges by fitting a beta distribution
#' with parameters \eqn{\alpha} = number of detected tags and \eqn{\beta} = number of missed
#' tags. The desired quantiles (argument `q`) are then calculated from distribution.
#' Plots are also drawn showing the distribution, the median point (dashed red line) and
#' the range between the lowest and largest quantile requested (red shaded section).
#'
#' Examples for inclusion in a paper:
#'
#' \enumerate{
#' \item If advEfficiency was run on an \code{overall.CJS} object (i.e. migration analysis):
#'
#'   "Array efficiency was estimated by fitting a beta distribution
#'   (\eqn{\alpha} = number of tags detected subsequently and at the array,
#'   \eqn{\beta} = number of tags detected subsequently but not at the array)
#'   and calculating the median estimated efficiency value using the R package actel \[citation\]."
#'
#' \item If advEfficiency was run on an \code{efficiency} object (i.e. residency analysis):
#'
#' - If you are using maximum efficiency estimates:
#'
#'     "Array efficiency was estimated by fitting a beta distribution
#'     (\eqn{\alpha} = number of events recorded by the array,
#'     \eqn{\beta} = number of events known to have been missed by the array).
#'     and calculating the median estimated efficiency value using the R package actel \[citation\]."
#'
#' - If you are using minimum efficiency estimates:
#'
#'     "Array efficiency was estimated by fitting a beta distribution
#'     (\eqn{\alpha} = number of events recorded by the array,
#'     \eqn{\beta} = number of events both known to have been missed and potentially missed by the array).
#'     and calculating the median estimated efficiency value using the R package actel \[citation\]."
#'
#' \item If advEfficiency was run on an \code{intra.array.CJS} object:
#'
#'   "Intra-array efficiency was estimated by comparing the tags detected at each of the
#'   two replicates. For each replicate, a beta distribution was fitted
#'   (\eqn{\alpha} = number of tags detected at both replicates, \eqn{\beta} = number
#'   of tags detected at the opposite replicate but not at the one for which efficiency
#'   is being calculated) and the median estimated efficiency value was calculated. The overall
#'   efficiency of the array was then estimated as 1-((1-R1)*(1-R2)), where R1 and R2 are
#'   the median efficiency estimates for each replicate. These calculations were performed
#'   using the R package actel \[citation\]."
#' }
#' Replace \[citation\] with the output of `citation('actel')`
#'
#' @param x An efficiency object from actel (\code{overall.CJS}, \code{intra.array.CJS[[...]]} or \code{efficiency} objects)
#' @param labels a vector of strings to substitute default plot labels
#' @param q The quantile values to be calculated. Defaults to \code{c(0.025, 0.5, 0.975)} (i.e. median and 95% CI)
#' @param force.grid A vector of format c(nrow, ncol) that allows the user to define the number of rows and columns to distribute the plots in.
#' @param paired Logical: For efficiency derived from residency analyses, should min. and max. estimates for an array be displayed next to each other?
#' @param title A title for the plot (feeds into title parameter of ggplot's labs function).
#'
#' @examples
#' # Example using the output of simpleCJS.
#' x <- matrix(
#' c(TRUE,  TRUE,  TRUE,  TRUE,  TRUE,
#'   TRUE, FALSE,  TRUE,  TRUE, FALSE,
#'   TRUE,  TRUE, FALSE, FALSE, FALSE,
#'   TRUE,  TRUE, FALSE,  TRUE,  TRUE,
#'   TRUE,  TRUE,  TRUE, FALSE, FALSE),
#' ncol = 5, byrow = TRUE)
#' colnames(x) <- c("Release", "A1", "A2", "A3", "A4")
#' cjs.results <- simpleCJS(x)
#'
#' # These cjs results can be used in advEfficiency
#' advEfficiency(cjs.results)
#'
#' # Example using the output of dualArrayCJS.
#' x <- matrix(
#' c( TRUE,  TRUE,
#'    TRUE, FALSE,
#'    TRUE,  TRUE,
#'   FALSE,  TRUE,
#'   FALSE,  TRUE),
#' ncol = 2, byrow = TRUE)
#' colnames(x) <- c("A1.1", "A1.2")
#' cjs.results <- dualArrayCJS(x)
#'
#' # These cjs results can be used in advEfficiency
#' advEfficiency(cjs.results)
#'
#' # advEfficiency can also be run with the output from the main analyses.
#' # the example.results dataset is the output of a migration analysis
#' advEfficiency(example.results$overall.CJS)
#'
#' @return A data frame with the required quantile values and a plot of the efficiency distributions.
#'
#' @export
#'
advEfficiency <- function(x, labels = NULL, q = c(0.025, 0.5, 0.975), force.grid = NULL, paired = TRUE, title = "") {

  if (!inherits(x, "list") & !inherits(x, "data.frame") & !inherits(x, "matrix"))
    stop("Could not recognise the input as an efficiency object from actel.\n", call. = FALSE)

  if (inherits(x, "list") & is.null(x$absolutes))
    stop("Could not recognise the input as an efficiency object from actel.\n", call. = FALSE)


  if (inherits(x, "list"))
    input <- x$absolutes
  else
    input <- x

  # Preliminary preparation of input
  if (ncol(input) == 1) {
    input <- data.frame(Replica.1 = c(input[3], input[2] - input[3]), Replica.2 = c(input[3], input[1] - input[3]))
    colnames(input)[1:2] <- names(x$single.efficiency)
    calc.combined <- TRUE
  } else {
    calc.combined <- FALSE
    if (nrow(input) > 4) {
      input <- input[2:3, apply(input[2:3, ], 2, function(i) all(!is.na(i)))]
    } else {
      aux <- input[, apply(input[2:3, ], 2, function(i) all(!is.na(i)))]
      auxA <- aux[1:2, ]
      colnames(auxA) <- paste0(colnames(aux), ".max")
      auxB <- aux[1:2, ]
      auxB[2, ] <- apply(aux[2:3, ], 2, sum)
      colnames(auxB) <- paste0(colnames(aux), ".min")
      if (paired)
        input <- cbind(auxA, auxB)[, c(t(replicate(2, 1:ncol(aux)))) + c(ncol(aux), 0)]
      else
        input <- cbind(auxA, auxB)
    }
  }

  if (!is.null(labels)) {
    if (ncol(input) != length(labels))
      stop("Wrong number of panel names")
    colnames(input) <- labels
  }

  beta.data <- lapply(1:ncol(input), function(i){
    alpha <- input[1, i]
    beta <- input[2, i]

    x <- seq(from = 0, to = 1, by = 0.005)
    y <- dbeta(x, alpha, beta)

    qb <- qbeta(q, alpha, beta)
    qm <- qbeta(0.5, alpha, beta)
    qci <- range(qb)

    pdist <- data.frame(x = x, ymin = rep(0, length(y)), ymax = y)
    pdist <- pdist[pdist$ymax > 0.0001, ]

    return(list(pdist = pdist, qb = qb, qm = qm, qci = qci))
  })
  names(beta.data) <- colnames(input)

  ranges <- as.data.frame(
    data.table::rbindlist(
      lapply(names(beta.data), function(i) {
        as.data.frame(t(beta.data[[i]]$qb))
        })
      )
    )
  colnames(ranges) <- paste0(round(q * 100, 1), "%")
  rownames(ranges) <- names(beta.data)

  if (calc.combined) {
    message("M: For each quantile, 'Combined' estimates are calculated as 1-((1-R1)*(1-R2)).")
    ranges <- rbind(ranges, Combined = apply(ranges, 2, function(i) (1 - prod(1 - i))))
  }

  link <- sapply(names(beta.data), function(i) {
      length(unique(beta.data[[i]]$qb)) != 1
    })


  if (all(!link)) {
    message("M: All arrays were estimated to have either 0% or 100% efficiency, skipping plotting for all arrays.")
    return(ranges)
  }

  if (any(!link))
    message("M: Some arrays were estimated to have either 0% or 100% efficiency, skipping plotting for those arrays.")

  # prepare density curve
  aux <- lapply(names(beta.data)[link], function(i) {
    return(beta.data[[i]]$pdist)
  })
  names(aux) <- names(beta.data)[link]

  pdist <- data.table::rbindlist(aux, idcol = "label")
  pdist$label <- factor(pdist$label, levels = colnames(input))

  # prepare CI intervals and median
  aux <- lapply(names(beta.data)[link], function(i) {
    return(data.frame(
      min = beta.data[[i]]$qci[1],
      qm = beta.data[[i]]$qm,
      max = beta.data[[i]]$qci[2]))
  })
  names(aux) <- names(beta.data)[link]

  qci <- data.table::rbindlist(aux, idcol = "label")
  qci$label <- factor(qci$label, levels = colnames(input))

  # calculate best grid
  if (is.null(force.grid))
    grid.dim <- nearsq(sum(link))
  else
    grid.dim <- force.grid

  x <- ymin <- ymax <- qm <- title <- label <- NULL
  p <- ggplot2::ggplot()
  p <- p + ggplot2::geom_rect(data = qci, ggplot2::aes(xmin = min, xmax = max, ymin = 0, ymax = Inf), fill = "red", alpha = .1)
  p <- p + ggplot2::geom_ribbon(data = pdist, ggplot2::aes(x = x, ymin = ymin, ymax = ymax))
  p <- p + ggplot2::geom_vline(data = qci, ggplot2::aes(xintercept = min), col = "red")
  p <- p + ggplot2::geom_vline(data = qci, ggplot2::aes(xintercept = max), col = "red")
  p <- p + ggplot2::geom_vline(data = qci, ggplot2::aes(xintercept = qm), col = "red", linetype = "dashed")
  p <- p + ggplot2::geom_point(data = qci, ggplot2::aes(x = qm, y = 0), size = 15, shape = "|", col = "red")
  p <- p + ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05)))
  p <- p + ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1))
  p <- p + ggplot2::labs(title = title, y = "Density", x = "Efficiency")
  p <- p + ggplot2::theme_bw()
  p <- p + ggplot2::facet_wrap(~ label, nrow = grid.dim[1], ncol = grid.dim[2], scales = "free", labeller = ggplot2::label_parsed)
  print(p)

  return(ranges)
}


#' Plot residency for a single tag
#'
#' The output of plotResidency is a ggplot object, which means you can then use it in combination
#' with other ggplot functions, or even together with other packages such as patchwork.
#'
#' @param input The results of an actel analysis (either explore, migration or residency).
#' @param tag The transmitter to be plotted.
#' @param title An optional title for the plot. If left empty, a default title will be added.
#' @param xlab,ylab Optional axis names for the plot. If left empty, default axis names will be added.
#' @param col An optional colour scheme for the detections. If left empty, default colours will be added.
#'
#' @return A ggplot object.
#'
#' @examples
#' # For this example, I have modified the example.results that come with actel,
#' # so they resemble a residency output
#' \dontshow{
#' example.residency.results <- c(example.results, additional.residency.results)
#' example.residency.results$rsp.info$analysis.type <- "residency"
#' }
#' plotResidency(example.residency.results, 'R64K-4451')
#'
#' # Because plotResidency returns a ggplot object, you can store
#' # it and edit it manually, e.g.:
#' library(ggplot2)
#' p <- plotResidency(example.residency.results, 'R64K-4451')
#' p <- p + xlab("changed the x axis label a posteriori")
#' p
#'
#' # You can also save the plot using ggsave!
#'
#' @export
#'
plotResidency <- function(input, tag, title, xlab, ylab, col) {
  # NOTE: The NULL variables below are actually column names used by ggplot.
  # This definition is just to prevent the package check from issuing a note due unknown variables.
  Timeslot <- NULL
  Location <- NULL
  n <- NULL

  cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")
  names(cbPalette) <- c("Orange", "Blue", "Green", "Yellow", "Darkblue", "Darkorange", "Pink", "Grey")

  if (!inherits(input, "list"))
    stop("Could not recognise the input as an actel results object.", call. = FALSE)

  if (is.null(input$valid.movements) | is.null(input$spatial) | is.null(input$rsp.info))
    stop("Could not recognise the input as an actel results object.", call. = FALSE)

  if (input$rsp.info$analysis.type != "residency")
    stop("plotResidency can only be used with residency results.", call. = FALSE)

  if (length(tag) > 1)
    stop("Please list only one tag", call. = FALSE)
 
  if (is.na(match(tag, names(input$time.ratios))))
    stop("Could not find tag '", tag, "' in the input.", call. = FALSE)

  # start preparing inputs
  tz <- input$rsp.info$tz
  timestep <- attributes(input$time.ratios)$timestep
  ratios <- input$time.ratios[[tag]]
  sections <- names(input$spatial$array.order)

  time.range <- range(input$global.ratios[[1]]$Timeslot)
  if (timestep == "days") {
    time.range[1] <- time.range[1] - 86400
    time.range[2] <- time.range[2] + 86400
  } else {
    time.range[1] <- time.range[1] - 3600
    time.range[2] <- time.range[2] + 3600
  }

  unordered.unique.values <- sort(unique(unlist(lapply(input$time.ratios, function(x) {
    aux <- which(grepl("^p", colnames(x)))
    aux <- aux[!is.na(match(colnames(x)[aux - 1], sub("p", "", colnames(x)[aux])))]
    return(colnames(x)[aux - 1])
  }))))
  link <- unlist(sapply(sections, function(i) which(grepl(paste0("^", i), unordered.unique.values))))
  unique.values <- unordered.unique.values[link]

  if (missing(col)) {
    if (length(unique.values) <= 8)
      unique.colours <- as.vector(cbPalette)[1:length(unique.values)]
    else
      unique.colours <- gg_colour_hue(length(unique.values))
  } else {
    if (length(col) < length(unique.values))
      warning("Not enough colours supplied in 'col' (", length(col)," supplied and ", length(unique.values), " needed). Reusing colours.", immediate. = TRUE, call. = FALSE)
    unique.colours <- rep(col, length.out = length(unique.values))
  }

  x <- ratios
  aux <- which(grepl("^p", colnames(ratios)))
  columns.to.use <- aux[!is.na(match(colnames(x)[aux - 1], sub("p", "", colnames(x)[aux])))]
  new.colnames <- colnames(x)[c(1, columns.to.use - 1)]
  x <- x[, c(1, columns.to.use)]
  colnames(x) <- new.colnames
  plotdata <- suppressMessages(reshape2::melt(x, id.vars = "Timeslot"))
  colnames(plotdata) <- c("Timeslot", "Location", "n")

  level.link <- !is.na(match(unique.values, unique(plotdata$Location)))
  use.levels <- unique.values[level.link]
  use.colours <- unique.colours[level.link]

  plotdata$Location <- factor(plotdata$Location, levels = use.levels)

  if (missing(title))
    title <- paste0(tag, " (", as.character(x$Timeslot[1]), " to ", as.character(x$Timeslot[nrow(x)]), ")")

  if (missing (xlab))
    xlab <- ""

  if (missing(ylab))
    ylab <- ifelse(timestep == "days", "% time per day", "% time per hour")

  p <- ggplot2::ggplot(data = plotdata, ggplot2::aes(x = Timeslot, y = n, fill = Location))
  p <- p + ggplot2::geom_bar(width = ifelse(timestep == "days", 86400, 3600), stat = "identity")
  p <- p + ggplot2::theme_bw()
  p <- p + ggplot2::scale_y_continuous(limits = c(0,  max(apply(ratios[, columns.to.use, drop = FALSE], 1, sum))), expand = c(0, 0))
  p <- p + ggplot2::labs(title = title, x = xlab, y = ylab)
  p <- p + ggplot2::scale_x_datetime(limits = time.range)
  p <- p + ggplot2::scale_fill_manual(values = use.colours, drop = TRUE, name = "Location")

  if (length(use.levels) > 5 & length(use.levels) <= 10)
    p <- p + ggplot2::guides(fill = ggplot2::guide_legend(ncol = 2))
  if (length(use.levels) > 10)
    p <- p + ggplot2::guides(fill = ggplot2::guide_legend(ncol = 3))

  return(p)
}

#' Plot global/group residency
#'
#' By default, this function plots the global residency. However, you can use the argument 'group'
#' to plot the results only from a specific animal group. Lastly, you can also use 'sections', rather
#' than 'group', to compare the residency at a specific section (or group of sections) between the 
#' different groups.
#' 
#' The output of plotRatios is a ggplot object, which means you can then use it in combination
#' with other ggplot functions, or even together with other packages such as patchwork.
#'
#' @param input The results of an actel analysis (either explore, migration or residency).
#' @param group An optional argument to plot only the data corresponding to one group.
#' @param sections An optional argument to plot the residency of the multiple groups for a specific subset of sections.
#' @param type The type of residency to be displayed. One of 'absolutes' (the default) or 'percentages'.
#' @param title An optional title for the plot. If left empty, a default title will be added.
#' @param xlab,ylab Optional axis names for the plot. If left empty, default axis names will be added.
#' @param col An optional colour scheme for the detections. If left empty, default colours will be added.
#'
#' @return A ggplot object.
#'
#' @examples
#' # For this example, I have modified the example.results that come with actel,
#' # so they resemble a residency output
#' \dontshow{
#' example.residency.results <- c(example.results, additional.residency.results)
#' example.residency.results$rsp.info$analysis.type <- "residency"
#' }
#' plotRatios(example.residency.results)
#'
#' # Because plotRatios returns a ggplot object, you can store
#' # it and edit it manually, e.g.:
#' library(ggplot2)
#' p <- plotRatios(example.residency.results, group = "A")
#' p <- p + xlab("changed the x axis label a posteriori")
#' p
#'
#' # You can also save the plot using ggsave!
#'
#' @export
#'
plotRatios <- function(input, group, sections, type = c("absolutes", "percentages"), title, xlab, ylab, col) {
  # NOTE: The NULL variables below are actually column names used by ggplot.
  # This definition is just to prevent the package check from issuing a note due unknown variables.
  type <- match.arg(type)
  Timeslot <- NULL
  Location <- NULL
  Group <- NULL
  n <- NULL

  cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")
  names(cbPalette) <- c("Orange", "Blue", "Green", "Yellow", "Darkblue", "Darkorange", "Pink", "Grey")

  if (!inherits(input, "list"))
    stop("Could not recognise the input as an actel results object.", call. = FALSE)

  if (is.null(input$valid.movements) | is.null(input$spatial) | is.null(input$rsp.info))
    stop("Could not recognise the input as an actel results object.", call. = FALSE)

  if (input$rsp.info$analysis.type != "residency")
    stop("plotRatios can only be used with residency results.", call. = FALSE)

  if (!missing(group) & !missing(sections))
    stop("Please use only one of 'group' or 'sections' at a time.", call. = FALSE)

  if (!missing(group) && length(group) > 1)
    stop("Please select only one group.", call. = FALSE)

  if (!missing(group) && is.na(match(group, names(input$group.ratios))))
    stop("Could not find group '", group, "' in the input.", call. = FALSE)

  if (!missing(sections)) {
    if (link <- any(is.na(match(sections, colnames(input$global.ratios$absolutes)))))
      stop("Section", ifelse(sum(link) > 1, "s '", " '"), paste0(sections[link], collapse = "', '"),
        ifelse(sum(link) > 1, "' do ", "' does "), "not exist, or no tags have ever been assigned to it.", call. = FALSE)
  }

  if (missing(group)) {
    if (missing(sections)) {
      the.ratios <- input$global.ratios[[type]]
    } else {
      if (type == "absolutes") {
        aux <- lapply(names(input$group.ratios), function(i) {
          x <- input$group.ratios[[i]]$absolutes
          x <- x[, c(1, match(sections, colnames(x)))]
          if (ncol(x) > 2)
            x$n <- apply(x[, -1], 1, sum)
          else
            colnames(x)[2] <- "n"
          x$Group <- i
          return(x[, c("Timeslot", "Group", "n")])
        })
        the.ratios <- as.data.frame(data.table::rbindlist(aux))
      } else {
        # because there is no easy cbindlist function, initiate the dataframe with the timeslots only
        # and then append the relevant information per group.
        the.ratios <- input$group.ratios[[1]]$absolutes[, "Timeslot", drop = FALSE]
        capture <- lapply(names(input$group.ratios), function(i) {
          if (any(!is.na(match(sections, colnames(input$group.ratios[[i]]$absolutes))))) {
            x <- input$group.ratios[[i]]$absolutes
            link <- match(sections, colnames(x))
            link <- link[!is.na(link)]
            x <- x[, c(1, link)]
            if (ncol(x) > 2)
              x[, i] <- apply(x[, -1], 1, sum)
            else
              colnames(x)[2] <- i
            the.ratios <<- cbind(the.ratios, x[, i, drop = FALSE])
          } # else skip this group
        })
        if (ncol(the.ratios) > 2) {
          # Turn into percentages (like it is done in globalRatios)
          the.ratios$Total <- apply(the.ratios[, -1], 1, sum)
          the.ratios[, -1] <- round(the.ratios[, -1] / the.ratios[, ncol(the.ratios)], 3)
          # failsafe in case totals are 0 in group ratios
          the.ratios[, 2:ncol(the.ratios)][is.na(the.ratios[, 2:ncol(the.ratios)])] <- 0
        } else {
          # else values are either 1 or 0
          the.ratios[, 2][the.ratios[, 2] > 1] <- 1
          the.ratios$Total <- the.ratios[, 2]
        }
      }      
      attributes(the.ratios)$timestep <- attributes(input$global.ratios[[1]])$timestep
    }
  } else {
    the.ratios <- input$group.ratios[[group]][[type]]
  }

  if (missing(title)) {
    if (missing(group)) {
      if (missing(sections))
        title <- "Global ratios"
      else
        title <- paste0("Group ratios for section", ifelse(length(sections) > 1, "s ", " "), paste0(sections, collapse = ", "))
    } else {
      title <- paste("Ratios for group", group)
    }
  }

  if (missing(xlab))
    xlab <- ""

  if (missing(ylab)) {
    if (type == "absolutes")
      ylab <- "n"
    else
      ylab <- "% tags"
  }

  if (missing(sections)) {
    plotdata <- suppressMessages(reshape2::melt(the.ratios[, -ncol(the.ratios)], id.vars = "Timeslot"))
    colnames(plotdata) <- c("Timeslot", "Location", "n")
    plotdata$Location <- factor(plotdata$Location, levels = colnames(input$global.ratios[[1]])[-c(1, ncol(input$global.ratios[[1]]))])
  } else {
    if (type == "absolutes") {
      plotdata <- the.ratios
    } else {
      plotdata <- suppressMessages(reshape2::melt(the.ratios[, -ncol(the.ratios)], id.vars = "Timeslot"))
      colnames(plotdata) <- c("Timeslot", "Group", "n")
    }
  }

  if (missing(col)) {
    if (missing(sections)) {
      if ((ncol(input$global.ratios[[1]]) - 2) <= 8)
        unique.colours <- as.vector(cbPalette)[1:(ncol(input$global.ratios[[1]]) - 2)]
      else
        unique.colours <- gg_colour_hue(ncol(input$global.ratios[[1]]) - 2)
    } else {
      if (length(unique(plotdata$Group)) <= 8)
        unique.colours <- as.vector(cbPalette)[1:length(unique(plotdata$Group))]
      else
        unique.colours <- gg_colour_hue(length(unique(plotdata$Group)))
    }
  } else {
    if (missing(sections)) {
      if (length(col) < length(unique(plotdata$Location)))
        warning("Not enough colours supplied in 'col' (", length(col)," supplied and ", length(unique(plotdata$Location)), " needed). Reusing colours.", immediate. = TRUE, call. = FALSE)
      unique.colours <- rep(col, length.out = length(unique(plotdata$Location)))
    } else {
      if (length(col) < length(unique(plotdata$Group)))
        warning("Not enough colours supplied in 'col' (", length(col)," supplied and ", length(unique(plotdata$Group)), " needed). Reusing colours.", immediate. = TRUE, call. = FALSE)
      unique.colours <- rep(col, length.out = length(unique(plotdata$Group)))
    }
  }

  if (missing(sections))
    p <- ggplot2::ggplot(data = plotdata, ggplot2::aes(x = Timeslot, y = n, fill = Location, col = Location))
  else
    p <- ggplot2::ggplot(data = plotdata, ggplot2::aes(x = Timeslot, y = n, fill = Group, col = Group))

  p <- p + ggplot2::geom_bar(width = ifelse(attributes(the.ratios)$timestep == "days", 86400, 3600), stat = "identity")
  p <- p + ggplot2::theme_bw()

  if (missing(sections) | type == "percentages") {
    if (ncol(the.ratios) > 3)
      max.y <- max(apply(the.ratios[, c(-1, -ncol(the.ratios))], 1, sum))
    else
      max.y <- max(the.ratios[, 2])
  } else {
    max.y <- max(with(the.ratios, aggregate(n, list(Timeslot), sum))$x)
  }

  if (type == "absolutes")
    p <- p + ggplot2::scale_y_continuous(limits = c(0,  max.y * 1.05), expand = c(0, 0))
  else
    p <- p + ggplot2::scale_y_continuous(limits = c(0,  max.y), expand = c(0, 0))

  p <- p + ggplot2::labs(title = title, x = xlab, y = ylab)

  p <- p + ggplot2::scale_fill_manual(values = unique.colours, drop = FALSE)
  p <- p + ggplot2::scale_colour_manual(values = unique.colours, drop = FALSE)

  if (length(unique.colours) > 10 & length(unique.colours) <= 20)
    p <- p + ggplot2::guides(fill = ggplot2::guide_legend(ncol = 2))
  if (length(unique.colours) > 20)
    p <- p + ggplot2::guides(fill = ggplot2::guide_legend(ncol = 3))

  return(p)
}
