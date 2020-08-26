#' Residency Analysis
#'
#' The \code{\link{residency}} analysis runs the same initial checks as
#' \code{\link{explore}}, but, similarly to \code{\link{migration}}, explores
#' particular points of the fish behaviour. If you want to know where your fish
#' were in each day of the study, how many fish were in each section each day,
#' and other residency-focused variables, this is the analysis you are looking
#' for!
#'
#' @param section.minimum If a fish has less than \code{section.minimum}
#'  consecutive detections in a section, a warning is issued. Defaults to 2.
#' @param timestep The resolution desired for the residency calculations.
#'  One of "days" (default) or "hours".
#' @inheritParams migration
#' @inheritParams explore
#'
#' @examples
#' \donttest{
#' # Start by moving to a temporary directory
#' old.wd <- getwd()
#' setwd(tempdir())
#'
#' # Deploy the example workspace
#' exampleWorkspace("exampleWorkspace")
#'
#' # Move your R session into the example workspace
#' setwd("exampleWorkspace")
#'
#' # run the residency analysis. Ensure the tz argument
#' # matches the time zone of the study area and that the
#' # sections match your array names. The line below works
#' # for the example data.
#' results <- residency(tz = "Europe/Copenhagen")
#'
#' # to obtain an HTML report, run the analysis with report = TRUE
#'
#' # return to original working directory
#' setwd(old.wd)
#' rm(old.wd)
#' }
#'
#' @return A list containing:
#' \itemize{
#'  \item \code{detections}: A list containing all detections for each target fish;
#'  \item \code{valid.detections}: A list containing the valid detections for each target fish;
#'  \item \code{spatial}: A list containing the spatial information used during the analysis;
#'  \item \code{deployments}: A data frame containing the deployments of each receiver;
#'  \item \code{arrays}: A list containing the array details used during the analysis;
#'  \item \code{movements}: A list containing all movement events for each target fish;
#'  \item \code{valid.movements}: A list containing the valid movement events for each target fish;
#'  \item \code{section.movements}: A list containing the valid section shifts for each target fish;
#'  \item \code{status.df}: A data frame containing summary information for each fish, including the
#'   following columns:
#'    \itemize{
#'      \item \emph{Times.entered.\[section\]}: Total number of times the fish
#'        entered a given section
#'      \item \emph{Average.entry.\[section\]}: Average entry time at a given
#'        section
#'      \item \emph{Average.time.\[section\]}: Average time the fish spent in a
#'        given section during each visit
#'      \item \emph{Average.departure.\[section\]}: Average departure time from
#'        a given section
#'      \item \emph{Total.time.\[section\]}: Total time spent in a given section
#'      \item \emph{Very.last.array}: Last array where the fish was detected
#'      \item \emph{Very.last.time}: Time of the last valid detection
#'      \item \emph{Status}: Fate assigned to the fish
#'      \item \emph{Valid.detections}: Number of valid detections
#'      \item \emph{Invalid.detections}: Number of invalid detections
#'      \item \emph{Valid.events}: Number of valid events
#'      \item \emph{Invalid.events}: Number of invalid events
#'      \item \emph{P.type}: Type of processing:
#'        \itemize{
#'          \item 'Skipped' if no data was found for the fish,
#'          \item 'Auto' if no user interaction was required,
#'          \item 'Manual' if user interaction was suggested and the user made
#'            changes to the validity of the events,
#'          \item 'Overridden' if the user listed the fish in the
#'            \code{override} argument.
#'        }
#'      \item \emph{Comments}: Comments left by the user during the analysis
#'    }
#'  \item \code{last.seen}: A data frame containing the number of fish last seen in
#'    each study area section;
#'  \item \code{array.times}: A data frame containing ALL the entry times of each fish
#'    in each array;
#'  \item \code{section.times}: A data frame containing all the entry times of each
#'    fish in each section;
#'  \item \code{residency.list}: A list containing the places of residency between first and last
#'    valid detection for each fish;
#'  \item \code{time.ratios}: A list containing the daily location per section (both in seconds spent
#'    and in percentage of day) for each fish;
#'  \item \code{time.positions}: A data frame showing the location where each
#'    fish spent the most time per day;
#'  \item \code{global.ratios}: A list containing summary tables showing the number of active fish
#'    (and respective percentages) present at each location per day;
#'  \item \code{efficiency}: A list containing the results of the inter-array Multi-way efficiency
#'    calculations (see vignettes for more details);
#'  \item \code{intra.array.CJS}: A list containing the results of the intra-array CJS calculations;
#'  \item \code{rsp.info}: A list containing appendix information for the RSP package;
#'  \item \code{dist.mat}: The distance matrix used in the analysis (if a valid
#'   distance matrix was supplied)
#' }
#'
#' @seealso \code{\link{explore}}, \code{\link{migration}}
#'
#' @export
#'
residency <- function(
  tz = NULL,
  sections,
  section.order = NULL,
  datapack = NULL,
  max.interval = 60,
  minimum.detections = 2,
  start.time = NULL,
  stop.time = NULL,
  speed.method = c("last to first", "last to last"),
  speed.warning = NULL,
  speed.error = NULL,
  jump.warning = 2,
  jump.error = 3,
  inactive.warning = NULL,
  inactive.error = NULL,
  exclude.tags = NULL,
  override = NULL,
  report = FALSE,
  auto.open = TRUE,
  discard.orphans = FALSE,
  discard.first = NULL,
  save.detections = FALSE,
  section.minimum = 2,
  timestep = c("days", "hours"),
  replicates = NULL,
  GUI = c("needed", "always", "never"),
  print.releases = TRUE,
  plot.detections.by = c("stations", "arrays"))
{

  if (!missing(sections))
    warning(
"Argument 'sections' has been deprecated and will be ignored. Please list the
sections to which each of the arrays belong in a new 'Section' column in the
spatial input. You can now use the argument 'section.order' to define the order
by which sections are presented", immediate. = TRUE, call. = FALSE)

# clean up any lost helpers
  deleteHelpers()
  if (file.exists(paste0(tempdir(), "/actel_debug_file.txt")))
    file.remove(paste0(tempdir(), "/actel_debug_file.txt"))
# ------------------------

# debug lines
  if (getOption("actel.debug", default = FALSE)) { # nocov start
    on.exit(message("Debug: Progress log available at ", gsub("\\\\", "/", paste0(tempdir(), "/actel_debug_file.txt"))))
    on.exit(message("Debug: Saving carbon copy to ", gsub("\\\\", "/", paste0(tempdir(), "/actel.debug.RData"))), add = TRUE)
    on.exit(save(list = ls(), file = paste0(tempdir(), "/actel.debug.RData")), add = TRUE)
    message("!!!--- Debug mode has been activated ---!!!")
  } # nocov end
# ------------------------

# check arguments quality
  if (!is.null(datapack))
    checkToken(token = attributes(datapack)$actel.token,
               timestamp = attributes(datapack)$timestamp)

  aux <- checkArguments(dp = datapack,
                        tz = tz,
                        minimum.detections = minimum.detections,
                        max.interval = max.interval,
                        speed.method = speed.method,
                        speed.warning = speed.warning,
                        speed.error = speed.error,
                        start.time = start.time,
                        stop.time = stop.time,
                        report = report,
                        auto.open = auto.open,
                        save.detections = save.detections,
                        jump.warning = jump.warning,
                        jump.error = jump.error,
                        inactive.warning = inactive.warning,
                        inactive.error = inactive.error,
                        exclude.tags = exclude.tags,
                        override = override,
                        print.releases = print.releases,
                        replicates = replicates,
                        section.minimum = section.minimum,
                        section.order = section.order,
                        plot.detections.by = plot.detections.by,
                        timestep = timestep)

  speed.method <- aux$speed.method
  speed.warning <- aux$speed.warning
  speed.error <- aux$speed.error
  inactive.warning <- aux$inactive.warning
  inactive.error <- aux$inactive.error
  plot.detections.by <- aux$plot.detections.by
  timestep <- aux$timestep
  rm(aux)

  GUI <- checkGUI(GUI)
# ------------------------

# Store function call
  the.function.call <- paste0("residency(tz = ", ifelse(is.null(tz), "NULL", paste0("'", tz, "'")),
    ", section.order = ", ifelse(is.null(section.order), "NULL", paste0("c('", paste(section.order, collapse = "', '"), "')")),
    ", datapack = ", ifelse(is.null(datapack), "NULL", deparse(substitute(datapack))),
    ", max.interval = ", max.interval,
    ", minimum.detections = ", minimum.detections,
    ", start.time = ", ifelse(is.null(start.time), "NULL", paste0("'", start.time, "'")),
    ", stop.time = ", ifelse(is.null(stop.time), "NULL", paste0("'", stop.time, "'")),
    ", speed.method = ", paste0("c('", speed.method, "')"),
    ", speed.warning = ", ifelse(is.null(speed.warning), "NULL", speed.warning),
    ", speed.error = ", ifelse(is.null(speed.error), "NULL", speed.error),
    ", jump.warning = ", jump.warning,
    ", jump.error = ", jump.error,
    ", inactive.warning = ", ifelse(is.null(inactive.warning), "NULL", inactive.warning),
    ", exclude.tags = ", ifelse(is.null(exclude.tags), "NULL", paste0("c('", paste(exclude.tags, collapse = "', '"), "')")),
    ", override = ", ifelse(is.null(override), "NULL", paste0("c(", paste(override, collapse = ", "), ")")),
    ", report = ", ifelse(report, "TRUE", "FALSE"),
    ", auto.open = ", ifelse(auto.open, "TRUE", "FALSE"),
    ", discard.orphans = ", ifelse(discard.orphans, "TRUE", "FALSE"),
    ", discard.first = ", ifelse(is.null(discard.first), "NULL", discard.first),
    ", save.detections = ", ifelse(save.detections, "TRUE", "FALSE"),
    ", section.minimum = ", section.minimum,
    ", timestep = '", timestep, "'",
    ", replicates = ", ifelse(is.null(replicates),"NULL", paste0("list(", paste(sapply(1:length(replicates), function(i) paste0("'", names(replicates)[i], "' = c('", paste(replicates[[i]], collapse = "', '"), "')")), collapse = ", "), ")")),
    ", inactive.error = ", ifelse(is.null(inactive.error), "NULL", inactive.error),
    ", GUI = '", GUI, "'",
    ", print.releases = ", ifelse(print.releases, "TRUE", "FALSE"),
    ", plot.detections.by = '", plot.detections.by, "'",
    ")")

  appendTo("debug", the.function.call)
# --------------------

# Prepare clean-up before function ends
  finished.unexpectedly <- TRUE
  on.exit({if (interactive() & finished.unexpectedly) emergencyBreak(the.function.call)}, add = TRUE)

  on.exit(deleteHelpers(), add = TRUE)
  on.exit(tryCatch(sink(), warning = function(w) {hide <- NA}), add = TRUE)
# --------------------------------------

# Final arrangements before beginning
  appendTo("Report", paste0("Actel R package report.\nVersion: ", utils::packageVersion("actel"), "\n"))

  appendTo(c("Report"), paste0("Target folder: ", getwd(), "\nTimestamp: ", the.time <- Sys.time(), "\nFunction: residency()\n"))

  report <- checkReport(report = report)
# -----------------------------------

# Load, structure and check the inputs
  if (is.null(datapack)) {
    study.data <- loadStudyData(tz = tz, override = override, save.detections = save.detections,
                                start.time = start.time, stop.time = stop.time, discard.orphans = discard.orphans,
                                section.order = section.order, exclude.tags = exclude.tags)
  } else {
    appendTo(c("Screen", "Report"), paste0("M: Running analysis on preloaded data (compiled on ", attributes(datapack)$timestamp, ")."))
    appendTo("Report", paste0("Messages displayed during preload:\n-------------------\n", paste0(attributes(datapack)$loading_messages, collapse = "\n"), "\n-------------------"))
    study.data <- datapack
    tz <- study.data$tz
    disregard.parallels <- study.data$disregard.parallels
  }

  bio <- study.data$bio
  deployments <- study.data$deployments
  spatial <- study.data$spatial
  dot <- study.data$dot
  arrays <- study.data$arrays
  dotmat <- study.data$dotmat
  paths <- study.data$paths
  dist.mat <- study.data$dist.mat
  attributes(dist.mat)$speed.method <- speed.method
  detections.list <- study.data$detections.list
# -------------------------------------

# Final quality checks
  # Verify the existance of sections
  if (all(!grepl("^Section$", colnames(spatial$stations))))
    stop("To run residency(), please assign the arrays to their sections using a 'Section' column in the spatial input.", call. = FALSE)

  # Verify that replicate information is valid
  if (!is.null(replicates) && any(is.na(match(names(replicates), names(arrays)))))
    stop("Some of the array names listed in the 'replicates' argument do not match the study's arrays.\n", call. = FALSE)

  capture <- lapply(names(replicates), function(i) {
    x <- replicates[[i]]
    all.stations <- spatial$stations$Standard.name[spatial$stations$Array == i]
    if (any(link <- !x %in% all.stations)) {
      stop(paste0("In replicates: Station", 
                  ifelse(sum(link) > 1, "s ", " "), 
                  paste(x[link], collapse = ", "), 
                  ifelse(sum(link) > 1, " are", " is"), 
                  " not part of ", i, " (available stations: ", 
                  paste(all.stations, collapse = ", "), ")."), 
           call. = FALSE)
    }
  })
# -------------------------------------

# Discard early detections, if required
  if (!is.null(discard.first) && discard.first > 0)
    detections.list <- discardFirst(input = detections.list, bio, trim = discard.first)

# Compile array movements
  appendTo(c("Screen", "Report"), "M: Creating movement records for the valid tags.")
  movements <- groupMovements(detections.list = detections.list, bio = bio, spatial = spatial,
    speed.method = speed.method, max.interval = max.interval, tz = tz,
    dist.mat = dist.mat)

  if (is.null(discard.first)) {
    aux <- names(movements)
    movements <- lapply(names(movements), function(fish) {
        speedReleaseToFirst(fish = fish, bio = bio, movements = movements[[fish]],
                            dist.mat = dist.mat, speed.method = speed.method)
      })
    names(movements) <- aux
    rm(aux)
  } else {
    appendTo(c("Screen", "Report"), "M: Not calculating time/speed from release to first detection because 'discard.first' was set.")
  }


  appendTo(c("Screen", "Report"), "M: Checking movement events quality.")

  do.checkSpeeds <- FALSE
  if (is.null(speed.warning)) {
    appendTo(c("Screen", "Report", "Warning"), "'speed.warning'/'speed.error' were not set, skipping speed checks.")
  } else {
    if(attributes(dist.mat)$valid)
      do.checkSpeeds <- TRUE
    else
      appendTo(c("Screen", "Report", "Warning"), "'speed.warning'/'speed.error' were set, but a valid distance matrix is not present. Aborting speed checks.")
  }

  do.checkInactiveness <- FALSE
  if (is.null(inactive.warning)) {
    appendTo(c("Screen", "Report", "Warning"), "'inactive.warning'/'inactive.error' were not set, skipping inactivity checks.")
  } else {
    if (!attributes(dist.mat)$valid)
      appendTo(c("Report", "Screen", "Warning"), "Running inactiveness checks without a distance matrix. Performance may be limited.")
    do.checkInactiveness <- TRUE
  }

  movement.names <- names(movements)

  if (any(link <- !override %in% extractSignals(movement.names))) {
    appendTo(c("Screen", "Warning", "Report"), paste0("Override has been triggered for fish ", paste(override[link], collapse = ", "), " but ",
      ifelse(sum(link) == 1, "this", "these"), " fish ", ifelse(sum(link) == 1, "was", "were")," not detected."))
    override <- override[!link]
  }

  movements <- lapply(seq_along(movements), function(i) {
    fish <- names(movements)[i]
    appendTo("debug", paste0("debug: Checking movement quality for fish ", fish,"."))

    if (is.na(match(extractSignals(fish), override))) {
      release <- as.character(bio$Release.site[na.as.false(bio$Transmitter == fish)])
      release <- unlist(strsplit(with(spatial, release.sites[release.sites$Standard.name == release, "Array"]), "|", fixed = TRUE))

      output <- checkMinimumN(movements = movements[[i]], fish = fish, minimum.detections = minimum.detections)

      output <- checkImpassables(movements = output, fish = fish, dotmat = dotmat, GUI = GUI)

      output <- checkJumpDistance(movements = output, release = release, fish = fish, dotmat = dotmat,
                                  jump.warning = jump.warning, jump.error = jump.error, GUI = GUI)

      if (do.checkSpeeds) {
        temp.valid.movements <- simplifyMovements(movements = output, fish = fish, bio = bio, discard.first = discard.first,
          speed.method = speed.method, dist.mat = dist.mat)
        output <- checkSpeeds(movements = output, fish = fish, valid.movements = temp.valid.movements,
          speed.warning = speed.warning, speed.error = speed.error, GUI = GUI)
        rm(temp.valid.movements)
      }

      if (do.checkInactiveness) {
        output <- checkInactiveness(movements = output, fish = fish, detections.list = detections.list[[fish]],
          inactive.warning = inactive.warning, inactive.error = inactive.error,
          dist.mat = dist.mat, GUI = GUI)
      }
    } else {
      output <- overrideValidityChecks(moves = movements[[i]], fish = names(movements)[i], GUI = GUI) # nocov
    }
    return(output)
  })
  names(movements) <- movement.names
  rm(movement.names)
# -------------------------


# Compile section movements
  appendTo(c("Screen", "Report"), "M: Compiling and checking section movements for the valid tags.")

  section.movements <- lapply(seq_along(movements), function(i) {
    fish <- names(movements)[i]
    appendTo("debug", paste0("debug: Compiling section movements for fish ", fish,"."))
    aux <- sectionMovements(movements = movements[[i]], spatial = spatial, valid.dist = attributes(dist.mat)$valid)
    output <- checkSMovesN(secmoves = aux, fish = fish, section.minimum = section.minimum, GUI = GUI)
    return(output)
  })
  names(section.movements) <- names(movements)

  # Update array movements based on section movements validity
  movements <- updateValidity(arrmoves = movements, secmoves = section.movements)

  # compile valid movements
  appendTo(c("Screen", "Report"), "M: Filtering valid array and section movements.")

  valid.movements <- lapply(seq_along(movements), function(i){
    output <- simplifyMovements(movements = movements[[i]], fish = names(movements)[i], bio = bio, discard.first = discard.first,
      speed.method = speed.method, dist.mat = dist.mat)
  })
  names(valid.movements) <- names(movements)
  valid.movements <- valid.movements[!unlist(lapply(valid.movements, is.null))]

  section.movements <- lapply(seq_along(valid.movements), function(i) {
    fish <- names(valid.movements)[i]
    appendTo("debug", paste0("debug: Compiling valid section movements for fish ", fish,"."))
    output <- sectionMovements(movements = valid.movements[[i]], spatial = spatial, valid.dist = attributes(dist.mat)$valid)
    return(output)
  })
  names(section.movements) <- names(valid.movements)

  # Grab summary information
  appendTo(c("Screen", "Report"), "M: Compiling residency objects.")

  res.df <- assembleResidency(secmoves = section.movements, movements = movements, spatial = spatial)

  status.df <- res_assembleOutput(res.df = res.df, bio = bio, spatial = spatial, tz = tz)

  last.seen <- as.data.frame.matrix(with(status.df, table(Group, Status)))

  aux <- list(valid.movements = valid.movements, section.movements = section.movements,
    spatial = spatial, rsp.info = list(bio = bio, analysis.type = "residency"))
  array.times <- getTimes(input = aux, move.type = "array", event.type = "arrival", n.events = "all")

  section.times <- list(
    arrival = getTimes(input = aux, move.type = "section", event.type = "arrival", n.events = "all"),
    departure = getTimes(input = aux, move.type = "section", event.type = "departure", n.events = "all"))
  rm(aux)

  residency.list <- getResidency(movements = section.movements, spatial = spatial)

  if (length(residency.list) == 0) {
    emergencyBreak()
    stop("No fish have enough data for residency analysis. Consider running explore() instead.\n", call. = FALSE)
  }

  if (timestep == "days")
    appendTo(c("Screen", "Report"), "M: Calculating daily locations for each fish.")
  else
    appendTo(c("Screen", "Report"), "M: Calculating hourly locations for each fish.")

  time.ratios <- resRatios(res = residency.list, timestep = timestep, tz = tz)

  time.positions <- resPositions(ratios = time.ratios, timestep = timestep)

  global.ratios <- globalRatios(positions = time.positions)

  appendTo("Screen", "M: Validating detections...")

  recipient <- validateDetections(detections.list = detections.list, movements = valid.movements)
  detections <- recipient$detections
  valid.detections <- recipient$valid.detections
  rm(recipient)

# ---------------

# Efficiency
  if (length(arrays) == 0) {
    appendTo(c("Screen", "Warning", "Report"), "Cannot calculate inter-array efficiency when there is only one array (will limit the report's output).")
    efficiency <- NULL
  } else {
    appendTo(c("Screen", "Report"), "M: Calculating inter-array efficiency.")
    efficiency <- res_efficiency(arrmoves = valid.movements, bio = bio, spatial = spatial, arrays = arrays, paths = paths, dotmat = dotmat)
  }
  if (!is.null(replicates)) {
    appendTo(c("Screen", "Report"), "M: Calculating intra-array efficiency.")
    intra.array.matrices <- getDualMatrices(replicates = replicates, CJS = NULL, spatial = spatial, detections.list = valid.detections)
    recipient <- includeIntraArrayEstimates(m = intra.array.matrices, efficiency = efficiency, CJS = NULL)
    efficiency <- recipient$efficiency
    intra.array.CJS <- recipient$intra.CJS
    rm(recipient)
  } else {
    intra.array.matrices <- NULL
    intra.array.CJS <- NULL
  }

# ----------


# ---------------

# wrap up in-R objects
  deployments <- do.call(rbind.data.frame, deployments)

  efficiency <- efficiency[1:3]

  # extra info for potential RSP analysis
  rsp.info <- list(analysis.type = "residency", analysis.time = the.time, bio = bio, tz = tz, actel.version = utils::packageVersion("actel"))

  if (!is.null(override))
    override.fragment <- paste0('<span style="color:red">Manual mode has been triggered for **', length(override),'** fish.</span>\n')
  else
    override.fragment <- ""

  if (file.exists(resultsname <- paste0("actel_residency_results.RData"))) {
    continue <- TRUE
    index <- 1
    while (continue) {
      if (file.exists(resultsname <- paste0("actel_residency_results.", index, ".RData"))) {
        index <- index + 1
      } else {
        continue <- FALSE
      }
    }
    rm(continue, index)
  }

  if (interactive()) { # nocov start
    decision <- userInput(paste0("Would you like to save a copy of the results to ", resultsname, "?(y/n) "), 
                          choices = c("y", "n"), hash = "# save results?")
  } else { # nocov end
    decision <- "n"
  }

  if (decision == "y") {
    appendTo(c("Screen", "Report"), paste0("M: Saving results as '", resultsname, "'."))
    if (attributes(dist.mat)$valid)
      save(detections, valid.detections, spatial, deployments, arrays, movements, valid.movements,
        section.movements, status.df, last.seen, array.times, section.times, intra.array.matrices,
        residency.list, time.ratios, time.positions, global.ratios, efficiency, intra.array.CJS, 
        rsp.info, dist.mat, file = resultsname)
    else
      save(detections, valid.detections, spatial, deployments, arrays, movements, valid.movements,
        section.movements, status.df, last.seen, array.times, section.times, intra.array.matrices,
        residency.list, time.ratios, time.positions, global.ratios, efficiency, intra.array.CJS, 
        rsp.info, file = resultsname)
  } else {
    appendTo(c("Screen", "Report"), paste0("M: Skipping saving of the results."))
  }
  rm(decision)

# ------------

# Print graphics
  if (report) {
    appendTo(c("Screen", "Report"), "M: Producing the report.")
    
    if (dir.exists(paste0(tempdir(), "/actel_report_auxiliary_files")))
      unlink(paste0(tempdir(), "/actel_report_auxiliary_files"), recursive = TRUE)

    dir.create(paste0(tempdir(), "/actel_report_auxiliary_files"))

    if (!getOption("actel.debug", default = FALSE))
      on.exit(unlink(paste0(tempdir(), "/actel_report_auxiliary_files"), recursive = TRUE), add = TRUE)

    biometric.fragment <- printBiometrics(bio = bio)
    
    printDot(dot = dot,
             spatial = spatial,
             print.releases = print.releases)

    printSectionTimes(section.times = section.times,
                      bio = bio,
                      detections = valid.detections)

    printGlobalRatios(global.ratios = global.ratios,
                      time.ratios = time.ratios,
                      spatial = spatial,
                      timestep = timestep)

    individual.detection.plots <- printIndividuals(detections.list = detections,
                                                   movements = movements,
                                                   valid.movements = valid.movements,
                                                   spatial = spatial,
                                                   status.df = status.df,
                                                   rsp.info = rsp.info,
                                                   type = plot.detections.by)

    array.circular.plots <- printCircular(times = timesToCircular(array.times),
                                          bio = bio,
                                          suffix = "_array")

    section.arrival.circular.plots <- printCircular(times = timesToCircular(section.times$arrival),
                                                    bio = bio, 
                                                    suffix = "_section_arrival")

    section.departure.circular.plots <- printCircular(times = timesToCircular(section.times$departure), 
                                                      bio = bio, 
                                                      suffix = "_section_departure")

    appendTo(c("Screen", "Report"), "M: Drawing individual residency graphics.")
    
    time.range <- range(global.ratios[[1]]$Timeslot)
    if (timestep == "days") {
      time.range[1] <- time.range[1] - 86400
      time.range[2] <- time.range[2] + 86400
    } else {
      time.range[1] <- time.range[1] - 3600
      time.range[2] <- time.range[2] + 3600
    }
    individual.residency.plots <- printIndividualResidency(ratios = time.ratios,
                                                           time.range = time.range,
                                                           spatial = spatial,
                                                           timestep = timestep)

    efficiency.fragment <- printEfficiency(efficiency = efficiency,
                                           intra.CJS = intra.array.CJS,
                                           type = "residency")

    printLastSection(input = last.seen,
                     spatial = spatial)

    if (nrow(last.seen) > 3)
      last.seen.graph.size <- "width=90%" else last.seen.graph.size <- "height=4in"

    if (any(sapply(valid.detections, function(x) any(!is.na(x$Sensor.Value))))) {
      sensor.plots <- printSensorData(detections = valid.detections, rsp.info = rsp.info)
    } else {
      sensor.plots <- NULL
    }
  }

  appendTo("Report", "M: Process finished successfully.")
# ---------------

# wrap up the txt report
  appendTo("Report", "\n-------------------")
  
  if (file.exists(paste(tempdir(), "temp_comments.txt", sep = "/")))
    appendTo("Report", paste0("User comments:\n-------------------\n", gsub("\t", ": ", gsub("\r", "", readr::read_file(paste(tempdir(), "temp_comments.txt", sep = "/")))), "-------------------")) # nocov

  if (file.exists(paste(tempdir(), "temp_UD.txt", sep = "/")))
    appendTo("Report", paste0("User interventions:\n-------------------\n", gsub("\r", "", readr::read_file(paste(tempdir(), "temp_UD.txt", sep = "/"))), "-------------------")) # nocov

  appendTo("Report", paste0("Function call:\n-------------------\n", the.function.call, "\n-------------------"))
# ------------------

# print html report
  if (report) {
    if (file.exists(reportname <- "actel_residency_report.html")) {
      continue <- TRUE
      index <- 1
      while (continue) {
        if(file.exists(reportname <- paste0("actel_residency_report.", index, ".html"))) {
          index <- index + 1
        } else {
          continue <- FALSE
        }
      }
      appendTo("Screen", paste0("M: An actel report is already present in the current directory.\n   Saving new report as ", reportname, "."))
      rm(continue, index)
    } else {
      appendTo("Screen", "M: Saving actel report as 'actel_residency_report.html'.")
    }

    appendTo("debug", "debug: Printing report md")
    printResidencyRmd(override.fragment = override.fragment,
                      biometric.fragment = biometric.fragment,
                      efficiency.fragment = efficiency.fragment,
                      individual.detection.plots = individual.detection.plots,
                      individual.residency.plots = individual.residency.plots,
                      array.circular.plots = array.circular.plots,
                      section.arrival.circular.plots = section.arrival.circular.plots,
                      section.departure.circular.plots = section.departure.circular.plots,
                      sensor.plots = sensor.plots,
                      spatial = spatial,
                      deployments = deployments,
                      detections = detections,
                      valid.detections = valid.detections,
                      last.seen = last.seen,
                      last.seen.graph.size = last.seen.graph.size,
                      plot.detections.by = plot.detections.by)

    appendTo("debug", "debug: Converting report to html")
    rmarkdown::render(input = paste0(tempdir(), "/actel_report_auxiliary_files/actel_residency_report.Rmd"),
                      output_dir = paste0(tempdir(), "/actel_report_auxiliary_files"), 
                      quiet = TRUE)

    appendTo("debug", "debug: Moving report")
    file.copy(paste0(tempdir(), "/actel_report_auxiliary_files/actel_residency_report.html"), reportname)
    if (interactive() & auto.open) { # nocov start
      appendTo("debug", "debug: Opening report.")
      browseURL(reportname)
    } # nocov end
  }
# ------------------

  jobname <- paste0(gsub(" |:", ".", as.character(Sys.time())), ".actel.log.txt")

  if (interactive() & !report) { # nocov start
    decision <- userInput(paste0("Would you like to save a copy of the analysis log to ", jobname, "?(y/n) "), 
                          choices = c("y", "n"), hash = "# save job log?")
  } else { # nocov end
    decision <- "n"
  }

  if (decision == "y") { # nocov start
    appendTo("Screen", paste0("M: Saving job log as '",jobname, "'."))
    file.copy(paste(tempdir(), "temp_log.txt", sep = "/"), jobname)
  } # nocov end

  appendTo("Screen", "M: Process finished successfully.")

  finished.unexpectedly <- FALSE

  output <- list(detections = detections,
                 valid.detections = valid.detections,
                 spatial = spatial,
                 deployments = deployments,
                 arrays = arrays,
                 movements = movements,
                 valid.movements = valid.movements,
                 section.movements = section.movements,
                 status.df = status.df,
                 efficiency = efficiency,
                 intra.array.matrices = intra.array.matrices,
                 intra.array.CJS = intra.array.CJS,
                 array.times = array.times,
                 section.times = section.times,
                 residency.list = residency.list,
                 time.ratios = time.ratios,
                 time.positions = time.positions,
                 global.ratios = global.ratios,
                 last.seen = last.seen,
                 rsp.info = rsp.info)

  if (attributes(dist.mat)$valid)
    output$dist.mat <- dist.mat

  return(output)
}

#' Print Rmd report
#'
#' Creates a Rmd report and converts it to html.
#'
#' @inheritParams printMigrationRmd
#' @inheritParams loadDetections
#'
#' @return No return value, called for side effects.
#'
#' @keywords internal
#'
printResidencyRmd <- function(
  override.fragment,
  biometric.fragment,
  efficiency.fragment,
  individual.detection.plots,
  individual.residency.plots,
  array.circular.plots,
  section.arrival.circular.plots,
  section.departure.circular.plots,
  sensor.plots,
  spatial,
  deployments,
  detections,
  valid.detections,
  last.seen,
  last.seen.graph.size,
  plot.detections.by)
{

  work.path <- paste0(tempdir(), "/actel_report_auxiliary_files/")

  if (any(grepl("Unknown", spatial$stations$Standard.name))) {
    unknown.fragment <- paste0('<span style="color:red"> Number of relevant unknown receivers: **', sum(grepl("Unknown", spatial$stations$Standard.name)), '**</span>\n')
  } else {
    unknown.fragment <- ""
  }
  if (!is.null(sensor.plots)) {
    sensor.fragment <- paste0("### Sensor plots

Note:
  : The data used for these graphics is stored in the `valid.detections` object.
  : You can replicate these graphics and edit them as needed using the `plotSensors()` function.

<center>\n", sensor.plots, "\n</center>")
  } else {
    sensor.fragment <- NULL
  }

  report <- readr::read_file(paste0(tempdir(), "/temp_log.txt"))

  oldoptions <- options(knitr.kable.NA = "-")
  on.exit(options(oldoptions), add = TRUE)

  sink(paste0(work.path, "actel_residency_report.Rmd"))
  cat(paste0(
'---
title: "Acoustic telemetry residency analysis"
author: "Actel R package (', utils::packageVersion("actel"), ')"
output:
  html_document:
    includes:
      after_body: ', work.path, 'toc_menu_residency.html
---

### Summary

Target folder: ', stringr::str_extract(pattern = '(?<=Target folder: )[^\r]*', string = report), '

Timestamp: **', stringr::str_extract(pattern = '(?<=Timestamp: )[^\r|^\n]*', string = report), '**

Number of target tags: **`r I(nrow(status.df))`**

', override.fragment,'

Number of listed receivers: **', stringr::str_extract(pattern = '(?<=Number of ALS: )[0-9]*', string = report), '** (of which **', stringr::str_extract(pattern = '(?<=of which )[0-9]*', string = report), '** had no detections)

', unknown.fragment,'

Data time range: ', stringr::str_extract(pattern = '(?<=Data time range: )[^\r|^\n]*', string = report), '

Percentage of post-release valid detections: ', round(sum(unlist(lapply(valid.detections, nrow))) / sum(unlist(lapply(detections, nrow))) * 100, 2), '%

Found a bug? [**Report it here.**](https://github.com/hugomflavio/actel/issues)

Want to cite actel in a publication? Run `citation(\'actel\')`

### Study area

Arrays with the same background belong to the same section. Release sites are marked with "R.S.". Arrays connected with an arrow indicate that the fish can only pass in one direction.

<img src=', work.path, ifelse(file.exists(paste0(work.path, "mb_arrays.svg")), "mb_arrays.svg", "mb_arrays.png"), ' style="padding-top: 15px;"/>

### Receiver stations

', paste(knitr::kable(spatial$stations, row.names = FALSE), collapse = "\n"), '

### Deployments

', paste(knitr::kable(deployments, row.names = FALSE), collapse = "\n"), '

### Release sites

', paste(knitr::kable(spatial$release.sites, row.names = FALSE), collapse = "\n"), '

### Array efficiency

', efficiency.fragment,'

### Warning messages

```{r warnings, echo = FALSE, comment = NA}
cat("', ifelse(file.exists(paste0(tempdir(), '/temp_warnings.txt')),
  gsub("\\r", "", readr::read_file(paste0(tempdir(), '/temp_warnings.txt'))),
  'No warnings were raised during the analysis.'), '")
```

### User comments

```{r comments, echo = FALSE, comment = NA}
cat("', ifelse(file.exists(paste0(tempdir(), '/temp_comments.txt')),
  gsub("\\r", "", readr::read_file(paste0(tempdir(), '/temp_comments.txt'))),
  'No comments were included during the analysis.'), '")
```

', ifelse(biometric.fragment == '', '', paste0('### Biometric graphics

Note:
  : The data used in this graphic is the data present in the biometrics.csv file.

<center>
', biometric.fragment,'
</center>
')), '

### Last seen

Note:
  : The data used in this table and graphic is stored in the `last.seen` object.

', paste(knitr::kable(last.seen), collapse = "\n"), '

<center>
![](', work.path, 'last_section.png){ ',last.seen.graph.size ,' }
</center>


### Average time of arrival at each Array

Note:
  : Coloured lines on the outer circle indicate the mean value for each group and the respective ranges show the standard error of the mean. Each group\'s bars sum to 100%. The number of data points in each group is presented between brackets in the legend of each pannel.
  : You can replicate these graphics and edit them as needed using the `plotTimes()` function.
  : The data used in these graphics is stored in the `array.times` object.

<center>
', array.circular.plots,'
</center>

### Time details for each section

#### Arrival days at each section

Note:
  : The data used in these graphics is stored in the `section.times$arrival` object.

<center>
![](', work.path, 'arrival_days.png){ width=95% }
</center>

#### Arrival times at each section

Note:
  : Coloured lines on the outer circle indicate the mean value for each group and the respective ranges show the standard error of the mean. Each group\'s bars sum to 100%. The number of data points in each group is presented between brackets in the legend of each pannel.
  : You can replicate these graphics and edit them as needed using the `plotTimes()` function.
  : The data used in these graphics is stored in the `section.times$arrival` object.

<center>
', section.arrival.circular.plots,'
</center>

#### Departure days at each section

Note:
  : The data used in these graphics is stored in the `section.times$departure` object.

<center>
![](', work.path, 'departure_days.png){ width=95% }
</center>

#### Departure times at each section

Note:
  : Coloured lines on the outer circle indicate the mean value for each group and the respective ranges show the standard error of the mean. Each group\'s bars sum to 100%. The number of data points in each group is presented between brackets in the legend of each pannel.
  : You can replicate these graphics and edit them as needed using the `plotTimes()` function.
  : The data used in these graphics is stored in the `section.times$departure` object.

<center>
', section.departure.circular.plots,'
</center>

### Section progression

Note:
  : The data used in these graphics is stored in the `global.ratios` and `time.positions` objects.


#### Absolutes

<center>
![](', work.path, 'global_ratios_absolutes', ifelse(file.exists(paste0(work.path, 'global_ratios_absolutes.svg')), ".svg", ".png"), '){ width=95% }
</center>

#### Percentages


<center>
![](', work.path, 'global_ratios_percentages', ifelse(file.exists(paste0(work.path, 'global_ratios_percentages.svg')), ".svg", ".png"), '){ width=95% }
</center>

### Individual residency plots

Note:
  : The data used in these graphics is stored in the `time.ratios` object (one table per fish). More condensed information can be found in the `section.movements` object.

<center>
', individual.residency.plots,'
</center>

### Individual detection plots

Note:  
  : You can choose to plot detections by station or by array using the `plot.detections.by` argument.
  : The detections are coloured by ', ifelse(plot.detections.by == "stations", 'array', 'section'), '. The vertical black dashed line shows the release time. The full dark-grey line shows the movement events considered valid, while the dashed dark-grey line shows the movement events considered invalid.
', ifelse(plot.detections.by == "stations", '  : The movement event lines move straight between the first and last station of each event (i.e. in-between detections will not be individually linked by the line).\n', ''),
'  : Manually **edited** fish are highlighted with **yellow** graphic borders.
  : Manually **overridden** fish are highlighted with **red** graphic borders.
  : The ', ifelse(plot.detections.by == "stations", 'stations', 'arrays'), ' have been aligned by ', ifelse(plot.detections.by == "stations", 'array', 'section'), ', following the order provided ', ifelse(plot.detections.by == "stations", '', 'either '), 'in the spatial input', ifelse(plot.detections.by == "stations", '.', ' or the `section.order` argument.'), '
  : You can replicate these graphics and edit them as needed using the `plotDetections()` function.
  : You can also see the movement events of multiple tags simultaneously using the `plotMoves()` function.
  : The data used in these graphics is stored in the `detections` and `movements` objects (and respective valid counterparts).

<center>
', individual.detection.plots,'
</center>

', sensor.fragment,'

### Full log

```{r log, echo = FALSE, comment = NA}
cat("', gsub("\\r", "", readr::read_file(paste0(tempdir(), '/temp_log.txt'))), '")
```

'), fill = TRUE)
sink()

sink(paste0(work.path, "toc_menu_residency.html"))
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

img[src*="#diagram"] {
  width = 100%;
  padding-top = 200px;
  padding-bottom = 15px;
}

/* The sidebar menu */
.sidenav {
  height: 100%;
  width: 130px;
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
  <a href="#study-area">Study area</a>
  <a href="#receiver-stations">Stations</a>
  <a href="#deployments">Deployments</a>
  <a href="#release-sites">Release sites</a>
  <a href="#array-efficiency">Array efficiency</a>
  <a href="#warning-messages">Warnings</a>
  <a href="#user-comments">Comments</a>',
  ifelse(biometric.fragment == '', '', '\n  <a href="#biometric-graphics">Biometrics</a>'),'
  <a href="#last-seen">Last seen</a>
  <a href="#average-time-of-arrival-at-each-array">Array times</a>
  <a href="#time-details-for-each-section">Section times</a>
  <a href="#section-progression">Section progression</a>
  <a href="#individual-residency-plots">Individual residency</a>
  <a href="#individual-detection-plots">Individual detections</a>',
  ifelse(is.null(sensor.fragment), '', '\n  <a href="#sensor-plots">Sensor data</a>'),'
  <a href="#full-log">Full log</a>
</div>
', fill = TRUE)
sink()
}


#' Collect summary information for the residency analysis
#'
#' @param movements the array-movements (Valid and invalid)
#' @inheritParams migration
#'
#' @return A data frame with the compiled residency values
#'
#' @keywords internal
#'
assembleResidency <- function(secmoves, movements, spatial) {
  Last.array <- NULL
  Last.time <- NULL
  Valid <- NULL
  Section <- NULL

  sections <- names(spatial$array.order)

  recipient <- vector()
  for (i in seq_along(sections)) {
    recipient <- c(recipient, paste(c("Times.entered", "Average.entry", "Average.time", "Average.departure", "Total.time"), sections[i], sep = "."))
  }
  recipient <- c(recipient, "Very.last.array", "Very.last.time", "Status", "Valid.detections", "Invalid.detections", "Valid.events", "Invalid.events", "P.type")

  res.df <- matrix(nrow = length(secmoves), ncol = length(recipient))
  res.df <- as.data.frame(res.df, stringsAsFactors = FALSE)

  colnames(res.df) <- recipient
  rm(recipient)
  rownames(res.df) <- names(secmoves)

  capture <- lapply(names(secmoves), function(fish) {
    # cat(fish, "\n")
    aux <- split(secmoves[[fish]], secmoves[[fish]]$Section)
    recipient <- lapply(seq_along(aux), function(i) {
      # cat(i, "\n")
      recipient <- rep(NA, ncol(res.df))
      names(recipient) <- colnames(res.df)
      recipient <- t(as.data.frame(recipient))
      total.time <- apply(aux[[i]][, c("First.time", "Last.time")], 1, function(x) difftime(x[2], x[1], units = "secs"))
      recipient[1, paste0("Total.time.", names(aux)[i])] <- sum(total.time)
      recipient[1, paste0("Times.entered.", names(aux)[i])] <- nrow(aux[[i]])
      entry.time <- mean(circular::circular(decimalTime(substr(aux[[i]]$First.time, start = 12, stop = 20)), units = "hours", template = "clock24"))
      if (entry.time < 0)
        entry.time <- 24 + entry.time
      recipient[1, paste0("Average.entry.", names(aux)[i])] <- minuteTime(entry.time, format = "h", seconds = FALSE)
      leave.time <- mean(circular::circular(decimalTime(substr(aux[[i]]$Last.time, start = 12, stop = 20)), units = "hours", template = "clock24"))
      recipient[1, paste0("Average.time.", names(aux)[i])] <- mean(total.time)
      if (leave.time < 0)
        leave.time <- 24 + leave.time
      recipient[1, paste0("Average.departure.", names(aux)[i])] <- minuteTime(leave.time, format = "h", seconds = FALSE)
      return(recipient)
    })
    recipient <- as.data.frame(combine(recipient), stringsAsFactors = FALSE)
    # convert Total.times to numeric and replace NAs
    the.cols <- which(grepl("Times.entered", colnames(recipient)))
    recipient[, the.cols] <- as.numeric(recipient[, the.cols])
    recipient[, the.cols[which(is.na(recipient[, the.cols]))]] <- 0
    # --
    recipient$Very.last.array <- secmoves[[fish]][.N, Last.array]
    recipient$Very.last.time <- as.character(secmoves[[fish]][.N, Last.time])
    recipient$Status <- paste0("Disap. in ", secmoves[[fish]][.N, Section])
    recipient$Valid.detections <- sum(secmoves[[fish]]$Detections)
    recipient$Valid.events <- sum(movements[[fish]]$Valid)
    if (any(!movements[[fish]]$Valid)) {
      recipient$Invalid.detections <- sum(movements[[fish]][!(Valid)]$Detections)
      recipient$Invalid.events <- sum(!movements[[fish]]$Valid)
    } else {
      recipient$Invalid.detections <- 0
      recipient$Invalid.events <- 0
    }
    recipient$P.type <- attributes(secmoves[[fish]])$p.type
    res.df[fish, ] <<- recipient
  })
  # Convert time data
  for (section in sections) {
    for (the.col in c("Average.time.", "Total.time.")) {
      # convert to numeric
      res.df[, paste0(the.col, section)] <- as.numeric(res.df[, paste0(the.col, section)])
      # grab the mean for later use
      aux <- mean(res.df[, paste0(the.col, section)], na.rm = TRUE)
      # convert to difftime
      res.df[, paste0(the.col, section)] <- as.difftime(res.df[, paste0(the.col, section)], units = "secs")
      units(res.df[, paste0(the.col, section)]) <- "secs"
      if (!is.nan(aux)) {
        if (aux > 86400)
          units(res.df[, paste0(the.col, section)]) <- "days"
        if (aux <= 86400 & aux > 3600)
          units(res.df[, paste0(the.col, section)]) <- "hours"
        if (aux <= 3600)
          units(res.df[, paste0(the.col, section)]) <- "mins"
      }
      res.df[, paste0(the.col, section)] <- round(res.df[, paste0(the.col, section)], 3)
    }
  }
  res.df$Transmitter <- row.names(res.df)
  return(res.df)
}

#' Create status.df
#'
#' Combines the timetable and the original biometrics.
#'
#' @inheritParams explore
#' @inheritParams splitDetections
#' @inheritParams simplifyMovements
#' @inheritParams loadDetections
#' @inheritParams groupMovements
#'
#' @return A data frame containing all the final data for each fish.
#'
#' @keywords internal
#'
res_assembleOutput <- function(res.df, bio, spatial, tz) {
  appendTo("debug", "Merging 'bio' and 'res.df'.")
  status.df <- merge(bio, res.df, by = "Transmitter", all = TRUE)

  sections <- names(spatial$array.order)

  appendTo("debug", "Completing entries for fish that were never detected.")
  status.df$Status[is.na(status.df$Status)] <- "Disap. at Release"
  status.df$Status <- factor(status.df$Status, levels = c(paste("Disap. in", sections), "Disap. at Release"))
  status.df$Very.last.array[is.na(status.df$Very.last.array)] <- "Release"
  status.df$Very.last.array <- factor(status.df$Very.last.array, levels = c("Release", levels(spatial$stations$Array)))
  status.df$P.type[is.na(status.df$P.type)] <- "Skipped"
  status.df$Valid.detections[is.na(status.df$Valid.detections)] <- 0
  status.df$Invalid.detections[is.na(status.df$Invalid.detections)] <- 0
  status.df$Valid.events[is.na(status.df$Valid.events)] <- 0
  status.df$Invalid.events[is.na(status.df$Invalid.events)] <- 0

  # Convert time stamps
  status.df$Release.date <- as.POSIXct(status.df$Release.date, tz = tz)
  status.df$Very.last.time <- as.POSIXct(status.df$Very.last.time, tz = tz)

  if (file.exists("temp_comments.txt")) { # nocov start
    temp <- read.table("temp_comments.txt", header = FALSE, sep = "\t")
    status.df[, "Comments"] <- NA_character_
    for (i in seq_len(nrow(temp))) {
      link <- match(temp[i, 1], status.df$Transmitter)
      if (is.na(status.df$Comments[link])) {
        status.df$Comments[link] <- paste(temp[i, 2])
      } else {
        status.df$Comments[link] <- paste(status.df$Comments[link], temp[i, 2], sep = "// ")
      }
    }
  } # nocov end
  appendTo("debug", "Done.")
  return(status.df)
}

#' Calculate array efficiency for residency analysis
#'
#' @inheritParams updateValidity
#' @inheritParams splitDetections
#' @inheritParams createStandards
#' @param arrays a list containing information for each array
#' @param paths a list containing the shortest paths between arrays with distance > 1
#' @inheritParams dotPaths
#'
#' @return A list containing:
#' \itemize{
#'  \item \code{absolutes}: A data frame with the number of fish detected and missed at each array.
#'  \item \code{max.efficiency}: A vector of efficiency values calculated disregarding potentially missed fish.
#'  \item \code{min.efficiency}: A vector of efficiency values calculated taking into account potentially missed fish.
#'  \item \code{values.per.fish}: A list containing details on the arrays that have failed for each fish.
#' }
#'
#' @keywords internal
#'
res_efficiency <- function(arrmoves, bio, spatial, arrays, paths, dotmat) {
  values.per.fish <- lapply(names(arrmoves), function(fish) {
    # cat(fish, "\n")
      first.array <- firstArrayFailure(fish = fish, bio = bio, spatial = spatial, first.array = arrmoves[[fish]]$Array[1], paths = paths, dotmat = dotmat)
      if (nrow(arrmoves[[fish]]) > 1)
        subsequent <- countArrayFailures(moves = arrmoves[[fish]], paths = paths, dotmat = dotmat)
      else
        subsequent <- NULL
      return(c(first.array, subsequent))
  })
  names(values.per.fish) <- names(arrmoves)
  aux <- unlist(values.per.fish)
  knownMissEvents <- table(aux[grepl("known", names(aux))])
  unsureMissEvents <- table(aux[grepl("unsure", names(aux))])

  aux <- lapply(arrmoves, function(x) rle(x$Array)$values)
  recEvents <- table(unlist(aux))

  absolutes <- as.data.frame(matrix(ncol = length(arrays), nrow = 3))
  colnames(absolutes) <- unlist(spatial$array.order)
  rownames(absolutes) <- c("Recorded events", "Known missed events", "Potentially missed events")
  absolutes[is.na(absolutes)] <- 0

  absolutes[1, match(names(recEvents), colnames(absolutes))] <- recEvents
  absolutes[2, match(names(knownMissEvents), colnames(absolutes))] <- knownMissEvents
  absolutes[3, match(names(unsureMissEvents), colnames(absolutes))] <- unsureMissEvents

  # do not calculate efficiency for arrays without before or after neighbours
  aux <- unlist(lapply(arrays, function(x) (!is.null(x$after) & is.null(x$before)) | (is.null(x$after) & !is.null(x$before))))
  one.side.neighbours <- names(aux)[aux]
  # exclude arrays which are connected to a release site from the list above
  one.side.neighbours <- one.side.neighbours[!one.side.neighbours %in% spatial$release.sites$Array]
  # do not calculate efficiency for arrays without any neighbour at all
  aux <- unlist(lapply(arrays, function(x) is.null(x$after) & is.null(x$before)))
  two.side.neighbours <- names(aux)[aux]
  # combine both types
  no.neighbours <- unique(c(one.side.neighbours, two.side.neighbours))
  # erase efficiency for them
  absolutes[2, match(no.neighbours, colnames(absolutes))] <- NA
  absolutes[3, match(no.neighbours, colnames(absolutes))] <- NA

  max.efficiency <- apply(absolutes, 2, function(x) 1 - (x[2] / (x[1] + x[2])))
  min.efficiency <- apply(absolutes, 2, function(x) 1 - ((x[2] + x[3]) / sum(x)))
  return(list(absolutes = absolutes, max.efficiency = max.efficiency, min.efficiency = min.efficiency, values.per.fish = values.per.fish))
}

#' Determine if the first array after release has failed
#'
#' @param fish The fish being analysed
#' @inheritParams splitDetections
#' @inheritParams createStandards
#' @param first.array The array of the first valid movement
#' @inheritParams res_efficiency
#' @inheritParams dotPaths
#'
#' @return NULL if no arrays failed, or a list of arrays that failed.
#'
#' @keywords internal
#'
firstArrayFailure <- function(fish, bio, spatial, first.array, paths, dotmat) {
  release <- as.character(bio$Release.site[na.as.false(bio$Transmitter == fish)])
  aux <- as.character(with(spatial, release.sites[release.sites$Standard.name == release, "Array"]))
  release.arrays <- unlist(strsplit(aux, "|", fixed = TRUE))
  if (any(release.arrays == first.array)) {
    return(NULL)
  } else {
    if (length(release.arrays) == 1)
      to.blame <- release.arrays
    else
      to.blame <- names(dotmat[release.arrays, first.array])[dotmat[release.arrays, first.array] == min(dotmat[release.arrays, first.array])]
    if (min(dotmat[release.arrays, first.array]) == 1) {
      if (table(dotmat[release.arrays, first.array])["1"] == 1)
        return(unlist(list(known = to.blame)))
      else
        return(unlist(list(unsure = to.blame)))
    } else {
      if (table(dotmat[release.arrays, first.array])[1] == 1) {
        aux <- blameArrays(from = to.blame, to = first.array, paths = paths)
        return(unlist(list(known = c(to.blame, aux$known), unsure = aux$unsure)))
      } else {
        aux <- lapply(to.blame, function(x) blameArrays(from = x, to = first.array, paths = paths))
        names(aux) <- to.blame
        combined.unsure <- unique(unlist(aux))
        aux.knowns <- unlist(lapply(aux, function(x) x$known))
        knowns <- table(aux.knowns)
        if (any(knowns == length(aux))) {
          combined.knowns <- names(knowns)[knowns == length(aux)]
          combined.unsure <- combined.unsure[!combined.unsure %in% combined.knowns]
        } else {
          combined.knowns <- NULL
        }
        return(unlist(list(known = combined.knowns, unsure = c(to.blame, combined.unsure))))
      }
    }
  }
}


#' Assemble residency tables per fish
#'
#' @param movements the movements list
#' @inheritParams loadDistances
#'
#' @return a list containing residency tables for each fish.
#'
#' @keywords internal
#'
getResidency <- function(movements, spatial){
  output <- lapply(movements, function(x) {
      recipient <- as.data.frame(x)[, c("Section", "First.time", "Last.time")]
      recipient$Index <- (1:nrow(recipient) * 2) - 1
      if (nrow(recipient) > 1) {
        to.add <- data.frame(
          A = match(recipient$Section[-nrow(recipient)], names(spatial$array.order)),
          B = match(recipient$Section[-1], names(spatial$array.order)),
          First.time = recipient$Last.time[-nrow(recipient)],
          Last.time = recipient$First.time[-1])
        to.add$Section <- apply(to.add, 1, function(xi) {
          if (xi[1] < xi[2]) {
            return(paste0(names(spatial$array.order)[as.numeric(xi[1])], "-", names(spatial$array.order)[as.numeric(xi[2])]))
          } else
            return(paste0(names(spatial$array.order)[as.numeric(xi[2])], "-", names(spatial$array.order)[as.numeric(xi[1])]))
        })
        to.add <- to.add[, c("Section", "First.time", "Last.time")]
        to.add$Index <- 1:nrow(to.add) * 2
        output <- rbind(recipient, to.add)
        output <- output[order(output$Index), ]
        output <- output[, -ncol(output)]
        row.names(output) <- 1:nrow(output)
      } else {
        output <- recipient
      }
      return(output)
    })
  inst.exception <- sapply(output, function(x) nrow(x) == 1 && x$First.time == x$Last.time)
  if (any(inst.exception)) {
    appendTo(c("Screen", "Report", "Warning"), paste("Valid detections for fish", paste(names(output)[inst.exception], collapse = ", "), "start and end on the same instant. Excluding them from residency analyses."))
    output <- output[!inst.exception]
  }
  return(output)
}

#' calculate residency ratios per fish
#'
#' @param res a residency list
#' @param timestep the size of each frame
#'
#' @return A list containing the residency ratios for each fish.
#'
#' @keywords internal
#'
resRatios <- function(res, timestep = c("days", "hours"), tz) {
  timestep <- match.arg(timestep)
  num.step <- ifelse(timestep == "days", 86400, 3600)
  counter <- 0
  if (interactive())
    pb <- txtProgressBar(min = 0, max = length(res), style = 3, width = 60) # nocov
  output <- lapply(res, function(x) {
    counter <<- counter + 1
    # cat("\n", counter, "\n")
    res.range <- seq(from = round.POSIXt(x$First.time[1] - (num.step / 2), units = timestep),
      to =  round.POSIXt(x$Last.time[nrow(x)] - (num.step / 2), units = timestep), by = num.step)
    res.list <- lapply(res.range, function(d) {
      # cat(as.character(d), "\n")
      findSecondsPerSection(res = x, frame = d, the.range = range(res.range), num.step = num.step)
    })
    if (interactive())
      setTxtProgressBar(pb, counter) # nocov
    resRatiosIndOut(input = res.list, slots = round.POSIXt(res.range, units = timestep), tz = tz)
  })
  if (interactive())
    close(pb) # nocov
  return(output)
}

#' Calculate number of seconds at each location per day
#'
#' @inheritParams resRatios
#' @param start.time the start time of the period being analysed (a Date object)
#' @param the.range the first and last start.time for the specific fish
#' @param num.step the size of the block
#'
#' @return A data frame containing the number of seconds spent at each location for a specific timeframe
#'
#' @keywords internal
#'
findSecondsPerSection <- function(res, frame, the.range, num.step) {
  aux <- c()
  candidate.events <- which(res$First.time <= frame + num.step)
  potential.events <- which(res$First.time[candidate.events] >= frame)
  # IF potential events returns 0, then the only valid event is the very last candidate
  if (length(potential.events) == 0)
    e <- tail(candidate.events, 1)
  else
    e <- potential.events
  # IF there is only one event
  if (length(e) == 1) {
    # and that event is the very first one
    if (e == 1) {
      # Exception for if the fish was only detected one frame, calculates difference between event start and stop.
      if (the.range[2] == the.range[1]) {
        aux[length(aux) + 1] <- difftime(res$Last.time[e], res$First.time[e], units = "secs")
        names(aux)[length(aux)] <- res$Section[e]
        aux[length(aux) + 1] <- 0
        names(aux)[length(aux)] <- "Changes"
      } else {
        # Exception for if the frame being analysed is the last frame, calculates difference between frane start and event stop.
        if (frame == the.range[2]) {
          aux[length(aux) + 1] <- difftime(res$Last.time[e], frame, units = "secs")
          names(aux)[length(aux)] <- res$Section[e]
        # Otherwise calculate difference between event start and frame end, and trim at a full frame.
        } else {
          aux[length(aux) + 1] <- min(num.step, as.numeric(difftime(frame + num.step, res$First.time[e], units = "secs")))
          names(aux)[length(aux)] <- res$Section[e]
        }
        # Regardless of path, if length(e) == 1 and the event is the first one, the number of changes is 0
        aux[length(aux) + 1] <- 0
        names(aux)[length(aux)] <- "Changes"
      }
    # If there is only one event but that event is not the very first one
    } else {
      # If the frame of the event is previous to the frame itself
      if (res$First.time[e] < frame) {
        # Exception for the last frame, calculates difference between frame start and event stop.
        if (frame == the.range[2]) {
          aux[length(aux) + 1] <- difftime(res$Last.time[e], frame, units = "secs")
          names(aux)[length(aux)] <- res$Section[e]
        # otherwise, since there are no more 'e', the fish has stayed the whole frame in the event
        } else {
          aux[length(aux) + 1] <- num.step
          names(aux)[length(aux)] <- res$Section[e]
        }
        # In either case above the fish spends the whole time in the event, so changes = 0
        aux[length(aux) + 1] <- 0
        names(aux)[length(aux)] <- "Changes"
      # If the frame of the event is already in the frame itself
      } else {
        # Start by storing the time spent in previous event (i.e. difference between frame start and event start)
        aux[length(aux) + 1] <- difftime(res$First.time[e], frame, units = "secs")
        names(aux)[length(aux)] <- res$Section[e - 1]
        # Then, since there are no more events, calculate difference between event start and frame end.
        aux[length(aux) + 1] <- difftime(frame + num.step, res$First.time[e], units = "secs")
        names(aux)[length(aux)] <- res$Section[e]
        # Since the frame is split in two events, there is 1 change
        aux[length(aux) + 1] <- 1
        names(aux)[length(aux)] <- "Changes"
      }
    }
  # IF there is more than one 'e'
  } else {
    n <- length(e)
    # calculate difference between frame start and first time
    aux[length(aux) + 1] <- difftime(res$First.time[e[1]], frame, units = "secs")
    # Exception for if the frame being analysed is the first frame (in which case this fragment should be excluded at the end)
    if (frame == the.range[1])
      names(aux)[length(aux)] <- "TO.EXCLUDE"
    else
      names(aux)[length(aux)] <- res$Section[e[1] - 1]
    # For all events except the last, calculate the difference between first and last time of the event.
    for (i in e[-n]) {
      aux[length(aux) + 1] <- difftime(res$Last.time[i], res$First.time[i], units = "secs")
      names(aux)[length(aux)] <- res$Section[i]
    }
    # For the very last frame
    # exception for if the frame being analysed is the last frame, where the behaviour should be the same as the above
    if (frame == the.range[2]) {
      aux[length(aux) + 1] <- difftime(res$Last.time[e[n]], res$First.time[e[n]], units = "secs")
      names(aux)[length(aux)] <- res$Section[e[n]]
    # otherwise, remove the seconds already accounted for to a full frame, and store the result
    } else {
      aux[length(aux) + 1] <- 86400 - sum(as.vector(aux))
      names(aux)[length(aux)] <- res$Section[e[n]]
    }
    # The number of changes will be the number of events - 1
    aux[length(aux) + 1] <- length(e) - 1
    names(aux)[length(aux)] <- "Changes"
  }
  # Conclude the exception started above, where the first fragment must be excluded
  if (any(link <- names(aux) == "TO.EXCLUDE")) {
    aux <- aux[!link]
    aux["Changes"] <- aux["Changes"] - 1
  }
  return(aux)
}

#' compile output of dailyRatios
#'
#' @param input a list containing the output of findSecondsPerSection for each day
#' @param slots the names of the time slots
#'
#' @return A data frame with the daily ratios table for the target fish
#'
#' @keywords internal
#'
resRatiosIndOut <- function(input, slots, tz) {
  # sort out all column names
  the.cols <- sort(unique(unlist(lapply(input, names))))
  the.cols <- the.cols[-match("Changes", the.cols)]
  the.cols <- data.frame(V1 = the.cols, V2 = paste0("p", the.cols))
  the.cols <- c(as.matrix(t(the.cols)))
  the.cols <- c(the.cols, "Changes")
  # turn vectors into tables
  days.tables <- vectorsIntoTables(input = input, columns = the.cols)
  # merge all tables
  if (is.data.frame(days.tables)) {
    output <- days.tables
    output$Timeslot <- as.POSIXct(slots, tz = tz)
    output <- output[, c(ncol(output), 1:(ncol(output) - 1))]
  } else {
    output <- as.data.frame(data.table::rbindlist(days.tables, idcol = "Timeslot"))
    output$Timeslot <- as.POSIXct(slots, tz = tz)
  }
  output[is.na(output)] <- 0
  aux <- which(grepl("^p", colnames(output)))
  aux <- aux[!is.na(match(colnames(output)[aux - 1], sub("p", "", colnames(output)[aux])))]
  if (length(aux) == 1)
    output[, aux] <- apply(output[, aux - 1, drop = FALSE], 1, function(x) x / sum(x))
  else
    output[, aux] <- t(apply(output[, aux - 1, drop = FALSE], 1, function(x) x / sum(x)))
  the.zone <- apply(output[, aux, drop = FALSE], 1, function(x) which(x == max(x)))
  output$Most.time <- colnames(output)[aux - 1][the.zone]
  output[, aux] <- round(output[, aux, drop = FALSE], 3)
  return(output)
}

#' Find the location where the fish spent most time per day
#'
#' @param ratios the daily ratios
#'
#' @return A data frame containing the section in which each fish spent more time per day.
#'
#' @keywords internal
#'
resPositions <- function(ratios, timestep = c("days", "hours")) {
  timestep <- match.arg(timestep)
  num.step <- ifelse(timestep == "days", 86400, 3600)

  # extract first and last time
  first.time <- as.POSIXct(NA)[-1]
  last.time <- as.POSIXct(NA)[-1]
  
  capture <- lapply(ratios, function(x) {
    first.time <<- c(first.time, x$Timeslot[1])
    last.time <<- c(last.time, x$Timeslot[nrow(x)])
  })
  # --
  res.range <- seq(from = min(first.time), to = max(last.time), by = num.step)
  attributes(res.range)$tzone <- attributes(ratios[[1]]$Timeslot)$tzone
  
  output <- matrix(ncol = length(ratios) + 1, nrow = length(res.range))
  rownames(output) <- 1:length(res.range)
  colnames(output) <- c("Timeslot", names(ratios))
  output <- as.data.frame(output)
  output$Timeslot <- res.range
  
  capture <- lapply(names(ratios), function(i) {
    link <- match.POSIXt(ratios[[i]]$Timeslot, output$Timeslot)
    if (any(is.na(link)))
      stop("Something went wrong when creating the recipient for the global ratios.")
    output[link, i] <<- ratios[[i]]$Most.time
  })
  
  return(output)
}

#' Transform vectors into data frames with specific columns
#'
#' Used to prepare lists of vectors with partially matching names for combination with do.call(rbind.data.frame, x)
#'
#' @param input the list of vectors
#' @param columns the columns that should be present in every element
#'
#' @return A list of tables with matching columns
#'
#' @keywords internal
#'
vectorsIntoTables <- function(input, columns) {
  counter <- 0
  output <- lapply(input, function(x) {
    counter <<- counter + 1
    recipient <- matrix(ncol = length(columns), nrow = 1)
    colnames(recipient) <- columns
    recipient <- as.data.frame(recipient)
    link <- colnames(recipient)[match(unique(names(x)), colnames(recipient))]
    for (i in link) {
      recipient[1, i] <- sum(x[grepl(paste0("^", i, "$"), names(x))])
    }
    return(recipient)
  })
  if (length(output) == 1)
    return(output[[1]])
  else
    return(output)
}

#' Calculate number/percentage of fish at each location for each day
#'
#' @param positions a positions table, supplied by dailyPositions
#'
#' @return A list containing:
#' \itemize{
#'  \item \code{absolutes}: A data frame containing the absolute number of fish at each location per day,
#'  \item \code{percentages}: A data frame containing the percentage of fish relative to the total
#'    number of active fish at each location per day.
#' }
#'
#' @keywords internal
#'
globalRatios <- function(positions, section.order) {
  aux <- apply(positions, 1, function(x) as.data.frame(t(as.matrix(table(x[-1])))))
  the.cols <- sort(unique(unlist(lapply(aux, names))))
  the.tables <- vectorsIntoTables(input = aux, columns = the.cols)
  absolutes <- do.call(rbind.data.frame, the.tables)
  absolutes$Timeslot <- positions$Timeslot
  absolutes <- absolutes[, c(ncol(absolutes), 1:(ncol(absolutes) - 1))]
  rownames(absolutes) <- 1:nrow(absolutes)
  absolutes[is.na(absolutes)] <- 0
  if (ncol(absolutes) > 2)
    absolutes$Total <- apply(absolutes[, -1], 1, sum)
  else
    absolutes$Total <- absolutes[, 2]
  percentages <- absolutes
  percentages[, -1] <- round(percentages[, -1] / percentages[, ncol(percentages)], 3)
  return(list(absolutes = absolutes, percentages = percentages))
}
