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
#' @inheritParams migration
#' @inheritParams explore
#' 
#' @return A list containing:
#' \itemize{
#'  \item \code{detections}: All detections for each target fish;
#'  \item \code{valid.detections}: Valid detections for each target fish;
#'  \item \code{spatial}: The spatial information used during the analysis;
#'  \item \code{deployments}: The deployments of each receiver;
#'  \item \code{arrays}: The array details used during the analysis;
#'  \item \code{movements}: All movement events for each target fish;
#'  \item \code{valid.movements}: Valid movemenet events for each target fish;
#'  \item \code{section.movements}: Valid section shifts for each target fish;
#'  \item \code{status.df}: Summary information for each fish, including the
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
#'  \item \code{last.seen}: Summary table of the number of fish last seen in
#'    each study area section;
#'  \item \code{array.times}: Table containing ALL the entry times of each fish
#'    in each array;
#'  \item \code{section.times}: Table containing all the entry times of each 
#'    fish in each section;
#'  \item \code{residency.list}: Places of residency between first and last
#'    valid detection for each fish;
#'  \item \code{daily.ratios}: Daily location per section (both in seconds spent
#'    and in percentage of day) for each fish;
#'  \item \code{daily.positions}: Summary table showing the location where each
#'    fish spent the most time per day;
#'  \item \code{global.ratios}: Summary tables showing the number of active fish
#'    (and respective percentages) present at each location per day;
#'  \item \code{efficiency}: Results of the inter-array Multi-way efficiency
#'    calculations (see vignettes for more details);
#'  \item \code{intra.array.CJS}: Results of the intra-array CJS calculations;
#'  \item \code{rsp.info}: Appendix information for the RSP package;
#'  \item \code{dist.mat}: The distance matrix used in the analysis (if a valid
#'   distance matrix was supplied)
#' }
#' 
#' @seealso \code{\link{explore}}, \code{\link{migration}}
#' 
#' @export
#' 
residency <- function(path = NULL, tz, sections, success.arrays = NULL, max.interval = 60, minimum.detections = 2, 
  start.time = NULL, stop.time = NULL, speed.method = c("last to first", "first to first"), 
  speed.warning = NULL, speed.error = NULL, jump.warning = 2, jump.error = 3, 
  inactive.warning = NULL, inactive.error = NULL, exclude.tags = NULL, override = NULL, report = TRUE,
  section.minimum = 2, replicates = NULL, GUI = c("needed", "always", "never"), debug = FALSE) {
# check argument quality
  my.home <- getwd()
  if (!is.null(path) && !is.character(path))
    path <- as.character(path)
  if (is.null(tz) || is.na(match(tz, OlsonNames())))
    stop("'tz' could not be recognized as a timezone. Check available timezones with OlsonNames()\n", call. = FALSE)
  if (!is.numeric(section.minimum))
    stop("'section.minimum' must be numeric.\n", call. = FALSE)
  if (!is.numeric(minimum.detections))
    stop("'minimum.detections' must be numeric.\n", call. = FALSE)
  if (minimum.detections <= 0)
    stop("'minimum.detections' must be positive.\n", call. = FALSE)
  if (!is.numeric(max.interval))
    stop("'max.interval' must be numeric.\n", call. = FALSE)
  if (max.interval <= 0)
    stop("'max.interval' must be positive.\n", call. = FALSE)

  if (!is.character(speed.method))
    stop("'speed.method' should be one of 'first to first' or 'last to first'.\n", call. = FALSE)
  speed.method <- match.arg(speed.method)

  if (!is.null(speed.warning) && !is.numeric(speed.warning))
    stop("'speed.warning' must be numeric.\n", call. = FALSE)    
  if (!is.null(speed.warning) && speed.warning <= 0)
    stop("'speed.warning' must be positive.\n", call. = FALSE) 

  if (!is.null(speed.error) && !is.numeric(speed.error))
    stop("'speed.error' must be numeric.\n", call. = FALSE)    
  if (!is.null(speed.error) && speed.error <= 0)
    stop("'speed.error' must be positive.\n", call. = FALSE)

  if (!is.null(speed.error) & is.null(speed.warning))
    speed.warning <- speed.error
  if (!is.null(speed.error) && speed.error < speed.warning)
    stop("'speed.error' must not be lower than 'speed.warning'.\n", call. = FALSE)
  if (!is.null(speed.warning) & is.null(speed.error))
    speed.error <- Inf

  if (!is.null(start.time) && !grepl("^[1-2][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9] [0-2][0-9]:[0-5][0-9]:[0-5][0-9]", start.time))
    stop("'start.time' must be in 'yyyy-mm-dd hh:mm:ss' format.\n", call. = FALSE)
  if (!is.null(stop.time) && !grepl("^[1-2][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9] [0-2][0-9]:[0-5][0-9]:[0-5][0-9]", stop.time))
    stop("'stop.time' must be in 'yyyy-mm-dd hh:mm:ss' format.\n", call. = FALSE)
  
  if (!is.logical(report))
    stop("'report' must be logical.\n", call. = FALSE)

  if (!is.null(replicates) && !is.list(replicates))
    stop("'replicates' must be a list.\n", call. = FALSE)

  if (length(names(replicates)) != length(replicates))
    stop("All list elements within 'replicates' must be named (i.e. list(Array = 'St.1') rather than list('St.1')).\n", call. = FALSE)

  if (!is.numeric(jump.warning))
    stop("'jump.warning' must be numeric.\n", call. = FALSE)
  if (jump.warning < 1)
    stop("'jump.warning' must not be lower than 1.\n", call. = FALSE)
  if (!is.numeric(jump.error))
    stop("'jump.error' must be numeric.\n", call. = FALSE)
  if (jump.error < 1)
    stop("'jump.error' must not be lower than 1.\n", call. = FALSE)
  if (jump.error < jump.warning)
    stop("'jump.error' must not be lower than 'jump.warning'.\n", call. = FALSE)

  if (!is.null(inactive.warning) && !is.numeric(inactive.warning))
    stop("'inactive.warning' must be numeric.\n", call. = FALSE)    
  if (!is.null(inactive.warning) && inactive.warning <= 0)
    stop("'inactive.warning' must be positive.\n", call. = FALSE)

  if (!is.null(inactive.error) && !is.numeric(inactive.error))
    stop("'inactive.error' must be numeric.\n", call. = FALSE)    
  if (!is.null(inactive.error) && inactive.error <= 0)
    stop("'inactive.error' must be positive.\n", call. = FALSE)

  if (!is.null(inactive.error) & is.null(inactive.warning))
    inactive.warning <- inactive.error
  if (!is.null(inactive.error) && inactive.error < inactive.warning)
    stop("'inactive.error' must not be lower than 'inactive.warning'.\n", call. = FALSE)
  if (!is.null(inactive.warning) & is.null(inactive.error))
    inactive.error <- Inf

  if (!is.null(exclude.tags) && any(!grepl("-", exclude.tags, fixed = TRUE)))
    stop("Not all contents in 'exclude.tags' could be recognized as tags (i.e. 'codespace-signal'). Valid examples: 'R64K-1234', A69-1303-1234'\n", call. = FALSE)
  if (!is.null(override) && any(!grepl("-", override, fixed = TRUE)))
    stop("Not all contents in 'override' could be recognized as tags (i.e. 'codespace-signal'). Valid examples: 'R64K-1234', A69-1303-1234'\n", call. = FALSE)

  GUI <- checkGUI(GUI)

  if (!is.logical(debug))
    stop("'debug' must be logical.\n", call. = FALSE)
# ------------------------

# Prepare clean-up before function ends
  if (debug) {
    on.exit(save(list = ls(), file = "residency_debug.RData"), add = TRUE)
    appendTo("Screen", "!!!--- Debug mode has been activated ---!!!")
  } else {
    on.exit(deleteHelpers(), add = TRUE)
  }
  on.exit(setwd(my.home), add = TRUE)
  on.exit(tryCatch(sink(), warning = function(w) {hide <- NA}), add = TRUE)
  if (!debug)
    on.exit(deleteHelpers(), add = TRUE)
  deleteHelpers()
# --------------------------------------

# Store function call
  the.function.call <- paste0("residency(path = ", ifelse(is.null(path), "NULL", paste0("'", path, "'")), 
      ", sections = ", paste0("c('", paste(sections, collapse = "', '"), "')"), 
      ", section.minimum = ", section.minimum,
      ", minimum.detections = ", minimum.detections,
      ", max.interval = ", max.interval,
      ", speed.method = ", paste0("c('", speed.method, "')"),
      ", speed.warning = ", ifelse(is.null(speed.warning), "NULL", speed.warning), 
      ", speed.error = ", ifelse(is.null(speed.error), "NULL", speed.error), 
      ", tz = ", ifelse(is.null(tz), "NULL", paste0("'", tz, "'")), 
      ", start.time = ", ifelse(is.null(start.time), "NULL", paste0("'", start.time, "'")),
      ", stop.time = ", ifelse(is.null(stop.time), "NULL", paste0("'", stop.time, "'")),
      ", report = ", ifelse(report, "TRUE", "FALSE"), 
      ", exclude.tags = ", ifelse(is.null(exclude.tags), "NULL", paste0("c('", paste(exclude.tags, collapse = "', '"), "')")), 
      ", override = ", ifelse(is.null(override), "NULL", paste0("c('", paste(override, collapse = "', '"), "')")),
      ", replicates = ", ifelse(is.null(replicates),"NULL", paste0("c('", paste(replicates, collapse = "', '"), "')")),
      ", jump.warning = ", jump.warning,
      ", jump.error = ", jump.error,
      ", inactive.warning = ", ifelse(is.null(inactive.warning), "NULL", inactive.warning), 
      ", inactive.error = ", ifelse(is.null(inactive.error), "NULL", inactive.error), 
      ", GUI = '", GUI, "'",
      ", debug = ", ifelse(debug, "TRUE", "FALSE"), 
      ")")
# --------------------

# Final arrangements before beginning
  inst.ver <- utils::packageVersion("actel")
  inst.ver.short <- substr(inst.ver, start = 1, stop = nchar(as.character(inst.ver)) - 5) 
  appendTo("Report", paste0("Actel R package report.\nVersion: ", inst.ver.short, "\n"))
  rm(inst.ver)

  path <- checkPath(my.home = my.home, path = path)  

  if (debug)
    appendTo("Report", "!!!--- Debug mode has been activated ---!!!\n")

  appendTo(c("Report"), paste0("Target folder: ", getwd(), "\nTimestamp: ", the.time <- Sys.time(), "\nFunction: residency()\n"))

  if (!is.null(path))
    appendTo(c("Screen"), "M: Moving to selected work directory")
  
  report <- checkReport(report = report)
# -----------------------------------

# Load, structure and check the inputs
  study.data <- loadStudyData(tz = tz, override = override,
                              start.time = start.time, stop.time = stop.time,
                              sections = sections, exclude.tags = exclude.tags)
  bio <- study.data$bio
  sections <- study.data$sections
  deployments <- study.data$deployments
  spatial <- study.data$spatial
  dot <- study.data$dot
  arrays <- study.data$arrays
  dotmat <- study.data$dotmat
  paths <- study.data$paths
  detections <- study.data$detections
  dist.mat <- study.data$dist.mat
  invalid.dist <- study.data$invalid.dist
  detections.list <- study.data$detections.list

  if (!is.null(replicates) && any(is.na(match(names(replicates), names(arrays)))))
    stop("Some of the array names listed in the 'replicates' argument do not match the study's arrays.\n", call. = FALSE)

# -------------------------------------
  
# Compile array movements
  appendTo(c("Screen", "Report"), "M: Creating movement records for the valid tags.")
  movements <- groupMovements(detections.list = detections.list, bio = bio, spatial = spatial,
    speed.method = speed.method, max.interval = max.interval, tz = tz, 
    dist.mat = dist.mat, invalid.dist = invalid.dist)

  aux <- names(movements)
  movements <- lapply(names(movements), function(fish) {
      speedReleaseToFirst(fish = fish, bio = bio, movements = movements[[fish]],
                          dist.mat = dist.mat, invalid.dist = invalid.dist)
    })
  names(movements) <- aux
  rm(aux)

  appendTo(c("Screen", "Report"), "M: Checking movement events quality.")

  do.checkSpeeds <- FALSE
  if (is.null(speed.warning)) {
    appendTo(c("Screen", "Report", "Warning"), "'speed.warning'/'speed.error' were not set, skipping speed checks.")
  } else {
    if(invalid.dist) {
      appendTo(c("Screen", "Report", "Warning"), "'speed.warning'/'speed.error' were set, but a valid distance matrix is not present. Aborting speed checks.")
    } else {
      do.checkSpeeds <- TRUE
    }
  }

  do.checkInactiveness <- FALSE
  if (is.null(inactive.warning)) {
    appendTo(c("Screen", "Report", "Warning"), "'inactive.warning'/'inactive.error' were not set, skipping inactivity checks.")
  } else {
    if (invalid.dist)
      appendTo(c("Report", "Screen", "Warning"), "Running inactiveness checks without a distance matrix. Performance may be limited.")
    do.checkInactiveness <- TRUE
  }

  movement.names <- names(movements)
  
  if (any(link <- !override %in% movement.names)) {
    appendTo(c("Screen", "Warning", "Report"), paste0("Override has been triggered for fish ", paste(override[link], collapse = ", "), " but ", 
      ifelse(sum(link) == 1, "this", "these"), " fish ", ifelse(sum(link) == 1, "was", "were")," not detected."))
    override <- override[!link]
  }

  movements <- lapply(seq_along(movements), function(i) {
    fish <- names(movements)[i]
    appendTo("debug", paste0("debug: Checking movement quality for fish ", fish,"."))
    
    if (is.na(match(fish, override))) {
      release <- as.character(bio$Release.site[na.as.false(bio$Transmitter == fish)])
      release <- unlist(strsplit(with(spatial, release.sites[release.sites$Standard.name == release, "Array"]), "|", fixed = TRUE))

      output <- checkMinimumN(movements = movements[[i]], fish = fish, minimum.detections = minimum.detections)
    
      output <- checkImpassables(movements = output, fish = fish, dotmat = dotmat, GUI = GUI)

      output <- checkJumpDistance(movements = output, release = release, fish = fish, dotmat = dotmat, 
                                  jump.warning = jump.warning, jump.error = jump.error, GUI = GUI)

      if (do.checkSpeeds) {
        temp.valid.movements <- simplifyMovements(movements = output, fish = fish, bio = bio, 
          speed.method = speed.method, dist.mat = dist.mat, invalid.dist = invalid.dist)
        output <- checkSpeeds(movements = output, fish = fish, valid.movements = temp.valid.movements, 
          speed.warning = speed.warning, speed.error = speed.error, GUI = GUI)
        rm(temp.valid.movements)
      }

      if (do.checkInactiveness) {
        output <- checkInactiveness(movements = output, fish = fish, detections.list = detections.list[[fish]], 
          inactive.warning = inactive.warning, inactive.error = inactive.error, 
          dist.mat = dist.mat, invalid.dist = invalid.dist, GUI = GUI)
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
    aux <- sectionMovements(movements = movements[[i]], sections = sections, invalid.dist = invalid.dist)
    output <- checkSMovesN(secmoves = aux, fish = fish, section.minimum = section.minimum, GUI = GUI)
    return(output)
  })
  names(section.movements) <- names(movements)

  # Update array movements based on section movements validity
  movements <- updateValidity(arrmoves = movements, secmoves = section.movements)

  # compile valid movements
  appendTo(c("Screen", "Report"), "M: Filtering valid array and section movements.")

  valid.movements <- lapply(seq_along(movements), function(i){
    output <- simplifyMovements(movements = movements[[i]], fish = names(movements)[i], bio = bio, 
      speed.method = speed.method, dist.mat = dist.mat, invalid.dist = invalid.dist)
  })
  names(valid.movements) <- names(movements)
  valid.movements <- valid.movements[!unlist(lapply(valid.movements, is.null))]

  section.movements <- lapply(seq_along(valid.movements), function(i) {
    fish <- names(valid.movements)[i]
    appendTo("debug", paste0("debug: Compiling valid section movements for fish ", fish,"."))
    output <- sectionMovements(movements = valid.movements[[i]], sections = sections, invalid.dist = invalid.dist)
    return(output)
  })
  names(section.movements) <- names(valid.movements)

  # Grab summary information
  res.df <- assembleResidency(secmoves = section.movements, movements = movements, sections = sections)
  
  appendTo(c("Screen", "Report"), "M: Timetable successfully filled. Fitting in the remaining variables.")
  
  status.df <- res_assembleOutput(res.df = res.df, bio = bio, spatial = spatial, 
                                  sections = sections, tz = tz)

  last.seen <- as.data.frame.matrix(with(status.df, table(Group, Status)))

  array.times <- getTimes(movements = valid.movements, spatial = spatial, type = "arrival", events = "all")

  section.times <- list(
    arrival = getTimes(movements = section.movements, spatial = spatial, type = "arrival", events = "all"),
    departure = getTimes(movements = section.movements, spatial = spatial, type = "departure", events = "all"))

  residency.list <- getResidency(movements = section.movements, spatial = spatial)

  if (length(residency.list) == 0) {
    emergencyBreak()
    stop("No fish have enough data for residency analysis. Consider running explore() instead.\n", call. = FALSE)
  }

  appendTo(c("Screen", "Report"), "M: Calculating daily locations for each fish.")

  daily.ratios <- dailyRatios(res = residency.list)

  daily.positions <- dailyPositions(ratios = daily.ratios)

  global.ratios <- globalRatios(positions = daily.positions)

  appendTo("Screen", "M: Validating detections...")

  recipient <- validateDetections(detections.list = detections.list, movements = valid.movements)
  detections <- recipient$detections
  valid.detections <- recipient$valid.detections
  rm(recipient)

# ---------------
  
# Efficiency
  appendTo(c("Screen", "Report"), "M: Calculating array efficiency.")
  efficiency <- res_efficiency(arrmoves = valid.movements, bio = bio, spatial = spatial, arrays = arrays, paths = paths, dotmat = dotmat)
  if (!is.null(replicates)) {
    intra.array.matrices <- getDualMatrices(replicates = replicates, CJS = NULL, spatial = spatial, detections.list = valid.detections)
    recipient <- includeIntraArrayEstimates(m = intra.array.matrices, efficiency = efficiency, CJS = NULL)
    efficiency <- recipient[[1]]
    intra.array.CJS <- recipient[[2]]
    rm(recipient)
  } else {
    intra.array.CJS <- NULL
  }

# ----------

  
  appendTo("Report", "M: Process finished successfully.")
# ---------------

# wrap up in-R objects
  deployments <- do.call(rbind.data.frame, deployments)

  if (!debug)
    efficiency <- efficiency[1:3]

  # extra info for potential RSP analysis
  rsp.info <- list(analysis.type = "residency", analysis.time = the.time, bio = bio, tz = tz, actel.version = inst.ver.short)

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
    appendTo("Screen", paste0("M: An actel residency results file is already present in the current directory.\n   Saving new results as '", resultsname,"'."))
    rm(continue, index)
  } else {
    appendTo(c("Screen", "Report"), paste0("M: Saving results to '", resultsname, "'."))
  }

  if (invalid.dist)
    save(detections, valid.detections, spatial, deployments, arrays, movements, valid.movements, 
      section.movements, status.df, last.seen, array.times, section.times,
      residency.list, daily.ratios, daily.positions, global.ratios, efficiency, intra.array.CJS, rsp.info, file = resultsname)
  else
    save(detections, valid.detections, spatial, deployments, arrays, movements, valid.movements, 
      section.movements, status.df, last.seen, array.times, section.times,
      residency.list, daily.ratios, daily.positions, global.ratios, efficiency, intra.array.CJS, rsp.info, dist.mat, file = resultsname)
# ------------

# Print graphics
  if (report) {
    appendTo(c("Screen", "Report"), "M: Producing the report.")
    biometric.fragment <- printBiometrics(bio = bio)
    printDot(dot = dot, sections = sections, spatial = spatial)
    printSectionTimes(section.times = section.times, bio = bio, detections = valid.detections)
    printGlobalRatios(global.ratios = global.ratios, daily.ratios = daily.ratios, sections = sections)
    individual.detection.plots <- printIndividuals(detections.list = detections, bio = bio, 
        tz = tz, spatial = spatial, movements = movements, valid.movements = valid.movements)
    array.circular.plots <- printCircular(times = convertTimesToCircular(array.times), bio = bio, suffix = "_array")
    section.arrival.circular.plots <- printCircular(times = convertTimesToCircular(section.times$arrival), bio = bio, suffix = "_array")
    section.departure.circular.plots <- printCircular(times = convertTimesToCircular(section.times$departure), bio = bio, suffix = "_array")
    appendTo(c("Screen", "Report"), "M: Drawing individual residency graphics.")
    dayrange <- range(as.Date(global.ratios[[1]]$Date))
    dayrange[1] <- dayrange[1] - 1
    dayrange[2] <- dayrange[2] + 1
    individual.residency.plots <- printIndividualResidency(ratios = daily.ratios, dayrange = dayrange, sections = sections)
    efficiency.fragment <- printEfficiency(intra.CJS = intra.array.CJS, type = "residency")
    printLastSeen(input = last.seen, sections = sections)
    if (nrow(last.seen) > 3) 
      last.seen.graph.size <- "width=90%" else last.seen.graph.size <- "height=4in"
  }
  
  appendTo("Report", "M: Process finished successfully.")
# ---------------

# wrap up the txt report
  appendTo("Report", "\n-------------------")
  if (file.exists("temp_UD.txt")) 
    appendTo("Report", paste0("User interventions:\n-------------------\n", gsub("\r", "", readr::read_file("temp_UD.txt")), "-------------------")) # nocov
  
  appendTo("Report", paste0("Function call:\n-------------------\n", the.function.call, "\n-------------------"))
# ------------------

# print html report
  if (report) {
    appendTo("debug", "debug: Printing report")
    rmarkdown::render(
      reportname <- printResidencyRmd(override.fragment = override.fragment, 
                                      biometric.fragment = biometric.fragment, 
                                      efficiency.fragment = efficiency.fragment, 
                                      individual.detection.plots = individual.detection.plots, 
                                      individual.residency.plots = individual.residency.plots, 
                                      array.circular.plots = array.circular.plots, 
                                      section.arrival.circular.plots = section.arrival.circular.plots, 
                                      section.departure.circular.plots = section.departure.circular.plots, 
                                      spatial = spatial, 
                                      deployments = deployments, 
                                      detections = detections, 
                                      valid.detections = valid.detections, 
                                      last.seen = last.seen, 
                                      last.seen.graph.size = last.seen.graph.size), 
      quiet = TRUE)
    appendTo("debug", "debug: Moving report")
    fs::file_move(sub("Rmd", "html", reportname), sub("Report/", "", sub("Rmd", "html", reportname)))
    if (interactive()) { # nocov start
      appendTo("debug", "debug: Opening report.")
      browseURL(sub("Report/", "", sub("Rmd", "html", reportname)))
    } # nocov end
    appendTo("debug", "debug: Removing toc_menu_explore.html")
    if(file.exists("Report/toc_menu_explore.html"))
      file.remove("Report/toc_menu_explore.html")
  }
  appendTo("Screen", "M: Process finished successfully.")
# ------------------

  jobname <- paste0(gsub(" |:", ".", as.character(Sys.time())), ".actel.log.txt")
  appendTo("Screen", paste0("M: Saving job log as '",jobname, "'."))
  file.rename("temp_log.txt", jobname)
  
  if (!debug)
    deleteHelpers()

  if (invalid.dist)
    return(list(detections = detections, valid.detections = valid.detections, spatial = spatial, deployments = deployments, arrays = arrays,
      movements = movements, valid.movements = valid.movements, section.movements = section.movements,
      status.df = status.df, efficiency = efficiency, intra.array.CJS = intra.array.CJS, array.times = array.times, section.times = section.times, 
      residency.list = residency.list, daily.ratios = daily.ratios, daily.positions = daily.positions, global.ratios = global.ratios, last.seen = last.seen, rsp.info = rsp.info))
  else
    return(list(detections = detections, valid.detections = valid.detections, spatial = spatial, deployments = deployments, arrays = arrays,
      movements = movements, valid.movements = valid.movements, section.movements = section.movements,
      status.df = status.df, efficiency = efficiency, intra.array.CJS = intra.array.CJS, array.times = array.times, section.times = section.times, 
      residency.list = residency.list, daily.ratios = daily.ratios, daily.positions = daily.positions, global.ratios = global.ratios, last.seen = last.seen, rsp.info = rsp.info, dist.mat = dist.mat))
}

#' Print Rmd report
#'
#' Creates a Rmd report and converts it to hmtl.
#' 
#' @inheritParams printMigrationRmd
#' @inheritParams loadDetections
#' 
#' @keywords internal
#' 
printResidencyRmd <- function(override.fragment, biometric.fragment, efficiency.fragment,
  individual.detection.plots, individual.residency.plots, array.circular.plots, 
  section.arrival.circular.plots, section.departure.circular.plots, spatial, 
  deployments, detections, valid.detections, last.seen, last.seen.graph.size){
  inst.ver <- utils::packageVersion("actel")
  inst.ver.short <- substr(inst.ver, start = 1, stop = nchar(as.character(inst.ver)) - 5) 
  if (file.exists(reportname <- "Report/actel_residency_report.Rmd")) {
    continue <- TRUE
    index <- 1
    while (continue) {
      if(file.exists(reportname <- paste0("Report/actel_residency_report.", index, ".Rmd"))) {
        index <- index + 1
      } else {
        continue <- FALSE
      }
    }
    appendTo("Screen", paste0("M: An actel report is already present in the current directory\n   Saving new report as 'actel_residency_report.", index, ".html'."))
    rm(continue,index)
  } else {
    appendTo("Screen", "M: Saving actel report as 'actel_residency_report.html'.")
  }
  if (any(grepl("Unknown", spatial$stations$Standard.name))) {
    unknown.fragment <- paste0('<span style="color:red"> Number of relevant unknown receivers: **', sum(grepl("Unknown", spatial$stations$Standard.name)), '**</span>\n')
  } else {
    unknown.fragment <- ""
  } 
  report <- readr::read_file("temp_log.txt")

  options(knitr.kable.NA = "-")

  sink(reportname)
  cat(paste0(
'---
title: "Acoustic telemetry residency analysis"
author: "Actel R package (', inst.ver.short, ')"
output: 
  html_document:
    includes:
      after_body: toc_menu_residency.html
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

### Study area

Arrays with the same background belong to the same section. Release sites are marked with "R.S.". Arrays connected with an arrow indicate that the fish can only pass in one direction.

<img src="mb_arrays.svg" alt="Missing file" style="padding-top: 15px;"/>

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
if(file.exists("../temp_warnings.txt")) cat(gsub("\\r", "", readr::read_file("../temp_warnings.txt"))) else cat("No warnings were raised during the analysis.")
```

### User comments

Note:
  : Comments are also stored in the `status.df` object.

```{r comments, echo = FALSE, comment = NA}
 if(file.exists("../temp_comments.txt")) cat(gsub("\\r", "", readr::read_file("../temp_comments.txt"))) else cat("No comments were included during the analysis.")
```

### Biometric graphics

Note:
  : The data used in this graphic is the data present in the biometrics.csv file.

<center>
', biometric.fragment,'
</center>


### Last seen

Note:
  : The data used in this table and graphic is stored in the `last.seen` object.

', paste(knitr::kable(last.seen), collapse = "\n"), '

<center>
![](last_seen.png){ ',last.seen.graph.size ,' }
</center>


### Average time of arrival at each Array

Note:
  : Coloured lines on the outer circle indicate the mean value for each group and the respective ranges show the standard error of the mean. Each group\'s bars sum to 100%. The number of data points in each group is presented between brackets in the legend of each pannel. 
  : These graphics have been saved in vectorial format (svg) in the "Results" folder, so you may edit them as needed.
  : The data used in these graphics is stored in the `array.times` object.

<center>
', array.circular.plots,'
</center>

### Time details for each section

#### Arrival days at each section

Note:
  : The data used in these graphics is stored in the `section.times$arrival` object.

<center>
![](arrival_days.png){ width=95% }
</center>

#### Arrival times at each section

Note:
  : Coloured lines on the outer circle indicate the mean value for each group and the respective ranges show the standard error of the mean. Each group\'s bars sum to 100%. The number of data points in each group is presented between brackets in the legend of each pannel. 
  : These graphics have been saved in vectorial format (svg) in the "Results" folder, so you may edit them as needed.
  : The data used in these graphics is stored in the `section.times$arrival` object.

<center>
', section.arrival.circular.plots,'
</center>

#### Departure days at each section

Note:
  : The data used in these graphics is stored in the `section.times$departure` object.

<center>
![](departure_days.png){ width=95% }
</center>

#### Departure times at each section

Note:
  : Coloured lines on the outer circle indicate the mean value for each group and the respective ranges show the standard error of the mean. Each group\'s bars sum to 100%. The number of data points in each group is presented between brackets in the legend of each pannel. 
  : These graphics have been saved in vectorial format (svg) in the "Results" folder, so you may edit them as needed.
  : The data used in these graphics is stored in the `section.times$departure` object.

<center>
', section.departure.circular.plots,'
</center>

### Section progression

Note:
  : These graphics have been saved in vectorial format (svg) in the "Results" folder, so you may edit them as needed.
  : The data used in these graphics is stored in the `global.ratios` and `daily.positions` objects.

#### Absolutes

<center>
![](global_ratios_absolutes.png){ width=95% }
</center>


#### Percentages

<center>
![](global_ratios_percentages.png){ width=95% }
</center>


### Individual residency plots

Note:
  : The data used in these graphics is stored in the `daily.ratios` object (one table per fish). More condensed information can be found in the `section.movements` object.

<center>
', individual.residency.plots,'
</center>

### Individual detection plots

Note:
  : The detections are coloured by array. The vertical black dashed line shows the time of release. The vertical grey dashed lines show the assigned moments of entry and exit for each study area section. The full dark-grey line shows the movement events considered valid, while the dashed dark-grey line shows the movement events considered invalid.
  : The movement event lines move straight between the first and last station of each event (i.e. in-between detections will not be individually linked by the line).
  : Manually **edited** fish are highlighted with **yellow** graphic borders.
  : The stations have been grouped by array, following the array order provided either in the spatial.csv file or in the spatial.txt file.
  : The data used in these graphics is stored in the `detections` and `movements` objects (and respective valid counterparts).

<center>
', individual.detection.plots,'
</center>

### Full log

```{r log, echo = FALSE, comment = NA}
cat(gsub("\\r", "", readr::read_file("../temp_log.txt")))
```

'), fill = TRUE)
sink()

if(file.exists("Report/toc_menu_residency.html"))
  file.remove("Report/toc_menu_residency.html")
sink("Report/toc_menu_residency.html")
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
  <a href="#user-comments">Comments</a>
  <a href="#biometric-graphics">Biometrics</a>
  <a href="#last-seen">Last seen</a>
  <a href="#average-time-of-arrival-at-each-array">Arrival days</a>
  <a href="#time-details-for-each-section">Section times</a>
  <a href="#section-progression">Section progression</a>
  <a href="#individual-residency-plots">Individual residency</a>
  <a href="#individual-detection-plots">Individual detections</a>
  <a href="#full-log">Full log</a>
</div>
', fill = TRUE)
sink()
return(reportname)
}


#' Collect summary information for the residency analysis
#' 
#' @param secmoves the section-movements
#' @param movements the array-movements (Valid and invalid)
#' @inheritParams migration
#' 
#' @return A residency summary table
#' 
#' @keywords internal
#' 
assembleResidency <- function(secmoves, movements, sections) {
  Last.array <- NULL
  Last.time <- NULL
  Valid <- NULL
  Section <- NULL
  
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
  		if (aux > 86400)
  			units(res.df[, paste0(the.col, section)]) <- "days"
  		if (aux <= 86400 & aux > 3600)
  			units(res.df[, paste0(the.col, section)]) <- "hours"
  		if (aux <= 3600)
  			units(res.df[, paste0(the.col, section)]) <- "mins"
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
res_assembleOutput <- function(res.df, bio, spatial, sections, tz) {
  appendTo("debug", "Merging 'bio' and 'res.df'.")
  status.df <- merge(bio, res.df, by = "Transmitter", all = TRUE)
  
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
#' @return An efficiency list, containing a table of absolutes, min and max efficiency 
#' and detailed information of where the arrays failed.
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

  # # do not calculate efficiency for arrays without peers
  # aux <- unlist(lapply(arrays, function(x) is.null(x$after.peers) & is.null(x$before.peers)))
  # no.peers <- names(aux)[aux]
  # absolutes[2, match(no.peers, colnames(absolutes))] <- NA

  # do not calculate efficiency for arrays without before or after neighbours
  aux <- unlist(lapply(arrays, function(x) is.null(x$after) | is.null(x$before)))
  no.neighbours <- names(aux)[aux]
  # exclude arrays which are connected to a release site from the list above
  no.neighbours <- no.neighbours[!no.neighbours %in% spatial$release.sites$Array]
  absolutes[2, match(no.neighbours, colnames(absolutes))] <- NA
  absolutes[3, match(no.neighbours, colnames(absolutes))] <- NA

  max.efficiency <- apply(absolutes, 2, function(x) 1 - (x[2] / sum(x)))
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
#' @return NULL if no arrays failed, or a list of arrays which failed.
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
#' @return a list of residency tables
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

#' calculate daily ratios per fish
#' 
#' @param res a residency list
#' 
#' @return the daily ratios
#' 
#' @keywords internal
#' 
dailyRatios <- function(res) {
  counter <- 0
  pb <- txtProgressBar(min = 0, max = length(res), style = 3, width = 60)
  output <- lapply(res, function(x) {
    counter <<- counter + 1
    # cat("\n", counter, "\n")
    dayrange <- seq(from = round.POSIXt(x$First.time[1] - 43200, units = "days"), 
      to =  round.POSIXt(x$Last.time[nrow(x)] - 43200, units = "days"), by = 86400)
    days.list <- lapply(dayrange, function(d) {
      # cat(as.character(d), "\n")
      findSecondsPerSection(res = x, day = d, the.range = range(dayrange))
    })
    setTxtProgressBar(pb, counter)
    names(days.list) <- round.POSIXt(dayrange, units = "days")
    dailyRatiosIndOut(input = days.list)
  })
  close(pb)
  return(output)
}

#' Calculate number of seconds at each location per day
#' 
#' @inheritParams dailyRatios
#' @param day the day being analysed (a Date object)
#' @param the.range the first and last day for the specific fish
#' 
#' @return the number of seconds spent at each location
#' 
#' @keywords internal
#' 
findSecondsPerSection <- function(res, day, the.range) {
  aux <- c()
  candidate.events <- which(res$First.time <= day + 86400)
  potential.events <- which(res$First.time[candidate.events] >= day)
  # IF potential events returns 0, then the only valid event is the very last candidate
  if (length(potential.events) == 0)
    e <- tail(candidate.events, 1)
  else
    e <- potential.events
  # IF there is only one event
  if (length(e) == 1) {
    # and that event is the very first one
    if (e == 1) {
      # Exception for if the fish was only detected one day, calculates difference between event start and stop.
      if (the.range[2] == the.range[1]) {
        aux[length(aux) + 1] <- difftime(res$Last.time[e], res$First.time[e], units = "secs")
        names(aux)[length(aux)] <- res$Section[e]        
        aux[length(aux) + 1] <- 0
        names(aux)[length(aux)] <- "Changes"
      } else {
        # Exception for if the day being analysed is the last day, calculates difference between day start and event stop.
        if (day == the.range[2]) {
          aux[length(aux) + 1] <- difftime(res$Last.time[e], day, units = "secs")
          names(aux)[length(aux)] <- res$Section[e]
        # Otherwise calculate difference between event start and day end, and trim at a full day.
        } else {
          aux[length(aux) + 1] <- min(86400, as.numeric(difftime(day + 86400, res$First.time[e], units = "secs")))
          names(aux)[length(aux)] <- res$Section[e]
        }
        # Regardless of path, if length(e) == 1 and the event is the first one, the number of changes is 0
        aux[length(aux) + 1] <- 0
        names(aux)[length(aux)] <- "Changes"
      }
    # If there is only one event but that event is not the very first one
    } else {
      # If the start time of the event is previous to the day itself
      if (res$First.time[e] < day) {
        # Exception for the last day, calculates difference between day start and event stop.
        if (day == the.range[2]) {
          aux[length(aux) + 1] <- difftime(res$Last.time[e], day, units = "secs")
          names(aux)[length(aux)] <- res$Section[e]
        # otherwise, since there are no more 'e', the fish has stayed the whole day in the event
        } else {
          aux[length(aux) + 1] <- 86400
          names(aux)[length(aux)] <- res$Section[e]        
        }
        # In either case above the fish spends the whole time in the event, so changes = 0
        aux[length(aux) + 1] <- 0
        names(aux)[length(aux)] <- "Changes"
      # If the start time of the event is already in the day itself
      } else {
        # Start by storing the time spent in previous event (i.e. difference between day start and event start)
        aux[length(aux) + 1] <- difftime(res$First.time[e], day, units = "secs")
        names(aux)[length(aux)] <- res$Section[e - 1]
        # Then, since there are no more events, calculate difference between event start and day end.
        aux[length(aux) + 1] <- difftime(day + 86400, res$First.time[e], units = "secs")
        names(aux)[length(aux)] <- res$Section[e]
        # Since the day is split in two events, there is 1 change
        aux[length(aux) + 1] <- 1
        names(aux)[length(aux)] <- "Changes"
      }
    }
  # IF there is more than one 'e'
  } else {
    n <- length(e)
    # calculate difference between day start and first time
    aux[length(aux) + 1] <- difftime(res$First.time[e[1]], day, units = "secs")
    # Exception for if the day being analysed is the first day (in which case this fragment should be excluded at the end)
    if (day == the.range[1])
      names(aux)[length(aux)] <- "TO.EXCLUDE"
    else
      names(aux)[length(aux)] <- res$Section[e[1] - 1]
    # For all events except the last, calculate the difference between first and last time of the event.
    for (i in e[-n]) {
      aux[length(aux) + 1] <- difftime(res$Last.time[i], res$First.time[i], units = "secs")
      names(aux)[length(aux)] <- res$Section[i]
    }
    # For the very last day
    # exception for if the day being analysed is the last day, where the behaviour should be the same as the above
    if (day == the.range[2]) {
      aux[length(aux) + 1] <- difftime(res$Last.time[e[n]], res$First.time[e[n]], units = "secs")
      names(aux)[length(aux)] <- res$Section[e[n]]
    # otherwise, remove the seconds already accounted for to a full day, and store the result
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
#' 
#' @return the daily ratios table for the fish
#' 
#' @keywords internal
#' 
dailyRatiosIndOut <- function(input) {
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
    output$Date <- names(input)
    output <- output[, c(ncol(output), 1:(ncol(output) - 1))]
  } else {
    output <- as.data.frame(data.table::rbindlist(days.tables, idcol = "Date"))    
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
#' @return a data frame with the transmitters as columns and the days as rows
#' 
#' @keywords internal
#' 
dailyPositions <- function(ratios) {
  aux <- range(unlist(lapply(ratios, function(x) x$Date)))
  dayrange <- seq(from = as.Date(aux[1]), to = as.Date(aux[2]), by = 1) 
  output <- matrix(ncol=length(ratios), nrow = length(dayrange)) 
  rownames(output) <- as.character(dayrange)
  colnames(output) <- names(ratios)
  capture <- lapply(names(ratios), function(i) {
    link <- match(substring(ratios[[i]]$Date, 1, 10), rownames(output))
    if (any(is.na(link)))
      stop("Something went wrong when creating the recipient for the global ratios.")
    output[link, i] <<- ratios[[i]]$Most.time
  })
  return(output)
}

#' Transform vectors into data frams with specific columns
#' 
#' Used to prepare lists of vectors with partially matching names for combination with do.call(rbind.data.frame, x)
#' 
#' @param input the list of vectors
#' @param columns the columns that should be present in every element
#' 
#' @return a list of tables with matching columns
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
#' @return A list with 1) a table containing the absolute number of fish at each location per day,
#'  and 2) the respective percentage table.
#' 
#' @keywords internal
#' 
globalRatios <- function(positions) {
  aux <- apply(positions, 1, function(x) as.data.frame(t(as.matrix(table(x)))))
  the.cols <- sort(unique(unlist(lapply(aux, names))))
  the.tables <- vectorsIntoTables(input = aux, columns = the.cols)
  absolutes <- do.call(rbind.data.frame, the.tables)
  absolutes$Date <- rownames(absolutes)
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
