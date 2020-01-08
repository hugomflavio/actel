#' Migration Analysis
#' 
#' The \code{migration} analysis runs the same initial checks as \code{explore},
#' but on top of it, it analyses the fish behaviour. By selecting the arrays
#' that lead to success, you can define whether or not your fish survived the
#' migration. Additional plots help you find out if some fish has been acting
#' odd. Multiple options allow you to tweak the analysis to fit your study
#' perfectly.
#' 
#' @param sections The sections in which the study area is divided. Must be 
#'  coincident with the names given to the arrays. See the vignettes for more
#'  details.
#' @param success.arrays The arrays that mark the end of the study area. If a 
#'  fish crosses one of these arrays, it is considered to have successfully 
#'  migrated through the study area.
#' @param if.last.skip.section Logical: Should a fish detected at the last array
#'  of a given section be considered to have disappeared in the next section?
#' @param disregard.parallels Logical:  Should the presence of parallel arrays
#'  invalidate potential efficiency peers? See the vignettes for more details.
#' @param replicates A list containing, for each array to which intra-array
#'  efficiency is to be calculated: The standard names of the stations to be 
#'  used as a replicate. See the vignettes for more details.
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
#'      \item \emph{Time.until.\[section\]}: Time spent between leaving one
#'        section and reaching the next.
#'      \item \emph{Speed.to.\[section\]}: Average speed from one section to the
#'        next (if a distance matrix is provided)
#'      \item \emph{First.station.\[section\]}: Standard name of the first station
#'        where the fish was detected in a given section
#'      \item \emph{Arrived.\[section\]}: Arrival time at a given section
#'      \item \emph{Time.in.\[section\]}: Total time spent within a given section
#'      \item \emph{Last.station.\[section\]}: Standard name of the last station
#'        where the fish was detected in a given section
#'      \item \emph{Left.\[section\]}: Departure time from a givem section
#'      \item \emph{Very.last.array}: Last array where the fish was detected
#'      \item \emph{Status}: Fate assigned to the fish
#'      \item \emph{Valid.detections}: Number of valid detections
#'      \item \emph{Invalid.detections}: Number of invalid detections
#'      \item \emph{Backwards.movements}: Number of backward movement events
#'      \item \emph{Max.cons.back.moves}: Longest successive backwards movements
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
#'  \item \code{section.overview}: Summary table of the number of fish that 
#'    disappeared in each section;
#'  \item \code{array.overview}: Number of known and estimated fish to have
#'    passed through each array;
#'  \item \code{matrices}: CJS matrices used for the efficiency calculations;
#'  \item \code{overall.CJS}: Results of the inter-array CJS calculations;
#'  \item \code{intra.array.CJS}: Results of the intra-array CJS calculations;
#'  \item \code{times}: All arrival times (per fish) at each array;
#'  \item \code{rsp.info}: Appendix information for the RSP package;
#'  \item \code{dist.mat}: The distance matrix used in the analysis (if a valid
#'   distance matrix was supplied)
#' }
#'
#' @seealso \code{\link{explore}}, \code{\link{residency}}

#' @export
#' 
migration <- function(path = NULL, tz, sections, success.arrays = NULL, max.interval = 60, minimum.detections = 2, 
  start.time = NULL, stop.time = NULL, speed.method = c("last to first", "first to first"), 
  speed.warning = NULL, speed.error = NULL, jump.warning = 2, jump.error = 3, 
  inactive.warning = NULL, inactive.error = NULL, exclude.tags = NULL, override = NULL, report = TRUE,
  if.last.skip.section = TRUE, replicates = NULL, disregard.parallels = TRUE, debug = FALSE,
  maximum.time = 60, tz.study.area = NULL, start.timestamp = NULL, end.timestamp = NULL) {

# Temporary: check deprecated options
  dep.warning <- "------------------------------------------------------------------\n!!! Deprecated arguments used!\n!!!\n"
  trigger.dep <- FALSE
  if (maximum.time != 60) {
    dep.warning <- paste0(dep.warning, "!!! 'maximum.time' is now 'max.interval'\n")
    max.interval <- maximum.time
    trigger.dep <- TRUE
  }
  if (!is.null(tz.study.area)) {
    dep.warning <- paste0(dep.warning, "!!! 'tz.study.area' is now 'tz'\n")
    tz <- tz.study.area
    trigger.dep <- TRUE
  }
  if (!is.null(start.timestamp)) {
    dep.warning <- paste0(dep.warning, "!!! 'start.timestamp' is now 'start.time'\n")
    start.time <- start.timestamp
    trigger.dep <- TRUE
  }
  if (!is.null(end.timestamp)) {
    dep.warning <- paste0(dep.warning, "!!! 'end.timestamp' is now 'stop.time'\n")
    stop.time <- end.timestamp
    trigger.dep <- TRUE
  }
  if (trigger.dep)
    warning(paste0("\n", dep.warning, "!!!\n!!! Please switch to the new arguments as soon as possible.\n!!! The deprecated arguments will stop working in future versions.\n------------------------------------------------------------------"),
      immediate. = TRUE, call. = FALSE)
  rm(maximum.time, tz.study.area, start.timestamp, end.timestamp)
  
# check argument quality
  my.home <- getwd()
  if (is.null(tz) || is.na(match(tz, OlsonNames())))
    stop("'tz' could not be recognized as a timezone. Check available timezones with OlsonNames()\n", call. = FALSE)
  if (!is.numeric(minimum.detections))
    stop("'minimum.detections' must be numeric.\n", call. = FALSE)
  if (!is.numeric(max.interval))
    stop("'max.interval' must be numeric.\n", call. = FALSE)

  speed.method <- match.arg(speed.method)
  if (!is.null(speed.warning) && !is.numeric(speed.warning))
    stop("'speed.warning' must be numeric.\n", call. = FALSE)    
  if (!is.null(speed.error) && !is.numeric(speed.error))
    stop("'speed.error' must be numeric.\n", call. = FALSE)    
  if (!is.null(speed.error) & is.null(speed.warning))
    speed.warning <- speed.error
  if (!is.null(speed.error) && speed.error < speed.warning)
    stop("'speed.error' must not be lower than 'speed.warning'.\n", call. = FALSE)
  if (!is.null(speed.warning) & is.null(speed.error))
    speed.error <- Inf

  if (!is.logical(if.last.skip.section))
    stop("'if.last.skip.section' must be logical.\n", call. = FALSE)

  if (!is.null(start.time) && !grepl("^[1-2][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9] [0-2][0-9]:[0-5][0-9]:[0-5][0-9]", start.time))
    stop("'start.time' must be in 'yyyy-mm-dd hh:mm:ss' format.\n", call. = FALSE)
  if (!is.null(stop.time) && !grepl("^[1-2][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9] [0-2][0-9]:[0-5][0-9]:[0-5][0-9]", stop.time))
    stop("'stop.time' must be in 'yyyy-mm-dd hh:mm:ss' format.\n", call. = FALSE)

  if (!is.logical(report))
    stop("'report' must be logical.\n", call. = FALSE)

  if (!is.null(replicates) && !is.list(replicates))
    stop("'replicates' must be a list.\n", call. = FALSE)

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
  if (!is.null(jump.warning) & is.null(jump.error))
    jump.error <- Inf
  
  if (!is.null(inactive.warning) && !is.numeric(inactive.warning))
    stop("'inactive.warning' must be numeric.\n", call. = FALSE)    
  if (!is.null(inactive.error) && !is.numeric(inactive.error))
    stop("'inactive.error' must be numeric.\n", call. = FALSE)    
  if (!is.null(inactive.error) & is.null(inactive.warning))
    inactive.warning <- inactive.error
  if (!is.null(inactive.error) && inactive.error < inactive.warning)
    stop("'inactive.error' must not be lower than 'inactive.warning'.\n", call. = FALSE)
  if (!is.null(inactive.warning) & is.null(inactive.error))
    inactive.error <- Inf
  
  if (!is.logical(debug))
    stop("'debug' must be logical.\n", call. = FALSE)
# ------------------------

# Prepare clean-up before function ends
  if (debug) {
    on.exit(save(list = ls(), file = "migration_debug.RData"), add = TRUE)
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
  the.function.call <- paste0("migration(path = ", ifelse(is.null(path), "NULL", paste0("'", path, "'")), 
      ", sections = ", paste0("c('", paste(sections, collapse = "', '"), "')"), 
      ", success.arrays = ", paste0("c('", paste(success.arrays, collapse = "', '"), "')"), 
      ", minimum.detections = ", minimum.detections,
      ", max.interval = ", max.interval,
      ", speed.method = ", paste0("c('", speed.method, "')"),
      ", speed.warning = ", ifelse(is.null(speed.warning), "NULL", speed.warning), 
      ", speed.error = ", ifelse(is.null(speed.error), "NULL", speed.error), 
      ", if.last.skip.section = ", ifelse(if.last.skip.section, "TRUE", "FALSE"),
      ", tz = ", ifelse(is.null(tz), "NULL", paste0("'", tz, "'")), 
      ", start.time = ", ifelse(is.null(start.time), "NULL", paste0("'", start.time, "'")),
      ", stop.time = ", ifelse(is.null(stop.time), "NULL", paste0("'", stop.time, "'")),
      ", report = ", ifelse(report, "TRUE", "FALSE"), 
      ", override = ", ifelse(is.null(override), "NULL", paste0("c('", paste(override, collapse = "', '"), "')")),
      ", exclude.tags = ", ifelse(is.null(exclude.tags), "NULL", paste0("c('", paste(exclude.tags, collapse = "', '"), "')")), 
      ", replicates = ", ifelse(is.null(replicates),"NULL", paste0("c('", paste(replicates, collapse = "', '"), "')")),
      ", disregard.parallels = ", ifelse(disregard.parallels, "TRUE", "FALSE"), 
      ", jump.warning = ", jump.warning,
      ", jump.error = ", jump.error,
      ", inactive.warning = ", ifelse(is.null(inactive.warning), "NULL", inactive.warning), 
      ", inactive.error = ", ifelse(is.null(inactive.error), "NULL", inactive.error), 
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

  appendTo(c("Report"), paste0("Target folder: ", getwd(), "\nTimestamp: ", the.time <- Sys.time(), "\nFunction: migration()\n"))

  if (!is.null(path))
    appendTo(c("Screen"), "M: Moving to selected work directory")
  
  report <- checkReport(report = report)
# -----------------------------------

# Load, structure and check the inputs
  study.data <- loadStudyData(tz = tz, override = override, 
                              start.time = start.time, stop.time = stop.time,
                              sections = sections, exclude.tags = exclude.tags, disregard.parallels = disregard.parallels)
  bio <- study.data$bio
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

  if (any(!sapply(arrays, function(x) is.null(x$parallel)))) {
    if (disregard.parallels)
      appendTo(c("Screen", "Report"), "M: 'disregard.parallels' is set to TRUE; the presence of parallel arrays will not invalidate efficiency peers.")
    else
      appendTo(c("Screen", "Report"), "M: 'disregard.parallels' is set to FALSE; the presence of parallel arrays can potentially invalidate efficiency peers.")
  }

  if (is.null(success.arrays)) {
    success.arrays <- names(arrays)[unlist(lapply(arrays, function(x) is.null(x$after)))]
    if (length(success.arrays) == 1)
      appendTo(c("Screen", "Warning", "Report"), paste0("'success.arrays' was not defined. Assuming success if fish are last detected at array ", success.arrays, "."))
    else
      appendTo(c("Screen", "Warning", "Report"), paste0("'success.arrays' was not defined. Assuming success if fish are last detected at arrays ", paste(success.arrays[-length(success.arrays)], collapse = ", "), " or ", tail(success.arrays, 1), "."))
  }
# -------------------------------------
  
# Compile array movements
  appendTo(c("Screen", "Report"), "M: Creating movement records for the valid tags.")
  movements <- groupMovements(detections.list = detections.list, bio = bio, spatial = spatial,
    speed.method = speed.method, max.interval = max.interval, tz = tz, 
    dist.mat = dist.mat, invalid.dist = invalid.dist)

  aux <- names(movements)
  movements <- lapply(names(movements), function(fish) {
      speedReleaseToFirst(fish = fish, bio = bio, movements = movements[[fish]],
                          dist.mat = dist.mat, invalid.dist = invalid.dist, silent = FALSE)
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
  movements <- lapply(seq_along(movements), function(i) {
    fish <- names(movements)[i]
    appendTo("debug", paste0("debug: Checking movement quality for fish ", fish,"."))
    
    if (is.na(match(fish, override))) {
      release <- as.character(bio$Release.site[na.as.false(bio$Transmitter == fish)])
      release <- with(spatial, release.sites[release.sites$Standard.name == release, "Array"])

      output <- checkMinimumN(movements = movements[[i]], fish = fish, minimum.detections = minimum.detections)

      output <- checkUpstream(movements = output, fish = fish, release = release, arrays = arrays)
    
      output <- checkImpassables(movements = output, fish = fish, dotmat = dotmat)

      output <- checkJumpDistance(movements = output, release = release, fish = fish, dotmat = dotmat, 
                                  jump.warning = jump.warning, jump.error = jump.error)

      if (do.checkSpeeds) {
        temp.valid.movements <- simplifyMovements(movements = output, fish = fish, bio = bio, 
          speed.method = speed.method, dist.mat = dist.mat, invalid.dist = invalid.dist)
        output <- checkSpeeds(movements = output, fish = fish, valid.movements = temp.valid.movements, 
          speed.warning = speed.warning, speed.error = speed.error)
        rm(temp.valid.movements)
      }

      if (do.checkInactiveness) {
        output <- checkInactiveness(movements = output, fish = fish, detections.list = detections.list[[fish]], 
          inactive.warning = inactive.warning, inactive.error = inactive.error, 
          dist.mat = dist.mat, invalid.dist = invalid.dist)
      }
    } else {
      output <- overrideValidityChecks(moves = movements[[i]], fish = names(movements)[i])
    }
    return(output)
  })
  names(movements) <- movement.names
  rm(movement.names)
# -------------------------
  
# Compile section movements
  section.movements <- lapply(seq_along(movements), function(i) {
    fish <- names(movements)[i]
    appendTo("debug", paste0("debug: Compiling section movements for fish ", fish,"."))
    aux <- sectionMovements(movements = movements[[i]], sections = sections, invalid.dist = invalid.dist)
    output <- checkLinearity(secmoves = aux, fish = fish, sections = sections, arrays = arrays)
    return(output)
  })
  names(section.movements) <- names(movements)

  # Update array movements based on section movements validity
  movements <- updateValidity(arrmoves = movements, secmoves = section.movements)

  # compile valid movements
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

  appendTo(c("Screen", "Report"), "M: Filling in the timetable.")

  timetable <- assembleTimetable(vm = valid.movements, all.moves = movements, sections = sections, 
    arrays = arrays, dist.mat = dist.mat, invalid.dist = invalid.dist, speed.method = speed.method, 
    if.last.skip.section = if.last.skip.section, success.arrays = success.arrays)

  appendTo(c("Screen", "Report"), "M: Timetable successfully filled. Fitting in the remaining variables.")
  
  status.df <- assembleOutput(timetable = timetable, bio = bio, spatial = spatial, 
    sections = sections, dist.mat = dist.mat, invalid.dist = invalid.dist, tz = tz)

  appendTo(c("Screen", "Report"), "M: Getting summary information tables.")
  
  section.overview <- assembleSectionOverview(status.df = status.df, sections = sections)

  times <- getTimes(movements = valid.movements, spatial = spatial, type = "arrival", events = "one")

  appendTo("Screen", "M: Validating detections...")

  valid.detections <- validateDetections(detections.list = detections.list, movements = valid.movements)

# -------------------------------------

# CJS stuff
  the.matrices <- assembleMatrices(spatial = spatial, movements = valid.movements, status.df = status.df,
    arrays = arrays, paths = paths, dotmat = dotmat)[[2]] # extract only the minimum matrix

  m.by.array <- breakMatricesByArray(m = the.matrices, arrays = arrays, type = "peers")

  if (is.null(m.by.array[[1]])) {
    calculate.efficiency <- FALSE
    appendTo(c("Screen", "Report", "Warning"), "Aborting efficiency calculations (will limit the report's output).")
  } else {
    calculate.efficiency <- TRUE
  }

  if (calculate.efficiency) {
    appendTo(c("Screen", "Report"), "M: Calculating array efficiency.")

    CJS.list <- lapply(m.by.array, function(m) {
      if (length(m) == 1)
        simpleCJS(m[[1]])
      else
        combineCJS(m)
      })

    overall.CJS <- assembleArrayCJS(mat = the.matrices, CJS = CJS.list, arrays = arrays)

    if (!is.null(replicates)) {
      intra.array.matrices <- getDualMatrices(replicates = replicates, CJS = overall.CJS, spatial = spatial, detections.list = detections.list)
      recipient <- includeIntraArrayEstimates(m = intra.array.matrices, CJS = overall.CJS)
      overall.CJS <- recipient[[1]]
      intra.array.CJS <- recipient[[2]]
      rm(recipient)
    } else {
      intra.array.CJS <- NULL
    }

    aux <- mbSplitCJS(m = m.by.array, fixed.efficiency = overall.CJS$efficiency)
    aux <- aux[names(the.matrices)]
    split.CJS <- assembleSplitCJS(mat = the.matrices, CJS = aux, arrays = arrays)
    rm(aux)

    aux <- mbGroupCJS(m = m.by.array, status.df = status.df, fixed.efficiency = overall.CJS$efficiency)
    group.CJS <- assembleGroupCJS(mat = the.matrices, CJS = aux, arrays = arrays)
    array.overview <- mbAssembleArrayOverview(input = group.CJS)
    rm(aux)
} else {
  overall.CJS <- NULL
  intra.array.CJS <- NULL
  array.overview <- NULL
}
# -------------------------------------

# wrap up in-R objects
  detections <- detections.list
  deployments <- do.call(rbind.data.frame, deployments)
  matrices <- the.matrices

  # extra info for potential RSP analysis
  rsp.info <- list(analysis.type = "migration", analysis.time = the.time, bio = bio, tz = tz, actel.version = inst.ver.short)

  if (!is.null(override)) {
    header.fragment <- paste0('<span style="color:red">Manual mode has been triggered for **', length(override),'** fish.</span>\n')
    name.fragment <- "_corrected"
  } else {
    header.fragment <- name.fragment <- ""
  }

  if (file.exists(resultsname <- "actel_migration_results.RData")) {
    continue <- TRUE
    index <- 1
    while (continue) {
      if (file.exists(resultsname <- paste0("actel_migration_results.", index, ".RData"))) {
        index <- index + 1
      } else {
        continue <- FALSE
      }
    }
    appendTo("Screen", paste0("M: An actel migration results file is already present in the current directory.\n   Saving new results as '", resultsname,"'."))
    rm(continue, index)
  } else {
    appendTo(c("Screen", "Report"), paste0("M: Saving results as '", resultsname, "'."))
  }

  if (invalid.dist)
    save(detections, valid.detections, spatial, deployments, arrays, movements, valid.movements, section.movements, status.df,
      section.overview, array.overview, matrices, overall.CJS, intra.array.CJS, times, rsp.info, file = resultsname)
  else
    save(detections, valid.detections, spatial, deployments, arrays, movements, valid.movements, section.movements, status.df,
      section.overview, array.overview, matrices, overall.CJS, intra.array.CJS, times, rsp.info, dist.mat, file = resultsname)
# ------------

# Print graphics
  if (report) {
    appendTo(c("Screen", "Report"), "M: Producing the report.")
    biometric.fragment <- printBiometrics(bio = bio)
    if (calculate.efficiency)
      efficiency.fragment <- printEfficiency(intra.CJS = intra.array.CJS, type = "migration")
    else
      efficiency.fragment <- "Array efficiency could not be calculated. See full log for more details.\n"
    printDotplots(status.df = status.df, invalid.dist = invalid.dist)
    printSurvivalGraphic(section.overview = section.overview)
    printDot(dot = dot, sections = sections, spatial = spatial)
    if (calculate.efficiency) {
      printProgression(dot = dot,  sections = sections, overall.CJS = overall.CJS, spatial = spatial, status.df = status.df)
      display.progression <- TRUE
      array.overview.fragment <- printArrayOverview(array.overview)
    } else {
      display.progression <- FALSE
      array.overview.fragment <- ""
    }
    individual.plots <- printIndividuals(redraw = TRUE, detections.list = detections.list, bio = bio, 
        status.df = status.df, tz = tz, movements = movements, valid.movements = valid.movements)
    circular.plots <- printCircular(times = convertTimesToCircular(times), bio = bio)
    if (nrow(section.overview) > 3) 
      survival.graph.size <- "width=90%" else survival.graph.size <- "height=4in"
  }
  
  appendTo("Report", "M: Process finished successfully.")
# ---------------

# wrap up the txt report
  appendTo("Report", "\n-------------------")
  if (file.exists("temp_UD.txt")) 
    appendTo("Report", paste0("User interventions:\n-------------------\n", gsub("\r", "", readr::read_file("temp_UD.txt")), "-------------------"))
  
  appendTo("Report", paste0("Function call:\n-------------------\n", the.function.call, "\n-------------------"))
# ------------------

# print html report
  if (report) {
    appendTo("debug", "debug: Printing report")
    rmarkdown::render(reportname <- printMigrationRmd(name.fragment = name.fragment, header.fragment = header.fragment, 
        biometric.fragment = biometric.fragment, survival.graph.size = survival.graph.size, circular.plots = circular.plots,
        individual.plots = individual.plots, spatial = spatial, efficiency.fragment = efficiency.fragment, 
        display.progression = display.progression, array.overview.fragment = array.overview.fragment, 
        detections = detections, valid.detections = valid.detections), quiet = TRUE)
    appendTo("debug", "debug: Moving report")
    fs::file_move(sub("Rmd", "html", reportname), sub("Report/", "", sub("Rmd", "html", reportname)))
    appendTo("debug", "debug: Opening report if the pc has internet.")
    openReport(file.name = sub("Report/", "", sub("Rmd", "html", reportname)))
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
      movements = movements, valid.movements = valid.movements, section.movements = section.movements, status.df = status.df, section.overview = section.overview, array.overview = array.overview,
      matrices = matrices, overall.CJS = overall.CJS, intra.array.CJS = intra.array.CJS, times = times, rsp.info = rsp.info))
  else
    return(list(detections = detections, valid.detections = valid.detections, spatial = spatial, deployments = deployments, arrays = arrays,
      movements = movements, valid.movements = valid.movements, section.movements = section.movements, status.df = status.df, section.overview = section.overview, array.overview = array.overview,
      matrices = matrices, overall.CJS = overall.CJS, intra.array.CJS = intra.array.CJS, times = times, rsp.info = rsp.info, dist.mat = dist.mat))
}

#' Print Rmd report
#'
#' Creates a Rmd report and converts it to hmtl.
#' 
#' @param override.fragment Rmarkdown string specifying the type of report for the header.
#' @param biometric.fragment Rmarkdown string specifying the biometric graphics drawn.
#' @param efficiency.fragment Rmarkdown string specifying the efficiency results.
#' @param display.progression Logical. If TRUE, the progression plot has been created and can be displayed.
#' @param array.overview.fragment Rmarkdown string specifying the array overview results.
#' @param survival.graph.size Rmarkdown string specifying the type size of the survival graphics.
#' @param individual.plots Rmarkdown string specifying the name of the individual plots.
#' @param circular.plots Rmarkdown string specifying the name of the circular plots.
#' @inheritParams loadDetections
#' 
#' @keywords internal
#' 
printMigrationRmd <- function(override.fragment, biometric.fragment, efficiency.fragment, display.progression, array.overview.fragment,
  survival.graph.size, individual.plots, circular.plots, spatial, deployments, valid.detections, detections){
  inst.ver <- utils::packageVersion("actel")
  inst.ver.short <- substr(inst.ver, start = 1, stop = nchar(as.character(inst.ver)) - 5) 
  if (file.exists(reportname <- "Report/actel_migration_report.Rmd")) {
    continue <- TRUE
    index <- 1
    while (continue) {
      if(file.exists(reportname <- paste0("Report/actel_migration_report.", index, ".Rmd"))) {
        index <- index + 1
      } else {
        continue <- FALSE
      }
    }
    appendTo("Screen", paste0("M: An actel report is already present in the current directory\n   Saving new report as 'actel_migration_report.", index, ".html'."))
    rm(continue,index)
  } else {
    appendTo("Screen", "M: Saving actel report as 'actel_migration_report.html'.")
  }
  if (any(grepl("Unknown", spatial$stations$Standard.name))) {
    unknown.fragment <- paste0('<span style="color:red"> Number of relevant unknown receivers: **', sum(grepl("Unknown", spatial$stations$Standard.name)), '**</span>\n')
  } else {
    unknown.fragment <- ""
  } 
  report <- readr::read_file("temp_log.txt")
  sink(reportname)
  cat(paste0(
'---
title: "Acoustic telemetry migration analysis"
author: "Actel R package (', inst.ver.short, ')"
output: 
  html_document:
    includes:
      after_body: toc_menu_migration.html
---

### Summary

Target folder: ', stringr::str_extract(pattern = '(?<=Target folder: )[^\r]*', string = report), '

Timestamp: **', stringr::str_extract(pattern = '(?<=Timestamp: )[^\r|^\n]*', string = report), '** 

Number of target tags: **`r I(nrow(status.df))`**

', header.fragment,' 

Number of listed receivers: **', stringr::str_extract(pattern = '(?<=Number of ALS: )[0-9]*', string = report), '** (of which **', stringr::str_extract(pattern = '(?<=of which )[0-9]*', string = report), '** had no detections)

', unknown.fragment,'

Data time range: ', stringr::str_extract(pattern = '(?<=Data time range: )[^\r|^\n]*', string = report), '

Percentage of post-release valid detections: ', round(sum(unlist(lapply(valid.detections, nrow))) / sum(unlist(lapply(detections, nrow))) * 100, 2), '%

Found a bug? [**Report it here.**](https://github.com/hugomflavio/actel/issues)

### Study area

Arrays with the same background belong to the same section. Release sites are marked with "R.S.". Arrays connected with an arrow indicate that the fish can only pass in one direction.

<img src="mb_arrays.svg" alt="Missing file" style="padding-top: 15px;"/>

### Receiver stations

```{r stations, echo = FALSE}
knitr::kable(spatial$stations, row.names = FALSE)
```

### Deployments

```{r deployments, echo = FALSE}
knitr::kable(deployments, row.names = FALSE)
```

### Release sites

```{r releases, echo = FALSE}
knitr::kable(spatial$release.sites, row.names = FALSE)
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

', ifelse(display.progression, 'Zoom in or open the figure in a new tab to clearly read the text within each circle.

Note:
  : The progression calculations **do not account for** intra-section backwards movements. This implies that the total number of fish to have been **last seen** at a given array **may be lower** than the displayed below. Please refer to the [section survival overview](#survival) to find out how many fish were considered to have disappeared per section.

<img src="mb_efficiency.svg" alt="Missing file" style="padding-top: 15px; padding-bottom: 15px;"/>

', 'Progression cannot be displayed if efficiencies are not calculated. See full log for more details.'), array.overview.fragment, '


### Time of arrival at each Array

Note:
  : Coloured lines on the outer circle indicate the mean value for each group and the respective ranges show the standard error of the mean. Each group\'s bars sum to 100%. The number of data points in each group is presented between brackets in the legend of each pannel. 

<center>
', circular.plots,'
</center>


### Dotplots

Note:
  : The **top** 10% of the values for each panel are marked in **red**.
  : The **bottom** 10% of the values for each panel are marked in **blue**.
  : The columns starting with "To" should be read as either "Time to ..." or "Speed to ...", depending on the unit used. The columns starting with "In" should be read as "Time in ...". These reductions were made to keep the column headers as short as possible.

<center>
![](dotplots.png){ width=95% }
</center>


### Individual plots

Note:
  : The detections are coloured by array. The vertical black dashed line shows the time of release. The vertical grey dashed lines show the assigned moments of entry and exit for each study area section. The full dark-grey line shows the movement events considered valid, while the dashed dark-grey line shows the movement events considered invalid.
  : The movement event lines move straight between the first and last station of each event (i.e. in-between detections will not be individually linked by the line).
  : Manually **edited** fish are highlighted with **yellow** graphic borders.
  : Manually **overridden** fish are highlighted with **red** graphic borders.

<center>
', individual.plots,'
</center>

### Full log

```{r log, echo = FALSE, comment = NA}
cat(gsub("\\r", "", readr::read_file("../temp_log.txt")))
```

'), fill = TRUE)
sink()

if(file.exists("Report/toc_menu_migration.html"))
  file.remove("Report/toc_menu_migration.html")
sink("Report/toc_menu_migration.html")
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
  <a href="#study-area">Study area</a>
  <a href="#receiver-stations">Stations</a>
  <a href="#deployments">Deployments</a>
  <a href="#release-sites">Release sites</a>
  <a href="#array-forward-efficiency">Efficiency</a>
  <a href="#warning-messages">Warnings</a>
  <a href="#user-comments">Comments</a>
  <a href="#biometric-graphics">Biometrics</a>
  <a href="#survival">Survival</a>
  <a href="#progression">Progression</a>
  <a href="#time-of-arrival-at-each-array">Arrival times</a>
  <a href="#dotplots">Dotplots</a>
  <a href="#individual-plots">Individuals</a>
  <a href="#full-log">Full log</a>
</div>
', fill = TRUE)
sink()
return(reportname)
}

#' Create the timetable
#'
#' Crawls trough the movement events of each fish to find when it entered and left each section of the study area.
#' 
#' @inheritParams explore
#' @inheritParams migration
#' @inheritParams simplifyMovements
#' @inheritParams loadDetections
#' @inheritParams groupMovements
#' @inheritParams assembleArrayCJS
#' @param movements A list of movements for each target tag, created by groupMovements.
#' 
#' @return A table of the entering and leaving points for each section per target tag
#' 
#' @keywords internal
#' 
assembleTimetable <- function(vm, all.moves, sections, arrays,
  dist.mat, invalid.dist, speed.method, if.last.skip.section, success.arrays) {
  appendTo("debug", "Running assembleTimetable.")

  # NOTE: The NULL variables below are actually column names used by data.table.
  # This definition is just to prevent the package check from issuing a note due unknown variables.
  Valid <- NULL 
  Array <- NULL
  Detections <- NULL
 
  # Create the timetable
  recipient <- vector()
  if (invalid.dist) {
    for (i in seq_len(length(sections))) {
      recipient <- c(recipient, paste(c("Time.until", "First.station",
        "Arrived", "Time.in", "Last.station", "Left"), sections[i], sep = "."))
    }
  } else {
    for (i in seq_len(length(sections))) {
      recipient <- c(recipient, paste(c("Time.until", "Speed.to", "First.station",
        "Arrived", "Time.in", "Speed.in", "Last.station", "Left"), sections[i], sep = "."))
    }
  }
  recipient <- c(recipient, "Very.last.array", "Status", "Valid.detections", "Invalid.detections", "Backwards.movements", "Max.cons.back.moves", "P.type")
  if (!invalid.dist && speed.method == "first to first")
    recipient <- recipient[!grepl("Speed.in",recipient)]

  timetable <- matrix(nrow = length(vm), ncol = length(recipient))
  timetable <- as.data.frame(timetable)
  
  colnames(timetable) <- recipient
  rm(recipient)
  rownames(timetable) <- names(vm)
  
  # Start filling it up
  capture <- lapply(names(all.moves), function(fish) {
    if (!is.null(vm[[fish]])) {
      # Find first and last events
      aux <- lapply(seq_along(sections), function(i) {
        x <- rep(NA_character_, nrow(vm[[fish]]))
        x[grepl(sections[i], vm[[fish]]$Array)] <- sections[i]
        return(x)
      })

      event.index <- combine(aux)
      aux <- rle(event.index)

      last.events <- cumsum(aux$lengths)
      names(last.events) <- aux$values
      
      first.events <- c(1, last.events[-length(last.events)] + 1)
      names(first.events) <- aux$values

      appendTo("debug", paste0("Deploying values for fish ", fish, "."))
      capture <- lapply(1:length(last.events), function(i) {
          the.section <- names(last.events)[i]
          timetable[fish, paste("Arrived", the.section, sep = ".")] <- paste(vm[[fish]][[first.events[i], "First.time"]])
          timetable[fish, paste("First.station", the.section, sep = ".")] <- vm[[fish]][[first.events[i], "First.station"]]
          timetable[fish, paste("Left", the.section, sep = ".")] <- paste(vm[[fish]][[last.events[i], "Last.time"]])
          timetable[fish, paste("Last.station", the.section, sep = ".")] <- vm[[fish]][[last.events[i], "Last.station"]]
          timetable[fish, paste("Time.in", the.section, sep = ".")] <- 
            as.vector(
              difftime(
                timetable[fish, paste("Left", the.section, sep = ".")], 
                timetable[fish, paste("Arrived", the.section, sep = ".")], units = "secs"
                )
            )

          # If Time.in is to be calculated
          if (speed.method == "last to first" && !invalid.dist) {
            to.col <- paste("Speed.in", the.section, sep = ".")
            dist.row <- timetable[fish, paste("First.station", the.section, sep = ".")]
            dist.col <- timetable[fish, paste("Last.station", the.section, sep = ".")]
            from.col <- paste("Time.in", the.section, sep = ".")
            timetable[fish, to.col] <- dist.mat[dist.row, dist.col] / timetable[fish, from.col]
          }

        # If this is not the first section
        if (match(the.section, sections) > 1) {
          previous.section <- sections[match(the.section, sections) - 1]
          testA <- !is.na(timetable[fish, paste("Arrived", the.section, sep = ".")])
          testB <- !is.na(timetable[fish, paste("Left", previous.section, sep = ".")])
          if (testA & testB) {
            to.col <- paste("Time.until", the.section, sep = ".")
            from.colA <- paste("Arrived", the.section, sep = ".")
            from.colB <- paste("Left", previous.section, sep = ".")
            AtoB <- difftime(timetable[fish, from.colA], timetable[fish, from.colB], units = "secs")
            timetable[fish, to.col] <- as.vector(AtoB)
            if (!invalid.dist) {
              to.col <- paste("Speed.to", the.section, sep = ".")
              dist.row <- timetable[fish, paste("First.station", the.section, sep = ".")]
              if (speed.method == "last to first"){
                dist.col <- timetable[fish, paste("Last.station", previous.section, sep = ".")]
                total.time <- timetable[fish, paste("Time.until", the.section, sep = ".")]
              }
              if (speed.method == "first to first"){
                dist.col <- timetable[fish, paste("First.station", previous.section, sep = ".")]
                total.time <- timetable[fish, paste("Time.in", previous.section, sep = ".")] + 
                              timetable[fish, paste("Time.until", the.section, sep = ".")]
              }
              timetable[fish, to.col] <- dist.mat[dist.row, dist.col] / total.time
            }
          }
        }
      
        # If this is the last last event
        if (i == length(last.events)) {
          # If we are not on the last section and the last last event was on an edge array of the section
          not.last.section <- match(the.section, sections) != length(sections)
          edge.array <- arrays[[vm[[fish]]$Array[tail(last.events, 1)]]]$edge
          if (if.last.skip.section && not.last.section && edge.array) {
            timetable[fish, "Status"] <- paste("Disap. in", sections[match(the.section, sections) + 1])
          } else {
            timetable[fish, "Status"] <- paste("Disap. in", the.section)
          }
          timetable[fish, "Very.last.array"] <- vm[[fish]]$Array[last.events[i]]
        }
        timetable <<- timetable
        return(NULL)
      })
      timetable[fish, "Valid.detections"] <- vm[[fish]][, sum(Detections)]
    } else {
      timetable[fish, "Valid.detections"] <- 0
    }
    if (any(!all.moves[[fish]]$Valid)) {
      timetable[fish, "Invalid.detections"] <- sum(all.moves[[fish]][!(Valid)]$Detections)
    } else {
      timetable[fish, "Invalid.detections"] <- 0
    }
    recipient <- countBackMoves(movements = all.moves[[fish]], arrays = arrays)
    timetable[fish, "Backwards.movements"] <- recipient[[1]]
    timetable[fish, "Max.cons.back.moves"] <- recipient[[2]]
    timetable[fish, "P.type"] <- attributes(all.moves[[fish]])$p.type

    if(!is.null(vm[[fish]]) && !is.na(match(vm[[fish]]$Array[tail(last.events, 1)], success.arrays))) 
      timetable[fish, "Status"] <- "Succeeded"

    timetable <<- timetable
    return(NULL)
  })
  timetable$Transmitter <- rownames(timetable)
  return(timetable)
}

#' Count backwards movements
#' 
#' @inheritParams simplifyMovements
#' @inheritParams assembleArrayCJS
#' 
#' @keywords internal
#' 
#' @return The number of backwards movements and the maximum consecutive backwards movements
#' 
countBackMoves <- function(movements, arrays){
  appendTo("debug", "Starting countBackMoves.")
  if (nrow(movements) > 1) {# Determine number of backwards movements
    aux <- data.frame(
      A = movements$Array[-nrow(movements)],
      B = movements$Array[-1])
    backwards.movements <- apply(aux, 1, function(x)
      if(x[1] != x[2]) 
        is.na(match(x[2], arrays[[x[1]]]$all.after))
      else
        FALSE
      )
    sum.back.moves <- sum(backwards.movements)
    if (sum.back.moves > 0) 
      max.back.moves <- max(rle(backwards.movements)$lengths[which(rle(backwards.movements)$values == TRUE)])
    else
      max.back.moves <- 0
  } else {
    sum.back.moves <- 0
    max.back.moves <- 0
  }
  appendTo("debug", "Terminating countBackMoves.")
  return(list(sum.back.moves = sum.back.moves, max.back.moves = max.back.moves))
}

#' Create status.df
#'
#' Combines the timetable and the original biometrics.
#' 
#' @inheritParams explore
#' @inheritParams migration
#' @param timetable A table of the entering and leaving points for each section per target tag, created by assembleTimetable.
#' @inheritParams splitDetections
#' @inheritParams simplifyMovements
#' @inheritParams loadDetections
#' @inheritParams groupMovements
#' 
#' @return A data frame containing all the final data for each fish.
#' 
#' @keywords internal
#' 
assembleOutput <- function(timetable, bio, spatial, sections, dist.mat, invalid.dist, tz) {
  appendTo("debug", "Merging 'bio' and 'timetable'.")
  status.df <- merge(bio, timetable, by = "Transmitter", all = TRUE)
  
  appendTo("debug", "Completing entries for fish that were never detected.")
  status.df$Status[is.na(status.df$Status)] <- paste("Disap. in", sections[1])
  status.df$Status <- factor(status.df$Status, levels = c(paste("Disap. in", sections), "Succeeded"))
  status.df$Very.last.array[is.na(status.df$Very.last.array)] <- "Release"
  status.df$Very.last.array <- factor(status.df$Very.last.array, levels = c("Release", levels(spatial$stations$Array)))
  status.df$P.type[is.na(status.df$P.type)] <- "Skipped"
  status.df$Valid.detections[is.na(status.df$Valid.detections)] <- 0
  status.df$Invalid.detections[is.na(status.df$Invalid.detections)] <- 0

  the.cols <- grepl("Release.date", colnames(status.df)) | grepl("Arrived",colnames(status.df)) | grepl("Left",colnames(status.df))
  for (i in colnames(status.df)[the.cols]) {
    status.df[, i] <- as.POSIXct(status.df[,i], tz = tz)
  }
  rm(the.cols)
  
  appendTo("debug", "Calculating time from release to first detection.")
  for (i in 1:nrow(status.df)) {
    appendTo("debug", paste0("(status.df) Analysing fish ", status.df$Signal[i], " (", i, ")."))
    arriving.points <- status.df[i, paste("Arrived", sections, sep = ".")]
    if (any(!is.na(arriving.points))) {
      first.section <- sections[head(which(!is.na(arriving.points)), 1)]
      pointA <- as.POSIXct(status.df[i, paste("Arrived", first.section, sep = ".")], tz = tz)
      pointB <- as.POSIXct(status.df[i, "Release.date"], tz = tz)
      AtoB <- as.vector(difftime(pointA, pointB, units = "secs"))
      status.df[i, paste("Time.until", first.section, sep = ".")] <- AtoB
      if (!invalid.dist) {
        dist.row <- status.df[i, paste("First.station", first.section, sep = ".")]
        dist.col <- as.character(status.df[i, "Release.site"])
        df.to.col <- paste("Speed.to", first.section, sep = ".")
        df.from.col <- paste("Time.until", first.section, sep = ".")
        status.df[i, df.to.col] <- dist.mat[dist.row, dist.col]/status.df[i, df.from.col]
      }
      rm(AtoB)
    }
  }
  rm(i)

  # Convert time data
  for (section in sections) {
    for (the.col in c("Time.in.", "Time.until.")) {
      # convert to numeric
      status.df[, paste0(the.col, section)] <- as.numeric(status.df[, paste0(the.col, section)])
      # grab the mean for later use
      aux <- mean(status.df[, paste0(the.col, section)], na.rm = TRUE)
      # convert to difftime
      status.df[, paste0(the.col, section)] <- as.difftime(status.df[, paste0(the.col, section)], units = "secs")
      units(status.df[, paste0(the.col, section)]) <- "secs"
      if (aux > 86400)
        units(status.df[, paste0(the.col, section)]) <- "days"
      if (aux <= 86400 & aux > 3600)
        units(status.df[, paste0(the.col, section)]) <- "hours"
      if (aux <= 3600)
        units(status.df[, paste0(the.col, section)]) <- "mins"
      status.df[, paste0(the.col, section)] <- round(status.df[, paste0(the.col, section)], 3)
    }
  }  

  if (file.exists("temp_comments.txt")) {
    temp <- read.table("temp_comments.txt", header = F, sep = "\t")
    status.df[, "Comments"] <- NA
    for (i in seq_len(nrow(temp))) {
      link <- match(temp[i, 1], status.df$Transmitter)
      if (is.na(status.df$Comments[link])) {
        status.df$Comments[link] <- paste(temp[i, 2])
      } else {
        status.df$Comments[link] <- paste(status.df$Comments[link], temp[i, 2], sep = "// ")
      }
    }
  }
  appendTo("debug", "Done.")
  return(status.df)
}

#' Create section.overview
#'
#' Produces a table with the survival per group of fish present in the biometrics.
#' 
#' @inheritParams explore
#' @inheritParams migration
#' @inheritParams simplifyMovements
#' 
#' @return A data frame containing the survival per group of fish present in the biometrics.
#' 
#' @keywords internal
#' 
assembleSectionOverview <- function(status.df, sections) {
  appendTo("debug", "Starting assembleSectionOverview.")
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
  appendTo("debug", "Terminating assembleSectionOverview.")
  return(section.overview[, recipient])
}



#' Create array.overview
#'
#' @return A data frame containing the progression per group of fish present in the biometrics.
#' 
#' @keywords internal
#' 
mbAssembleArrayOverview <- function(input) {
  appendTo("debug", "Starting mbAssembleArrayOverview.")
  for (i in 1:length(input)) {
    input[[i]][1, ] <- apply(input[[i]][c(1,3), ], 2, sum, na.rm = TRUE)
    input[[i]][2, ] <- input[[i]][4, ]
    input[[i]][3, ] <- input[[i]][2, ] - input[[i]][1, ]
    input[[i]] <- input[[i]][1:3, ]
    rownames(input[[i]]) <- c("Known", "Estimated", "Difference")
  }
  appendTo("debug", "Terminating mbAssembleArrayOverview.")  
  return(input)
}
