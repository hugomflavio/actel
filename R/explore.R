#' Explorative Analysis
#'
#' \code{explore} allows you to quickly get a summary of your data. You can use
#' \code{explore} to get a general feel for the study results, and check if the
#' input files are behaving as expected. It is also a good candidate if you just
#' want to validate your detections for later use in other analyses.
#'
#' @param auto.open Logical: Should the report be automatically opened once the
#'  analysis is over? Defaults to TRUE. NOTE: If report = TRUE and auto.open = TRUE,
#'  the web browser will automatically be launched to open the report once the
#'  function terminates.
#' @param datapack A data bundle pre-compiled through the function \code{\link{preload}}.
#'  May be used to run actel analyses based on R objects, rather than input files.
#' @param discard.first A threshold amount of time (in hours) that must pass after
#'  release for the respective detections to be valid. Set to 0 to discard only
#'  the release-to-first-detection calculations.
#' @param discard.orphans Logical: Should actel automatically discard
#'  detections that do not fall within receiver deployment periods, or that
#'  were recorded before the respective animals were released?
#' @param exclude.tags A vector of tags that should be excluded from the
#'  detection data before any analyses are performed. Intended to be used if
#'  stray tags from a different code space but with the same signal as a target
#'  tag are detected in the study area.
#' @param GUI One of "needed", "always" or "never". If "needed", a new window is
#'  opened to inspect the movements only when the movements table is too big to be
#'  displayed in R's console. If "always", a graphical interface is always created
#'  when the possibility to invalidate events emerges. If "never", a graphical
#'  interface is never invoked. In this case, if the table to be displayed does
#'  not fit in R's console, a temporary file will be saved and the user will be
#'  prompted to open that file and examine it. Defaults to "needed".
#' @param inactive.error If a tag spends a number of days equal or greater than
#'  \code{inactive.error} in a given array at the tail of the respective
#'  detections, user intervention is suggested. If left NULL (default), user
#' intervention is never suggested.
#' @param inactive.warning If a tag spends a number of days equal or greater
#'  than \code{inactive.warning} in a given array at the tail of the respective
#'  detections, a warning is issued. If left NULL (default), no warnings are
#'  issued. Must be equal to or lower than \code{innactive.error}.
#' @param jump.error If a tag crosses a number of arrays equal or greater than
#'  \code{jump.error} without being detected, user intervention is suggested.
#'  Defaults to 3. To disable user intervention suggestions, set to Inf.
#' @param jump.warning If a tag crosses a number of arrays equal or greater
#'  than \code{jump.warning} without being detected, a warning is issued. Defaults
#'  to 2. To disable jump warnings, set to Inf. Must be equal to or lower than \code{jump.error}.
#' @param max.interval The number of minutes that must pass between detections
#'  for a new event to be created. Defaults to 60.
#' @param minimum.detections DEPRECATED. Please use the arguments min.total.detections
#'  and min.per.event instead.
#' @param min.total.detections Minimum number of times a tag must have
#'  been detected during the study period for the detections to be considered true
#'  and not just random noise. Defaults to 2.
#' @param min.per.event Minimum number of detections an event must have to be
#'  deemed valid. For analyses with both array and section events, a vector of
#'  two values can be provided. If only one value is provided, the same threshold
#'  applies for both types of events. Defaults to 1.
#' @param override A vector of signals for which the user intends to manually
#'  define which movement events are valid and invalid.
#' @param detections.y.axis The type of y axis desired for the individual
#'  detection plots. While the argument defaults to "auto", it can be hard-set 
#'  to one of "stations" or "arrays".
#' @param print.releases Logical: Should the release sites be printed in the
#'  study area diagrams?
#' @param report Logical. Should an HTML report be created at the end of the
#'  analysis? NOTE: Setting report to TRUE will generate an HTML file in the current
#'  directory. Additionally, if auto.open = TRUE (default), the web browser will
#'  automatically be launched to open the report once the function terminates.
#' @param save.detections Logical: Should the processed detections be saved for
#'  future runs?
#' @param save.tables.locally Logical: If a table must be temporarily stored into a file
#'  for user inspection, should it be saved in the current working directory, or
#'  in R's temporary folder?
#' @param speed.error If a tag moves at a speed equal or greater than
#'  \code{speed.error} (in metres per second), user intervention is suggested.
#'  If left NULL (default), user intervention is never suggested.
#' @param speed.method Can take two forms: 'last to first' or 'last to last'.
#'  If 'last to first' (default), the last detection on the previous array is matched
#'  to the first detection on the target array to perform the calculations.
#'  If 'last to last', the last detection on ´the previous array is matched to the
#'  last detection on the target array to perform the calculations.
#' @param speed.warning If a tag moves at a speed equal or greater than
#'  \code{speed.warning} (in metres per second), a warning is issued. If left
#'  NULL (default), no warnings are issued. Must be equal to or lower than \code{speed.error}
#' @param start.time Detection data prior to the timestamp set in
#'  \code{start.time} (in YYYY-MM-DD HH:MM:SS format) is not considered during
#'  the analysis.
#' @param stop.time Detection data posterior to the timestamp set in
#'  \code{stop.time} (in YYYY-MM-DD HH:MM:SS format) is not considered during
#'  the analysis.
#' @param tz The time zone of the study area. Must match one of the values
#'  present in \code{\link[base]{timezones}}.
#'
#' @examples
#' \donttest{
#' # Start by moving to a temporary directory
#' old.wd <- getwd()
#' setwd(tempdir())
#'
#' # Deploy the example workspace
#' exampleWorkspace("explore_example")
#'
#' # Move your R session into the example workspace
#' setwd("explore_example")
#'
#' # run the explore analysis. Ensure the tz argument
#' # matches the time zone of the study area. For the
#' # example dataset, tz = "Europe/Copenhagen"
#' results <- explore(tz = "Europe/Copenhagen")
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
#'  \item \code{bio}: A copy of the biometrics input;
#'  \item \code{detections}: A list containing all detections for each target tag;
#'  \item \code{valid.detections}: A list containing the valid detections for each target tag;
#'  \item \code{spatial}: A list containing the spatial information used during the analysis;
#'  \item \code{deployments}: A data frame containing the deployments of each receiver;
#'  \item \code{arrays}: A list containing the array details used during the analysis;
#'  \item \code{movements}: A list containing all movement events for each target tag;
#'  \item \code{valid.movements}: A list containing the valid movement events for each target tag;
#'  \item \code{times}: A data frame containing all arrival times (per tag) at each array;
#'  \item \code{rsp.info}: A list containing containing appendix information for the RSP package;
#'  \item \code{dist.mat}: A matrix containing the distance matrix used in the analysis (if a valid
#'   distance matrix was supplied)
#' }
#'
#' @seealso \code{\link{migration}}, \code{\link{residency}}
#'
#' @export
#'
explore <- function(
  tz = NULL,
  datapack = NULL,
  max.interval = 60,
  minimum.detections,
  min.total.detections = 2,
  min.per.event = 1,
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
  GUI = c("needed", "always", "never"),
  save.tables.locally = FALSE,
  print.releases = TRUE,
  detections.y.axis = c("auto", "stations", "arrays")) 
{

# check deprecated argument
  if (!missing(minimum.detections))
    stop("'minimum.detections' has been deprecated. Please use 'min.total.detections' and 'min.per.event' instead.", call. = FALSE)

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

  if (length(min.per.event) > 1) 
    appendTo(c('screen', 'warning', 'report'),
      'explore() only has array movements but two values were set for min.per.event. Disregarding second value.')
  
  aux <- checkArguments(dp = datapack,
                        tz = tz,
                        min.total.detections = min.total.detections,
                        min.per.event = min.per.event,
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
                        detections.y.axis = detections.y.axis)

  min.per.event <- aux$min.per.event[1]
  speed.method <- aux$speed.method
  speed.warning <- aux$speed.warning
  speed.error <- aux$speed.error
  jump.warning <- aux$jump.warning
  jump.error <- aux$jump.error
  inactive.warning <- aux$inactive.warning
  inactive.error <- aux$inactive.error
  detections.y.axis <- aux$detections.y.axis
  rm(aux)

  GUI <- checkGUI(GUI, save.tables.locally = save.tables.locally)
# ------------------------

# Store function call
  the.function.call <- paste0("explore(tz = ", ifelse(is.null(tz), "NULL", paste0("'", tz, "'")),
    ", datapack = ", ifelse(is.null(datapack), "NULL", deparse(substitute(datapack))),
    ", max.interval = ", max.interval,
    ", min.total.detections = ", min.total.detections,
    ", min.per.event = ", min.per.event,
    ", start.time = ", ifelse(is.null(start.time), "NULL", paste0("'", start.time, "'")),
    ", stop.time = ", ifelse(is.null(stop.time), "NULL", paste0("'", stop.time, "'")),
    ", speed.method = ", paste0("c('", speed.method, "')"),
    ", speed.warning = ", ifelse(is.null(speed.warning), "NULL", speed.warning),
    ", speed.error = ", ifelse(is.null(speed.error), "NULL", speed.error),
    ", jump.warning = ", jump.warning,
    ", jump.error = ", jump.error,
    ", inactive.warning = ", ifelse(is.null(inactive.warning), "NULL", inactive.warning),
    ", inactive.error = ", ifelse(is.null(inactive.error), "NULL", inactive.error),
    ", exclude.tags = ", ifelse(is.null(exclude.tags), "NULL", paste0("c('", paste(exclude.tags, collapse = "', '"), "')")),
    ", override = ", ifelse(is.null(override), "NULL", paste0("c(", paste(override, collapse = ", "), ")")),
    ", report = ", ifelse(report, "TRUE", "FALSE"),
    ", discard.orphans = ", ifelse(discard.orphans, "TRUE", "FALSE"),
    ", discard.first = ", ifelse(is.null(discard.first), "NULL", discard.first),
    ", auto.open = ", ifelse(auto.open, "TRUE", "FALSE"),
    ", save.detections = ", ifelse(save.detections, "TRUE", "FALSE"),
    ", GUI = '", GUI, "'",
    ", save.tables.locally = '", ifelse(save.tables.locally, "TRUE", "FALSE"),
    ", print.releases = ", ifelse(print.releases, "TRUE", "FALSE"),
    ", detections.y.axis = '", detections.y.axis, "'",
    ")")

  appendTo("debug", the.function.call)
# --------------------

# Prepare clean-up before function ends
  finished.unexpectedly <- TRUE
  on.exit({if (interactive() & finished.unexpectedly) emergencyBreak(the.function.call)}, add = TRUE)

  if (!getOption("actel.debug", default = FALSE))
    on.exit(deleteHelpers(), add = TRUE)

  on.exit(tryCatch(sink(), warning = function(w) {hide <- NA}), add = TRUE)
# --------------------------------------

# Final arrangements before beginning
  appendTo("Report", paste0("Actel R package report.\nVersion: ", utils::packageVersion("actel"), "\n"))

  appendTo(c("Report"), paste0("Target folder: ", getwd(), "\nTimestamp: ", the.time <- Sys.time(), "\nFunction: explore()\n"))

  report <- checkReport(report = report)
# -----------------------------------

# Load, structure and check the inputs
  if (is.null(datapack)) {
    study.data <- loadStudyData(tz = tz, override = override, save.detections = save.detections,
                                start.time = start.time, stop.time = stop.time, discard.orphans = discard.orphans,
                                section.order = NULL, exclude.tags = exclude.tags)
  } else {
    appendTo(c("Screen", "Report"), paste0("M: Running analysis on preloaded data (compiled on ", attributes(datapack)$timestamp, ")."))
    appendTo("Report", paste0("Messages displayed during preload:\n-------------------\n", paste0(attributes(datapack)$loading_messages, collapse = "\n"), "\n-------------------"))
    study.data <- datapack
    tz <- study.data$tz
    disregard.parallels <- study.data$disregard.parallels
  }

  bio <- study.data$bio
  sections <- study.data$sections
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

# Process the data
  # exclude head of detections, if requested
  if (!is.null(discard.first) && discard.first > 0)
    detections.list <- discardFirst(input = detections.list, bio, trim = discard.first)

  # group detections into array movements
  appendTo(c("Screen", "Report"), "M: Creating movement records for the valid tags.")
  movements <- groupMovements(detections.list = detections.list, bio = bio, spatial = spatial,
    speed.method = speed.method, max.interval = max.interval, tz = tz, dist.mat = dist.mat)

  # calculate time/speed sinse release
  if (is.null(discard.first)) {
    aux <- names(movements)
    movements <- lapply(names(movements), function(tag) {
        speedReleaseToFirst(tag = tag, bio = bio, movements = movements[[tag]],
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

  movement.names <- names(movements) # this will be used further down to reinstate the names in the movements list.

  # clean override based on movements
  if (is.numeric(override))
    trigger_override_warning <- any(link <- !override %in% extractSignals(movement.names))
  else
    trigger_override_warning <- any(link <- !override %in% movement.names)

  if (trigger_override_warning) {
    appendTo(c("Screen", "Warning", "Report"), paste0("Override has been triggered for ",
      ifelse(sum(link) == 1, "tag ", "tags "), paste(override[link], collapse = ", "), " but ",
      ifelse(sum(link) == 1, "this signal was", "these signals were"), " not detected."))
    override <- override[!link]
  }

  # convert numeric override to full tag override to prevent problems downstream
  if (is.numeric(override))
    override <- movement.names[match(override, extractSignals(movement.names))]

  # Check movement quality
  movements <- lapply(seq_along(movements), function(i) {
    tag <- names(movements)[i]
    counter <- paste0("(", i, "/", length(movements), ")")

    appendTo("debug", paste0("debug: Checking movement quality for tag ", tag,"."))

    if (is.na(match(tag, override))) {
      output <- checkMinimumN(movements = movements[[tag]], tag = tag, min.total.detections = min.total.detections,
                               min.per.event = min.per.event[1], n = counter)

      output <- checkImpassables(movements = output, tag = tag, bio = bio, detections = detections.list[[tag]], n = counter, 
                                 spatial = spatial, dotmat = dotmat, GUI = GUI, save.tables.locally = save.tables.locally)

      output <- checkJumpDistance(movements = output, bio = bio, tag = tag, dotmat = dotmat, paths = paths, arrays = arrays,
                                  spatial = spatial, jump.warning = jump.warning, jump.error = jump.error, GUI = GUI, n = counter,
                                  detections = detections.list[[tag]], save.tables.locally = save.tables.locally)

      if (do.checkSpeeds) {
        temp.valid.movements <- simplifyMovements(movements = output, tag = tag, bio = bio, discard.first = discard.first,
                                                  speed.method = speed.method, dist.mat = dist.mat)
        output <- checkSpeeds(movements = output, tag = tag, detections = detections.list[[tag]], n = counter, 
                              valid.movements = temp.valid.movements, speed.warning = speed.warning, 
                              speed.error = speed.error, GUI = GUI, save.tables.locally = save.tables.locally)
        rm(temp.valid.movements)
      }

      if (do.checkInactiveness) {
        output <- checkInactiveness(movements = output, tag = tag, detections = detections.list[[tag]], n = counter,
                                    inactive.warning = inactive.warning, inactive.error = inactive.error,
                                    dist.mat = dist.mat, GUI = GUI, save.tables.locally = save.tables.locally)
      }  
    } else { # nocov start
      output <- overrideValidityChecks(moves = movements[[tag]], tag = tag, detections = detections.list[[tag]], 
                                       GUI = GUI, save.tables.locally = save.tables.locally, n = counter)
    } # nocov end
    return(output)
  })
  names(movements) <- movement.names
  rm(movement.names)

  appendTo(c("Screen", "Report"), "M: Filtering valid array movements.")

  valid.movements <- assembleValidMoves(movements = movements, bio = bio, discard.first = discard.first,
                                         speed.method = speed.method, dist.mat = dist.mat)


  appendTo(c("Screen", "Report"), "M: Compiling circular times.")

  aux <- list(valid.movements = valid.movements,
              spatial = spatial,
              rsp.info = list(bio = bio, 
                              analysis.type = "explore"))
  times <- getTimes(input = aux, move.type = "array", event.type = "arrival", n.events = "first")
  rm(aux)

  appendTo("Screen", "M: Validating detections.")

  recipient <- validateDetections(detections.list = detections.list, movements = valid.movements)
  detections <- recipient$detections
  valid.detections <- recipient$valid.detections
  rm(recipient)
# -------------------------------------

# wrap up in-R objects
  deployments <- do.call(rbind.data.frame, deployments)

  # extra info for potential RSP analysis
  rsp.info <- list(analysis.type = "explore", analysis.time = the.time, 
                   bio = bio, tz = tz, actel.version = utils::packageVersion("actel"))

  if (!is.null(override))
    override.fragment <- paste0('<span style="color:red">Manual mode has been triggered for **',
                                length(override), '** tag(s).</span>\n')
  else
    override.fragment <- ""

  if (file.exists(resultsname <- "actel_explore_results.RData")) {
    continue <- TRUE
    index <- 1
    while (continue) {
      if (file.exists(resultsname <- paste0("actel_explore_results.", index, ".RData"))) {
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

  if (decision == "y") { # nocov start
    appendTo(c("Screen", "Report"), paste0("M: Saving results as '", resultsname, "'."))
    if (attributes(dist.mat)$valid)
      save(bio, detections, valid.detections, spatial, deployments, arrays, 
        movements, valid.movements, times, rsp.info, dist.mat, file = resultsname)
    else
      save(bio, detections, valid.detections, spatial, deployments, arrays, 
        movements, valid.movements, times, rsp.info, file = resultsname)
  } else {
    appendTo(c("Screen", "Report"), paste0("M: Skipping saving of the results."))
  } # nocov end
  rm(decision)

# ------------

# Print graphics
  trigger.report.error.message <- TRUE
  if (report) {
    appendTo(c("Screen", "Report"), "M: Producing the report.")
    on.exit({if (trigger.report.error.message) message("M: Producing the report failed. If you have saved a copy of the results, you can reload them using dataToList().")}, add = TRUE)

    if (dir.exists(paste0(tempdir(), "/actel_report_auxiliary_files")))
      unlink(paste0(tempdir(), "/actel_report_auxiliary_files"), recursive = TRUE)

    dir.create(paste0(tempdir(), "/actel_report_auxiliary_files"))

    if (!getOption("actel.debug", default = FALSE))
      on.exit(unlink(paste0(tempdir(), "/actel_report_auxiliary_files"), recursive = TRUE), add = TRUE)

    biometric.fragment <- printBiometrics(bio = bio)

    printDot(dot = dot, 
             spatial = spatial, 
             print.releases = print.releases)

    individual.plots <- printIndividuals(detections.list = detections, 
                                         movements = movements,
                                         valid.movements = valid.movements,
                                         spatial = spatial,
                                         rsp.info = rsp.info,
                                         y.axis = detections.y.axis)

    circular.plots <- printCircular(times = timesToCircular(times), 
                                    bio = bio)

    if (any(sapply(valid.detections, function(x) any(!is.na(x$Sensor.Value))))) {
      sensor.plots <- printSensorData(detections = valid.detections, 
                                      spatial = spatial,
                                      rsp.info = rsp.info, 
                                      colour.by = detections.y.axis)
    } else {
      sensor.plots <- NULL
    }
  }

# ---------------

# wrap up the txt report
  appendTo("Report", "M: Analysis completed!\n\n-------------------")

  if (file.exists(paste(tempdir(), "temp_comments.txt", sep = "/")))
    appendTo("Report", paste0("User comments:\n-------------------\n", gsub("\t", ": ", gsub("\r", "", readr::read_file(paste(tempdir(), "temp_comments.txt", sep = "/")))), "-------------------")) # nocov

  if (file.exists(paste(tempdir(), "temp_UD.txt", sep = "/")))
    appendTo("Report", paste0("User interventions:\n-------------------\n", gsub("\r", "", readr::read_file(paste(tempdir(), "temp_UD.txt", sep = "/"))), "-------------------")) # nocov

  if (!is.null(datapack))
    appendTo("Report", paste0("Preload function call:\n-------------------\n", attributes(datapack)$function_call, "\n-------------------"))

  appendTo("Report", paste0("Explore function call:\n-------------------\n", the.function.call, "\n-------------------"))
# ------------------

# print html report
  if (report) {
    if (file.exists(reportname <- "actel_explore_report.html")) {
      continue <- TRUE
      index <- 1
      while (continue) {
        if(file.exists(reportname <- paste0("actel_explore_report.", index, ".html"))) {
          index <- index + 1
        } else {
          continue <- FALSE
        }
      }
      appendTo("Screen", paste0("M: An actel report is already present in the current directory.\n   Saving new report as ", reportname, "."))
      rm(continue, index)
    } else {
      appendTo("Screen", "M: Saving actel report as 'actel_explore_report.html'.")
    }

    appendTo("debug", "debug: Printing report rmd")
    printExploreRmd(override.fragment = override.fragment,
                    biometric.fragment = biometric.fragment,
                    individual.plots = individual.plots,
                    circular.plots = circular.plots,
                    sensor.plots = sensor.plots,
                    spatial = spatial,
                    deployments = deployments,
                    detections = detections,
                    valid.detections = valid.detections,
                    detections.y.axis = detections.y.axis)

    appendTo("debug", "debug: Converting report to html")
    rmarkdown::render(input = paste0(tempdir(), "/actel_report_auxiliary_files/actel_explore_report.Rmd"),
      output_dir = paste0(tempdir(), "/actel_report_auxiliary_files"), quiet = TRUE)

    appendTo("debug", "debug: Moving report")
    file.copy(paste0(tempdir(), "/actel_report_auxiliary_files/actel_explore_report.html"), reportname)
    if (interactive() & auto.open) { # nocov start
      appendTo("debug", "debug: Opening report.")
      browseURL(reportname)
    } # nocov end
  }
  trigger.report.error.message <- FALSE
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

  output <- list(bio = bio,
                 detections = detections, 
                 valid.detections = valid.detections, 
                 spatial = spatial, 
                 deployments = deployments, 
                 arrays = arrays,
                 movements = movements, 
                 valid.movements = valid.movements, 
                 times = times, 
                 rsp.info = rsp.info)

  if (attributes(dist.mat)$valid)
    output$dist.mat <- dist.mat

  appendTo("Screen", "M: Analysis completed!")
  finished.unexpectedly <- FALSE

  return(output)
}


#' Print Rmd report
#'
#' Creates a Rmd report and converts it to html.
#'
#' @param override.fragment Rmarkdown string specifying the type of report for the header.
#' @param biometric.fragment Rmarkdown string specifying the biometric graphics drawn.
#' @param individual.plots Rmarkdown string specifying the name of the individual plots.
#' @param circular.plots Rmarkdown string specifying the name of the circular plots.
#' @param sensor.plots Rmarkdown string specifying the name of the sensor plots.
#' @param detections All the detections used in the study
#' @param valid.detections The valid detections used in the study
#' @inheritParams loadDetections
#'
#' @return No return value, called for side effects.
#'
#' @keywords internal
#'
printExploreRmd <- function(override.fragment, biometric.fragment, individual.plots,
  circular.plots, sensor.plots, spatial, deployments, detections, valid.detections, detections.y.axis){

  work.path <- paste0(tempdir(), "/actel_report_auxiliary_files/")

 if (!is.null(spatial$unknowns)) {
    unknown.fragment <- paste0('<span style="color:red"> Number of relevant unknown receivers: **', sum(sapply(spatial$unknowns, length)), '** (of which ', length(spatial$unknowns$included),' were included)</span>\n')
  } else {
    unknown.fragment <- ""
  }
  if (!is.null(sensor.plots)) {
    sensor.fragment <- paste0("### Sensor plots

Note:
  : The colouring in these plots will follow that of the individual detection plots, which can be modified using `detections.y.axis`.
  : The data used for these graphics is stored in the `valid.detections` object.
  : You can replicate these graphics and edit them as needed using the `plotSensors()` function.

<center>\n", sensor.plots, "\n</center>")
  } else {
    sensor.fragment <- NULL
  }

  report <- readr::read_file(paste0(tempdir(), "/temp_log.txt"))
  report <- gsub("(\\\\|\")", "\\\\\\1", report)

  if (file.exists(paste0(tempdir(), '/temp_warnings.txt'))) {
    warning.messages <- gsub("\\r", "", readr::read_file(paste0(tempdir(), '/temp_warnings.txt')))
    warning.messages <- gsub("(\\\\|\")", "\\\\\\1", warning.messages)
  } else {
    warning.messages <- 'No warnings were raised during the analysis.'
  }

  if (file.exists(paste0(tempdir(), '/temp_comments.txt'))) {
    comment.fragment <- gsub("\\r", "", readr::read_file(paste0(tempdir(), '/temp_comments.txt')))
    comment.fragment <- gsub("(\\\\|\")", "\\\\\\1", comment.fragment)
  } else {
    comment.fragment <- 'No comments were included during the analysis.'
  }

  oldoptions <- options(knitr.kable.NA = "-")
  on.exit(options(oldoptions), add = TRUE)

  sink(paste0(work.path, "actel_explore_report.Rmd"))
  cat(paste0(
'---
title: "Acoustic telemetry exploratory analysis"
author: "Actel R package (', utils::packageVersion("actel"), ')"
output:
  html_document:
    includes:
      after_body: ', work.path, 'toc_menu_explore.html
---

### Summary

Target folder: ', stringr::str_extract(pattern = '(?<=Target folder: )[^\r|^\n]*', string = report), '

Timestamp: **', stringr::str_extract(pattern = '(?<=Timestamp: )[^\r|^\n]*', string = report), '**

Number of target tags: **`r I(nrow(bio))`**

', override.fragment,'

Number of listed receivers: **', stringr::str_extract(pattern = '(?<=Number of ALS: )[0-9]*', string = report), '** (of which **', stringr::str_extract(pattern = '(?<=of which )[0-9]*', string = report), '** had no detections)

', unknown.fragment,'

Data time range: ', stringr::str_extract(pattern = '(?<=Data time range: )[^\r|^\n]*', string = report), '

Percentage of post-release valid detections: ', round(sum(unlist(lapply(valid.detections, nrow))) / sum(unlist(lapply(detections, nrow))) * 100, 2), '%

Found a bug? [**Report it here.**](https://github.com/hugomflavio/actel/issues)

Want to cite actel in a publication? Run `citation(\'actel\')`

### Study area

Release sites are marked with "R.S.". Arrays connected with an arrow indicate that the tags can only pass in one direction.

<img src=', work.path, ifelse(file.exists(paste0(work.path, "mb_arrays.svg")), "mb_arrays.svg", "mb_arrays.png"), ' style="padding-top: 15px;"/>

### Receiver stations

', paste(knitr::kable(spatial$stations, row.names = FALSE), collapse = "\n"), '

### Deployments

', paste(knitr::kable(deployments, row.names = FALSE), collapse = "\n"), '

### Release sites

', paste(knitr::kable(spatial$release.sites, row.names = FALSE), collapse = "\n"), '

### Warning messages

```{r warnings, echo = FALSE, comment = NA}
cat("', warning.messages, '")
```

### User comments

```{r comments, echo = FALSE, comment = NA}
cat("', comment.fragment, '")
```

', ifelse(biometric.fragment == '', '', paste0('### Biometric graphics

Note:
  : The data used in this graphic is the data present in the biometrics.csv file.

<center>
', biometric.fragment,'
</center>
')), '

### Average time of arrival at each Array

Note:
  : Coloured lines on the outer circle indicate the mean value for each group and the respective ranges show the standard error of the mean. Each group\'s bars sum to 100%. The number of data points in each group is presented between brackets in the legend of each pannel.
  : You can replicate these graphics and edit them as needed using the `plotTimes()` function.
  : The data used in these graphics is stored in the `times` object.
  : To obtain reports with the legacy linear circular scale, run `options(actel.circular.scale = "linear")` before running your analyses.

<center>
', circular.plots,'
</center>


### Individual detection plots

Note:
  : You can choose to plot detections by station or by array using the `detections.y.axis` argument.
  : The detections are coloured by ', ifelse(detections.y.axis == "stations", 'array', 'section'), '. The vertical black dashed line shows the release time. The full dark-grey line shows the movement events considered valid, while the dashed dark-grey line shows the movement events considered invalid.
', ifelse(detections.y.axis == "stations", '  : The movement event lines move straight between the first and last station of each event (i.e. in-between detections will not be individually linked by the line).\n', ''),
'  : Manually **edited** tags are highlighted with **yellow** graphic borders.
  : The ', ifelse(detections.y.axis == "stations", 'stations', 'arrays'), ' have been aligned by ', ifelse(detections.y.axis == "stations", 'array', 'section'), ', following the order provided ', ifelse(detections.y.axis == "stations", '', 'either '), 'in the spatial input', ifelse(detections.y.axis == "stations", '.', ' or the `section.order` argument.'), '
  : You can replicate these graphics and edit them as needed using the `plotDetections()` function.
  : You can also see the movement events of multiple tags simultaneously using the `plotMoves()` function.
  : The data used in these graphics is stored in the `detections` and `movements` objects (and respective valid counterparts).

<center>
', individual.plots,'
</center>

', sensor.fragment,'

### Full log

```{r log, echo = FALSE, comment = NA}
cat("', gsub("\\r", "", report), '")
```

'), fill = TRUE)
sink()

sink(paste0(work.path, "toc_menu_explore.html"))
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
  <a href="#study-area">Study area</a>
  <a href="#receiver-stations">Stations</a>
  <a href="#deployments">Deployments</a>
  <a href="#release-sites">Release sites</a>
  <a href="#warning-messages">Warnings</a>
  <a href="#user-comments">Comments</a>',
  ifelse(biometric.fragment == '', '', '\n  <a href="#biometric-graphics">Biometrics</a>'),'
  <a href="#average-time-of-arrival-at-each-array">Arrival times</a>
  <a href="#individual-detection-plots">Individual detections</a>',
  ifelse(is.null(sensor.fragment), '', '\n  <a href="#sensor-plots">Sensor data</a>'),'
  <a href="#full-log">Full log</a>
</div>
', fill = TRUE)
sink()
}

#' Compare original detections with the valid movements and exclude invalid detections
#'
#' @param detections.list The list of detections per tag
#' @param movements The list of movements to be matched
#'
#' @return A list containing the valid detections per tag.
#'
#' @keywords internal
#'
validateDetections <- function(detections.list, movements) {
  Valid <- NULL
  counter <- 0
  if (interactive())
    pb <- txtProgressBar(min = 0, max = sum(unlist(lapply(movements, nrow))), style = 3, width = 60) # nocov
  output.all <- lapply(names(detections.list), function(i) {
    # cat(i, "\n")
    aux <- detections.list[[i]]
    aux$Valid <- FALSE
    if (!is.null(movements[[i]])) {
      counter <<- counter + nrow(movements[[i]])
      valid.rows <- unlist(lapply(1:nrow(movements[[i]]), function(j) {
        start <- min(which(aux$Timestamp == movements[[i]]$First.time[j] & aux$Standard.name == movements[[i]]$First.station[j]))
        stop <- start + (movements[[i]]$Detections[j] - 1)
        # cat(j, ":", start, ":", stop, "\n"); flush.console()
        return(start:stop)
      }))
      aux$Valid[valid.rows] <- TRUE
    }
    if (interactive())
      setTxtProgressBar(pb, counter) # nocov
    return(data.table::as.data.table(aux))
  })
  if(interactive())
    close(pb) # nocov
  names(output.all) <- names(detections.list)
  attributes(output.all)$actel <- "all.detections"
  output.valid <- lapply(output.all, function(x) {
    if (any(x$Valid))
      return(x[(Valid)])
    else
      return(NULL)
  })
  output.valid <- output.valid[!sapply(output.valid, is.null)]
  attributes(output.valid)$actel <- "valid.detections"
  return(list(detections = output.all, valid.detections = output.valid))
}
