#' Explorative Analysis
#' 
#' \code{explore} allows you to quickly get a summary of your data. You can use
#' \code{explore} to get a general feel for the study results, and check if the
#' input files are behaving as expected. It is also a good candidate if you just
#' want to validate your detections for later use in other analyses.
#' 
#' @param auto.open Logical: Should the report be automatically opened once the
#'  analysis is over? Defaults to TRUE.
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
#' @param inactive.error If a fish spends a number of days equal or greater than 
#'  \code{inactive.error} in a given array at the tail of the respective 
#'  detections, user intervention is suggested. If left NULL (default), user 
#' intervention is never suggested.
#' @param inactive.warning If a fish spends a number of days equal or greater 
#'  than \code{inactive.error} in a given array at the tail of the respective 
#'  detections, a warning is issued. If left NULL (default), no warnings are
#'  issued.
#' @param jump.error If a fish crosses a number of arrays equal or greater than
#'  \code{jump.error} without being detected, user intervention is suggested.
#'  If left NULL (default), user intervention is never suggested.
#' @param jump.warning If a fish crosses a number of arrays equal or greater 
#'  than \code{jump.error} without being detected, a warning is issued. If left 
#'  NULL (default), no warnings are issued.
#' @param max.interval The number of minutes that must pass between detections 
#'  for a new event to be created. Defaults to 60.
#' @param minimum.detections For tags with only one movement event, defines the
#'  minimum number of times a tag must have been recorded during the study 
#'  period for it to be considered true detections and not random noise.
#'  Defaults to 2.
#' @param override A vector of tags for which the user intends to manually 
#' define which movement events are valid and invalid.
#' @param print.releases Logical: Should the release sites be printed in the
#'  study area diagrams?
#' @param report Logical. Should an HTML report be created at the end of the
#'  analysis?
#' @param speed.error If a fish moves at a speed equal or greater than 
#'  \code{speed.error} (in metres per second), user intervention is suggested. 
#'  If left NULL (default), user intervention is never suggested. 
#' @param speed.method Can take two forms: 'last to first' or 'first to first'. 
#'  If 'last to first' (default), the last detection on a given array is matched 
#'  to the first detection on the next array to perform the calculations. 
#'  If 'first to first', the first detection on a given array is matched to the
#'  first detection on the next array to perform the calculations.
#' @param speed.warning If a fish moves at a speed equal or greater than 
#'  \code{speed.warning} (in metres per second), a warning is issued. If left 
#'  NULL (default), no warnings are issued.
#' @param start.time Detection data prior to the timestamp set in 
#'  \code{start.time} (in YYYY-MM-DD HH:MM:SS format) is not considered during 
#'  the analysis.
#' @param save.detections Logical: Should the processed detections be saved for
#'  future runs?
#' @param stop.time Detection data posterior to the timestamp set in 
#'  \code{stop.time} (in YYYY-MM-DD HH:MM:SS format) is not considered during 
#'  the analysis.
#' @param tz The time zone of the study area. Must match one of the values
#'  present in \code{\link[base]{timezones}}.
#' 
#' @return A list containing:
#' \itemize{
#'  \item \code{detections}: All detections for each target fish;
#'  \item \code{valid.detections}: Valid detections for each target fish;
#'  \item \code{spatial}: The spatial information used during the analysis;
#'  \item \code{deployments}: The deployments of each receiver;
#'  \item \code{arrays}: The array details used during the analysis;
#'  \item \code{movements}: All movement events for each target fish;
#'  \item \code{valid.movements}: Valid movement events for each target fish;
#'  \item \code{times}: All arrival times (per fish) at each array;
#'  \item \code{rsp.info}: Appendix information for the RSP package;
#'  \item \code{dist.mat}: The distance matrix used in the analysis (if a valid
#'   distance matrix was supplied)
#' }
#' 
#' @seealso \code{\link{migration}}, \code{\link{residency}}

#' @export
#' 
explore <- function(tz, max.interval = 60, minimum.detections = 2, start.time = NULL, stop.time = NULL, 
  speed.method = c("last to first", "first to first"), speed.warning = NULL, speed.error = NULL, 
  jump.warning = 2, jump.error = 3, inactive.warning = NULL, inactive.error = NULL, 
  exclude.tags = NULL, override = NULL, report = FALSE, auto.open = TRUE, save.detections = FALSE, 
  GUI = c("needed", "always", "never"), print.releases = TRUE) {

# check arguments quality
  if (is.null(tz) || is.na(match(tz, OlsonNames())))
    stop("'tz' could not be recognized as a timezone. Check available timezones with OlsonNames()\n", call. = FALSE)
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

  if (!is.logical(print.releases))
    stop("'print.releases' must be logical.\n", call. = FALSE)
# ------------------------

# Prepare clean-up before function ends
  if (file.exists(paste0(tempdir(), "/actel_debug_file.txt")))
    file.remove(paste0(tempdir(), "/actel_debug_file.txt"))
  on.exit(deleteHelpers(), add = TRUE)
  on.exit(tryCatch(sink(), warning = function(w) {hide <- NA}), add = TRUE)
# --------------------------------------

# Store function call
  the.function.call <- paste0("explore(tz = ", ifelse(is.null(tz), "NULL", paste0("'", tz, "'")), 
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
      ", inactive.error = ", ifelse(is.null(inactive.error), "NULL", inactive.error), 
      ", exclude.tags = ", ifelse(is.null(exclude.tags), "NULL", paste0("c('", paste(exclude.tags, collapse = "', '"), "')")), 
      ", override = ", ifelse(is.null(override), "NULL", paste0("c('", paste(override, collapse = "', '"), "')")),
      ", report = ", ifelse(report, "TRUE", "FALSE"), 
      ", auto.open = ", ifelse(auto.open, "TRUE", "FALSE"), 
      ", save.detections = ", ifelse(save.detections, "TRUE", "FALSE"),       
      ", GUI = '", GUI, "'",
      ", print.releases = ", ifelse(print.releases, "TRUE", "FALSE"), 
      ")")
# --------------------

# Final arrangements before beginning
  appendTo("Report", paste0("Actel R package report.\nVersion: ", utils::packageVersion("actel"), "\n"))

  appendTo(c("Report"), paste0("Target folder: ", getwd(), "\nTimestamp: ", the.time <- Sys.time(), "\nFunction: explore()\n"))

  report <- checkReport(report = report)
# -----------------------------------

# Load, structure and check the inputs
study.data <- loadStudyData(tz = tz, override = override, save.detections = save.detections,
                            start.time = start.time, stop.time = stop.time,
                            sections = NULL, exclude.tags = exclude.tags)
bio <- study.data$bio
sections <- study.data$sections
deployments <- study.data$deployments
spatial <- study.data$spatial
dot <- study.data$dot
arrays <- study.data$arrays
dotmat <- study.data$dotmat
detections <- study.data$detections
dist.mat <- study.data$dist.mat
invalid.dist <- study.data$invalid.dist
detections.list <- study.data$detections.list
# -------------------------------------
  
# Process the data
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

  appendTo(c("Screen", "Report"), "M: Filtering valid array movements.")

  valid.movements <- lapply(seq_along(movements), function(i){
    output <- simplifyMovements(movements = movements[[i]], fish = names(movements)[i], bio = bio, 
      speed.method = speed.method, dist.mat = dist.mat, invalid.dist = invalid.dist)
  })
  names(valid.movements) <- names(movements)
  valid.movements <- valid.movements[!unlist(lapply(valid.movements, is.null))]

  aux <- list(valid.movements = valid.movements, spatial = spatial, rsp.info = list(bio = bio, analysis.type = "explore"))
  times <- getTimes(input = aux, move.type = "array", event.type = "arrival", n.events = "first")
  rm(aux)

  appendTo("Screen", "M: Validating detections...")

  recipient <- validateDetections(detections.list = detections.list, movements = valid.movements)
  detections <- recipient$detections
  valid.detections <- recipient$valid.detections
  rm(recipient)
# -------------------------------------

# wrap up in-R objects
  deployments <- do.call(rbind.data.frame, deployments)
  
  # extra info for potential RSP analysis
  rsp.info <- list(analysis.type = "explore", analysis.time = the.time, bio = bio, tz = tz, actel.version = utils::packageVersion("actel"))

  if (!is.null(override))
    override.fragment <- paste0('<span style="color:red">Manual mode has been triggered for **', length(override),'** fish.</span>\n')
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

  decision <- readline(paste0("Would you like to save a copy of the results to ", resultsname, "?(y/N) "))
  appendTo("UD", decision)

  if (decision == "y" | decision == "Y") {
    appendTo(c("Screen", "Report"), paste0("M: Saving results as '", resultsname, "'."))
    if (invalid.dist)
      save(detections, valid.detections, spatial, deployments, arrays, movements, valid.movements, times, rsp.info, file = resultsname)
    else
      save(detections, valid.detections, spatial, deployments, arrays, movements, valid.movements, times, rsp.info, dist.mat, file = resultsname)
  } else {
    appendTo(c("Screen", "Report"), paste0("M: Skipping saving of the results."))
  }
  rm(decision)

# ------------

# Print graphics
  if (report) {
    appendTo(c("Screen", "Report"), "M: Producing the report.")
    biometric.fragment <- printBiometrics(bio = bio)
    printDot(dot = dot, sections = NULL, spatial = spatial, print.releases = print.releases)
    individual.plots <- printIndividuals(detections.list = detections, spatial = spatial, 
      tz = tz, movements = movements, valid.movements = valid.movements, bio = bio)
    circular.plots <- printCircular(times = timesToCircular(times), bio = bio)
    if (any(sapply(valid.detections, function(x) any(!is.na(x$Sensor.Value))))) {
      appendTo(c("Screen", "Report"), "M: Printing sensor values for tags with sensor data.")
      sensor.plots <- printSensorData(detections = valid.detections)
    } else {
      sensor.plots <- NULL
    } 
  }
  
  appendTo("Report", "M: Process finished successfully.")
# ---------------
  
# wrap up the txt report
  appendTo("Report", "\n-------------------")
  if (file.exists(paste(tempdir(), "temp_UD.txt", sep = "/")))
    appendTo("Report", paste0("User interventions:\n-------------------\n", gsub("\r", "", readr::read_file(paste(tempdir(), "temp_UD.txt", sep = "/"))), "-------------------")) # nocov
  
  appendTo("Report", paste0("Function call:\n-------------------\n", the.function.call, "\n-------------------"))
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
                    valid.detections = valid.detections)

    appendTo("debug", "debug: Converting report to html")
    rmarkdown::render(input = paste0(tempdir(), "/actel_explore_report.Rmd"), 
      output_dir = tempdir(), quiet = TRUE)

    appendTo("debug", "debug: Moving report")
    file.copy(paste0(tempdir(), "/actel_explore_report.html"), reportname)
    if (interactive() & auto.open) { # nocov start
      appendTo("debug", "debug: Opening report.")
      browseURL(reportname)
    } # nocov end
  }
# ------------------
  
  jobname <- paste0(gsub(" |:", ".", as.character(Sys.time())), ".actel.log.txt")

  if (!report) {
    decision <- readline(paste0("Would you like to save a copy of the analysis log to ", jobname, "?(y/N) "))
    appendTo("UD", decision)

    if (decision == "y" | decision == "Y") {
      appendTo("Screen", paste0("M: Saving job log as '",jobname, "'."))
      file.copy(paste(tempdir(), "temp_log.txt", sep = "/"), jobname)
    }
  }

  appendTo("Screen", "M: Process finished successfully.")

  if (invalid.dist) {
    return(list(detections = detections, valid.detections = valid.detections, spatial = spatial, deployments = deployments, arrays = arrays,
      movements = movements, valid.movements = valid.movements, times = times, rsp.info = rsp.info))
  } else {
    return(list(detections = detections, valid.detections = valid.detections, spatial = spatial, deployments = deployments, arrays = arrays,
      movements = movements, valid.movements = valid.movements, times = times, rsp.info = rsp.info, dist.mat = dist.mat))
  }
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
#' @param valid.detectiosn The valid detections used in the study
#' @inheritParams loadDetections
#' 
#' @keywords internal
#' 
printExploreRmd <- function(override.fragment, biometric.fragment, individual.plots,
  circular.plots, sensor.plots, spatial, deployments, detections, valid.detections){
 if (any(grepl("Ukn.", spatial$stations$Standard.name))) {
    unknown.fragment <- paste0('<span style="color:red"> Number of relevant unknown receivers: **', sum(grepl("Ukn.", spatial$stations$Standard.name)), '**</span>\n')
  } else {
    unknown.fragment <- ""
  } 
  if (!is.null(sensor.plots)) {
    sensor.fragment <- paste0("### Sensor plots

Note:
  : The data used for these graphics is stored in the `valid.detections` object.

<center>\n", sensor.plots, "\n</center>")
  } else {
    sensor.fragment <- NULL
  }

  report <- readr::read_file(paste0(tempdir(), "/temp_log.txt"))

  oldoptions <- options(knitr.kable.NA = "-")
  on.exit(options(oldoptions), add = TRUE)

  sink(paste0(tempdir(), "/actel_explore_report.rmd"))
  cat(paste0(
'---
title: "Acoustic telemetry exploratory analysis"
author: "Actel R package (', utils::packageVersion("actel"), ')"
output: 
  html_document:
    includes:
      after_body: ', tempdir(), '/toc_menu_explore.html
---

### Summary

Target folder: ', stringr::str_extract(pattern = '(?<=Target folder: )[^\r]*', string = report), '

Timestamp: **', stringr::str_extract(pattern = '(?<=Timestamp: )[^\r]*', string = report), '** 

Number of target tags: **`r I(nrow(bio))`**

', override.fragment,' 

Number of listed receivers: **', stringr::str_extract(pattern = '(?<=Number of ALS: )[0-9]*', string = report), '** (of which **', stringr::str_extract(pattern = '(?<=of which )[0-9]*', string = report), '** had no detections)

', unknown.fragment,'

Data time range: ', stringr::str_extract(pattern = '(?<=Data time range: )[^\r]*', string = report), '

Percentage of post-release valid detections: ', round(sum(unlist(lapply(valid.detections, nrow))) / sum(unlist(lapply(detections, nrow))) * 100, 2), '%

Found a bug? [**Report it here.**](https://github.com/hugomflavio/actel/issues)

### Study area

Release sites are marked with "R.S.". Arrays connected with an arrow indicate that the fish can only pass in one direction.

<img src="', tempdir(), '/mb_arrays.svg" alt="Missing file" style="padding-top: 15px;"/>

### Receiver stations

', paste(knitr::kable(spatial$stations, row.names = FALSE), collapse = "\n"), '

### Deployments

', paste(knitr::kable(deployments, row.names = FALSE), collapse = "\n"), '

### Release sites

', paste(knitr::kable(spatial$release.sites, row.names = FALSE), collapse = "\n"), '

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

### Average time of arrival at each Array

Note:
  : Coloured lines on the outer circle indicate the mean value for each group and the respective ranges show the standard error of the mean. Each group\'s bars sum to 100%. The number of data points in each group is presented between brackets in the legend of each pannel. 
  : The data used in these graphics is stored in the `times` object.

<center>
', circular.plots,'
</center>


### Individual plots

Note:
  : The detections are coloured by array. The full dark-grey line shows the movement events considered valid, while the dashed dark-grey line shows the movement events considered invalid.
  : The movement event lines move straight between the first and last station of each event (i.e. in-between detections will not be individually linked by the line).
  : Manually **edited** fish are highlighted with **yellow** graphic borders.
  : The stations have been grouped by array, following the array order provided either in the spatial.csv file or in the spatial.txt file.
  : The data used in these graphics is stored in the `detections` and `movements` objects (and respective valid counterparts).

<center>
', individual.plots,'
</center>

', sensor.fragment,'

### Full log

```{r log, echo = FALSE, comment = NA}
cat("', gsub("\\r", "", readr::read_file(paste0(tempdir(), '/temp_log.txt'))), '")
```

'), fill = TRUE)
sink()

sink(paste0(tempdir(), "/toc_menu_explore.html"))
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
  <a href="#individual-plots">Individuals</a>',
  ifelse(is.null(sensor.fragment), '', '\n  <a href="#sensor-plots">Sensor data</a>'),'
  <a href="#full-log">Full log</a>
</div>
', fill = TRUE)
sink()
}

#' Compare original detections with the valid movements and exclude invalid detections
#' 
#' @param detections.list The list of detections per fish
#' @param movements The list of movements to be matched
#' 
#' @return A list of valid detections per fish
#' 
#' @keywords internal
#' 
validateDetections <- function(detections.list, movements) {
  Valid <- NULL
  counter <- 0
  if (interactive())
    pb <- txtProgressBar(min = 0, max = sum(unlist(lapply(movements, nrow))), style = 3, width = 60)
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
      setTxtProgressBar(pb, counter)
    return(data.table::as.data.table(aux))
  })
  if(interactive())
    close(pb)
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


