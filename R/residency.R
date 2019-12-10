#' Residency analysis
#' 
#' The actel package provides a systematic way of analysing fish residency data.
#' residency() collects the input present in the target folder and analyses the telemetry data, extracting residency-related metrics.
#' It is strongly recommended to read the package vignettes before attempting to run the analyses. You can find the vignettes by running browseVignettes('actel') .
#'  
#' @inheritParams migration
#' @inheritParams explore
#' @param section.minimum threshold consecutive number of detections in a section that awards confidence in the results. If the number of detections is lower than this argument, a warning is issued and user interaction is allowed.
#' 
#' @return A list containing 1) the detections used during the analysis, 2) the movement events, 3) the status dataframe, 4) the survival overview per group, 5) the progression through the study area, 6) the ALS array/sections' efficiency, 7) the list of spatial objects used during the analysis.
#' 
#' @export
#' 
residency <- function(path = NULL, sections, section.minimum = 2, 
  max.interval = 60, maximum.time = 60, speed.method = c("last to first", "first to first"), speed.warning = NULL,
  speed.error = NULL, if.last.skip.section = TRUE, tz.study.area = NULL, tz = NULL, start.time = NULL, start.timestamp = NULL, 
  stop.time = NULL, end.timestamp = NULL, report = TRUE, exclude.tags = NULL, replicates = NULL,
  jump.warning = 2, jump.error = 3, inactive.warning = NULL, inactive.error = NULL, debug = FALSE) {
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
  if (!is.numeric(section.minimum))
    stop("'section.minimum' must be numeric.\n", call. = FALSE)
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
  
  if (!is.null(inactive.warning) && !is.numeric(inactive.warning))
    stop("'inactive.warning' must be numeric.\n", call. = FALSE)    
  if (!is.null(inactive.error) && !is.numeric(inactive.error))
    stop("'inactive.error' must be numeric.\n", call. = FALSE)    
  if (!is.null(inactive.error) & is.null(inactive.warning))
    inactive.warning <- inactive.error
  if (!is.null(inactive.error) && inactive.error < inactive.warning)
    stop("'inactive.error' must not be lower than 'inactive.warning'.\n", call. = FALSE)

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
      ", max.interval = ", max.interval,
      ", speed.method = ", paste0("c('", speed.method, "')"),
      ", speed.warning = ", ifelse(is.null(speed.warning), "NULL", speed.warning), 
      ", speed.error = ", ifelse(is.null(speed.error), "NULL", speed.error), 
      ", if.last.skip.section = ", ifelse(if.last.skip.section, "TRUE", "FALSE"),
      ", tz = ", ifelse(is.null(tz), "NULL", paste0("'", tz, "'")), 
      ", start.time = ", ifelse(is.null(start.time), "NULL", paste0("'", start.time, "'")),
      ", stop.time = ", ifelse(is.null(stop.time), "NULL", paste0("'", stop.time, "'")),
      ", report = ", ifelse(report, "TRUE", "FALSE"), 
      ", exclude.tags = ", ifelse(is.null(exclude.tags), "NULL", paste0("c('", paste(exclude.tags, collapse = "', '"), "')")), 
      ", replicates = ", ifelse(is.null(replicates),"NULL", paste0("c('", paste(replicates, collapse = "', '"), "')")),
      ", jump.warning = ", jump.warning,
      ", jump.error = ", jump.error,
      ", inactive.warning = ", ifelse(is.null(inactive.warning), "NULL", inactive.warning), 
      ", inactive.error = ", ifelse(is.null(inactive.error), "NULL", inactive.error), 
      ", debug = ", ifelse(debug, "TRUE", "FALSE"), 
      ")")
# --------------------

# Final arrangements before beginning
  appendTo("Report", "Acoustic telemetry data analysis report.\n") 

  path <- checkPath(my.home = my.home, path = path)  

  if (debug)
    appendTo("Report", "!!!--- Debug mode has been activated ---!!!\n")

  appendTo(c("Report"), paste0("Timestamp:", the.time <- Sys.time(), "\n\nM: Selected folder: ", getwd()))

  if (!is.null(path))
    appendTo(c("Screen"), "M: Moving to selected work directory")
  
  report <- checkReport(report = report)
# -----------------------------------

# Load, structure and check the inputs
study.data <- loadStudyData(tz = tz, override = NULL,
                            start.time = start.time, stop.time = stop.time,
                            sections = sections, exclude.tags = exclude.tags)
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
# -------------------------------------
  
# Process the data
  appendTo(c("Screen", "Report"), "M: Creating movement records for the valid tags.")
  movements <- groupMovements(detections.list = detections.list, bio = bio, spatial = spatial,
                              speed.method = speed.method, max.interval = max.interval, 
                              tz = tz, dist.mat = dist.mat, invalid.dist = invalid.dist)

  aux <- names(movements)
  movements <- lapply(names(movements), 
    function(fish) {
      speedReleaseToFirst(fish = fish, bio = bio, movements = movements[[fish]],
                          dist.mat = dist.mat, invalid.dist = invalid.dist, silent = FALSE)
    })
  names(movements) <- aux
  rm(aux)

  appendTo(c("Screen", "Report"), "M: Checking movement events quality.")

  movements <- checkImpassables(movements = movements, dotmat = dotmat)

  movements <- checkJumpDistance(movements = movements, bio = bio, dotmat = dotmat, 
                                 spatial = spatial, jump.warning = jump.warning, jump.error = jump.error)

  if (is.null(speed.warning)) {
    appendTo(c("Screen", "Report", "Warning"), "M: 'speed.warning'/'speed.error' were not set, skipping speed checks.")
  } else {
    if(invalid.dist) {
      appendTo(c("Screen", "Report", "Warning"), "W: 'speed.warning'/'speed.error' were set, but a valid distance matrix is not present. Aborting speed checks.")
    } else {
       temp.valid.movements <- simplifyMovements(movements = movements, bio = bio, 
         speed.method = speed.method, dist.mat = dist.mat, invalid.dist = invalid.dist)
      movements <- checkSpeeds(movements = movements, valid.movements = temp.valid.movements, 
        speed.warning = speed.warning, speed.error = speed.error)
       rm(temp.valid.movements)
     }
  }
  
  if (is.null(inactive.warning))
    appendTo(c("Screen", "Report", "Warning"), "M: 'inactive.warning'/'inactive.error' were not set, skipping inactivity checks.")
  else
    movements <- checkInactiveness(movements = movements, detections.list = detections.list, 
      inactive.warning = inactive.warning, inactive.error = inactive.error, 
      dist.mat = dist.mat, invalid.dist = invalid.dist)

  valid.movements <- simplifyMovements(movements = movements, bio = bio, 
    speed.method = speed.method, dist.mat = dist.mat, invalid.dist = invalid.dist)

  # Residency-exclusive area

  # Compress array movements into section movements
  section.movements <- sectionMovements(movements = valid.movements, sections = sections, invalid.dist = invalid.dist)
  # Look for isolated section movements
  section.movements <- checkSMovesN(secmoves = section.movements, section.minimum = section.minimum)
  # Update array movements based on secmove validity
  movements <- updateAMValidity(arrmoves = movements, secmoves = section.movements)

  # Resimplify array moves and calculate simple section moves
  valid.movements <- simplifyMovements(movements = movements, bio = bio, 
    speed.method = speed.method, dist.mat = dist.mat, invalid.dist = invalid.dist)

  valid.section.movements <- sectionMovements(movements = valid.movements, sections = sections, invalid.dist = invalid.dist)

  # Grab summary information
  res.df <- assembleResidency(secmoves = valid.section.movements, movements = valid.movements, sections = sections)
  
  appendTo(c("Screen", "Report"), "M: Timetable successfully filled. Fitting in the remaining variables.")
  
  status.df <- res_assembleOutput(res.df = res.df, bio = bio, spatial = spatial, 
                                  sections = sections, tz = tz)

  last.seen <- as.data.frame.matrix(with(status.df, table(Group, Status)))

  array.times <- getTimes(movements = valid.movements, spatial = spatial, type = "arrival", events = "all")

  section.times <- list(
    arrival = getTimes(movements = valid.section.movements, spatial = spatial, type = "arrival", events = "all"),
    departure = getTimes(movements = valid.section.movements, spatial = spatial, type = "departure", events = "all"))

  residency.list <- getResidency(movements = valid.section.movements, spatial = spatial)
  
  appendTo(c("Screen", "Report"), "M: Calculating daily locations for each fish.")

  daily.ratios <- dailyRatios(res = residency.list)

  daily.positions <- dailyPositions(ratios = daily.ratios)

  global.ratios <- globalRatios(positions = daily.positions)

  appendTo("Screen", "M: Validating detections...")

  valid.detections <- validateDetections(detections.list = detections.list, movements = valid.movements)

# ---------------
  
# Efficiency
  appendTo(c("Screen", "Report"), "M: Calculating array efficiency.")
  efficiency <- res_efficiency(arrmoves = valid.movements, bio = bio, spatial = spatial, arrays = arrays, paths = paths, dotmat = dotmat)
  if (!is.null(replicates)) {
    intra.array.matrices <- getDualMatrices(replicates = replicates, CJS = NULL, spatial = spatial, detections.list = detections.list)
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
  if (file.exists(resultsname <- paste("actel_residency_results.RData", sep = ""))) {
    continue <- TRUE
    index <- 1
    while (continue) {
      if (file.exists(resultsname <- paste("actel_residency_results.", index, ".RData", sep = ""))) {
        index <- index + 1
      } else {
        continue <- FALSE
      }
    }
    appendTo("Screen", paste("M: An actel residency results file is already present in the current directory.\n   Saving new results as '", resultsname,"'.", sep = ""))
    rm(continue, index)
  } else {
    appendTo(c("Screen", "Report"), paste("M: Saving results to '", resultsname, "'.", sep = ""))
  }

  detections <- detections.list
  deployments <- do.call(rbind.data.frame, deployments)

  if (!debug)
    efficiency <- efficiency[1:3]

  # extra info for potential RSP analysis
  rsp.info <- list(analysis.type = "residency", analysis.time = the.time, bio = bio, tz = tz)

  if (invalid.dist)
    save(detections, valid.detections, spatial, deployments, arrays, movements, valid.movements, 
      section.movements, valid.section.movements, status.df, last.seen, array.times, section.times,
      residency.list, daily.ratios, daily.positions, global.ratios, efficiency, intra.array.CJS, rsp.info, file = resultsname)
  else
    save(detections, valid.detections, spatial, deployments, arrays, movements, valid.movements, 
      section.movements, valid.section.movements, status.df, last.seen, array.times, section.times,
      residency.list, daily.ratios, daily.positions, global.ratios, efficiency, intra.array.CJS, rsp.info, dist.mat, file = resultsname)
# ------------

# Print graphics
  if (report) {
    appendTo(c("Screen", "Report"), "M: Producing the report.")
    biometric.fragment <- printBiometrics(bio = bio)
    printDot(dot = dot, sections = sections, spatial = spatial)
    printSectionTimes(section.times = section.times, bio = bio, detections = valid.detections)
    printGlobalRatios(ratios = global.ratios)
    individual.detection.plots <- printIndividuals(redraw = TRUE, detections.list = detections.list, bio = bio, 
        tz = tz, movements = movements, valid.movements = valid.movements)
    array.circular.plots <- printCircular(times = convertTimesToCircular(array.times), bio = bio, suffix = "_array")
    section.arrival.circular.plots <- printCircular(times = convertTimesToCircular(section.times$arrival), bio = bio, suffix = "_array")
    section.departure.circular.plots <- printCircular(times = convertTimesToCircular(section.times$departure), bio = bio, suffix = "_array")
    appendTo(c("Screen", "Report"), "M: Drawing individual residency graphics.")
    dayrange <- range(as.Date(global.ratios[[1]]$Date))
    dayrange[1] <- dayrange[1] - 1
    dayrange[2] <- dayrange[2] + 1
    individual.residency.plots <- printIndividualResidency(ratios = daily.ratios, dayrange = dayrange)
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
    appendTo("Report", paste0("User interventions:\n-------------------\n", gsub("\r", "", readr::read_file("temp_UD.txt")), "-------------------"))
  
  appendTo("Report", paste0("Function call:\n-------------------\n", the.function.call, "\n-------------------"))
# ------------------

# print html report
  if (report) {
    appendTo("debug", "debug: Printing report")
    rmarkdown::render(reportname <- printResidencyRmd(biometric.fragment = biometric.fragment, efficiency.fragment = efficiency.fragment,
        array.circular.plots = array.circular.plots, section.arrival.circular.plots = section.arrival.circular.plots, 
        section.departure.circular.plots = section.departure.circular.plots, individual.detection.plots = individual.detection.plots, 
        individual.residency.plots = individual.residency.plots, spatial = spatial, detections = detections, valid.detections = valid.detections,
        last.seen = last.seen, last.seen.graph.size = last.seen.graph.size), quiet = TRUE)
    appendTo("debug", "debug: Moving report")
    fs::file_move(sub("Rmd", "html", reportname), sub("Report/", "", sub("Rmd", "html", reportname)))
    appendTo("debug", "debug: Opening report if the pc has internet.")
    openReport(file.name = sub("Report/", "", sub("Rmd", "html", reportname)))
  }
  appendTo("Screen", "M: Process finished successfully.")
# ------------------

  appendTo("Screen", paste("M: Saving job log as '", paste(gsub(":", ".", sub(" ", ".", as.character(Sys.time()))), "actel.log.txt", sep = "."), "'.", sep = ""))
  file.rename("temp_log.txt", paste(gsub(":", ".", sub(" ", ".", as.character(Sys.time()))), "actel.log.txt", sep = "."))
  
  if (!debug)
    deleteHelpers()

  if (invalid.dist)
    return(list(detections = detections, valid.detections = valid.detections, spatial = spatial, deployments = deployments, arrays = arrays,
      movements = movements, valid.movements = valid.movements, section.movements = section.movements, valid.section.movements = valid.section.movements,
      status.df = status.df, efficiency = efficiency, intra.array.CJS = intra.array.CJS, array.times = array.times, section.times = section.times, 
      residency.list = residency.list, daily.ratios = daily.ratios, daily.positions = daily.positions, global.ratios = global.ratios, last.seen = last.seen, rsp.info = rsp.info))
  else
    return(list(detections = detections, valid.detections = valid.detections, spatial = spatial, deployments = deployments, arrays = arrays,
      movements = movements, valid.movements = valid.movements, section.movements = section.movements, valid.section.movements = valid.section.movements,
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
printResidencyRmd <- function(biometric.fragment, efficiency.fragment, individual.detection.plots, individual.residency.plots, array.circular.plots, 
  section.arrival.circular.plots, section.departure.circular.plots, spatial, deployments, detections, valid.detections, last.seen, last.seen.graph.size){
  if (file.exists(reportname <- paste("Report/actel_residency_report.Rmd", sep = ""))) {
    continue <- TRUE
    index <- 1
    while (continue) {
      if(file.exists(reportname <- paste("Report/actel_residency_report.", index, ".Rmd", sep = ""))) {
        index <- index + 1
      } else {
        continue <- FALSE
      }
    }
    appendTo("Screen",paste("M: An actel report is already present in the current directory\n   Saving new report as 'actel_residency_report.", index, ".html'.", sep = ""))
    rm(continue,index)
  } else {
    appendTo("Screen",paste("M: Saving actel report as 'actel_residency_report.html'.", sep = ""))
  }
  if (any(grepl("Unknown", spatial$stations$Standard.Name))) {
    unknown.fragment <- paste('<span style="color:red"> Number of relevant unknown receivers: **', sum(grepl("Unknown", spatial$stations$Standard.Name)), '**</span>\n', sep = "")
  } else {
    unknown.fragment <- ""
  } 
  report <- readr::read_file("temp_log.txt")
  sink(reportname)
  cat(paste(
'---
title: "Acoustic telemetry residency analysis"
author: "Actel package"
output: 
  html_document:
    includes:
      after_body: toc_menu_residency.html
---

### Summary

Selected folder: ', stringr::str_extract(pattern = '(?<=M: Selected folder: )[^\r|^\n]*', string = report), '

Timestamp: **', stringr::str_extract(pattern = '(?<=Timestamp:)[^\r|^\n]*', string = report), '** 

Number of target tags: **`r I(nrow(status.df))`**

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

### Array efficiency

More information on the differences between "Known missed events" and "Potentially missed events" can be found in the package vignettes.

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


### Last seen

```{r survival, echo = FALSE}
knitr::kable(last.seen)
```

<center>
![](last_seen.png){ ',last.seen.graph.size ,' }
</center>


### Average time of arrival at each Array

Note:
  : Coloured lines on the outer circle indicate the mean value for each group and the respective ranges show the standard error of the mean. Each group\'s bars sum to 100%. The number of data points in each group is presented between brackets in the legend of each pannel. 
  : These graphics have been saved in vectorial format (svg) in the "Results" folder, so you may edit them as needed.

<center>
', array.circular.plots,'
</center>

### Time details for each section

#### Arrival days at each section

<center>
![](arrival_days.png){ width=95% }
</center>

#### Arrival times at each section

Note:
  : Coloured lines on the outer circle indicate the mean value for each group and the respective ranges show the standard error of the mean. Each group\'s bars sum to 100%. The number of data points in each group is presented between brackets in the legend of each pannel. 
  : These graphics have been saved in vectorial format (svg) in the "Results" folder, so you may edit them as needed.

<center>
', section.arrival.circular.plots,'
</center>

#### Departure days at each section

<center>
![](departure_days.png){ width=95% }
</center>

#### Departure times at each section

Note:
  : Coloured lines on the outer circle indicate the mean value for each group and the respective ranges show the standard error of the mean. Each group\'s bars sum to 100%. The number of data points in each group is presented between brackets in the legend of each pannel. 
  : These graphics have been saved in vectorial format (svg) in the "Results" folder, so you may edit them as needed.

<center>
', section.departure.circular.plots,'
</center>

### Section progression

Note:
  : These graphics have been saved in vectorial format (svg) in the "Results" folder, so you may edit them as needed.

#### Absolutes

<center>
![](global_ratios_absolutes.png){ width=95% }
</center>


#### Percentages

<center>
![](global_ratios_percentages.png){ width=95% }
</center>


### Individual residency plots

<center>
', individual.residency.plots,'
</center>

### Individual detection plots

Note:
  : The detections are coloured by array. The vertical black dashed line shows the time of release. The vertical grey dashed lines show the assigned moments of entry and exit for each study area section. The full dark-grey line shows the movement events considered valid, while the dashed dark-grey line shows the movement events considered invalid.
  : The movement event lines move straight between the first and last station of each event (i.e. in-between detections will not be individually linked by the line).
  : Manually **edited** fish are highlighted with **yellow** graphic borders.

<center>
', individual.detection.plots,'
</center>

### Full log

```{r log, echo = FALSE, comment = NA}
cat(gsub("\\r", "", readr::read_file("../temp_log.txt")))
```

', sep = ""), fill = TRUE)
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


#' update array-movement validity based on the section-movements
#' 
#' @param arrmoves the array movements
#' @param secmoves the section movements
#' 
#' @return the updated array movements
#' 
#' @keywords internal
#' 
updateAMValidity <- function(arrmoves, secmoves) {
  Valid <- NULL
  output <- lapply(names(arrmoves), 
    function(i) {
      if (!is.null(secmoves[[i]]) && any(!secmoves[[i]]$Valid)) {
        aux <- secmoves[[i]][(!Valid)]
        to.change <- unlist(lapply(1:nrow(aux),
          function(j) {
            A <- which(arrmoves[[i]]$First.time == aux$First.time)
            B <- A + (aux$Events - 1)
            return(A:B)
          }))
        appendTo(c("Screen", "Report"), paste0("M: Rendering ", length(to.change), " array movement(s) invalid for fish ", i ," as the respective section movements were discarded by the user."))
        arrmoves[[i]]$Valid[to.change] <- FALSE
      }
      return(arrmoves[[i]])
    })
  names(output) <- names(arrmoves)
  return(output)
}

#' Compress array-movements into section-movements
#' 
#' @inheritParams simplifyMovements
#' @inheritParams migration
#' 
#' @return the section movements
#' 
#' @keywords internal
#' 
sectionMovements <- function(movements, sections, invalid.dist) {
	Valid <- NULL
	output <- list()
	for (fish in names(movements)) {
		valid.movements <- movements[[fish]][(Valid)]
		if (nrow(valid.movements) > 0) {
			aux <- lapply(seq_along(sections), function(i) {
				x <- rep(NA_character_, nrow(valid.movements))
				x[grepl(sections[i], valid.movements$Array)] <- sections[i]
				return(x)
				})

			event.index <- combine(aux)
			aux <- rle(event.index)
			last.events <- cumsum(aux$lengths)
			first.events <- c(1, last.events[-length(last.events)] + 1)
			
      if (invalid.dist) {
        recipient <- data.frame(
          Section = aux$values,
          Events = aux$lengths,
          Detections = unlist(lapply(seq_along(aux$values), function(i) sum(valid.movements$Detections[first.events[i]:last.events[i]]))),
          First.array = valid.movements$Array[first.events],
          Last.array = valid.movements$Array[last.events],
          First.time = valid.movements$First.time[first.events],
          Last.time = valid.movements$Last.time[last.events],
          Time.travelling = c(valid.movements$Time.travelling[1], rep(NA_character_, length(aux$values) - 1)),
          Time.in.section = rep(NA_character_, length(aux$values)),
          Valid = rep(TRUE, length(aux$values)),
          stringsAsFactors = FALSE
          )
      } else {
        recipient <- data.frame(
          Section = aux$values,
          Events = aux$lengths,
          Detections = unlist(lapply(seq_along(aux$values), function(i) sum(valid.movements$Detections[first.events[i]:last.events[i]]))),
          First.array = valid.movements$Array[first.events],
          Last.array = valid.movements$Array[last.events],
          First.time = valid.movements$First.time[first.events],
          Last.time = valid.movements$Last.time[last.events],
          Time.travelling = c(valid.movements$Time.travelling[1], rep(NA_character_, length(aux$values) - 1)),
          Time.in.section = rep(NA_character_, length(aux$values)),
          Speed.in.section.m.s = unlist(lapply(seq_along(aux$values), function(i) mean(valid.movements$Average.speed.m.s[first.events[i]:last.events[i]], na.rm = TRUE))),
          Valid = rep(TRUE, length(aux$values)),
          stringsAsFactors = FALSE
          )
      }
			output[[length(output) + 1]] <- as.data.table(movementTimes(movements = recipient, type = "section"))
			names(output)[length(output)] <- fish
			attributes(output[[length(output)]])$p.type <- attributes(movements[[fish]])$p.type
		}
	}
	return(output)
}

#' Collect summary information for the residency analysis
#' 
#' @param secmoves the section-movements
#' @param movements the array-movements
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
	  
  # for (fish in names(secmoves)) {
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
    # replace NAs
    the.cols <- which(grepl("Total.time|Times.entered|Average.time", colnames(recipient)))
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
#' @inheritParams actel
#' @inheritParams deployValues
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
  
  if (file.exists("temp_comments.txt")) {
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
  }
  appendTo("debug", "Done.")
  return(status.df)
}

#' Calculate array efficiency for residency analysis
#' 
#' @inheritParams updateAMValidity
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
  release.array <- as.character(with(spatial, release.sites[release.sites$Standard.Name == release, "Array"]))
  if (release.array == first.array) {
    return(NULL)
  } else {
    if (dotmat[release.array, first.array] == 1) {
      return(unlist(list(known = release.array)))
    } else {
      aux <- blameArrays(from = release.array, to = first.array, paths = paths)
      return(unlist(list(known = c(release.array, aux[[1]]), unsure = aux[[2]])))
    }
  }
}

#' Find and list arrays which failed during the movements of the fish
#' 
#' @param moves the simplified array movements
#' @inheritParams res_efficiency
#' @inheritParams dotPaths
#' 
#' @return NULL if no arrays failed, or a list of arrays which failed
#' 
#' @keywords internal
#' 
countArrayFailures <- function(moves, paths, dotmat) {
  x <- lapply(1:(nrow(moves) - 1), function(i) {
    A <- moves$Array[i]
    B <- moves$Array[i + 1]
    if (A != B & dotmat[A, B] != 1)
      blameArrays(from = A, to = B, paths = paths)
    else
      NULL
  })
  return(unlist(x))
}

#' Find which arrays to blame for a jump in movement events
#' 
#' @param from The array where the fish started
#' @param to The array where the fish was next detected
#' @inheritParams res_efficiency
#' 
#' @return A list of arrays which failed
#' 
#' @keywords internal
#' 
blameArrays <- function(from, to, paths) {
  the.paths <- paths[[paste0(from, "_to_", to)]]
  if (is.null(the.paths))
    stop("Either 'from' is not connected to 'to', or both are neighbours.\n")
  output <- unique(unlist(strsplit(the.paths, " -> ")))
  if (length(the.paths) == 1)
    return(list(known = output))
  else
    return(list(unsure = output))
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
        output <- rbind(recipient, to.add)
        output <- output[order(output$First.time), ]
      } else {
        output <- recipient
      }
      return(output)
    })
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
    dayrange <- seq(from = round.POSIXt(x$First.time[1] - 43200, units = "days"), 
      to =  round.POSIXt(x$Last.time[nrow(x)] - 43200, units = "days"), by = 86400)
    days.list <- lapply(dayrange, function(d) {
      # cat(as.character(d), "\n")
      findSecondsPerSection(res = x, day = d, the.range = range(dayrange))
    })
    setTxtProgressBar(pb, counter)
    names(days.list) <- dayrange
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
        aux[[length(aux) + 1]] <- difftime(res$Last.time[e], res$First.time[e], units = "secs")
        names(aux)[length(aux)] <- res$Section[e]        
        aux[[length(aux) + 1]] <- 0
        names(aux)[length(aux)] <- "Changes"
      } else {
        # Exception for if the day being analysed is the last day, calculates difference between day start and event stop.
        if (day == the.range[2]) {
          aux[[length(aux) + 1]] <- difftime(res$Last.time[e], day, units = "secs")
          names(aux)[length(aux)] <- res$Section[e]
        # Otherwise calculate difference between event start and day end, and trim at a full day.
        } else {
          aux[[length(aux) + 1]] <- min(86400, as.numeric(difftime(day + 86400, res$First.time[e], units = "secs")))
          names(aux)[length(aux)] <- res$Section[e]
        }
        # Regardless of path, if length(e) == 1 and the event is the first one, the number of changes is 0
        aux[[length(aux) + 1]] <- 0
        names(aux)[length(aux)] <- "Changes"
      }
    # If there is only one event but that event is not the very first one
    } else {
      # If the start time of the event is previous to the day itself
      if (res$First.time[e] < day) {
        # Exception for the last day, calculates difference between day start and event stop.
        if (day == the.range[2]) {
          aux[[length(aux) + 1]] <- difftime(res$Last.time[e], day, units = "secs")
          names(aux)[length(aux)] <- res$Section[e]
        # otherwise, since there are no more 'e', the fish has stayed the whole day in the event
        } else {
          aux[[length(aux) + 1]] <- 86400
          names(aux)[length(aux)] <- res$Section[e]        
        }
        # In either case above the fish spends the whole time in the event, so changes = 0
        aux[[length(aux) + 1]] <- 0
        names(aux)[length(aux)] <- "Changes"
      # If the start time of the event is already in the day itself
      } else {
        # Start by storing the time spent in previous event (i.e. difference between day start and event start)
        aux[[length(aux) + 1]] <- difftime(res$First.time[e], day, units = "secs")
        names(aux)[length(aux)] <- res$Section[e - 1]
        # Then, since there are no more events, calculate difference between event start and day end.
        aux[[length(aux) + 1]] <- difftime(day + 86400, res$First.time[e], units = "secs")
        names(aux)[length(aux)] <- res$Section[e]
        # Since the day is split in two events, there is 1 change
        aux[[length(aux) + 1]] <- 1
        names(aux)[length(aux)] <- "Changes"
      }
    }
  # IF there is more than one 'e'
  } else {
    n <- length(e)
    # calculate difference between day start and first time
    aux[[length(aux) + 1]] <- difftime(res$First.time[e[1]], day, units = "secs")
    # Exception for if the day being analysed is the first day (in which case this fragment should be excluded at the end)
    if (day == the.range[1])
      names(aux)[length(aux)] <- "TO.EXCLUDE"
    else
      names(aux)[length(aux)] <- res$Section[e[1] - 1]
    # For all events except the last, calculate the difference between first and last time of the event.
    for (i in e[-n]) {
      aux[[length(aux) + 1]] <- difftime(res$Last.time[i], res$First.time[i], units = "secs")
      names(aux)[length(aux)] <- res$Section[i]
    }
    # For the very last day
    # exception for if the day being analysed is the last day, where the behaviour should be the same as the above
    if (day == the.range[2]) {
      aux[[length(aux) + 1]] <- difftime(res$Last.time[e[n]], res$First.time[e[n]], units = "secs")
      names(aux)[length(aux)] <- res$Section[e[n]]
    # otherwise, remove the seconds already accounted for to a full day, and store the result
    } else {
      aux[[length(aux) + 1]] <- 86400 - sum(aux)
      names(aux)[length(aux)] <- res$Section[e[n]]        
    }
    # The number of changes will be the number of events - 1
    aux[[length(aux) + 1]] <- length(e) - 1
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
    output$one.last.col <- names(input)
  } else {
    output <- listToTable(days.tables, type = "frame", source = TRUE)
  }
  output <- output[, c(ncol(output), 1:(ncol(output) - 1)), drop = FALSE]
  colnames(output)[1] <- "Date"
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
  aux <- apply(positions, 1, table)
  the.cols <- sort(unique(unlist(lapply(aux, names))))
  the.tables <- vectorsIntoTables(input = aux, columns = the.cols)
  absolutes <- do.call(rbind.data.frame, the.tables)
  absolutes$Date <- rownames(absolutes)
  absolutes <- absolutes[, c(ncol(absolutes), 1:(ncol(absolutes) - 1))]
  rownames(absolutes) <- 1:nrow(absolutes)
  absolutes[is.na(absolutes)] <- 0
  absolutes$Total <- apply(absolutes[, -1], 1, sum)
  percentages <- absolutes
  percentages[, -1] <- round(percentages[, -1] / percentages[, ncol(percentages)], 3)
  return(list(absolutes = absolutes, percentages = percentages))
}
