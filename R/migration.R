#' Migration analysis
#' 
#' The actel package provides a systematic way of analysing fish migration data.
#' migration() collects the input present in the target folder and analyses the telemetry data, extracting migration-related metrics.
#' It is strongly recommended to read the package vignettes before attempting to run the analyses. You can find the vignettes by running browseVignettes('actel') .
#' 
#' @param sections The sections in which the study was divided. Must be coincident with the names given to the ALS arrays. (i.e. if an array is 'River1', then the respective section is 'River') 
#' @param success.arrays The ALS arrays mark the end of the study area. If a fish crosses one of these arrays, it is considered to have successfully migrated through the area.
#' @param minimum.detections The minimum number of times a tag must have been recorded during the study period for it to be considered a true tag and not random noise.
#' @param if.last.skip.section Indicates whether a fish detected at the last array of a given section should be considered to have disappeared in the next section. Defaults to TRUE. I.e.: In a study with sections 'River' and 'Fjord', where 'River3' is the last river array, a fish last detected at River3 will be considered as 'disappeared in the Fjord'.
#' @param override A list of tags for which the user intends to manually define entering and leaving points for each study section.
#' @param cautious.assignment If TRUE, actel avoids assigning events with one detection as first and/or last events of a section.
#' @param disregard.parallels Logical. If TRUE, the presence of parallel arrays does not invalidate potential efficiency peers. For more details on this, have a look at the vignettes.
#' @param replicates A list containing, for each desired array, the standard names of the stations to be used as a replicate, for efficiency estimations.
#' @inheritParams explore
#' 
#' @return A list containing 1) the detections used during the analysis, 2) the movement events, 3) the status dataframe, 4) the survival overview per group, 5) the progression through the study area, 6) the ALS array/sections' efficiency, 7) the list of spatial objects used during the analysis.
#' 
#' @export
#' 
migration <- function(path = NULL, sections, success.arrays = NULL, minimum.detections = 2, 
  maximum.time = 60, max.interval = 60, speed.method = c("last to first", "first to first"), speed.warning = NULL,
  speed.error = NULL, if.last.skip.section = TRUE, tz.study.area = NULL, tz = NULL, start.time = NULL, start.timestamp = NULL, 
  stop.time = NULL, end.timestamp = NULL, report = TRUE, override = NULL, 
  exclude.tags = NULL, cautious.assignment = TRUE, replicates = NULL, disregard.parallels = TRUE,
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

  if (!is.logical(if.last.skip.section))
    stop("'if.last.skip.section' must be logical.\n", call. = FALSE)

  if (!is.null(start.time) && !grepl("^[1-2][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9] [0-2][0-9]:[0-5][0-9]:[0-5][0-9]", start.time))
    stop("'start.time' must be in 'yyyy-mm-dd hh:mm:ss' format.\n", call. = FALSE)
  if (!is.null(stop.time) && !grepl("^[1-2][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9] [0-2][0-9]:[0-5][0-9]:[0-5][0-9]", stop.time))
    stop("'stop.time' must be in 'yyyy-mm-dd hh:mm:ss' format.\n", call. = FALSE)

  if (!is.logical(report))
    stop("'report' must be logical.\n", call. = FALSE)

  if (!is.logical(cautious.assignment))
    stop("'cautious.assignment' must be logical.\n", call. = FALSE)

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
      ", cautious.assignment = ", ifelse(cautious.assignment, "TRUE", "FALSE"), 
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
  appendTo("Report", "Acoustic telemetry data analysis report.\n") 

  path <- checkPath(my.home = my.home, path = path)  

  if (debug)
    appendTo("Report", "!!!--- Debug mode has been activated ---!!!\n")

  if (is.null(success.arrays))
    appendTo(c("Report"), paste0("Timestamp:", the.time <- Sys.time(), "\n\nM: Selected folder: ", getwd(), "\nM: Success has not been defined. Assuming last arrays are success arrays."))
  else
    appendTo(c("Report"), paste0("Timestamp:", the.time <- Sys.time(), "\n\nM: Selected folder: ", getwd(), "\nM: Success has been defined as last detection in: ", paste(success.arrays, collapse = ", "), "."))

  if (!is.null(path))
    appendTo(c("Screen"), "M: Moving to selected work directory")
  
  report <- checkReport(report = report)
# -----------------------------------

# Load, structure and check the inputs
  if (disregard.parallels)
    appendTo("Screen", "M: 'disregard.parallels' is set to TRUE; the presence of parallel arrays will not invalidate efficiency peers.")
  else
    appendTo("Screen", "M: 'disregard.parallels' is set to FALSE; the presence of parallel arrays can potentially invalidate efficiency peers.")
  
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

  if (is.null(success.arrays)) 
    success.arrays <- names(arrays)[unlist(lapply(arrays, function(x) is.null(x$after)))]
# -------------------------------------
  
# Process the data
  appendTo(c("Screen", "Report"), "M: Creating movement records for the valid tags.")
  movements <- groupMovements(detections.list = detections.list, bio = bio, spatial = spatial,
    speed.method = speed.method, max.interval = max.interval, tz = tz, 
    dist.mat = dist.mat, invalid.dist = invalid.dist)

  for(fish in names(movements)){
    movements[[fish]] <- speedReleaseToFirst(fish = fish, bio = bio, movements = movements[[fish]],
     dist.mat = dist.mat, invalid.dist = invalid.dist, silent = FALSE)
  }
  appendTo(c("Screen", "Report"), "M: Checking movement events quality.")
  
  movements <- checkUpstream(movements = movements, bio = bio, spatial = spatial, arrays = arrays)

  movements <- checkImpassables(movements = movements, dotmat = dotmat)

  movements <- checkJumpDistance(movements = movements, bio = bio, dotmat = dotmat, 
                                 spatial = spatial, jump.warning = jump.warning, jump.error = jump.error)

  if (is.null(speed.warning)) {
    appendTo(c("Screen", "Report", "Warning"), "'speed.warning'/'speed.error' were not set, skipping speed checks.")
  } else {
    if(invalid.dist) {
      appendTo(c("Screen", "Report", "Warning"), "'speed.warning'/'speed.error' were set, but a valid distance matrix is not present. Aborting speed checks.")
    } else {
       temp.valid.movements <- simplifyMovements(movements = movements, bio = bio, 
         speed.method = speed.method, dist.mat = dist.mat, invalid.dist = invalid.dist)
      movements <- checkSpeeds(movements = movements, valid.movements = temp.valid.movements, 
        speed.warning = speed.warning, speed.error = speed.error)
       rm(temp.valid.movements)
     }
  }
  
  if (is.null(inactive.warning))
    appendTo(c("Screen", "Report", "Warning"), "'inactive.warning'/'inactive.error' were not set, skipping inactivity checks.")
  else
    movements <- checkInactiveness(movements = movements, detections.list = detections.list, 
      inactive.warning = inactive.warning, inactive.error = inactive.error, 
      dist.mat = dist.mat, invalid.dist = invalid.dist)

  appendTo(c("Screen", "Report"), "M: Initiating timetable development. Your assistance may be needed during the process.")

  recipient <- assembleTimetable(movements = movements, sections = sections, spatial = spatial, arrays = arrays,
    minimum.detections = minimum.detections, dist.mat = dist.mat, invalid.dist = invalid.dist, 
    speed.method = speed.method, if.last.skip.section = if.last.skip.section, success.arrays = success.arrays, 
    override = override, cautious.assignment = cautious.assignment)
  timetable <- recipient[[1]]
  movements <- recipient[[2]]
  rm(recipient)
  appendTo(c("Screen", "Report"), "M: Timetable successfully filled. Fitting in the remaining variables.")
  
  status.df <- assembleOutput(timetable = timetable, bio = bio, spatial = spatial, 
    sections = sections, dist.mat = dist.mat, invalid.dist = invalid.dist, tz = tz)

  valid.movements <- simplifyMovements(movements = movements, bio = bio, 
    speed.method = speed.method, dist.mat = dist.mat, invalid.dist = invalid.dist)

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
  if (!is.null(override)) {
    header.fragment <- paste0('<span style="color:red">Manual mode has been triggered for **', length(override),'** fish.</span>\n')
    name.fragment <- "_corrected"
  } else {
    header.fragment <- name.fragment <- ""
  }

  if (file.exists(resultsname <- paste0("actel_migration_results", name.fragment, ".RData"))) {
    continue <- TRUE
    index <- 1
    while (continue) {
      if (file.exists(resultsname <- paste0("actel_migration_results", name.fragment, ".", index, ".RData"))) {
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

  detections <- detections.list
  deployments <- do.call(rbind.data.frame, deployments)
  matrices <- the.matrices

  # extra info for potential RSP analysis
  rsp.info <- list(analysis.type = "migration", analysis.time = the.time, bio = bio, tz = tz)

  if (invalid.dist)
    save(detections, valid.detections, spatial, deployments, arrays, movements, valid.movements, status.df,
      section.overview, array.overview, matrices, overall.CJS, intra.array.CJS, times, rsp.info, file = resultsname)
  else
    save(detections, valid.detections, spatial, deployments, arrays, movements, valid.movements, status.df,
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

  appendTo("Screen", paste0("M: Saving job log as '", paste(gsub(":", ".", sub(" ", ".", as.character(Sys.time()))), "actel.log.txt", sep = "."), "'."))
  file.rename("temp_log.txt", paste0(gsub(":", ".", sub(" ", ".", as.character(Sys.time()))), "actel.log.txt"))
  
  if (!debug)
    deleteHelpers()

  if (invalid.dist)
    return(list(detections = detections, valid.detections = valid.detections, spatial = spatial, deployments = deployments, arrays = arrays,
      movements = movements, valid.movements = valid.movements, status.df = status.df, section.overview = section.overview, array.overview = array.overview,
      matrices = matrices, overall.CJS = overall.CJS, intra.array.CJS = intra.array.CJS, times = times, rsp.info = rsp.info))
  else
    return(list(detections = detections, valid.detections = valid.detections, spatial = spatial, deployments = deployments, arrays = arrays,
      movements = movements, valid.movements = valid.movements, status.df = status.df, section.overview = section.overview, array.overview = array.overview,
      matrices = matrices, overall.CJS = overall.CJS, intra.array.CJS = intra.array.CJS, times = times, rsp.info = rsp.info, dist.mat = dist.mat))
}

#' Print Rmd report
#'
#' Creates a Rmd report and converts it to hmtl.
#' 
#' @param name.fragment Rmarkdown string specifying the type of report for the title.
#' @param header.fragment Rmarkdown string specifying the type of report for the header.
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
printMigrationRmd <- function(name.fragment, header.fragment, biometric.fragment, efficiency.fragment, display.progression, array.overview.fragment,
  survival.graph.size, individual.plots, circular.plots, spatial, deployments, valid.detections, detections){
  if (file.exists(reportname <- paste0("Report/actel_migration_report", name.fragment, ".Rmd"))) {
    continue <- TRUE
    index <- 1
    while (continue) {
      if(file.exists(reportname <- paste0("Report/actel_migration_report", name.fragment, ".", index, ".Rmd"))) {
        index <- index + 1
      } else {
        continue <- FALSE
      }
    }
    appendTo("Screen", paste0("M: An actel report is already present in the current directory\n   Saving new report as 'actel_migration_report", name.fragment, ".", index, ".html'."))
    rm(continue,index)
  } else {
    appendTo("Screen", paste0("M: Saving actel report as 'actel_migration_report", name.fragment, ".html'."))
  }
  if (any(grepl("Unknown", spatial$stations$Standard.Name))) {
    unknown.fragment <- paste0('<span style="color:red"> Number of relevant unknown receivers: **', sum(grepl("Unknown", spatial$stations$Standard.Name)), '**</span>\n')
  } else {
    unknown.fragment <- ""
  } 
  report <- readr::read_file("temp_log.txt")
  sink(reportname)
  cat(paste0(
'---
title: "Acoustic telemetry migration analysis"
author: "Actel package"
output: 
  html_document:
    includes:
      after_body: toc_menu_migration.html
---

### Summary

Selected folder: ', stringr::str_extract(pattern = '(?<=M: Selected folder: )[^\r|^\n]*', string = report), '

Timestamp: **', stringr::str_extract(pattern = '(?<=Timestamp:)[^\r|^\n]*', string = report), '** 

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
#' @inheritParams actel
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
assembleTimetable <- function(movements, sections, spatial, arrays, minimum.detections, 
  dist.mat, invalid.dist, speed.method, if.last.skip.section, success.arrays, override, cautious.assignment) {
  appendTo("debug", "Starting assembleTimetable.")

  # NOTE: The NULL variables below are actually column names used by data.table.
  # This definition is just to prevent the package check from issuing a note due unknown variables.
  Valid <- NULL 
  Array <- NULL
  
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
  recipient <- c(recipient, "Very.last.array", "Status", "Detections", "Backwards.movements", "Max.cons.back.moves", "P.type")
  if (!invalid.dist && speed.method == "first to first")
    recipient <- recipient[!grepl("Speed.in",recipient)]

  timetable <- matrix(nrow = length(movements), ncol = length(recipient))
  timetable <- as.data.frame(timetable)
  
  colnames(timetable) <- recipient
  rm(recipient)
  rownames(timetable) <- names(movements)
  
  for (i in names(movements)) {
    if (!is.null(override) && any(grepl(i, override))) {
      ## If the fish is marked for manual mode
      attributes(movements[[i]])$p.type <- "Overridden"
      events <- overrideDefaults(i, movements[[i]], sections)
      ## Jump to updating movements and deploying values
    } else {
      if (movements[[i]][, any(Valid)]) {
        ## If there are any valid events (invalid could only be caused by upstream detection)
        ## Find the last events for each section If there is more than one detection event
        if (movements[[i]][(Valid), all(Array == "Unknown")]) {
          ## If all the events are in unknown arrays
          appendTo(c("Screen","Report", "Warning"), paste0("Fish ", i, " only moved through unknown hydrophones. Considered invalid."))
          last.events <- rep(NA, length(sections))
          movements[[i]]$Valid <- FALSE
          ## Jump to find first events
        } else {
          ## If all or some events are in known locations
          if (movements[[i]][(Valid), any(Array == "Unknown")]) {
            ## If some unknown events must be discarded
            appendTo(c("Screen","Report"), paste0("M: Disregarding ", sum(movements[[i]][, "Array"] == "Unknown"), " unknown events from fish ", i, "'s movement table."))
            movements[[i]][(Array == "Unknown")]$Valid <- FALSE
          }
          recipient <- findLastEvents(i = i, movements = movements[[i]], 
            sections = sections, minimum.detections = minimum.detections,
            cautious.assignment = cautious.assignment)
          last.events <- recipient[[1]]
          attributes(movements[[i]])$p.type <- recipient[[2]]
        }
      } else {
        ## If all the events are invalid
        last.events <- rep(NA, length(sections))
        attributes(movements)$p.type <- "Manual"
      }      
      events <- findFirstEvents(i = i, last.events = last.events, 
        movements = movements[[i]], sections = sections, 
        cautious.assignment = cautious.assignment)
    }
    ## First update movement validity based on selected events
    movements[[i]] <- updateMovementValidity(movements = movements[[i]], events = events, sections = sections)
    ## Second, deploy the values
    timetable <- deployValues(i = i, timetable = timetable, movements = movements[[i]], 
      events = events, sections = sections, spatial = spatial, arrays = arrays, dist.mat = dist.mat, 
      invalid.dist = invalid.dist, if.last.skip.section = if.last.skip.section, 
      success.arrays = success.arrays, speed.method = speed.method)
  }
  timetable$Transmitter <- rownames(timetable)
  appendTo("debug", "Terminating assembleTimetable.")
  return(list(timetable = timetable, movements = movements))
}

#' Override automated logics
#'
#' If the user has called for an override for a specific tag, this function will allow for a fully manual choice of events.
#' 
#' @inheritParams actel
#' @inheritParams simplifyMovements
#' @param i The tag number currently under scrutiny. Only used for messaging purposes.
#' 
#' @return The first and last events for each section for the target fish.
#' 
#' @keywords internal
#' 
overrideDefaults <- function(i, movements, sections) {
  appendTo("debug", "Starting overrideDefaults.")
  message("----------------------------")
  appendTo(c("Screen", "Report"), paste0("M: Override has been triggered for fish ", i, ". Entering full manual mode."))
  last.events <- vector()
  first.events <- vector()
  message("Opening movements list for inspection.\n")
  print(movements, topn = nrow(movements))
  message("")
  for (j in seq_len(length(sections))) {
    check = TRUE
    while (check) {
      last.events[j] <- suppressWarnings(as.numeric(commentCheck(line = paste0("Last ", sections[j], " event: "), tag = i)))
      if (!is.na(last.events[j])) {
        valid = TRUE
        check <- !overrideEventCheck(type = "last", the.event = last.events[j], 
                    up.boundary = tail(which(grepl(sections[j], movements$Array)), 1),
                    t = checkPrevious(last.events = last.events, j = j),
                    last.events = last.events,
                    j = j, sections = sections, movements = movements)
      } else {
        decision <- commentCheck(line = paste("Confirm that the fish did not pass this section?(y/N/comment) "), tag = i)
        if (decision == "y" | decision == "Y") {
          check = FALSE
        } else {
          check = TRUE
        }
      }
    }
    appendTo("UD", last.events[j])
    if (is.na(last.events[j]))
      appendTo("UD", "Y")
    rm(check)
  }
  rm(j)
  for (j in seq_len(length(sections))) {
    # If the fish left the section
    if (is.na(last.events[j])) {
      message("M: Skipping first event selection for ", sections[j], " as this fish never left this section.")
      first.events[j] <- NA
    } else {
      check = TRUE
      while (check) {
        first.events[j] <- suppressWarnings(as.numeric(commentCheck(line = paste0("First ", sections[j], " event: "), tag = i)))
        if (is.na(first.events[j])) {
          message("M: The inserted value is not numeric. Please try again.")
        } else {
          valid = TRUE
          check <- !overrideEventCheck(type = "first", the.event = first.events[j], 
                      up.boundary = last.events[j],
                      t = checkPrevious(last.events = last.events, j = j),
                      last.events = last.events,
                      j = j, sections = sections, movements = movements)
        }
      }
      appendTo("UD", first.events[j])
      rm(check)
    }
  }
  message("Terminating full manual mode\n----------------------------")
  appendTo("debug", "Terminating overrideDefaults.")
  return(list(first.events = first.events, last.events = last.events))
}

#' Find previous valid last event index
#' 
#' @param last.events A vector of the movement numbers corresponding to each last event. Is supplied by the function findLastEvents.
#' @param j the row being analysed
#' 
#' @return The previous valid last event index
#' 
#' @keywords internal
#' 
checkPrevious <- function(last.events, j){
  t <- j-1
  failsafe = TRUE
  while (failsafe) {
    if (t > 0 && is.na(last.events[t])) {
      t <- t-1
    } else {
      failsafe = FALSE
    }
  }
  return(t)
}

#' Check event validity during manual mode
#' 
#' @inheritParams actel
#' @inheritParams simplifyMovements
#' @inheritParams checkPrevious
#' @param type The type of event being checked, only affects message display
#' @param the.event The event being evaluated (chosen by the user during overrideDefaults)
#' @param up.boundary The last event for the target section
#' @param t The previous valid last event index
#' @param j The current section index
#' 
#' @return TRUE if the event is valid, FALSE if the event is not valid
#' 
#' @keywords internal
#' 
overrideEventCheck <- function(type, the.event, up.boundary, t, last.events, j, sections, movements){
  valid = TRUE
  if (t == 0 && (the.event < 1 | the.event > up.boundary)) {
    message("M: Invalid row (", type ," event must be between 1 and ", up.boundary, "). Please try again.")
    if (type == "last") message("   Leave empty to indicate that the fish did not pass by this section.")
    valid = FALSE
  }
  if (t > 0 && (the.event <= last.events[t] | the.event > up.boundary)) {
   message("M: Invalid row (", type ," event must be between ", (last.events[t] + 1), " and ", up.boundary, "). Please try again.")
    if (type == "last") message("   Leave empty to indicate that the fish did not pass by this section.")
    valid = FALSE
  }
  if (valid && !grepl(sections[j], movements[the.event, "Array"])) {
    message("M: Invalid row (not a ", sections[j], " event), please try again.")
    valid = FALSE
  }
  return(valid)
}

#' Find last events
#'
#' Finds the last event for each section in the study area.
#' 
#' @inheritParams actel
#' @inheritParams simplifyMovements
#' @inheritParams overrideDefaults
#' 
#' @return A vector of the movement numbers corresponding to each last event.
#' 
#' @keywords internal
#' 
findLastEvents <- function(i, movements, sections, minimum.detections, cautious.assignment) {
  # NOTE: The NULL variables below are actually column names used by data.table.
  # This definition is just to prevent the package check from issuing a note due unknown variables.
  Array <- NULL
  Valid <- NULL
  Detections <- NULL

  appendTo("debug", paste0("Starting findLastEvents for fish ", i, "."))
  if (nrow(movements) > 1) {
    last.events <- rep(NA, length(sections))
    for (l in seq_len(length(sections))) {
      trigger <- FALSE
      matching.events <- movements[, which(grepl(sections[l], Array) & Valid)]
      if(length(matching.events) >= 1){
        if(cautious.assignment){
          trigger <- TRUE
          for(j in rev(matching.events)){
            if (movements[j, Detections] > 1) {
              last.events[l] <- j
              (break)
            } 
          }
        } else {
          last.events[l] <- tail(matching.events, 1)
        }
      }
      if (trigger && is.na(last.events[l])) {
        attributes(movements)$p.type <- "Manual"
        ## I used cat here because the appendTo will be used later on, when the findFirstEvents comes up
        message(paste0("No ", sections[l], " detection events with more than one detection were found for fish ", i, ". Opening movements list for inspection."))
        print(movements[(Valid), -c("Valid")], topn = nrow(movements[(Valid), -c("Valid")]))
        message("")
        check = TRUE
        while (check) {
          new.value <- suppressWarnings(as.numeric(commentCheck(line = paste("Which event should be considered the LAST", sections[l], "detection event?(comment) "), tag = i)))
          appendTo("UD", new.value)
          if (is.na(new.value)) {
            decision <- commentCheck(line = paste("The inserted value is not numeric. Continuing will erase all", sections[l], "related timestamps. Proceed?(y/N/comment) "), tag = i)
            appendTo("UD", decision)
            if (decision == "Y" | decision == "y") {
              appendTo("Screen", paste0("M: Erasing ", sections[l], " related timestamps for fish ", i, "."))
              last.events[l] <- NA
              appendTo(c("Report", "Warning"), paste0("No ", sections[l], " detection events with more than one detection were found for fish ", i, ". Erasing ", sections[l], " timestamps per user command."))
              check = FALSE
            }
            rm(decision)
          } else {
            valid.row = TRUE
            new.value <- movements[(Valid), which = TRUE][new.value]
            if (!grepl(sections[l], movements[new.value, "Array"])) {
              message("M: Invalid row (not a ", sections[l], " event), please try again.")
              valid.row = FALSE
            }
            if (valid.row) {
              check = FALSE
              last.events[l] <- new.value
              appendTo(c("Report", "Warning"), paste0("No ", sections[l], " detection events with more than one detection were found for fish ", i, ". Last ", sections[l], " row manually set to ", new.value, "."))
            }
          }
        }
      }
    }
    recipient <- eventOrderCheck(i = i, last.events = last.events, 
      sections = sections, movements = movements)
    last.events <- recipient[[1]]
    attributes(movements)$p.type <- recipient[[2]]
    rm(recipient)
  } else {
    # = there is only one detection event
    if (movements[1, "Detections"] >= minimum.detections) {
      last.events <- pmatch(sections, movements[, "Array"])
    } else {
      # = not enough detections
      appendTo(c("Screen", "Report"), paste0("M: Fish ", i, " only has one movement entry (", movements[1, "Array"], ") with ", movements[1, "Detections"], " detections. Considered invalid."))
      last.events <- rep(NA, length(sections))
    }
  }
  appendTo("debug", paste0("Terminating findLastEvents for fish ", i, "."))
  return(list(last.events = last.events, p.type = attributes(movements)$p.type))
}

#' Check event order
#'
#' Looks for evidence of backwards movements.
#' 
#' @inheritParams actel
#' @inheritParams simplifyMovements
#' @inheritParams overrideDefaults
#' @inheritParams findLastEvents
#' 
#' @return A vector of the movement numbers corresponding to each last event.
#' 
#' @keywords internal
#' 
eventOrderCheck <- function(i, last.events, sections, movements) {
  # NOTE: The NULL variables below are actually column names used by data.table.
  # This definition is just to prevent the package check from issuing a note due unknown variables.
  Valid <- NULL
  Array <- NULL  
  appendTo("debug", paste0("Starting eventOrderCheck for fish ", i, "."))
  trigger <- vector()
  for (l in seq_len(length(sections))) {
    trigger[movements[(Valid), grep(sections[l], Array)]] <- l
  }
  rm(l)
  if (is.unsorted(trigger)) {
    ## If Inter-section backward movements detected
    if (is.unsorted(last.events, na.rm = T)) {
      ## If last events are not ordered
      appendTo(c("Screen", "Report", "Warning"), paste0("Inter-section backwards movements were detected for Fish ", i, " and the last events are not ordered!"))
      not.ordered.trigger <- TRUE
      decision <- "Y"
      message("   Opening movements list for inspection.")
    } else {
      ## If last events are ordered
      appendTo(c("Screen", "Report", "Warning"), paste0("Inter-section backwards movements were detected for Fish ", i, "."))
      not.ordered.trigger <- FALSE
      decision <- commentCheck(line = paste0("Would you like to see the movement table for fish ", i, "?(y/N/comment) "), tag = i)
      appendTo("UD", decision)
    }
    if (decision == "Y" | decision == "y") {
      ## If the user decides to see the movements
      print(movements[(Valid), -c("Valid")], topn = nrow(movements[(Valid), -c("Valid")]))
      rm(decision)
      message("")
      appendTo("Screen", paste0("Current last events: ", paste(last.events, collapse = ", "), " (", paste(sections, collapse = ", "), ")."))
      message("")
      if (not.ordered.trigger) {
        appendTo(c("Screen"), "The last movement event of a section must NOT precede the last movement event of a \nprevious section (i.e. the migration analysis cannot cope with inter-section U turns).\nPlease edit the last valid events so this is not the case any more.\n")
        decision <- "Y"
      } else {
        decision <- commentCheck(line = "Would you like to edit the last valid events?(y/N/comment) ", tag = i)
      }
      appendTo("UD", decision)
      if (decision == "Y" | decision == "y") {
        ## If the user decides to edit the last events
        while (decision == "Y" | decision == "y") {
          position <- suppressWarnings(as.numeric(commentCheck(line = paste0("Which last valid event would you like to edit?(1-", length(last.events), "/comment) "), tag = i)))
          ## The position is the last event to edit
          appendTo("UD", position)
          if (is.na(position)) {
            ## If no position was set
            decision <- commentCheck(line = "The inserted value is not numeric. Abort last valid events' edition?(y/N/comment) ", tag = i)
            appendTo("UD", decision)
            if (decision != "Y" & decision != "y") {
              decision <- "Y"
            } else {
              if (is.unsorted(last.events, na.rm = T)) {
                ## If the user decides to abort but last events are still not ordered
                message("")
                appendTo("Screen", paste0("Current last events: ", paste(last.events, collapse = ", "), " (", paste(sections, collapse = ", "), ")."))
                message("")
                appendTo(c("Screen"), "The last movement event of a section must NOT precede the last movement event\nof a previous section (i.e. Actel cannot cope with inter-section U turns).\nPlease edit the last valid events so this is not the case anymore.\n")
                decision <- "Y"
              } else {
                decision <- "N"
              }
            }
          } else {
            ## If the position is numeric
            if (position > 0 & position <= length(last.events)) {
              ## If the position is within the limits of the last events
              check = TRUE
              invalidate = FALSE
              while (check) {
                new.value <- suppressWarnings(as.numeric(commentCheck(line = paste0("New last valid event for ", sections[position], ": "), tag = i)))
                appendTo("UD", new.value)
                if (is.na(new.value)) {
                  ## If the selected row is not numeric
                  decision <- commentCheck(line = paste0("The inserted value is not numeric. Continuing will render ", sections[position], " events invalid. Proceed?(y/N/comment) "), tag = i)
                  appendTo("UD", decision)
                  if (decision == "Y" | decision == "y") {
                    ## Decides to remove the last event
                    check = FALSE
                    invalidate = TRUE
                  }
                  rm(decision)
                } else {
                  # If the selected row is numeric
                  new.value <- movements[(Valid), which = TRUE][new.value]
                  if (!grepl(sections[position], movements[new.value, "Array"])) {
                    ## If the elected row does not belong to the right section
                    message("M: Invalid row (not a ", sections[position], " event), please try again.")
                  } else {
                    ## else terminate the while
                    check = FALSE
                  }
                }
              }
              rm(check)
              if (invalidate) {
                ## If the user decided to remove the last event
                last.events[position] <- NA
              } else {
                ## If a new value was set
                last.events[position] <- new.value
              }
              rm(position, new.value, invalidate)
              if (is.unsorted(last.events, na.rm = T)) {
                ## if after the edition the values are still not ordered, edits must continue
                message("")
                appendTo("Screen", paste0("Current last events: ", paste(last.events, collapse = ", "), " (", paste(sections, collapse = ", "), ")."))
                message("")
                appendTo("Screen", "The last events are not ordered. Please continue editing the last events until the last movement \nevent of each section is NOT lower than the last movement event of a section that precedes it.")
                decision <- "Y"
              } else {
                ## Else the user can decide if he wants to continue or stop
                decision <- commentCheck(line = "Continue editing last valid events?(y/N/comment) ", tag = i)
              }
              appendTo("UD", decision)
            } else {
              ## If the position is outside the limits of the last events
              message("M: That event is not available, please try again.")
            }
          }
        }
        appendTo("Report", paste0("M: Last valid events for fish ", i, " were manually edited to: ", paste(last.events, collapse = ", "), "."))
        attributes(movements)$p.type <- "Manual"
      } else {
        appendTo("Report", paste0("M: Default last valid events for fish ", i, " were kept upon inspection."))
      }
    } else {
      appendTo("Report", paste0("M: Default last valid events for fish ", i, " were kept without inspection."))
    }
  }
  appendTo("debug", paste0("Terminating eventOrderCheck for fish ", i, "."))
  return(list(last.events = last.events, p.type = attributes(movements)$p.type))
}

#' Find starting row
#'
#' Finds the row from which the function findFirstEvents should start looking for the first event.
#' 
#' @inheritParams actel
#' @inheritParams simplifyMovements
#' @inheritParams checkPrevious
#' @param l The section currently being analysed. Supplied by findFirstEvents
#' 
#' @return The row number
#' 
#' @keywords internal
#' 
findFirstRow <- function(l, last.events, movements, sections) {
  # NOTE: The NULL variables below are actually column names used by data.table.
  # This definition is just to prevent the package check from issuing a note due unknown variables.
  Array <- NULL
  Valid <- NULL

  appendTo("debug", "Starting findFirstRow.")
  if (l > 1) {
    for (k in rev(seq_len(l - 1))) {
      # Prevent script from breaking if the fish was not detected on the previous section
      if (!is.na(last.events[k])) {
        first.row = last.events[k] + 1
        (break)
      }
    }
    rm(k)
  }
  if (!exists("first.row")) {
    for (k in seq_len(nrow(movements))) {
      if (movements[k, grepl(sections[l], Array) & Valid]) {
        first.row = k
        (break)
      }
    }
    rm(k)
  }
  appendTo("debug", "Terminating findFirstRow.")
  return(first.row)
}

#' Find first events
#'
#' Finds the first event for each section in the study area that has a last event.
#' 
#' @inheritParams actel
#' @inheritParams simplifyMovements
#' @inheritParams overrideDefaults
#' @inheritParams checkPrevious
#' @inheritParams findLastEvents
#' 
#' @return A list containing both the first and last events.
#' 
#' @keywords internal
#' 
findFirstEvents <- function(i, last.events, movements, sections, cautious.assignment) {
  # NOTE: The NULL variables below are actually column names used by data.table.
  # This definition is just to prevent the package check from issuing a note due unknown variables.
  Array <- NULL
  Valid <- NULL
  Detections <- NULL

  appendTo("debug", paste0("Starting findFirstEvents for fish ", i, "."))
  first.events <- rep(NA, length(sections))
  for (l in seq_len(length(sections))) {
    # If the fish left the section
    trigger <- FALSE
    if (!is.na(last.events[l])) {
      first.row <- findFirstRow(l, last.events, movements, sections)
      matching.events <- movements[, which(grepl(sections[l], Array) & Valid)]
      valid.events <- matching.events[matching.events >= first.row]
      if(cautious.assignment){
        trigger <- TRUE
        for(j in valid.events){
          if (movements[j, Detections] > 1) {
            first.events[l] <- j
            (break)
          } 
        }
      } else {
        first.events[l] <- head(valid.events, 1)
      }
    }
    if (trigger && is.na(first.events[l])) {
      check = TRUE
      while (check) {
        new.value <- suppressWarnings(as.numeric(commentCheck(line = paste("Which event should be considered the FIRST", sections[l], "detection event?(comment) "), tag = i)))
        appendTo("UD", new.value)
        if (is.na(new.value)) {
          decision <- commentCheck(line = paste("The inserted value is not numeric. Continuing will erase all", sections[l], "related timestamps. Proceed?(y/N) "), tag = i)
          appendTo("UD", decision)
          if (decision == "Y" | decision == "y") {
            appendTo("Screen", paste0("M: Erasing ", sections[l], " related timestamps for fish ", i, ".\n"))
            first.events[l] <- last.events[l] <- NA
            appendTo(c("Report", "Warning"), paste0("No ", sections[l], " detection events with more than one detection were found for fish ", i, ". Erasing ", sections[l], 
            " timestamps per user command."))
            check = FALSE
          }
          rm(decision)
        } else {
          valid.row = TRUE
          new.value <- movements[(Valid), which = TRUE][new.value]
          if (!grepl(sections[l], movements[new.value, "Array"])) {
            message("M: Invalid row (not a ", sections[l], " event), please try again.")
            valid.row = FALSE
          }
          if (new.value < first.row | new.value > last.events[l]) {
            message("M: Invalid row (out of bounds), please try again. Valid range is ", first.row, " to ", last.events[l], ".")
            valid.row = FALSE
          }
          if (valid.row) {
            check = FALSE
            first.events[l] <- new.value
            appendTo(c("Report", "Warning"), paste0("No ", sections[l], " detection events with more than one detection were found for fish ", i, ". First ", sections[l], " row manually set to ", 
            new.value, "."))
          }
        }
      }
    }
  }
  appendTo("debug", paste("Terminating findFirstEvents for fish ", i, ".", sep = ""))
  return(list(first.events = first.events, last.events = last.events))
}

#' Update movement validity based on the chosen first and last events for each section
#' 
#' @inheritParams actel
#' @inheritParams deployValues
#' @inheritParams assembleTimetable
#' 
#' @keywords internal
#' 
#' @return the updated movement's table
#' 
updateMovementValidity <- function(movements, events, sections){
  # NOTE: The NULL variables below are actually column names used by data.table.
  # This definition is just to prevent the package check from issuing a note due unknown variables.
  Array <- NULL
    
  appendTo("Debug", "Starting updateMovementValidity.")
  for (j in seq_len(length(sections))) {
    link <- movements[, grepl(sections[j], Array)]
    if (is.na(events$last.events[j])) {
      if (any(link))
        movements[link, "Valid"] <- FALSE
    } else {
      if (any(which(link) > events$last.events[j]))
        movements[which(link)[which(link) > events$last.events[j]]]$Valid <- FALSE
      if (any(which(link) < events$first.events[j]))
        movements[which(link)[which(link) < events$first.events[j]]]$Valid <- FALSE
    }
    rm(link)
  }
  appendTo("Debug", "Terminating updateMovementValidity.")
  return(movements)
}

#' Deploy chosen values
#'
#' Compiles the information supplied and deploys it into the correct timetable row.
#' 
#' @param timetable A table of the entering and leaving points for each section per target tag, created by assembleTimetable.
#' @inheritParams actel
#' @inheritParams simplifyMovements
#' @inheritParams overrideDefaults
#' @inheritParams loadDetections
#' @inheritParams groupMovements
#' @inheritParams findLastEvents
#' @inheritParams assembleArrayCJS
#' @param events A list containing both the first and last events. Supplied by findFirstEvents.
#' 
#' @return A list containing both the first and last events.
#' 
#' @keywords internal
#' 
deployValues <- function(i, timetable, movements, events, sections, spatial, arrays,
  dist.mat, invalid.dist, if.last.skip.section, success.arrays, speed.method) {
  # NOTE: The NULL variables below are actually column names used by data.table.
  # This definition is just to prevent the package check from issuing a note due unknown variables.
  Detections <- NULL
  Array <- NULL

  appendTo("debug", paste0("Starting deployValues for fish ", i, "."))
  if (any(!is.na(events$last.events))) {
    last.last <- tail(events$last.events[!is.na(events$last.events)], 1)
    for (j in seq_len(length(sections))) {
      if (!is.na(events$first.events[j])) {
        timetable[i, paste("Arrived", sections[j], sep = ".")] <- paste(movements[[events$first.events[j], "First.time"]])
        timetable[i, paste("First.station", sections[j], sep = ".")] <- movements[[events$first.events[j], "First.station"]]
        timetable[i, paste("Left", sections[j], sep = ".")] <- paste(movements[[events$last.events[j], "Last.time"]])
        timetable[i, paste("Last.station", sections[j], sep = ".")] <- movements[[events$last.events[j], "Last.station"]]
        timetable[i, paste("Time.in", sections[j], sep = ".")] <- as.vector(difftime(timetable[i, paste("Left", sections[j], sep = ".")], timetable[i, paste("Arrived", sections[j], sep = ".")], 
          units = "secs"))
        if (speed.method == "last to first" && !invalid.dist) {
          to.col <- paste("Speed.in", sections[j], sep = ".")
          dist.row <- timetable[i, paste("First.station", sections[j], sep = ".")]
          dist.col <- timetable[i, paste("Last.station", sections[j], sep = ".")]
          from.col <- paste("Time.in", sections[j], sep = ".")
          timetable[i, to.col] <- dist.mat[dist.row, dist.col]/timetable[i, from.col]
        }
      }
      if (j > 1) {
        testA <- !is.na(timetable[i, paste("Arrived", sections[j], sep = ".")])
        testB <- !is.na(timetable[i, paste("Left", sections[j - 1], sep = ".")])
        if (testA & testB) {
          to.col <- paste("Time.until", sections[j], sep = ".")
          from.colA <- paste("Arrived", sections[j], sep = ".")
          from.colB <- paste("Left", sections[j - 1], sep = ".")
          AtoB <- difftime(timetable[i, from.colA], timetable[i, from.colB], units = "secs")
          timetable[i, to.col] <- as.vector(AtoB)
          if (!invalid.dist) {
            to.col <- paste("Speed.to", sections[j], sep = ".")
            dist.row <- timetable[i, paste("First.station", sections[j], sep = ".")]
            if (speed.method == "last to first"){
              dist.col <- timetable[i, paste("Last.station", sections[j - 1], sep = ".")]
              total.time <- timetable[i, paste("Time.until", sections[j], sep = ".")]
            }
            if (speed.method == "first to first"){
              dist.col <- timetable[i, paste("First.station", sections[j - 1], sep = ".")]
              total.time <- timetable[i, paste("Time.in", sections[j - 1], sep = ".")] + 
                            timetable[i, paste("Time.until", sections[j], sep = ".")]
            }
            timetable[i, to.col] <- dist.mat[dist.row, dist.col]/ total.time
          }
        }
      }
      # If the last last event was on the active section
      if (grepl(sections[j], movements$Array[last.last])) {
        # If we are not on the last section and the last last event was on the last array of the section
        partA <- movements$Array[last.last]
        partB <- spatial$array.order[[sections[j]]]
        long.test <- match(partA, partB) == length(spatial$array.order[[sections[j]]])
        if (if.last.skip.section && j < length(sections) && long.test) {
          timetable[i, "Status"] <- paste("Disap. in", sections[j + 1])
        } else {
          timetable[i, "Status"] <- paste("Disap. in", sections[j])
        }
      }
      timetable[i, "Very.last.array"] <- movements$Array[last.last]
    }
  } else {
    timetable[i, "Status"] <- paste("Disap. in", sections[1])
    timetable[i, "Very.last.array"] <- "Release"
  }
  timetable[i, "Detections"] <- movements[, sum(Detections)]

  if (movements[, any(Array != "Unknown")]) {
    temp <- movements[Array != "Unknown", ]
    recipient <- countBackMoves(movements = temp, arrays = arrays)
    timetable[i, "Backwards.movements"] <- recipient[[1]]
    timetable[i, "Max.cons.back.moves"] <- recipient[[2]]
  } else {
    timetable[i, "Backwards.movements"] <- 0
    timetable[i, "Max.cons.back.moves"] <- 0
  }
  timetable[i, "P.type"] <- attributes(movements)$p.type
  # testA <- !is.na(timetable[i, paste("Left", tail(sections, 1), sep = ".")])
  testB <- any(!is.na(match(success.arrays, movements[tail(events$last.events, 1), Array])))
  # if (testA & testB)
  if (testB) 
    timetable[i, "Status"] <- "Succeeded"
  appendTo("debug", paste0("Terminating deployValues for fish ", i, "."))
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
    direction <- NULL
    A <- movements$Array[-nrow(movements)]
    B <- movements$Array[-1]
    aux <- cbind(A, B)
    backwards.movements <- apply(aux, 1, function(x)
      if(x[1] != x[2]) 
        is.na(match(x[2], arrays[[x[1]]]$downstream))
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
assembleOutput <- function(timetable, bio, spatial, sections, dist.mat, invalid.dist, tz) {
  appendTo("debug", "Merging 'bio' and 'timetable'.")
  status.df <- merge(bio, timetable, by = "Transmitter", all = TRUE)
  
  appendTo("debug", "Completing entries for fish that were never detected.")
  status.df$Status[is.na(status.df$Status)] <- paste("Disap. in", sections[1])
  status.df$Status <- factor(status.df$Status, levels = c(paste("Disap. in", sections), "Succeeded"))
  status.df$Very.last.array[is.na(status.df$Very.last.array)] <- "Release"
  status.df$Very.last.array <- factor(status.df$Very.last.array, levels = c("Release", levels(spatial$stations$Array)))
  status.df$P.type[is.na(status.df$P.type)] <- "Skipped"
  status.df$Detections[is.na(status.df$Detections)] <- 0

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
#' @inheritParams actel
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
