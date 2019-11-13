#' Actel: Acoustic telemetry data sorting
#' 
#' The actel package provides a systematic way of analysing fish migration data.
#' Its main function, actel, collects the input present in the target folder and analyses the telemetry data.
#' It is strongly recommended to read the package vignettes before attempting to run the analyses. You can find the vignettes by running browseVignettes('actel') .
#' 
#' @param path Path to the folder containing the data. If the R session is already running in the target folder, path may be left as NA
#' @param maximum.time The number of minutes that must pass between detections for a new event to be created.
#' @param speed.method One of 'last to first' or 'first to first'. In the former, the last detection on a given array/section is matched to the first detection on the next array/section (default). If changed to 'first to first', the first detection on two consecutive arrays/sections are used to perform the calculations.
#' @param tz.study.area The time-zone of the study area. Necessary to convert the ALS time data, which is in UTC.
#' @param start.timestamp Detection data prior to this date is not analysed. Improves processing time when loading large amounts of detection data.
#' @param end.timestamp Detection data posterior to this date is not analysed. Improves processing time when loading large amounts of detection data.
#' @param report Whether graphics, tables and LaTeX report files should be created. Defaults to TRUE. Allows automatic compiling of a PDF report after the analysis.
#' @param exclude.tags A list of tags that should be excluded from the detection data before any analyses are performed. Intended to be used if stray tags from a different code space but with the same signal as a target tag are detected in the study area.
#' @param debug If TRUE, temporary files are not deleted at the end of the analysis. Defaults to FALSE.
#' @param jump.warning Integer value. If a fish crosses ´jump.warning´ arrays without being detected, a warning is issued.
#' @param jump.error Integer value. If a fish crosses ´jump.error´ arrays without being detected, user intervention is suggested.
#' @param speed.warning Numeric value (m/s). If a fish moves at a speed greater or equal to this argument, a warning is issued. Defaults to NULL (i.e. no speed checks are performed, with a message)
#' @param speed.error Numeric value (m/s). If a fish moves at a speed greater or equal to this argument, user intervention is suggested. Defaults to NULL (i.e. no errors are issued)
#' @param inactive.warning Numeric value (days). If a fish spends a number of days greater or equal to this argument in a given place, a warning is issued. Defaults to NULL (i.e. no speed checks are performed, with a message)
#' @param inactive.error Numeric value (days). If a fish spends a number of days greater or equal to this argument in a given place, user intervention is suggested. Defaults to NULL (i.e. no errors are issued)
#' 
#' @return A list containing 1) the detections used during the analysis, 2) the movement events, 3) the status dataframe, 4) the survival overview per group, 5) the progression through the study area, 6) the ALS array/sections' efficiency, 7) the list of spatial objects used during the analysis.
#' 
#' @export
#' 
#' @import stats
#' @import utils
#' @import graphics
#' 
explore <- function(path = NULL, maximum.time = 60, speed.method = c("last to first", "first to first"),
    speed.warning = NULL, speed.error = NULL, tz.study.area, start.timestamp = NULL, end.timestamp = NULL, 
    report = TRUE, exclude.tags = NULL, jump.warning = 2, jump.error = 3, inactive.warning = NULL, 
    inactive.error = NULL,  debug = FALSE) {

# check arguments quality
  my.home <- getwd()
  if (!is.numeric(maximum.time))
    stop("'maximum.time' must be numerical.\n", call. = FALSE)

  speed.method <- match.arg(speed.method)
  if (!is.null(speed.warning) && !is.numeric(speed.warning))
    stop("'speed.warning' must be numeric.\n", call. = FALSE)    
  if (!is.null(speed.error) && !is.numeric(speed.error))
    stop("'speed.error' must be numeric.\n", call. = FALSE)    
  if (!is.null(speed.error) & is.null(speed.warning))
    speed.warning <- speed.error
  if (!is.null(speed.error) && speed.error < speed.warning)
    stop("'speed.error' must not be lower than 'speed.warning'.\n", call. = FALSE)
  
  if (!is.null(start.timestamp) && !grepl("^[1-2][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9] [0-2][0-9]:[0-5][0-9]:[0-5][0-9]", start.timestamp))
    stop("'start.timestamp' must be in 'yyyy-mm-dd hh:mm:ss' format.\n", call. = FALSE)
  if (!is.null(end.timestamp) && !grepl("^[1-2][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9] [0-2][0-9]:[0-5][0-9]:[0-5][0-9]", end.timestamp))
    stop("'end.timestamp' must be in 'yyyy-mm-dd hh:mm:ss' format.\n", call. = FALSE)
  
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
    on.exit(save(list = ls(), file = "explore_debug.RData"), add = TRUE)
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
  the.function.call <- paste0("explore(path = ", ifelse(is.null(path), "NULL", paste0("'", path, "'")), 
      ", maximum.time = ", maximum.time,
      ", speed.method = ", paste0("c('", speed.method, "')"),
      ", speed.warning = ", ifelse(is.null(speed.warning), "NULL", speed.warning), 
      ", speed.error = ", ifelse(is.null(speed.error), "NULL", speed.error), 
      ", tz.study.area = ", ifelse(is.null(tz.study.area), "NULL", paste0("'", tz.study.area, "'")), 
      ", start.timestamp = ", ifelse(is.null(start.timestamp), "NULL", paste0("'", start.timestamp, "'")),
      ", end.timestamp = ", ifelse(is.null(end.timestamp), "NULL", paste0("'", end.timestamp, "'")),
      ", report = ", ifelse(report, "TRUE", "FALSE"), 
      ", exclude.tags = ", ifelse(is.null(exclude.tags), "NULL", paste0("c('", paste(exclude.tags, collapse = "', '"), "')")), 
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

  appendTo(c("Report"), paste0("Timestamp:", Sys.time(), "\n\nM: Selected folder: ", getwd()))

  if (!is.null(path))
    appendTo(c("Screen"), "M: Moving to selected work directory")
  
  report <- checkReport(report = report)
# -----------------------------------

# Load, structure and check the inputs
study.data <- loadStudyData(tz.study.area = tz.study.area, override = NULL, 
                            start.timestamp = start.timestamp, end.timestamp = end.timestamp,
                            sections = NULL, exclude.tags = exclude.tags)
bio <- study.data$bio
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
    speed.method = speed.method, maximum.time = maximum.time, tz.study.area = tz.study.area, 
    dist.mat = dist.mat, invalid.dist = invalid.dist)

  for (fish in names(movements)) {
    movements[[fish]] <- speedReleaseToFirst(fish = fish, bio = bio, movements = movements[[fish]],
     dist.mat = dist.mat, invalid.dist = invalid.dist, silent = FALSE)
  }

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

  times <- getTimes(movements = valid.movements, spatial = spatial, type = "arrival", events = "all")

  appendTo("Screen", "M: Validating detections...")

  valid.detections <- validateDetections(detections.list = detections.list, movements = valid.movements)

# -------------------------------------

# wrap up in-R objects
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
    appendTo("Screen", paste0("M: An actel results file is already present in the present directory, saving new results as '", resultsname,"'."))
    rm(continue, index)
  } else {
    appendTo(c("Screen", "Report"), paste0("M: Saving results to '", resultsname, "'."))
  }

  detections <- detections.list
  deployments <- do.call(rbind.data.frame, deployments)
  if (invalid.dist)
    save(detections, valid.detections, spatial, deployments, arrays, movements, valid.movements, times, file = resultsname)
  else
    save(detections, valid.detections, spatial, deployments, arrays, movements, valid.movements, times, dist.mat, file = resultsname)
# ------------

# Print graphics
  if (report) {
    biometric.fragment <- printBiometrics(bio = bio)
    printDot(dot = dot, sections = NULL, spatial = spatial)
    individual.plots <- printIndividuals(redraw = TRUE, detections.list = detections.list, 
      tz.study.area = tz.study.area, movements = movements, bio = bio)
    circular.plots <- printCircular(times = convertTimesToCircular(times), bio = bio)
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
    rmarkdown::render(reportname <- printExploreRmd(biometric.fragment = biometric.fragment, 
      circular.plots = circular.plots, individual.plots = individual.plots, spatial = spatial,
      detections = detections, valid.detections = valid.detections), quiet = TRUE)
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

  if (invalid.dist) {
    return(list(detections = detections, valid.detections = valid.detections, spatial = spatial, deployments = deployments, arrays = arrays,
      movements = movements, valid.movements = valid.movements, times = times))
  } else {
    return(list(detections = detections, valid.detections = valid.detections, spatial = spatial, deployments = deployments, arrays = arrays,
      movements = movements, valid.movements = valid.movements, times = times, dist.mat = dist.mat))
  }
}


#' Print Rmd report
#'
#' Creates a Rmd report and converts it to hmtl.
#' 
#' @param biometric.fragment Rmarkdown string specifying the biometric graphics drawn.
#' @param individual.plots Rmarkdown string specifying the name of the individual plots.
#' @param circular.plots Rmarkdown string specifying the name of the circular plots.
#' @param detections All the detections used in the study
#' @param valid.detectiosn The valid detections used in the study
#' @inheritParams loadDetections
#' 
#' @keywords internal
#' 
printExploreRmd <- function(biometric.fragment, individual.plots, circular.plots, spatial, detections, valid.detections){
  appendTo("Screen", "M: Producing final report.")
  if (file.exists(reportname <- "Report/actel_explore_report.Rmd")) {
    continue <- TRUE
    index <- 1
    while (continue) {
      if(file.exists(reportname <- paste0("Report/actel_explore_report.", index, ".Rmd"))) {
        index <- index + 1
      } else {
        continue <- FALSE
      }
    }
    appendTo("Screen", paste0("M: An actel report is already present in the present directory, saving new report as 'actel_explore_report.", index, ".html'."))
    rm(continue,index)
  } else {
    appendTo("Screen", "M: Saving actel report as 'actel_explore_report.html'.")
  }
  if (any(grepl("Ukn.", spatial$stations$Standard.Name))) {
    unknown.fragment <- paste('<span style="color:red"> Number of relevant unknown receivers: **', sum(grepl("Ukn.", spatial$stations$Standard.Name)), '**</span>\n', sep = "")
  } else {
    unknown.fragment <- ""
  } 
  report <- readr::read_file("temp_log.txt")
  sink(reportname)
  cat(paste(
'---
title: "Acoustic telemetry exploratory analysis"
author: "Automatically generated by actel"
output: 
  html_document:
    includes:
      after_body: toc_menu_explore.html
---

### Summary

Selected folder: ', stringr::str_extract(pattern = '(?<=M: Selected folder: )[^\r]*', string = report), '

Timestamp: **', stringr::str_extract(pattern = '(?<=Timestamp:)[^\r]*', string = report), '** 

Number of target tags: **`r I(nrow(bio))`**

Number of listed receivers: **', stringr::str_extract(pattern = '(?<=Number of ALS: )[0-9]*', string = report), '** (of which **', stringr::str_extract(pattern = '(?<=of which )[0-9]*', string = report), '** had no detections)

', unknown.fragment,'

Data time range: ', stringr::str_extract(pattern = '(?<=Data time range: )[^\r]*', string = report), '

Percentage of valid detections: ', round(sum(unlist(lapply(valid.detections, nrow))) / sum(unlist(lapply(detections, nrow))) * 100, 2), '%

Found a bug? [**Report it here.**](https://github.com/hugomflavio/actel/issues)

### Study area

Release sites are marked with "R.S."

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

### Warning messages

```{r warnings, echo = FALSE, comment = NA}
if(file.exists("../temp_warnings.txt")) cat(gsub("\\r", "", readr::read_file("../temp_warnings.txt"))) else cat("No warnings were raised during the analysis.")
```

### Biometric graphics

<center>
', biometric.fragment,'
</center>


### Time of first arrival at each Array

Note:
  : Coloured lines on the outer circle indicate the mean value for each group and the respective ranges show the standard error of the mean. Each group\'s bars sum to 100%. The number of data points in each group is presented between brackets in the legend of each pannel. 

<center>
', circular.plots,'
</center>


### Full log

```{r log, echo = FALSE, comment = NA}
cat(gsub("\\r", "", readr::read_file("../temp_log.txt")))
```


### Individual plots

Note:
  : The detections are coloured by array. The vertical black dashed line shows the time of release. The dashed dark-grey line shows the generated movement events.
  : The movement event lines move straight between the first and last station of each event (i.e. in-between detections will not be individually linked by the line).
  : Manually **edited** fish are highlighted with **yellow** graphic borders.


<center>
', individual.plots,'
</center>

', sep = ""), fill = TRUE)
sink()

if(file.exists("Report/toc_menu_explore.html"))
  file.remove("Report/toc_menu_explore.html")
sink("Report/toc_menu_explore.html")
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
  <a href="#biometric-graphics">Biometrics</a>
  <a href="#time-of-first-arrival-at-each-array">Arrival times</a>
  <a href="#full-log">Full log</a>
  <a href="#individual-plots">Individuals</a>
</div>
', fill = TRUE)
sink()
return(reportname)
}

#' Compare original detections with the valid movements and exclude invalid detections
#' 
#' @param detections.list the list of detections per fish
#' @param movements the list of movements to be matched
#' 
#' @return A list of valid detections per fish
#' 
#' @keywords internal
#' 
validateDetections <- function(detections.list, movements) {
  output <- lapply(names(movements), function(i) {
    # cat(i, "\n")
    aux <- detections.list[[i]]
    valid.rows <- unlist(lapply(1:nrow(movements[[i]]), function(j) {
      start <- min(which(aux$Timestamp == movements[[i]]$First.time[j] & aux$Standard.Name == movements[[i]]$First.station[j]))
      stop <- start + (movements[[i]]$Detections[j] - 1)
      # cat(start, ":", stop, "\n")
      return(start:stop)
    }))
    return(aux[valid.rows, ])
  })
  names(output) <- names(movements)
  attributes(output)$actel <- "valid.detections"
  return(output)
}


