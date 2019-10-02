#' @importFrom circular sd mean.circular var
NULL

#' Actel: Acoustic telemetry data sorting
#' 
#' The actel package provides a systematic way of analysing fish migration data.
#' Its main function, actel, collects the input present in the target folder and analyses the telemetry data.
#' It is strongly recommended to read the package vignettes before attempting to run the analyses. You can find the vignettes by running browseVignettes('actel') .
#' 
#' @param path Path to the folder containing the data. If the R session is already running in the target folder, path may be left as NA
#' @param sections The sections in which the study was divided. Must be coincident with the names given to the ALS arrays. (i.e. if an array is 'River1', then the respective section is 'River') 
#' @param success.arrays The ALS arrays mark the end of the study area. If a fish crosses one of these arrays, it is considered to have successfully migrated through the area.
#' @param minimum.detections The minimum number of times a tag must have been recorded during the study period for it to be considered a true tag and not random noise.
#' @param maximum.time The number of minutes that must pass between detections for a new event to be created.
#' @param speed.method One of 'last to first' or 'first to first'. In the former, the last detection on a given array/section is matched to the first detection on the next array/section (default). If changed to 'first to first', the first detection on two consecutive arrays/sections are used to perform the calculations.
#' @param if.last.skip.section Indicates whether a fish detected at the last array of a given section should be considered to have disappeared in the next section. Defaults to TRUE. I.e.: In a study with sections 'River' and 'Fjord', where 'River3' is the last river array, a fish last detected at River3 will be considered as 'disappeared in the Fjord'.
#' @param tz.study.area The time-zone of the study area. Necessary to convert the ALS time data, which is in UTC.
#' @param start.timestamp Detection data prior to this date is not analysed. Improves processing time when loading large amounts of detection data.
#' @param end.timestamp Detection data posterior to this date is not analysed. Improves processing time when loading large amounts of detection data.
#' @param report Whether graphics, tables and LaTeX report files should be created. Defaults to TRUE. Allows automatic compiling of a PDF report after the analysis.
#' @param redraw When report is set to TRUE, controls if files created in a previous run of the function should be overridden.
#' @param override A list of tags for which the user intends to manually define entering and leaving points for each study section.
#' @param exclude.tags A list of tags that should be excluded from the detection data before any analyses are performed. Intended to be used if stray tags from a different code space but with the same signal as a target tag are detected in the study area.
#' @param debug If TRUE, temporary files are not deleted at the end of the analysis. Defaults to FALSE.
#' @param cautious.assignment If TRUE, actel avoids assigning events with one detection as first and/or last events of a section.
#' 
#' @return A list containing 1) the detections used during the analysis, 2) the movement events, 3) the status dataframe, 4) the survival overview per group, 5) the progression through the study area, 6) the ALS array/sections' efficiency, 7) the list of spatial objects used during the analysis.
#' 
#' @export
#' 
actel <- function(path = NULL, sections, success.arrays, minimum.detections = 2, 
    maximum.time = 60, speed.method = c("last to first", "first to first"), 
    if.last.skip.section = TRUE, tz.study.area, start.timestamp = NULL, 
    end.timestamp = NULL, report = TRUE, redraw = TRUE, override = NULL, 
    exclude.tags = NULL, debug = FALSE, cautious.assignment = TRUE, replicate = NULL) {
  
  cat(paste0(
"NOTE: The function 'actel' is deprecated. Please switch to the function 'migration' as soon as possible.
  The new function requires a 'deployments.csv' file. To convert your study data automatically
  to the new format, run: updateStudy(tz.study.area = '", tz.study.area, "')\n"))

  readline("Press ENTER to continue with actel, or ESC to stop the function.\n")

  my.home <- getwd()

  if (debug) {
    on.exit(save(list = ls(), file = "actel_debug.RData"), add = TRUE)
    appendTo("Screen", "!!!--- Debug mode has been activated ---!!!")
  }
  
  if (!debug)
    on.exit(deleteHelpers(), add = TRUE)
  
  on.exit(setwd(my.home), add = TRUE)
  on.exit(tryCatch(sink(), warning = function(w) {hide <- NA}), add = TRUE)
  
  if (!debug)
    on.exit(deleteHelpers(), add = TRUE)
  
  speed.method <- match.arg(speed.method)
  
  deleteHelpers()
  
  the.function.call <- paste0("actel(path = ", ifelse(is.null(path), "NULL", paste0("'", path, "'")), 
      ", sections = ", paste0("c('", paste(sections, collapse = "', '"), "')"), 
      ", success.arrays = ", paste0("c('", paste(success.arrays, collapse = "', '"), "')"), 
      ", minimum.detections = ", minimum.detections,
      ", maximum.time = ", maximum.time,
      ", speed.method = ", paste0("c('", speed.method, "')"),
      ", if.last.skip.section = ", ifelse(if.last.skip.section, "TRUE", "FALSE"),
      ", tz.study.area = ", ifelse(is.null(tz.study.area), "NULL", paste0("'", tz.study.area, "'")), 
      ", start.timestamp = ", ifelse(is.null(start.timestamp), "NULL", paste0("'", start.timestamp, "'")),
      ", end.timestamp = ", ifelse(is.null(end.timestamp), "NULL", paste0("'", end.timestamp, "'")),
      ", report = ", ifelse(report, "TRUE", "FALSE"), 
      ", redraw = ", ifelse(redraw, "TRUE", "FALSE"),
      ", override = ", ifelse(is.null(override), "NULL", paste0("c('", paste(override, collapse = "', '"), "')")),
      ", exclude.tags = ", ifelse(is.null(exclude.tags), "NULL", paste0("c('", paste(exclude.tags, collapse = "', '"), "')")), 
      ", debug = ", ifelse(debug, "TRUE", "FALSE"), 
      ", cautious.assignment = ", ifelse(cautious.assignment, "TRUE", "FALSE"), 
      ", replicate = ", ifelse(is.null(replicate),"NULL", paste0("c('", paste(replicate, collapse = "', '"), "')")), ")"
      )

  appendTo("Report", "Acoustic telemetry data analysis report.\n") 
  
  path <- pathCheck(my.home = my.home, path = path)
  
  if (debug)
    appendTo("Report", "!!!--- Debug mode has been activated ---!!!\n")

  appendTo(c("Report"), paste("Timestamp:", Sys.time(), "\n\nM: Selected folder: ", getwd(), "\nM: Success has been defined as last detection in: ", paste(success.arrays, collapse = ", "), 
    ".", sep = ""))

  appendTo(c("Screen"), "M: Moving to selected work directory")
  
  report <- folderCheck(report = report, redraw = redraw)
 
  appendTo(c("Screen", "Report"), "M: Importing data. This process may take a while.")
  
  bio <- loadBio(file = "biometrics.csv")

  # Check that all the overriden fish are part of the study
  if (!is.null(override) && any(link <- is.na(match(unlist(lapply(strsplit(override, "-"), function(x) tail(x, 1))), bio$Signal))))
    stop("Some tag signals listed in 'override' ('", paste0(override[link], collapse = "', '"), "') are not listed in the biometrics file.\n")

  spatial <- assembleSpatial(file = "spatial.csv", bio = bio, sections = sections)
  appendTo(c("Screen", "Report"), paste("M: Number of target tags: ", nrow(bio), ".", sep = ""))
  
  # Prepare detection loading
  detections <- loadDetections(start.timestamp = start.timestamp, end.timestamp = end.timestamp, tz.study.area = tz.study.area)
  # Standardize the station names
  detections <- standardizeStations(input = detections, spatial = spatial)

  unknownReceiversCheckA(spatial = spatial, detections = detections)
  
  checkEmptyReceivers(spatial = spatial, detections = detections)
  

  invalid.dist <- TRUE
  appendTo("Debug", "Creating 'dist.mat' if distances file is present.")
  if (file.exists("distances.csv")) {
    appendTo(c("Screen", "Report"), "M: A distances matrix file is present, activating speed calculations.")
    dist.mat <- read.csv("distances.csv", row.names = 1)
    invalid.dist <- FALSE
    if (nrow(dist.mat) != ncol(dist.mat)){
      appendTo(c("Screen", "Report", "Warning"), "Error: The distance matrix appears to be missing data (ncol != nrow). Deactivating speed calculation to avoid function failure.")
      invalid.dist <- TRUE
    }
    if (!invalid.dist && sum(nrow(spatial$stations), nrow(spatial$release.sites)) != nrow(dist.mat)) {
      appendTo(c("Screen", "Report", "Warning"), "Error: The number of spatial points does not match the number of rows in the distance matrix. Deactivating speed calculation to avoid function failure.")
      invalid.dist <- TRUE
    }
    if (!invalid.dist && (any(!matchl(spatial$stations$Standard.Name, colnames(dist.mat))) | any(!matchl(spatial$release.sites$Standard.Name, colnames(dist.mat))))) {
      appendTo(c("Screen", "Report", "Warning"), "Error: Some stations and/or release sites are not present in the distance matrix. Deactivating speed calculation to avoid function failure.")
      invalid.dist <- TRUE
    }    
    if (!invalid.dist && any(rownames(dist.mat) != colnames(dist.mat))) {
      appendTo(c("Screen", "Report", "Warning"), "Error: Some stations and/or release sites are not present in the distance matrix. Deactivating speed calculation to avoid function failure.")
      invalid.dist <- TRUE
    }    
  } else {
    dist.mat <- NA
  }
  
  recipient <- deprecated_splitDetections(detections = detections, bio = bio, spatial = spatial, exclude.tags = exclude.tags)
  detections.list <- recipient[[1]]
  bio <- recipient[[2]]
  rm(recipient)

  recipient <- unknownReceiversCheckB(detections.list = detections.list, spatial = spatial)
  spatial <- recipient[[1]]
  if (recipient[[2]]) {
    detections <- standardizeStations(input = detections, spatial = spatial)
    recipient <- deprecated_splitDetections(detections = detections, bio = bio, spatial = spatial, exclude.tags = exclude.tags, silent = TRUE)
    detections.list <- recipient[[1]]
    bio <- recipient[[2]]
  }
  rm(recipient)

  detections.list <- checkDetectionsBeforeRelease(input = detections.list, bio = bio)

  appendTo(c("Screen", "Report"), "M: Data successfully imported!\nM: Creating movement records for the valid tags.")
  movements <- groupMovements(detections.list = detections.list, bio = bio, spatial = spatial,
    speed.method = speed.method, maximum.time = maximum.time, tz.study.area = tz.study.area, dist.mat = dist.mat, invalid.dist = invalid.dist)

  for(fish in names(movements)){
    movements[[fish]] <- speedReleaseToFirst(fish = fish, bio = bio, movements = movements[[fish]],
     dist.mat = dist.mat, invalid.dist = invalid.dist, silent = FALSE)
  }
  
  recipient <- assembleTimetable(movements = movements, sections = sections, spatial = spatial, 
    minimum.detections = minimum.detections, dist.mat = dist.mat, invalid.dist = invalid.dist, 
    speed.method = speed.method, if.last.skip.section = if.last.skip.section, success.arrays = success.arrays, 
    override = override, cautious.assignment = cautious.assignment)
  timetable <- recipient[[1]]
  movements <- recipient[[2]]
  appendTo(c("Screen", "Report"), "M: Timetable successfully filled. Fitting in the remaining variables.")
  
  status.df <- assembleOutput(timetable = timetable, bio = bio, movements = movements, spatial = spatial, 
    sections = sections, dist.mat = dist.mat, invalid.dist = invalid.dist, tz.study.area = tz.study.area)
  
  simple.movements <- simplifyMovements(movements = movements, bio = bio, 
    speed.method = speed.method, dist.mat = dist.mat, invalid.dist = invalid.dist)

  appendTo(c("Screen", "Report"), "M: Getting summary information tables.")
  
  the.matrices <- assembleMatrices(spatial = spatial, simple.movements = simple.movements, minimum.detections = minimum.detections, status.df = status.df)
  last.array.results <- getEstimate(spatial = spatial, detections.list = detections.list, replicate = replicate)
  if (is.list(last.array.results)) 
    estimate <- last.array.results$results$combined.efficiency
  else
    estimate <- NULL
  if (length(the.matrices) == 1)
    overall.CJS <- simpleCJS(the.matrices[[1]], estimate = estimate)
  else
    overall.CJS <- combineCJS(the.matrices, estimate = estimate)
  split.CJS <- getSplitCJS(the.matrices, fixed.efficiency = overall.CJS$efficiency)
  group.CJS <- getGroupCJS(the.matrices, status.df, fixed.efficiency = overall.CJS$efficiency)

  array.overview <- assembleArrayOverview(group.CJS = group.CJS)
  section.overview <- assembleSectionOverview(status.df = status.df, sections = sections)

  times <- getTimes(simple.movements = simple.movements, spatial = spatial, 
    tz.study.area = tz.study.area, type = "Arrival")

  
  if (!is.null(override)) {
    header.fragment <- paste('<span style="color:red">Manual mode has been triggered for **', length(override),'** fish.</span>\n', sep = "")
    name.fragment <- "_corrected"
  } else {
    header.fragment <- name.fragment <- ""
  }

  if (file.exists(resultsname <- paste("actel_results", name.fragment, ".RData", sep = ""))) {
    continue <- TRUE
    index <- 1
    while (continue) {
      if (file.exists(resultsname <- paste("actel_results", name.fragment, ".", index, ".RData", sep = ""))) {
        index <- index + 1
      } else {
        continue <- FALSE
      }
    }
    appendTo("Screen", paste("M: An actel results file is already present in the present directory, saving new results as '", resultsname,"'.", sep = ""))
    rm(continue, index)
  } else {
    appendTo(c("Screen", "Report"), paste("M: Saving results to '", resultsname, "'.", sep = ""))
  }

  detections <- detections.list
  efficiency <- overall.CJS
  if (invalid.dist)
    save(detections, movements, simple.movements, status.df, 
      section.overview, array.overview, the.matrices, efficiency, 
      last.array.results, spatial, times, file = resultsname)
  else
    save(detections, movements, simple.movements, status.df, 
      section.overview, array.overview, the.matrices, efficiency,
      last.array.results, spatial, times, dist.mat, file = resultsname)

  if (report) {
    biometric.fragment <- printBiometrics(bio = bio)
    efficiency.fragment <- printEfficiency(overall.CJS = overall.CJS, last.array.results = last.array.results)
    printDotplots(status.df = status.df, invalid.dist = invalid.dist)
    printSurvivalGraphic(section.overview = section.overview)
    printProgression(status.df = status.df, overall.CJS = overall.CJS, split.CJS = split.CJS, group.CJS = group.CJS)
    individual.plots <- printIndividuals(redraw = redraw, detections.list = detections.list, bio = bio, 
        status.df = status.df, tz.study.area = tz.study.area, movements = movements, simple.movements = simple.movements)
    circular.plots <- printCircular(times = convertTimesToCircular(times), bio = bio)
    array.overview.fragment <- printArrayOverview(array.overview)
    if (nrow(section.overview) > 3) 
      survival.graph.size <- "width=90%" else survival.graph.size <- "height=4in"
  }
  
  appendTo("Report", "M: Process finished successfuly.")
  
  appendTo("Report", "\n-------------------")
  if (file.exists("temp_UD.txt")) 
    appendTo("Report", paste0("User inverventions:\n-------------------\n", gsub("\r", "", readr::read_file("temp_UD.txt")), "-------------------"))
  
  appendTo("Report", paste0("Function call:\n-------------------\n", the.function.call, "\n-------------------"))

  if (report) {
    appendTo("debug", "debug: Printing report")
    rmarkdown::render(reportname <- printRmd(name.fragment = name.fragment, header.fragment = header.fragment, 
        biometric.fragment = biometric.fragment, survival.graph.size = survival.graph.size, circular.plots = circular.plots,
        individual.plots = individual.plots, spatial = spatial, efficiency.fragment = efficiency.fragment, array.overview.fragment = array.overview.fragment), quiet = TRUE)
    appendTo("debug", "debug: Moving report")
    fs::file_move(sub("Rmd", "html", reportname), sub("Report/", "", sub("Rmd", "html", reportname)))
    appendTo("debug", "debug: Opening report if the pc has internet.")
    if (havingIP())
      hide <- system(paste0('open "', sub("Report/", "", sub("Rmd", "html", reportname)), '"'), show.output.on.console = FALSE)
    else
      appendTo("Screen", "M: Skipping auto-opening of the report as R has been crashing when opening the html without an internet connection.")
    appendTo("debug", "debug: Removing toc_menu.html")
    if(file.exists("Report/toc_menu.html"))
      file.remove("Report/toc_menu.html")
  }
  appendTo("Screen", "M: Process finished successfuly.")
  
  appendTo("Screen", paste("M: Saving job log as '", paste(gsub(":", ".", sub(" ", ".", as.character(Sys.time()))), "actel.log.txt", sep = "."), "'.", sep = ""))
  file.rename("temp_log.txt", paste(gsub(":", ".", sub(" ", ".", as.character(Sys.time()))), "actel.log.txt", sep = "."))
  
  if (!debug)
    deleteHelpers()

  if (invalid.dist)
    return(list(detections = detections, movements = movements, simple.movements = simple.movements,
      status.df = status.df, section.overview = section.overview, array.overview = array.overview,
      matrices = the.matrices, efficiency = overall.CJS, times = times, 
      last.array.results = last.array.results, spatial = spatial))
  else
    return(list(detections = detections, movements = movements, simple.movements = simple.movements,
      status.df = status.df, section.overview = section.overview, array.overview = array.overview,
      matrices = the.matrices, efficiency = overall.CJS, times = times, 
      last.array.results = last.array.results, spatial = spatial, dist.mat = dist.mat))
}

#' Print Rmd report
#'
#' Creates a Rmd report and converts it to hmtl.
#' 
#' @param name.fragment Rmarkdown string specifying the type of report for the title.
#' @param header.fragment Rmarkdown string specifying the type of report for the header.
#' @param biometric.fragment Rmarkdown string specifying the biometric graphics drawn.
#' @param efficiency.fragment Rmarkdown string specifying the efficiency results.
#' @param array.overview.fragment Rmarkdown string specifying the array overview results.
#' @param survival.graph.size Rmarkdown string specifying the type size of the survival graphics.
#' @param individual.plots Rmarkdown string specifying the name of the individual plots.
#' @param circular.plots Rmarkdown string specifying the name of the circular plots.
#' @inheritParams loadDetections
#' 
#' @keywords internal
#' 
printRmd <- function(name.fragment, header.fragment, biometric.fragment, efficiency.fragment, array.overview.fragment,
  survival.graph.size, individual.plots, circular.plots, spatial){
  appendTo("Screen", "M: Producing final report.")
  if (file.exists(reportname <- paste("Report/actel_report", name.fragment, ".Rmd", sep = ""))) {
    continue <- TRUE
    index <- 1
    while (continue) {
      if(file.exists(reportname <- paste("Report/actel_report", name.fragment, ".", index, ".Rmd", sep = ""))) {
        index <- index + 1
      } else {
        continue <- FALSE
      }
    }
    appendTo("Screen",paste("M: An actel report is already present in the present directory, saving new report as 'actel_report", name.fragment, ".", index, ".html'.", sep = ""))
    rm(continue,index)
  } else {
    appendTo("Screen",paste("M: Saving actel report as 'actel_report", name.fragment, ".html'.", sep = ""))
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
title: "Acoustic telemetry analysis report"
author: "Actel package"
output: 
  html_document:
    includes:
      after_body: toc_menu.html
---

### Summary

Selected folder: ', stringr::str_extract(pattern = '(?<=M: Selected folder: )[^\r]*', string = report), '

Timestamp: **', stringr::str_extract(pattern = '(?<=Timestamp:)[^\r]*', string = report), '** 

Number of target tags: **`r I(nrow(status.df))`**

', header.fragment,' 

Number of listed receivers: **`r I(spatial$number.of.receivers)`** (of which **', stringr::str_extract(pattern = '(?<=of which )[0-9]*', string = report), '** had no detections)

', unknown.fragment,'

Data time range: ', stringr::str_extract(pattern = '(?<=Data time range: )[^\r]*', string = report), '

Found a bug? [**Report it here.**](https://github.com/hugomflavio/actel/issues)

### List of Stations

```{r stations, echo = FALSE}
knitr::kable(spatial$stations, row.names = FALSE)
```


### List of Release sites

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

Note:
  : The progression calculations do not account for intra-section backwards movements. This implies that the total number of fish to have been **last seen** at a given array may be lower than the displayed below. Please refer to the [section survival overview](#survival) to find out where your fish were considered to have disappeared.
  
<center>
![](progression.png){ width=75% }
</center>

', array.overview.fragment, '


### Time of arrival at each Array

Note:
  : Coloured lines on the outer circle indicate the mean value for each group and the respective ranges show the standard error of the mean. Each group\'s bars sum to 100%. The number of data points in each group is presented between brackets in the legend of each pannel. 

<center>
', circular.plots,'
</center>


### Dotplots

Note:
  : The **top** 10% of the values for each panel are marked in **red**.
  : The **bottom** 10% of the values for each panel are marked in **orange**.

<center>
![](dotplots.png){ width=95% }
</center>


### Full log

```{r log, echo = FALSE, comment = NA}
cat(gsub("\\r", "", readr::read_file("../temp_log.txt")))
```


### Individual plots

Note:
  : The detections are coloured by array. The vertical black dashed line shows the time of release. The vertical grey dashed lines show the assigned moments of entry and exit for each study area section. The full dark-grey line shows the movement events considered valid, while the dashed dark-grey line shows the movement events considered invalid.
  : The movement event lines move straight between the first and last station of each event (i.e. in-between detections will not be individually linked by the line).
  : Manually **edited** fish are highlighted with **yellow** graphic borders.
  : Manually **overridden** fish are highlighted with **red** graphic borders.

<center>
', individual.plots,'
</center>

', sep = ""), fill = TRUE)
sink()

if(file.exists("Report/toc_menu.html"))
  file.remove("Report/toc_menu.html")
sink("Report/toc_menu.html")
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
  <a href="#list-of-stations">Stations</a>
  <a href="#list-of-release-sites">Releases</a>
  <a href="#array-forward-efficiency">Efficiency</a>
  <a href="#warning-messages">Warnings</a>
  <a href="#user-comments">Comments</a>
  <a href="#biometric-graphics">Biometrics</a>
  <a href="#survival">Survival</a>
  <a href="#progression">Progression</a>
  <a href="#time-of-arrival-at-each-array">Arrival times</a>
  <a href="#dotplots">Dotplots</a>
  <a href="#full-log">Full log</a>
  <a href="#individual-plots">Individuals</a>
</div>
', fill = TRUE)
sink()
return(reportname)
}

