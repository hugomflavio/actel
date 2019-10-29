#' Actel: Acoustic telemetry data sorting
#' 
#' The actel package provides a systematic way of analysing fish migration data.
#' Its main function, actel, collects the input present in the target folder and analyses the telemetry data.
#' It is strongly recommended to read the package vignettes before attempting to run the analyses. You can find the vignettes by running browseVignettes('actel') .
#' 
#' @inheritParams migration
#' @inheritParams explore
#' 
#' @return A list containing 1) the detections used during the analysis, 2) the movement events, 3) the status dataframe, 4) the survival overview per group, 5) the progression through the study area, 6) the ALS array/sections' efficiency, 7) the list of spatial objects used during the analysis.
#' 
#' @export
#' 
residency <- function(path = NULL, sections, minimum.detections = 2, 
  maximum.time = 60, speed.method = c("last to first", "first to first"), 
  if.last.skip.section = TRUE, tz.study.area, start.timestamp = NULL, 
  end.timestamp = NULL, report = TRUE, override = NULL, 
  exclude.tags = NULL, cautious.assignment = TRUE, replicates = NULL,
  jump.warning = 2, jump.error = 3, debug = FALSE) {
  
# check argument quality
  my.home <- getwd()
  if (!is.numeric(minimum.detections))
    stop("'minimum.detections' must be numeric.\n", call. = FALSE)
  if (!is.numeric(maximum.time))
    stop("'maximum.time' must be numeric.\n", call. = FALSE)
  speed.method <- match.arg(speed.method)
  if (!is.logical(if.last.skip.section))
    stop("'if.last.skip.section' must be logical.\n", call. = FALSE)
  if (!is.null(start.timestamp) && !grepl("^[1-2][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9] [0-2][0-9]:[0-5][0-9]:[0-5][0-9]", start.timestamp))
    stop("'start.timestamp' must be in 'yyyy-mm-dd hh:mm:ss' format.\n", call. = FALSE)
  if (!is.null(end.timestamp) && !grepl("^[1-2][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9] [0-2][0-9]:[0-5][0-9]:[0-5][0-9]", end.timestamp))
    stop("'end.timestamp' must be in 'yyyy-mm-dd hh:mm:ss' format.\n", call. = FALSE)
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
  the.function.call <- paste0("migration(path = ", ifelse(is.null(path), "NULL", paste0("'", path, "'")), 
      ", sections = ", paste0("c('", paste(sections, collapse = "', '"), "')"), 
      # ", success.arrays = ", paste0("c('", paste(success.arrays, collapse = "', '"), "')"), 
      ", minimum.detections = ", minimum.detections,
      ", maximum.time = ", maximum.time,
      ", speed.method = ", paste0("c('", speed.method, "')"),
      ", if.last.skip.section = ", ifelse(if.last.skip.section, "TRUE", "FALSE"),
      ", tz.study.area = ", ifelse(is.null(tz.study.area), "NULL", paste0("'", tz.study.area, "'")), 
      ", start.timestamp = ", ifelse(is.null(start.timestamp), "NULL", paste0("'", start.timestamp, "'")),
      ", end.timestamp = ", ifelse(is.null(end.timestamp), "NULL", paste0("'", end.timestamp, "'")),
      ", report = ", ifelse(report, "TRUE", "FALSE"), 
      # ", redraw = ", ifelse(redraw, "TRUE", "FALSE"),
      ", override = ", ifelse(is.null(override), "NULL", paste0("c('", paste(override, collapse = "', '"), "')")),
      ", exclude.tags = ", ifelse(is.null(exclude.tags), "NULL", paste0("c('", paste(exclude.tags, collapse = "', '"), "')")), 
      ", debug = ", ifelse(debug, "TRUE", "FALSE"), 
      ", cautious.assignment = ", ifelse(cautious.assignment, "TRUE", "FALSE"), 
      ", replicates = ", ifelse(is.null(replicates),"NULL", paste0("c('", paste(replicates, collapse = "', '"), "')")),
      ", jump.warning = ", jump.warning,
      ", jump.error = ", jump.error,
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
  appendTo(c("Screen", "Report"), "M: Importing data. This process may take a while.")
  bio <- loadBio(file = "biometrics.csv", tz.study.area = tz.study.area)
  appendTo(c("Screen", "Report"), paste("M: Number of target tags: ", nrow(bio), ".", sep = ""))
  
  # Check that all the overriden fish are part of the study
  if (!is.null(override) && any(link <- is.na(match(unlist(lapply(strsplit(override, "-"), function(x) tail(x, 1))), bio$Signal))))
    stop("Some tag signals listed in 'override' ('", paste0(override[link], collapse = "', '"), "') are not listed in the biometrics file.\n")
  deployments <- loadDeployments(file = "deployments.csv", tz.study.area = tz.study.area)
  checkDeploymentTimes(input = deployments) # check that receivers are not deployed before being retrieved
  spatial <- loadSpatial(file = "spatial.csv", verbose = TRUE)
  deployments <- checkDeploymentStations(input = deployments, spatial = spatial) # match Station.Name in the deployments to Station.Name in spatial, and vice-versa
  deployments <- createUniqueSerials(input = deployments) # Prepare serial numbers to overwrite the serials in detections

  detections <- loadDetections(start.timestamp = start.timestamp, end.timestamp = end.timestamp, tz.study.area = tz.study.area)
  detections <- createStandards(detections = detections, spatial = spatial, deployments = deployments) # get standardize station and receiver names, check for receivers with no detections
  checkUnknownReceivers(input = detections) # Check if there are detections from unknown detections

  if (file.exists("spatial.dot")) {
    appendTo(c("Screen", "Report"), "M: A 'spatial.dot' file was detected, activating multi-branch analysis.")
    recipient <- loadDot(input = "spatial.dot", spatial = spatial, sections = sections)
  } else {
    fakedot <- paste(unique(spatial$Array), collapse = "->")
    recipient <- loadDot(string = fakedot, spatial = spatial, sections = sections)
  }
  dot <- recipient[[1]]
  arrays <- recipient[[2]]
  dotmat <- recipient[[3]]
  rm(recipient)

  # Check if there is a logical first array in the study area, should a replacement release site need to be created.
  if (sum(unlist(lapply(arrays, function(a) is.null(a$before)))) == 1)
    first.array <- names(arrays)[unlist(lapply(arrays, function(a) is.null(a$before)))]
  else
    first.array <- NULL
  spatial <- transformSpatial(spatial = spatial, bio = bio, sections = sections, first.array = first.array) # Finish structuring the spatial file
  arrays <- arrays[unlist(spatial$array.order)]

  recipient <- loadDistances(spatial = spatial) # Load distances and check if they are valid
  dist.mat <- recipient[[1]]
  invalid.dist <- recipient[[2]]
  rm(recipient)

  recipient <- splitDetections(detections = detections, bio = bio, exclude.tags = exclude.tags) # Split the detections by tag, store full transmitter names in bio
  detections.list <- recipient[[1]]
  bio <- recipient[[2]]
  rm(recipient)

  recipient <- checkTagsInUnknownReceivers(detections.list = detections.list, deployments = deployments, spatial = spatial) # Check if there is any data loss due to unknown receivers
  spatial <- recipient[[1]]
  deployments <- recipient[[2]]
  rm(recipient)

  detections.list <- labelUnknowns(detections.list = detections.list)
  detections.list <- checkDetectionsBeforeRelease(input = detections.list, bio = bio)
  appendTo(c("Screen", "Report"), "M: Data successfully imported!")
# -------------------------------------
  
# Process the data
  appendTo(c("Screen", "Report"), "M: Creating movement records for the valid tags.")
  movements <- groupMovements(detections.list = detections.list, bio = bio, spatial = spatial,
                              speed.method = speed.method, maximum.time = maximum.time, 
                              tz.study.area = tz.study.area, dist.mat = dist.mat, invalid.dist = invalid.dist)

  aux <- names(movements)
  movements <- lapply(names(movements), 
    function(fish) {
      speedReleaseToFirst(fish = fish, bio = bio, movements = movements[[fish]],
                          dist.mat = dist.mat, invalid.dist = invalid.dist, silent = FALSE)
    })
  names(movements) <- aux
  rm(aux)

  movements <- checkUpstream(movements = movements, bio = bio, spatial = spatial, arrays = arrays)

  movements <- checkJumpDistance(movements = movements, bio = bio, dotmat = dotmat, 
                                 spatial = spatial, jump.warning = jump.warning, jump.error = jump.error)

  simple.movements <- simplifyMovements(movements = movements, bio = bio, 
    speed.method = speed.method, dist.mat = dist.mat, invalid.dist = invalid.dist)

  # Residency-exclusive area

  # Compress array movements into section movements
  section.movements <- sectionMovements(movements = simple.movements, sections = sections)
  # Look for isolated section movements
  section.movements <- checkSMovesN(secmoves = section.movements)
  # Update array movements based on secmove validity
  movements <- updateAMValidity(arrmoves = movements, secmoves = section.movements)

  # Resimplify array moves and calculate simple section moves
  simple.movements <- simplifyMovements(movements = movements, bio = bio, 
    speed.method = speed.method, dist.mat = dist.mat, invalid.dist = invalid.dist)
  simple.secmoves <- sectionMovements(movements = simple.movements, sections = sections)

  # Grab summary information
  res.df <- assembleResidency(secmoves = section.movements, movements = movements, sections = sections)
  appendTo(c("Screen", "Report"), "M: Timetable successfully filled. Fitting in the remaining variables.")
  status.df <- res_assembleOutput(res.df = res.df, bio = bio, spatial = spatial, 
                                  sections = sections, tz.study.area = tz.study.area)
  
  # simple.movements <- simplifyMovements(movements = movements, bio = bio, 
  #   speed.method = speed.method, dist.mat = dist.mat, invalid.dist = invalid.dist)

  # appendTo(c("Screen", "Report"), "M: Getting summary information tables.")
  
  # section.overview <- assembleSectionOverview(status.df = status.df, sections = sections)

  # times <- getTimes(simple.movements = simple.movements, spatial = spatial, 
  #   tz.study.area = tz.study.area, type = "Arrival")
# -------------------------------------

# CJS stuff
#   the.matrices <- assembleMatrices(spatial = spatial, simple.movements = simple.movements, minimum.detections = minimum.detections, status.df = status.df)

#   m.by.array <- breakMatricesByArray(m = the.matrices, arrays = arrays)
#   CJS.list <- lapply(m.by.array, function(m) {
#     if (length(m) == 1)
#       simpleCJS(m[[1]])
#     else
#       combineCJS(m)
#     })

#   overall.CJS <- assembleArrayCJS(mat = the.matrices, CJS = CJS.list, arrays = arrays)

#   if (!is.null(replicates)) {
#     intra.array.matrices <- getDualMatrices(replicates = replicates, CJS = overall.CJS, spatial = spatial, detections.list = detections.list)
#     recipient <- includeIntraArrayEstimates(m = intra.array.matrices, CJS = overall.CJS)
#     overall.CJS <- recipient[[1]]
#     intra.array.CJS <- recipient[[2]]
#     rm(recipient)
#   } else {
#     intra.array.CJS <- NULL
#   }

#   aux <- mbSplitCJS(m = m.by.array, fixed.efficiency = overall.CJS$efficiency)
#   aux <- aux[names(the.matrices)]
#   split.CJS <- assembleSplitCJS(mat = the.matrices, CJS = aux, arrays = arrays)
#   rm(aux)

#   aux <- mbGroupCJS(m = m.by.array, status.df = status.df, fixed.efficiency = overall.CJS$efficiency)
#   group.CJS <- assembleGroupCJS(mat = the.matrices, CJS = aux, arrays = arrays)
#   array.overview <- mbAssembleArrayOverview(input = group.CJS)
#   rm(aux)
# # -------------------------------------

# wrap up in-R objects
#   if (!is.null(override)) {
#     header.fragment <- paste('<span style="color:red">Manual mode has been triggered for **', length(override),'** fish.</span>\n', sep = "")
#     name.fragment <- "_corrected"
#   } else {
#     header.fragment <- name.fragment <- ""
#   }

#   if (file.exists(resultsname <- paste("actel_migration_results", name.fragment, ".RData", sep = ""))) {
#     continue <- TRUE
#     index <- 1
#     while (continue) {
#       if (file.exists(resultsname <- paste("actel_migration_results", name.fragment, ".", index, ".RData", sep = ""))) {
#         index <- index + 1
#       } else {
#         continue <- FALSE
#       }
#     }
#     appendTo("Screen", paste("M: An actel migration results file is already present in the present directory, saving new results as '", resultsname,"'.", sep = ""))
#     rm(continue, index)
#   } else {
#     appendTo(c("Screen", "Report"), paste("M: Saving results to '", resultsname, "'.", sep = ""))
#   }

#   detections <- detections.list
#   deployments <- do.call(rbind.data.frame, deployments)
#   matrices <- the.matrices
#   if (invalid.dist)
#     save(detections, spatial, deployments, arrays, movements, simple.movements, status.df,
#       section.overview, array.overview, matrices, overall.CJS, 
#       intra.array.CJS, times, file = resultsname)
#   else
#     save(detections, spatial, deployments, arrays, movements, simple.movements, status.df,
#       section.overview, array.overview, matrices, overall.CJS,
#       intra.array.CJS, times, dist.mat, file = resultsname)
# # ------------

# # Print graphics
#   if (report) {
#     biometric.fragment <- printBiometrics(bio = bio)
#     efficiency.fragment <- mbPrintEfficiency(overall.CJS = overall.CJS, intra.CJS = intra.array.CJS)
#     printDotplots(status.df = status.df, invalid.dist = invalid.dist)
#     printSurvivalGraphic(section.overview = section.overview)
#     # printProgression(status.df = status.df, overall.CJS = overall.CJS, split.CJS = split.CJS, group.CJS = group.CJS)
#     printDot(dot = dot, sections = sections, spatial = spatial)
#     mbPrintProgression(dot = dot,  sections = sections, overall.CJS = overall.CJS, spatial = spatial, status.df = status.df)
#     individual.plots <- printIndividuals(redraw = TRUE, detections.list = detections.list, bio = bio, 
#         status.df = status.df, tz.study.area = tz.study.area, movements = movements, simple.movements = simple.movements)
#     circular.plots <- printCircular(times = convertTimesToCircular(times), bio = bio)
#     array.overview.fragment <- printArrayOverview(array.overview)
#     if (nrow(section.overview) > 3) 
#       survival.graph.size <- "width=90%" else survival.graph.size <- "height=4in"
#   }
  
  appendTo("Report", "M: Process finished successfuly.")
# # ---------------

# wrap up the txt report
  appendTo("Report", "\n-------------------")
  if (file.exists("temp_UD.txt")) 
    appendTo("Report", paste0("User inverventions:\n-------------------\n", gsub("\r", "", readr::read_file("temp_UD.txt")), "-------------------"))
  
  appendTo("Report", paste0("Function call:\n-------------------\n", the.function.call, "\n-------------------"))
# ------------------

# # print html report
#   if (report) {
#     appendTo("debug", "debug: Printing report")
#     rmarkdown::render(reportname <- printMigrationRmd(name.fragment = name.fragment, header.fragment = header.fragment, 
#         biometric.fragment = biometric.fragment, survival.graph.size = survival.graph.size, circular.plots = circular.plots,
#         individual.plots = individual.plots, spatial = spatial, efficiency.fragment = efficiency.fragment, array.overview.fragment = array.overview.fragment), quiet = TRUE)
#     appendTo("debug", "debug: Moving report")
#     fs::file_move(sub("Rmd", "html", reportname), sub("Report/", "", sub("Rmd", "html", reportname)))
#     appendTo("debug", "debug: Opening report if the pc has internet.")
#     openReport(file.name = sub("Report/", "", sub("Rmd", "html", reportname)))
#   }
  appendTo("Screen", "M: Process finished successfuly.")
# # ------------------

  appendTo("Screen", paste("M: Saving job log as '", paste(gsub(":", ".", sub(" ", ".", as.character(Sys.time()))), "actel.log.txt", sep = "."), "'.", sep = ""))
  file.rename("temp_log.txt", paste(gsub(":", ".", sub(" ", ".", as.character(Sys.time()))), "actel.log.txt", sep = "."))
  
  if (!debug)
    deleteHelpers()

  if (invalid.dist)
    return(list(detections = detections, spatial = spatial, deployments = deployments, arrays = arrays,
      movements = movements, simple.movements = simple.movements, section.movements = section.movements,
      status.df = status.df 
      # section.overview = section.overview, array.overview = array.overview,
      # matrices = matrices, overall.CJS = overall.CJS, 
      # intra.array.CJS = intra.array.CJS, times = times
  ))
  else
    return(list(detections = detections, spatial = spatial, deployments = deployments, arrays = arrays,
      movements = movements, simple.movements = simple.movements, section.movements = section.movements,
      status.df = status.df, 
      # section.overview = section.overview, array.overview = array.overview,
      # matrices = matrices, overall.CJS = overall.CJS, 
      # intra.array.CJS = intra.array.CJS, times = times, 
      dist.mat = dist.mat))
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
printResidencyRmd <- function(name.fragment, header.fragment, biometric.fragment, efficiency.fragment, array.overview.fragment,
  survival.graph.size, individual.plots, circular.plots, spatial, deployments){
  appendTo("Screen", "M: Producing final report.")
  if (file.exists(reportname <- paste("Report/actel_migration_report", name.fragment, ".Rmd", sep = ""))) {
    continue <- TRUE
    index <- 1
    while (continue) {
      if(file.exists(reportname <- paste("Report/actel_migration_report", name.fragment, ".", index, ".Rmd", sep = ""))) {
        index <- index + 1
      } else {
        continue <- FALSE
      }
    }
    appendTo("Screen",paste("M: An actel report is already present in the present directory, saving new report as 'actel_migration_report", name.fragment, ".", index, ".html'.", sep = ""))
    rm(continue,index)
  } else {
    appendTo("Screen",paste("M: Saving actel report as 'actel_migration_report", name.fragment, ".html'.", sep = ""))
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

', header.fragment,' 

Number of listed receivers: **', stringr::str_extract(pattern = '(?<=Number of ALS: )[0-9]*', string = report), '** (of which **', stringr::str_extract(pattern = '(?<=of which )[0-9]*', string = report), '** had no detections)

', unknown.fragment,'

Data time range: ', stringr::str_extract(pattern = '(?<=Data time range: )[^\r|^\n]*', string = report), '

Found a bug? [**Report it here.**](https://github.com/hugomflavio/actel/issues)

### Study area

Arrays with the same background belong to the same section. Release sites are marked with "R.S."

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

Zoom in or open the figure in a new tab to clearly read the text within each circle.

Note:
  : The progression calculations **do not account for** intra-section backwards movements. This implies that the total number of fish to have been **last seen** at a given array **may be lower** than the displayed below. Please refer to the [section survival overview](#survival) to find out how many fish were considered to have disappeared per section.

<img src="mb_efficiency.svg" alt="Missing file" style="padding-top: 15px; padding-bottom: 15px;"/>

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
  <a href="#full-log">Full log</a>
  <a href="#individual-plots">Individuals</a>
</div>
', fill = TRUE)
sink()
return(reportname)
}




