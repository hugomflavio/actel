#' Residency analysis
#' 
#' The actel package provides a systematic way of analysing fish residency data.
#' residency() collects the input present in the target folder and analyses the telemetry data, extracting residency-related metrics.
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
  the.function.call <- paste0("residency(path = ", ifelse(is.null(path), "NULL", paste0("'", path, "'")), 
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
study.data <- loadStudyData(tz.study.area = tz.study.area, override = override, 
                            start.timestamp = start.timestamp, end.timestamp = end.timestamp,
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

  movements <- checkImpassables(movements = movements, dotmat = dotmat)

  movements <- checkJumpDistance(movements = movements, bio = bio, dotmat = dotmat, 
                                 spatial = spatial, jump.warning = jump.warning, jump.error = jump.error)

  valid.movements <- simplifyMovements(movements = movements, bio = bio, 
    speed.method = speed.method, dist.mat = dist.mat, invalid.dist = invalid.dist)

  # Residency-exclusive area

  # Compress array movements into section movements
  section.movements <- sectionMovements(movements = valid.movements, sections = sections, invalid.dist = invalid.dist)
  # Look for isolated section movements
  section.movements <- checkSMovesN(secmoves = section.movements)
  # Update array movements based on secmove validity
  movements <- updateAMValidity(arrmoves = movements, secmoves = section.movements)

  # Resimplify array moves and calculate simple section moves
  valid.movements <- simplifyMovements(movements = movements, bio = bio, 
    speed.method = speed.method, dist.mat = dist.mat, invalid.dist = invalid.dist)
  valid.section.movements <- sectionMovements(movements = valid.movements, sections = sections, invalid.dist = invalid.dist)

  valid.detections <- validateDetections(detections.list = detections.list, movements = valid.movements)

  # Grab summary information
  res.df <- assembleResidency(secmoves = valid.section.movements, movements = valid.movements, sections = sections)
  appendTo(c("Screen", "Report"), "M: Timetable successfully filled. Fitting in the remaining variables.")
  status.df <- res_assembleOutput(res.df = res.df, bio = bio, spatial = spatial, 
                                  sections = sections, tz.study.area = tz.study.area)
# ---------------
  
# Efficiency
  efficiency <- res_efficiency(arrmoves = valid.movements, bio = bio, spatial = spatial, arrays = arrays, paths = paths, dotmat = dotmat)
# ----------

  
  appendTo("Report", "M: Process finished successfully.")
# ---------------

# wrap up in-R objects
  # if (!is.null(override)) {
  #   header.fragment <- paste('<span style="color:red">Manual mode has been triggered for **', length(override),'** fish.</span>\n', sep = "")
  #   name.fragment <- "_corrected"
  # } else {
  #   header.fragment <- name.fragment <- ""
  # }

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
    appendTo("Screen", paste("M: An actel residency results file is already present in the present directory, saving new results as '", resultsname,"'.", sep = ""))
    rm(continue, index)
  } else {
    appendTo(c("Screen", "Report"), paste("M: Saving results to '", resultsname, "'.", sep = ""))
  }

  detections <- detections.list
  deployments <- do.call(rbind.data.frame, deployments)
  if (invalid.dist)
    save(detections, valid.detections, spatial, deployments, arrays, movements, valid.movements, 
      section.movements, valid.section.movements, status.df, file = resultsname)
  else
    save(detections, valid.detections, spatial, deployments, arrays, movements, valid.movements, 
      section.movements, valid.section.movements, status.df, dist.mat, file = resultsname)
# ------------

# Print graphics
  if (report) {
    biometric.fragment <- printBiometrics(bio = bio)
    # efficiency.fragment <- mbPrintEfficiency(overall.CJS = overall.CJS, intra.CJS = intra.array.CJS)
    # printDotplots(status.df = status.df, invalid.dist = invalid.dist)
    # printSurvivalGraphic(section.overview = section.overview)
    printDot(dot = dot, sections = sections, spatial = spatial)
    # mbPrintProgression(dot = dot,  sections = sections, overall.CJS = overall.CJS, spatial = spatial, status.df = status.df)
    individual.plots <- printIndividuals(redraw = TRUE, detections.list = detections.list, bio = bio, 
        tz.study.area = tz.study.area, movements = movements, simple.movements = valid.movements)
    # circular.plots <- printCircular(times = convertTimesToCircular(times), bio = bio)
    # array.overview.fragment <- printArrayOverview(array.overview)
    # if (nrow(section.overview) > 3) 
      # survival.graph.size <- "width=90%" else survival.graph.size <- "height=4in"
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
    rmarkdown::render(reportname <- printResidencyRmd(biometric.fragment = biometric.fragment,
        individual.plots = individual.plots, spatial = spatial, detections = detections, valid.detections = valid.detections), quiet = TRUE)
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
      status.df = status.df, efficiency = efficiency))
  else
    return(list(detections = detections, valid.detections = valid.detections, spatial = spatial, deployments = deployments, arrays = arrays,
      movements = movements, valid.movements = valid.movements, section.movements = section.movements, valid.section.movements = valid.section.movements,
      status.df = status.df, efficiency = efficiency, dist.mat = dist.mat))
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
printResidencyRmd <- function(biometric.fragment, individual.plots, spatial, deployments, detections, valid.detections){
  appendTo("Screen", "M: Producing final report.")
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
    appendTo("Screen",paste("M: An actel report is already present in the present directory, saving new report as 'actel_residency_report.", index, ".html'.", sep = ""))
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

Percentage of valid detections: ', round(sum(unlist(lapply(valid.detections, nrow))) / sum(unlist(lapply(detections, nrow))) * 100, 2), '%

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
  <a href="#user-comments">Comments</a>
  <a href="#biometric-graphics">Biometrics</a>
  <a href="#full-log">Full log</a>
  <a href="#individual-plots">Individuals</a>
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
  output <- lapply(names(secmoves), 
    function(i) {
      if (any(!secmoves[[i]]$Valid)) {
        aux <- secmoves[[i]][(!Valid)]
        to.change <- unlist(lapply(1:nrow(aux),
          function(j) {
            A <- which(arrmoves[[i]]$First.time == aux$First.time)
            B <- A + (aux$Events - 1)
            return(A:B)
          }))
        appendTo(c("Screen", "Report"), paste0("M: Rendering ", length(to.change), " array movements invalid for fish ", i ," as the respective section movements were discarded by the user."))
        arrmoves[[i]]$Valid[to.change] <- FALSE
      }
      return(arrmoves[[i]])
    })
  names(output) <- names(secmoves)
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
    recipient <- c(recipient, paste(c("Total.time", "Times.entered", "Average.entry", "Average.time", "Average.departure"), sections[i], sep = "."))
  }
  recipient <- c(recipient, "Very.last.array", "Very.last.time", "Status", "Valid.detections", "Invalid.detections", "Valid.events", "Invalid.events", "P.type")

  res.df <- matrix(nrow = length(secmoves), ncol = length(recipient))
  res.df <- as.data.frame(res.df, stringsAsFactors = FALSE)
  
  colnames(res.df) <- recipient
  rm(recipient)
  rownames(res.df) <- names(secmoves)
	  
  for (fish in names(secmoves)) {
  	aux <- split(secmoves[[fish]], secmoves[[fish]]$Section)
  	recipient <- lapply(seq_along(aux), function(i) {
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
  	res.df[fish, ] <- recipient
  	rm(recipient)
  }
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
  			units(res.df[, paste0(the.col, section)]) <- "minutes"
  		res.df[, paste0(the.col, section)] <- round(res.df[, paste0(the.col, section)], 2)
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
res_assembleOutput <- function(res.df, bio, spatial, sections, tz.study.area) {
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
  status.df$Release.date <- as.POSIXct(status.df$Release.date, tz = tz.study.area)
  status.df$Very.last.time <- as.POSIXct(status.df$Very.last.time, tz = tz.study.area)
  
  if (file.exists("temp_comments.txt")) {
    temp <- read.table("temp_comments.txt", header = FALSE, sep = "\t")
    status.df[, "Script.comments"] <- NA_character_
    for (i in seq_len(nrow(temp))) {
      link <- match(temp[i, 1], status.df$Transmitter)
      if (is.na(status.df$Script.comments[link])) {
        status.df$Script.comments[link] <- paste(temp[i, 2])
      } else {
        status.df$Script.comments[link] <- paste(status.df$Script.comments[link], temp[i, 2], sep = "// ")
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