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
  
  my.home <- getwd()
  
  if (!debug)
    on.exit(deleteHelpers(), add = TRUE)
  
  on.exit(setwd(my.home), add = TRUE)
  on.exit(tryCatch(sink(), warning = function(w) {hide <- NA}), add = TRUE)
  
  if (!debug)
    on.exit(deleteHelpers(), add = TRUE)
  
  speed.method <- match.arg(speed.method)
  
  deleteHelpers()
  
  appendTo("Report", "Acoustic telemetry data analysis report.\n") 
  
  path <- pathCheck(my.home = my.home, path = path)
  
  appendTo(c("Report"), paste("Timestamp:", Sys.time(), "\n\nM: Selected folder: ", getwd(), "\nM: Success has been defined as last detection in: ", paste(success.arrays, collapse = ", "), 
    ".", sep = ""))
  appendTo(c("Screen"), "M: Moving to selected work directory")
  
  report <- folderCheck(report = report, redraw = redraw)
 
  appendTo(c("Screen", "Report"), "M: Importing data. This process may take a while.")
  
  bio <- loadBio()

  spatial <- assembleSpatial(bio = bio, sections = sections)
  appendTo("Report", paste("Number of target tags: ", nrow(bio), ".", sep = ""))
  
  recompile <- TRUE
  detection.paths <- c(file.exists("actel.detections.RData"), file.exists("detections/actel.detections.RData"))
  if (any(detection.paths)) {
    if(detection.paths[2]) load("detections/actel.detections.RData") else load("actel.detections.RData")
    appendTo("Screen", paste("M: The detections have been processed on ", actel.detections$timestamp, ".\n   If the input detection files were not changed, it is safe to use these again.", sep = ""))
    decision <- readline("   Reuse processed detections?(Y/n) ")
    appendTo("UD",decision)
    if (decision != "N" & decision != "n"){
      appendTo(c("Screen","Report"), paste("M: Using detections previously compiled on ", actel.detections$timestamp, ".", sep = ""))
      detections <- standardizeStations(input = actel.detections$detections, spatial = spatial)
      detections <- convertTimes (input = detections, start.timestamp = start.timestamp, end.timestamp = end.timestamp, tz.study.area = tz.study.area)
      appendTo(c("Screen","Report"), paste("Data time range: ", as.character(head(detections$Timestamp, 1)), " to ", as.character(tail(detections$Timestamp, 1)), " (", tz.study.area, ").", sep = ""))
      recompile <- FALSE
    } else {
      appendTo("Screen", "M: Reprocessing the detections.")
    }
    rm(actel.detections)
  }

  if (recompile)
    detections <- loadDetections(path = path, spatial = spatial, 
      start.timestamp = start.timestamp, end.timestamp = end.timestamp, tz.study.area = tz.study.area)
  
  unknownReceiversCheckA(spatial = spatial, detections = detections)
  
  emptyReceiversCheck(spatial = spatial, detections = detections)
  

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
  
  if (debug)
    save(path, sections, success.arrays, minimum.detections, maximum.time, speed.method, if.last.skip.section, 
    tz.study.area, start.timestamp, end.timestamp, report, redraw, override, exclude.tags, debug, 
    cautious.assignment, bio, spatial, detections, dist.mat, invalid.dist, replicate, file="debug_start.RData")

  recipient <- splitDetections(detections = detections, bio = bio, spatial = spatial, exclude.tags = exclude.tags)
  detections.list <- recipient[[1]]
  bio <- recipient[[2]]
  rm(recipient)
  if (debug)
    save(detections.list, file = "debug_detections.list.RData")

  recipient <- unknownReceiversCheckB(detections.list = detections.list, spatial = spatial)
  spatial <- recipient[[1]]
  if (recipient[[2]]) {
    detections <- standardizeStations(input = detections, spatial = spatial)
    recipient <- splitDetections(detections = detections, bio = bio, spatial = spatial, exclude.tags = exclude.tags, silent = TRUE)
    detections.list <- recipient[[1]]
    bio <- recipient[[2]]
  }
  rm(recipient)
  if (debug)
    save(detections.list, file = "debug_detections.list.RData")

  appendTo(c("Screen", "Report"), "M: Data successfully imported!\nM: Creating movement records for the valid tags.")
  movements <- groupMovements(detections.list = detections.list, bio = bio, spatial = spatial,
    speed.method = speed.method, maximum.time = maximum.time, tz.study.area = tz.study.area, dist.mat = dist.mat, invalid.dist = invalid.dist)
  if (debug)
    save(movements, file = "debug_movements.RData")
  
  timetable <- assembleTimetable(movements = movements, sections = sections, spatial = spatial, 
    minimum.detections = minimum.detections, dist.mat = dist.mat, invalid.dist = invalid.dist, 
    speed.method = speed.method, if.last.skip.section = if.last.skip.section, success.arrays = success.arrays, 
    override = override, cautious.assignment = cautious.assignment)
  appendTo(c("Screen", "Report"), "M: Timetable successfully filled. Fitting in the remaining variables.")
  if (debug)
    save(timetable, file = "debug_timetable.RData")
  
  status.df <- assembleOutput(timetable = timetable, bio = bio, movements = movements, spatial = spatial, 
    sections = sections, dist.mat = dist.mat, invalid.dist = invalid.dist, tz.study.area = tz.study.area)
  if (debug)
    save(status.df, file = "debug_status.df.RData")
  
  for(fish in names(movements)){
    movements[[fish]] <- speedReleaseToFirst(fish = fish, status.df = status.df, movements = movements[[fish]],
     dist.mat = dist.mat, invalid.dist = invalid.dist, silent = FALSE)
  }
  if (debug)
    save(movements, file = "debug_movements.RData")

  simple.movements <- simplifyMovements(movements = movements, status.df = status.df, sections = sections)
  if (debug)
    save(simple.movements, file = "debug_simple.movements.RData")
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
  if (debug)
    save(the.matrices, last.array.results, overall.CJS, split.CJS, group.CJS, file = "debug_CJS.RData")

  array.overview <- assembleArrayOverview(group.CJS = group.CJS)
  section.overview <- assembleSectionOverview(status.df = status.df, sections = sections)

  times <- getTimes(simple.movements = simple.movements, spatial = spatial, 
    tz.study.area = tz.study.area, type = "Arrival")

  if (debug)
    save(array.overview, section.overview, times, file = "debug_summary.RData")
  
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
    rm(continue,index)
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
    individual.plots <- printIndividuals(redraw = redraw, detections.list = detections.list, 
        status.df = status.df, tz.study.area = tz.study.area, movements = movements, simple.movements = simple.movements)
    circular.plots <- printCircular(times = convertTimesToCircular(times), status.df = status.df)
    array.overview.fragment <- printArrayOverview(array.overview)
    if (nrow(section.overview) > 3) 
      survival.graph.size <- "width=90%" else survival.graph.size <- "height=4in"
  }
  
  appendTo("Report", "M: Process finished successfuly.")
  
  appendTo("Report", "\n-------------------")
  if (file.exists("temp_UD.txt")) 
    appendTo("Report", paste("User inverventions:\n-------------------\n", gsub("\r", "", readr::read_file("temp_UD.txt")), "-------------------", sep = ""))
  
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
      matrices = the.matrices, efficiency = overall.CJS, arrival.times = times, 
      last.array.results = last.array.results, spatial = spatial))
  else
    return(list(detections = detections, movements = movements, simple.movements = simple.movements,
      status.df = status.df, section.overview = section.overview, array.overview = array.overview,
      matrices = the.matrices, efficiency = overall.CJS, arrival.times = times, 
      last.array.results = last.array.results, spatial = spatial, dist.mat = dist.mat))
}
