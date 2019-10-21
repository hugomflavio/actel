#' @importFrom circular sd.circular mean.circular var.circular
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
#' @param replicate A vector of station standard names to use as a replicate of the last array, for efficiency estimations.
#' 
#' @return A list containing 1) the detections used during the analysis, 2) the movement events, 3) the status dataframe, 4) the survival overview per group, 5) the progression through the study area, 6) the ALS array/sections' efficiency, 7) the list of spatial objects used during the analysis.
#' 
#' @export
#' 
actel <- function(path = NULL, sections = NULL, success.arrays = NULL, minimum.detections = 2, 
    maximum.time = 60, speed.method = c("last to first", "first to first"), 
    if.last.skip.section = TRUE, tz.study.area, start.timestamp = NULL, 
    end.timestamp = NULL, report = TRUE, redraw = TRUE, override = NULL, 
    exclude.tags = NULL, debug = FALSE, cautious.assignment = TRUE, replicate = NULL) {
  
  cat(paste0(
"Error: The function 'actel()' is no longer operational. Please switch to the functions 'explore()' or 'migration()'.
  The new functions require a 'deployments.csv' file. If you were previously running 'actel()' in your current directory,
  convert your study data automatically to the new format by running: 

  updateStudy(tz.study.area = '", tz.study.area, "')\n"))
}