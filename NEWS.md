# News<img src="vignettes/actel_logo.png" align="right" width="120" />

Find out the main highlights of each update.

## actel 1.0.0

Fixes:
  * Sort deployment rows by start time. Avoids false triggering of redeployment-before-retrieval errors.
  * Prevent failure to identify receivers if the model-serial combination has two or more "-" characters.
  * Prevent residence crash if no fish entered a given section.
  * Prevent crash if study area only has one array.
  * Fix replicates argument formulation in the log's function call carbon copy.
  * Prevent crash if array names contain regex characters.
  * Fix bug causing crash when attempting to save a movements table to a temporary file.

Changes:
  * maximum efficiency estimates in residency now only take into account known missed events and recorded events in the denominator (previously, potentially missed events were being included as well).
  * GUI interaction mechanisms were changed as a consequence of updating from package gWidgets to gWidgets2.
  * The argument 'path' has been removed from `explore`, `migration` and `residency`. The user must now move to the target directory using `setwd()` before running the analyses.
  * Analysis files are saved in a temporary folder and are only made permanent by user request.
  * The 'report' argument now defaults to FALSE. By setting 'report' to TRUE, the user acknowledges that a file will be written in the current working directory.
  * Auxiliary files to the report are now saved in a temporary directory. The user can still access the report figures by right-clicking them in the html report and saving them.
  * actel no longer saves compiled detections to the user's working directory by default. A new argument 'save.detections' (defaults to FALSE) was added to allow users to mimic the old behaviour.
  * actel no longer saves the stray tags summary to the user's working directory by default. If there are stray tags in the data, the users will be asked if they would like to save a copy of the summary.
  * transitionLayer no longer saves the output automatically into the working directory.
  * distancesMatrix now receives the transition layer as an R object rather than a file name.
  * distancesMatrix now requires user confirmation before writing a 'distances.csv' file into the working directory.
  * createMatrix and completeMatrix now work with R objects rather than files. The user must manually save the function output to a 'distances.csv' file to include it in the next analysis.

Enhancements:
  * Perform early quality checks on the content of 'sections' before advancing with migration and residency analysis.
  * Allow user to hide release sites from the study area diagrams.
  * Improve spatial.csv vs spatial.txt mismatch error messaging by showing missing arrays.
  * Improve colour and caption handling on individual residency plots for study areas with many sections.
  * Improve handling of spatial file in distancesMatrix (including new fail-safes).
  * Added support for tags that emit multiple signals (i.e. multi-sensor tags).
  * Plot sensor data in the reports.
  * Allow intra-array efficiency to be calculated even if no inter-array efficiency could be calculated.
  * Allow the GUI to pop up if the number of lines to display goes over getOptions("max.print").
  * Hide biometrics' section from the report if no biometric data was found.
  * New `advEfficiency` function allows user to calculate more robust efficiency estimates.
  * Improved release.overview and group.overview objects so they can be used as an input to advEfficiency().
  * New `plotTimes` function allows the user to create circular plots of specific subsets of the time data, with the ability to include night shades too.
  * `getTimes` and `timesToCircular` are now exported, allowing the user to quickly format the input for `plotTimes`.
  * A new page has been included in the vignettes aiming to explore what can be done with the results of actel's analyses.
  * Included usage examples on exported functions.
  * Verify the names of the release site arrays as soon as the spatial.csv is loaded.
  * Added support for timestamps using a "T" separator between date and time in the biometrics and deployments files.
  * new 'discard.orphans' argument allows the user to automatically discard all detections that do not fall within receiver deployment periods.

## actel 0.1.3

Fixes:
  * Force R to assume detection files are separated by commas (prevents occasional Thelma log crash).
  * Fix bug where fish whose data started in summer time and extended into winter time would cause one day to be lost in the daily positions during residency analysis.
  * Prevent arrays from having maximum estimated fish passing through greater than previous arrays (unless new fish are released).
  * Fix internal bug where efficiency matrices did not have "1" at Release for all fish (did not impact results).
  * Fix bug where arrays with no detections for a group\*release combination would cause skewed estimates.
  * Fix estimation of passed fish when using intra-array estimates.
  * Fix split CJS's not receiving information about intra-array estimates.
  * Avoid crash if a fish makes a U turn without being detected in all sections.
  * Avoid crash if group names have '.' characters in them.
  * Avoid crash if some group names are entirely contained within other groups.
  * Fix bug where 'disregard.parallels' was ironically disregarded and parallels always blocked peer assignment.
  * Prevent crash if the spatial.csv file cannot be updated due to being open elsewhere.
  * Fix bug where the presence of multiple instantaneous shifts in section would mess up the residency lists.
  * Fix bug preventing distancesMatrix from recognising the transition layer object.
  * Fix bug where the temporary detections object would be overwritten during inactiveness checks with a distance matrix present.
  * Fix bug causing speed calculations  to crash in the presence of unknown detections.
  * Fix bug where the Invalid.detections and Invalid.events columns in the status.df of residency would always return 0.
  * Prevent invalid detections from being considered during intra-array efficiency estimations.
  * Fix bug where fish with only one movement event would not count towards efficiency calculations.
  * Fix very specific bug where 8 shaped study areas would not have the bottleneck array accounted as a "known" failure in case a fish moved from one side of the constriction to the other without being detected.

Changes:
  * `array.overview` is now called `group.overview`.
  * Fish that never entered a section now have NA total and average times for that section during residency analysis. This allows to distinguish between fish that never entered a section (NA time) and fish that only had one detection at a section (0 time).

Enhancements:
  * The object `detections` now contains a "Valid" column, similarly to the `movements` objects.
  * Invalid detections are now painted in grey in the individual detection plots.
  * Check for duplicated detections and give the user a chance to clear them out before continuing.
  * Prevent unhandled crashes if the 'replicates' argument is badly structured.
  * Show number of released fish per release location in the release sites table.
  * Detailed progression per group and release site is now exported through a `release.overview` object.
  * Prevent migration from continuing if some arrays are not associated to a section.
  * Warn user if override is requested for a fish that is not detected.
  * Improve mechanics behind suggestions to linearise movements.
  * Movement tables can now be visualised in a new window with the argument GUI.
  * Improve handling of partially empty groups and release sites in the biometrics file.
  * Improve flexibility in handling shape files with dimensions incompatible with the requested pixel size.
  * Improve the distances matrix so it can cope with unspecified release sites.
  * transitionLayer can now take into account the station positions and expand the study area as needed.
  * transitionLayer now has a "buffer" argument, which can be used to expand the study area beyond the limits of the shape file.
  * distanceMatrix will now warn the user if some stations are completely cut off from the rest (i.e. there is no water path).
  * Reports are now opened using utils::browseURL, allowing the report to be open even when the computer is not connected to the internet.
  * Sensor values are now transported through the analysis and returned in the valid detections (for Vemco loggers only for now).
  * Ensure the legend in the residency graphics follows the order specified in the sections argument.
  * Improve Thelma .csv file handling for files exported with new column structure.
  * Allow for multiple expected first arrays (e.g. if fish are released in a lake whose exits are covered with receivers).

## actel 0.1.2

Fixes:
  * Fix mechanisms that deal with unknown receivers
  * Edge arrays are now recognized if sections are provided
  * The columns "Backwards.movements" and "Backwards.movements" now display correct values
  * Prevent crash if inactive.warning is set but inactive.error is left null (applicable for speed checks as well)
  * Prevent crash when calculating multi-way efficiency estimations for fish that jumped at release
  * Prevent crash during residency analysis if a fish had more than one detection but all at the exact same time
  * Prevent crash when loading empty detection files
  * Display correct event numbers and names when speed warnings are triggered

Changes:
  * 'maximum.time' has been renamed to 'max.interval'
  * 'tz.study.area' has been renamed to 'tz'
  * 'start.timestamp' has been renamed to 'start.time'
  * 'end.timestamp' has been renamed to 'stop.time'
  * After a transition period, `actel` function has been removed
  * 'cautious.assignment' has been removed from the migration analysis
  * Analysis run with 'override' are no longer saved with a "\_corrected" appendix

Enhancements:
  * 'tz' is now checked against the output of `OlsonNames` to ensure a valid time zone is entered.
  * messages and warnings are now printed using `message` and `warning`
  * Stop if any of the input files has duplicated column names
  * Warn user if success.arrays is not defined in `migration`
  * 'minimum.detections' is now available in all functions.
  * 'override' is now available in all functions, and allows the user to directly invalidate specific events.
  * 'Detections' column in the object *status.df* (migration analysis) has been split into 'Valid.detections' and 'Invalid.detections'.
  * Package-level documentation has been implemented (can be found using ?actel).
  * `explore`, `migration` and `residency` documentation has been expanded.
  * Force all individual residency plots to include all used locations in the caption, to ensure locations always show up with the same colour.
  * Stations are now grouped by array in the individual plots, following the array order provided either by the spatial.csv file or the spatial.txt file.
  * Improved links between the report plots and the R objects by naming the objects containing the data used in the plots.

## actel 0.1.1

Fixes:
  * Make checkInactiveness start counting time in the right event.
  * Remove spaces in bio's release sites before matching with stations' standard names.

Enhancements:
  * explore(), migration() and residency() now output auxiliary information for the upcoming RSP functions.
