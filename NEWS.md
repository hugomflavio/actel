# News<img src="vignettes/actel_logo.png" align="right" width="120" />

Find out the main highlights of each update.

## actel 1.3.0

Fixes:
  * Fix `printLastSection()` crash for very large datasets.
  * Fix `printCircular()` crash if there are six or more groups but only one is present at the array.
  * Fix actel missing some efficiency peers as per issue [#72](https://github.com/hugomflavio/actel/issues/72).
  * Fix comments not being appended to status.df in the `residency()` analysis.
  * Fix minor bug where comments could not be saved after interacting with the graphical interface.
  * Prevent crash in `migration()` if an array has 0 efficiency and there are no arrays coming before it.
  * Prevent crash if the dot file/string line breaks are coded as `\r\n`.
  * Prevent crash while printing circular plots for the reports on Macs where the svg engines are not working.
  * Prevent crash in `residency()` if one group has no valid detections at all.
  * Fix bug in `plotRatios()` brought up in issue [#77](https://github.com/hugomflavio/actel/issues/77) which would occur when the user specified a section for which not all groups were detected.
  * Prevent crashes in `residency()` when the data has an unusually high percentage of exact-time detections (e.g. midnight).
  * Prevent crash if only some tags have multiple sensors and Sensor.unit is only included for those tags.
  * Prevent crash if the study area/population is so large that it clashes with ggplot's size limit failsafes.
  * Fix message overflow reported in issue [#78](https://github.com/hugomflavio/actel/issues/78).

Changes:
  * Changed the inner mechanics of the graphical widgets from RGtk2 to tcltk, as RGtk2 is now gone from CRAN. Should not affect user experience.
  * Renamed `createWorkspace()` to `blankWorkspace()` following cases of confusion regarding function purpose. Closes [#80](https://github.com/hugomflavio/actel/issues/80)
  * Removed dependency from package RGDAL; updated distance calculations code accordingly.
  * Moved package vignettes to https://hugomflavio.github.io/actel-website/index.html.
  * Removed deprecated argument `plot.detections.by`.
  * `minimum.detections` has been replaced by `min.total.detections` and `min.per.event`.
  * `section.minimum` has been replaced by `section.warning` and `section.error`.

Enhancements:
  * Improve handling of manually set `section.warning` and `section.error` arguments when the set values don't respect the `section.warning >= section.error` rule.
  * Implement area-based scales for circular plots. To obtain reports with the legacy linear circular scale, users can still run `options(actel.circular.scale = "linear")` before running analyses. `plotTimes()` received a new argument `circular.scale`.
  * Improve handling of manually set `jump.warning` and `jump.error` arguments when the set values don't respect the `jump.warning <= jump.error` rule.
  * Change the behaviour of `recoverLog()` so it attempts to save the log to `actel_job_log.txt` if a `file` argument is not provided.
  * Added new `force` argument to `blankWorkspace()` and `exampleWorkspace()`.
  * New `Code.space` column in the biometrics allows the user to specify the code space of the target tags.
  * New arguments in `plotArray()`: `by.group` and `y.style`. See function documentation for more details.
  * Include `preload()` log in reports where preloaded data is used.
  * `plotRatios()` can now subset multiple groups and sections simultaneously. The user can also decide to either colour the plot by group or by section using the new `col.by` argument (issue [#77](https://github.com/hugomflavio/actel/issues/77))).

## actel 1.2.1

Fixes:
  * Prevent crash in residency if the overall data period starts in winter time but the data for a given fish starts in summer time.
  * Prevent crash in residency caused by missing "save.tables.locally" argument.
  * Fix bug related to wrong type of detections object in preload().
  * Fix bad formatting of the summary section in reports produced in Linux OS's.

Changes:
  * The argument `plot.detections.by` in functions explore(), migration() and residency() has been renamed to `detections.y.axis`.
  * The argument `type` in the function plotDetections() has been renamed to `y.axis`.

Enhancements:
  * Verify that deployments do not end before they start.
  * Improve verification of station and array names, to avoid troublesome characters slipping through and potentially crashing the analyses.
  * The function plotDetections() has been expanded with the following arguments: `section.alias`, `x.label.format`, `only.valid` and `like.migration`.


## actel 1.2.0

Fixes:
  * Prevent warning during printing of the progression flowchart, related to overlapping array names (that could also lead to an actual error).
  * Fix undesired behaviour in unknown receiver mechanisms and improve unknown handling.
  * Prevent crash in array efficiency estimation if release sites have spaces.
  * Prevent wrong output in array efficiency estimations if array names are contained within each other.
  * Prevent missing arrays when finding array paths due to overlapping array names.
  * Prevent crash in distancesMatrix if spatial contains a "longitude" or "latitude" column.
  * **Fix silent bug that could cause array efficiency estimates to be misplaced in the output.**
  * Prevent crash during circular plot saving caused by illegal characters in the file name.

Enhancements:
  * New functionality: expand events and edit the validity of individual detections.
  * Added Section information to the detections and the movement tables.
  * Automatically convert "-" to NA in the Data column of Thelma log files.
  * Automatically rename Y axis in plotDetections to "Array" if type = "arrays".
  * Added possibility to save orphan detection files for later inspection.
  * Include detections source file in a new "Source.file" column.
  * New argument 'save.tables.locally' allows the user to save temporary tables in the current working directory. Particularly useful for Mac users that cannot activate the graphical user interface.
  * Improve job log recovery method so it is not as intrusive. Including new function recoverLog().
  * Only display GUI windows once they are ready.
  * Check for impassable jumps between release and first event too.
  * Array live times are now calculated and included in the 'arrays' object.
  * The 'deployments' object has been extended and now includes array and standard station name.
  * actel can now dynamically check for array live times when checking jump distances, to avoid triggering warnings and errors simply because some arrays were not active.
  * New `plotLive()` function allows the user to plot the periods during which each array was active. The option 'show.stations' can be used to plot each individual station live time as well.
  * Group warnings if more than four of the same type of warnings shows up for any given tag.
  * The argument `plot.detections.by` now defaults to 'auto', so actel can try to optimize space use in the detection plots.
  * Included citation information for the recently accepted paper describing actel.

## actel 1.1.1

Fixes:
  * Prevent crash if data used as input for `preload()` contains factors.
  * Prevent crash during individual plot printing if number of stations is higher than 29.
  * Prevent bad structuring of the legend in individual plots if `plot.detections.by = 'arrays'`.
  * Prevent crash of CJS functions when running tags with multiple sensors.
  * Prevent CJS crash due to overlapping array names.
  * Prevent crash if spatial has release sites, but biometrics doesn't (in CJS calculations of migration).
  * Fixed a bug where the sensor data would get scrambled when reading standard format detection files.
  * Fixed visual bug where the progress bar for sensor plots would not reach 100%.

Changes:
  * **`plotMoves()` has been renamed to `plotDetections()`**
  * In the residency analysis, the following output **objects have changed name**:
    * `daily.ratios` is now `time.ratios`
    * `daily.positions` is now `time.positions`
  * In the residency analysis report, the section 'Section progression' has been renamed to 'Global residency'.

Enhancements:
  * Never show unknown events during movement table display.
  * Show how many invalid events were omitted in the message that opens movement events.
  * Expand unknown receiver options to allow discarding unknown detections and repeating the same action for all occurrences.
  * Improve error messages when something fails during detections loading.
  * Improve detection file recognition mechanics.
  * New function `plotSensors()` can be used to plot the sensor data for each tag.
  * Sensor plots are now painted by array or section (matching the detection plots).
  * New function `plotMoves()` can be used to plot the movement evolution of multiple tags simultaneously.
  * New function `plotArray()` can be used to plot the simultaneous number of fish present at a subset of arrays or,
  if `cumulative = TRUE`, the cumulative number of individual fish to have reached that subset of arrays per unit of time.
  * New argument in the `residency()` analysis: `timestep` allows the user to decide whether residency calculations should be done on a daily basis (default) or an hourly basis (at cost of computing time).
  * New fail-safe prevents unhandled function failure if arrays were accidentally assigned to more than one section.
  * Introduced fail-safes so report compiling does not fail when R is not being able to produce SVG graphics.
  * Improved cleanliness by moving temporary report files to a dedicated actel folder.
  * New function `plotResidency()` can be used to plot the individual residency for each fish.
  * The column order in global.ratios now follows the section order.
  * New output in the residency analysis: group.ratios (similar to global.ratios, but per group).
  * New function `plotRatios()` can be used to plot the global and group residency.
  * Improve handling of very long legends in the global residency plots.
  * Display times as hh:mm:ss in the movement tables to avoid ambiguity in interpretation.
  * Colour the SEM ranges in the circular plots (matching the respective groups). The level of darkening can be controlled with the new `mean.range.darken.factor` argument in `plotTimes()`.

## actel 1.1.0

Fixes:
  * Prevent crash if all detections for a given stray tag were removed during detection quality checks.
  * Prevent migration crash if all movement events are rendered invalid before section movements are created.
  * Prevent crash related with circular plotting when running an analysis on more than eight fish groups.
  * Prevent bug in older R versions where actel did not display all table rows, for long tables displayed in the console.
  * **Fixed 'last to last' speed calculations**. A bug was found where this speed method was accidentally matching first stations for distance calculation, rather than last stations.
  * Fix bug where valid.movements would retain the invalid speed values if the fish no longer moved between arrays.
  * Fix bug where the wrong tags would be stored in stray_tags.csv.
  * Fig bug where stray tags from previous runs would leak into stray_tags.csv.

Changes
  * `stripCodeSpaces()` has been replaced with `extractSignals()`.
  * **Sections are now set up using a 'Section' column in the spatial.csv file**. The 'sections' argument has been deprecated. See the updated vignettes for details.
  * **Arrays can now be named freely** (short names are still recommended).
  * Perform quality checks on the 'replicates' argument earlier in the analyses.
  * User decisions no longer have default options.
  * Users must now list **only** the tag signals in the 'override' argument.

Enhancements:
  * The shapefiles of the study areas can now be water or land polygons. Use the new argument "type" in `loadShape()` to switch from land shapefiles (the default) to water shapefiles.
  * Paint groups consistently in circular plots.
  * Improved handling of long array lists in individual detection plots.
  * Display event number on inactiveness checks.
  * New argument `discard.first` allows the user to discard detections that happen before a given amount of time has passed after release.
  * New function `extractCodeSpaces()` has been implemented.
  * New `preload()` function allows advanced users to run actel without requiring input files. See more in the new dedicated vignette.
  * New argument `section.order` allows the user to specify the order by which the sections should be listed.
  * `plotTimes()` has been expanded with new arguments and better compatibility for multiple groups.
  * New argument `plot.detections.by` allows the user to plot the detections by array, rather than by station. Applied both to the analyses and to `plotMoves()`.
  * User decisions can now be followed by a in-line comment (started with "#"). Additionally, actel automatically adds default comments to decisions so these are easier to read through in the job log.
  * Message that opens movement events now includes the respective tag and the type of movements displayed (section or array movements).
  * Include user comments in the txt job logs.
  * In case of error, the user can now save a copy of the log up to the point where the function crashed.
  * Verify number of columns on all detection files.
  * Run quality checks on standard detection files.
  * New `getSpeeds()` function allows the user to quickly extract all speed information from the analyses.
  
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
  * The possible values of speed.method have been changed to 'last to first' and 'last to last', for a more logical interpretation of the results. The explore vignette has been updated accordingly.
  * Separated part of `transitionLayer` into a preceding function `loadShape`.
  * `exampleWorkspace` and `createWorkspace` now require a target directory.

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
  * Users can now create and use a generic detections file. Details for the format of this file are provided in the "Detections" section of vignette 1.0.
  * migration() has been upgraded to be capable of handling backwards movements. The status.df has also been improved, and a new graphic was included to the report (last array).
  * new `plotMoves` function allows the user to create personalised detection plots for specific tags.
  * EPSGcode argument has been removed from all distances matrix related functions. actel now looks for the coordinate system within the shape file supplied.
  * distances matrix functions can now work with degree-based coordinate systems too.


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
  * distancesMatrix will now warn the user if some stations are completely cut off from the rest (i.e. there is no water path).
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
