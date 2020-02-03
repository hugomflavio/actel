# News<img src="vignettes/actel_logo.png" align="right" width="120" />

Find out the main highlights of each update.

## development

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

Changes:
  * `array.overview` is now called `group.overview`.

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
