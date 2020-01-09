# News<img src="vignettes/actel_logo.png" align="right" width="120" />

Find out the main highlights of each update.

## development

Fixes:
  * Fix mechanisms that deal with unknown receivers
  * Edge arrays are now recognized if sections are provided
  * The columns "Backwards.movements" and "Backwards.movements" now display correct values
  * Prevent crash if inactive.warning is set but inactive.error is left null (applicable for speed checks as well)
  * Prevent crash when calculating multi-way efficiency estimations for fish that jumped at release
  * Prevent crash during residency analysis if a fish had more than one detection but all at the exact same time
  * Prevent crash when loading empty detection files

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
