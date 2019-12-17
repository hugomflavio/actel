# News<img src="vignettes/actel_logo.png" align="right" width="120" />

Find out the main highlights of each update.

## development

Fixes:
  * Fix mechanisms that deal with unknown receivers
  * Edge arrays are now recognized if sections are provided
  * The columns "Backwards.movements" and "Backwards.movements" now display correct values
  
Changes:
  * 'maximum.time' has been renamed to 'max.interval'
  * 'tz.study.area' has been renamed to 'tz'
  * 'start.timestamp' has been renamed to 'start.time'
  * 'end.timestamp' has been renamed to 'stop.time'
  * After a transition period, actel() has been removed
  * 'cautious.assignment' has been removed from the migration analysis

Enhancements:
  * 'tz' is now checked against the output of OlsonNames() to ensure a valid timezone is entered.
  * messages and warnings are now printed using message() and warning()
  * Stop if any of the input files has duplicated column names
  * Warn user if success.arrays is not defined in migration()
  * 'minimum.detections' is now available in all functions.
  * 'override' is now available in all functions, and allows the user to directly invalidate specific events.
  * 'Detections' column in the object *status.df* has been split into 'Valid.detections' and 'All.detections'.

## actel 0.1.1

Fixes:
  * Make checkInactiveness start counting time in the right event.
  * Remove spaces in bio's release sites before matching with stations' standard names.

Enhancements:
  * explore(), migration() and residency() now output auxiliary information for the upcoming RSP functions.
