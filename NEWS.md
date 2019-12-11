# News<img src="vignettes/actel_logo.png" align="right" width="120" />

Find out the main highlights of each update.

## development

Fixes:
  * Fix mechanisms that deal with unknown receivers
  
Changes:
  * 'maximum.time' has been renamed to 'max.interval'
  * 'tz.study.area' has been renamed to 'tz'
  * 'start.timestamp' has been renamed to 'start.time'
  * 'end.timestamp' has been renamed to 'stop.time'
  * After a transition period, actel() has been removed

Note: Newly deprecated arguments will continue working during a transition period

Enhancements:
  * 'tz' is now checked against the output of OlsonNames() to ensure a valid timezone is entered.
  * messages and warnings are now printed using message() and warning()


## actel 0.1.1

Fixes:
  * Make checkInactiveness start counting time in the right event.
  * Remove spaces in bio's release sites before matching with stations' standard names.

Enhancements:
  * explore(), migration() and residency() now output auxiliary information for the upcoming RSP functions.
