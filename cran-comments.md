## Comments:

* The missing package note for MacOS in R 3.6.3 corresponds to a non-essential 
  package for actel and failsafes are in place to prevent function failure 
  in its absense.

* On some of the CRAN checks run for actel 1.2.0, a note is issued regarding 
  svglite being an unused dependency. While actel does not require svglite 
  directly, it is a necessary "suggested" package of ggplot2, and I want 
  to ensure that it is installed.

## Test environments

* Windows Server 2019 (on GitHub), R 3.6.3, 4.0.3 and devel
* Mac OS (on GitHub), R 3.6.3 and 4.0.3
* Ubuntu 16.04 (on GitHub), R 3.6.3 and 4.0.3
* Local Windows 10, R 4.0.3


## R CMD check results

On Windows Server (R 3.6.3, 4.0.3 and devel), Mac OS (R 4.0.3),
Ubuntu (R 3.6.3 and 4.0.3) and local Windows 10 (R 4.0.3):

0 errors | 0 warnings | 0 notes

---

On Mac OS (R 3.6.3):

0 errors | 0 warnings | 1 note

> checking package dependencies ... NOTE
  Package suggested but not available for checking: ‘gWidgets2RGtk2’
  