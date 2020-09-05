## Comments:

* Fixed errors found on some CRAN checks (namely macOS-oldrel and windows-oldrel)

* Timestamp notes found on most environments below are apparently
  related to the fact that http://worldclockapi.com/ is currently
  down.

* The missing package for Mac OS builds is non-essential for actel
  and failsafes are in place to prevent function failure in its
  absense.

* On some of the CRAN checks run for actel 1.1.0, a note is issued regarding 
  svglite being an unused dependency. While actel does not require svglite 
  directly, it is a necessary "suggested" package of ggplot2, and I want 
  to ensure that it is installed.

## Test environments

* Windows Server 2019 (on GitHub), R 3.6.3, 4.0.2 and devel
* Mac OS (on GitHub), R 3.6.3, 4.0.2
* Ubuntu 16.04 (on GitHub), R 3.6.3, 4.0.2
* local Windows 10, R 4.0.2

## R CMD check results

On windows server R devel:

0 errors | 0 warnings | 0 notes

---

On R 3.6.3 and R 4.0.2 for Windows Server, Ubuntu and local Windows:

0 errors | 0 warnings | 1 note

> checking for future file timestamps ... NOTE
  unable to verify current time

---

On R 3.6.3 and R 4.0.2 for Mac OS:

0 errors | 0 warnings | 2 notes

> checking package dependencies ... NOTE
  Package suggested but not available for checking: ‘gWidgets2RGtk2’

> checking for future file timestamps ... NOTE
  unable to verify current time
