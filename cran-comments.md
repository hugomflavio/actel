## Comments:

* On the checks for actel 1.3.0, a note is issued regarding 
  svglite being an unused dependency. While actel does not require svglite 
  directly, it is a necessary "suggested" package of ggplot2, and I want 
  to ensure that it is installed.


## Test environments

* Windows Server 2022: R 4.2.3, 4.3.1 and devel
* Ubuntu 22.04       : R 4.2.3, 4.3.1 and devel
* Mac OS 12.6.9      : R 4.3.1

All tests conducted through GitHub actions.


## R CMD check results

On all test environments:

0 errors | 0 warnings | 2 notes

> checking package dependencies ... NOTE
Packages suggested but not available for checking:
  'gWidgets2', 'gWidgets2tcltk', 'sf'

> checking dependencies in R code ... NOTE
Namespace in Imports field not imported from: 'svglite'

See explanation for second note at the top.
