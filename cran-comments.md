## Comments:

* On the checks for actel 1.4.0, a note is issued regarding 
  svglite being an unused dependency. While actel does not require svglite 
  directly, it is a necessary "suggested" package of ggplot2, and I want 
  to ensure that it is installed.
* Updated the maintainer email from my old institutional email to my new one.

## Test environments

* Windows Server 2022: R 4.4.3, 4.5.2 and devel
* Ubuntu 24.04.3     : R 4.4.3, 4.5.2 and devel
* Mac OS 15.7.4      : R 4.5.2

All tests conducted through GitHub actions.

## R CMD check results

On release (R 4.5.2) and devel environments:

0 errors | 0 warnings | 1 note

> checking dependencies in R code ... NOTE
Namespace in Imports field not imported from: 'svglite'

On old-release environments (R 4.4.3):

0 errors | 0 warnings | 2 notes

> checking package dependencies ... NOTE
Packages suggested but not available for checking:
  'gWidgets2', 'gWidgets2tcltk', 'sf'

> checking dependencies in R code ... NOTE
Namespace in Imports field not imported from: 'svglite'

See explanation for svglite notes at the top.
