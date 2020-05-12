## Package resubmission (2nd revision)

This is a resubmission. As requested by Jelena Saf,
In this version I have:

* Included a reference to a supporting paper in the package
description

* Included "on.exit()" calls immediately after any changes
imposed on the options() or par(), to ensure these changes are
reverted when the function ends.

* removed the argument 'path' from explore(), migration() and
residency() to remove the need to rely on work directory changes.

* set the 'report' argument default to FALSE, so that explore()
migration() and residency() only write to the file system by
explicit user request (i.e. if the user sets 'report' to TRUE).


* Removed markup from the package description

* Removed the LICENSE file

* Corrected an invalid URL in the vignettes
('e-0_messages.html' to 'g-0_messages.html')

* Removed "This package" from the description start.

* Considerably lowered Overall checktime by disabling
most of the testing suites during CRAN checks only. 
All components are still being checked on GitHub to ensure
the functions are behaving as expected.

## Test environments

* Windows Server 2019 (on GitHub), R 4.0.0 and devel
* Mac OS (on GitHub), R 4.0.0
* Ubuntu 16.04 (on GitHub), R 4.0.0
* local Windows 10, R 4.0.0

## R CMD check results

On Windows server, MacOS and local Windows 10:

0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Hugo Flávio <hdmfla@aqua.dtu.dk>’

New submission

On Ubuntu 16.04:

0 errors | 0 warnings | 2 notes

* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Hugo Flávio <hdmfla@aqua.dtu.dk>’

New submission

* checking package dependencies ... NOTE
Package suggested but not available for checking: ‘rgdal’

## Comments

'rgdal' is not an essential package for actel, and measures 
were put in place to ensure that actel's functions stop orderly
if the 'rgdal' is missing.

actel must read/write files to operate. I understand that this
is a sensible topic, and the user must be informed about it. The
startup message of actel asks the users to run startNote(). This
is a message-only function that contains the following text:

```
Writing/editing files:
  To operate, actel must write/change files present in the target 
  directory and create subdirectories. This includes the functions 
  transitionLayer, distancesMatrix, emptyMatrix, createWorkspace, 
  exampleWorkspace, clearWorkspace, explore, migration and residency. 
  These actions are always related to the analysis processes being 
  carried on (e.g. deploy examples, write reports, print graphics). 

Opening the web browser:
  actel has an auto-open feature for generated reports, which will 
  trigger your browser to open at the end of the explore, migration 
  and residency functions. If you would like to disable this, please 
  run these functions with auto.open = FALSE. 

Please only use actel if you agree with this.

To get acquainted with how actel works, read the package vignettes.
You can find them by running browseVignettes('actel')
```

I have also deployed checkpoints to transitionLayer, distancesMatrix,
emptyMatrix, createWorkspace, exampleWorkspace, and clearWorkspace 
that require user confirmation when there is a risk of file overwriting
or deletion.
