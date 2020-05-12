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

* The function appendTo() now writes to R's temporary folder.

* The user is now asked for confirmation before the results
are saved as an .RData file and the analysis log is saved as a
.txt file.

* A new argument 'save.detections' (defaults to FALSE) ensures
that compiled detections are only saved for reuse by explicit
command of the user.

* the functions transitionLayer and distancesMatrix were adapted
so that they do not write into the file system by default, but rather
return the created objects into the R session. distancesMatrix still
offers the possibility to write the output to 'distances.csv', but only
through explicit user consent.

* the functions createMatrix and completeMatrix were adapted so that
they do not write into the file system. It is now up to the user to
save the output with the correct name (this is explained in the vignettes).

* The functions exampleWorkspace, createWorkspace are explicitly 
intended to write supporting files. I have expanded the functions' 
documentation to make it clear that files will be written and
included a request for additional confirmation from the user when 
files/folders with the same name already exist.

* print(x) calls were wrapped to:

message(paste0(capture.output(print(x)), collapse = "\n"))

To allow the display of tables without preventing message
suppression if requested. An exception was made in 1) the function
clearWorkspace, as this function always requires user intervention,
and using print allows for the multiple file names to flow
orderly into the console dimensions, and 2) the function 
advEfficiency, where the print call is intended to trigger the
plotting of a graphic.

* All automated tests that involve writing/reading files are
now performed inside R's temporary directory. This change as also
been applied for tests that do not run on CRAN.

* All installed.packages() calls were removed and the respective
functions were addapted to work with packageDescription()


## Package resubmission (1st revision)

This is a resubmission. As requested by Uwe Ligges, 
in this version I have:

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
