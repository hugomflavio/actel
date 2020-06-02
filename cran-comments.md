## Package resubmission (4th revision)

This is a resubmission. As requested by Martina Schmirl,
in this version I have:

* Created an example.results object and deployed an example 
shapefile (in 'inst/example_shapefile') to remove all instances
of \dontrun{} in the examples. Some examples require moving to
tempdir() to avoid writing in the user's home directory, but 
that is always reversed at the end of the example.

* Fixed a missing comment sign in an example as part of the
restructuring done above.

* The reviewer made a coment regarding changing options(), working
directories or par(). All potentially troublesome instances of
this have been fixed in an earlier revision round, so I believe 
the reviewer was just reminding me to do the same in the examples, 
which I did.

* The reviewer commented regarding modifying the global 
environment (e.g. by using <<-) in the package's functions.
I have repeatedly used "<<-" within lapply loops, but these
only bring the respective variables higher within the function
being carried out, never bleeding out to the global environment.
The only instance where "<<-" is not used within a lapply loop
is in "btn_function", a child function of "graphicalInvalidate".
However, this btn_function only works within graphicalInvalidate
and, as such, the respective variable is only transported into the
master function, and never into the global environment.

I have throughly searched for "<<" instances in my code before
writing this reply, but if I have missed something, I would be
really grateful if you could point out the exception location
more specifically.

* In parallel with the revisions, I have merged a few updates,
The most relevant including a new internal function (processStandarFile)
and a new exported function (plotMoves). Additionally, a new
argument was added to explore(), residency() and migration().
These updates should not have any impact on the issues raised 
before. In particular, I have ensured that plotMoves contains
working examples, does not change options, and does not change
the user's global environment.


## Package resubmission (3rd revision)

This is a resubmission. As requested by Swetlana Herbrandt,
in this version I have:

* Replaced "(explore, migration and residency,  allow" with 
"(explore(), migration() and residency()) allow". Thanks for
pointing out that lost parenthesis.

* Replaced 'createWorkspace(dir = "my_new_folder")' with
'createWorkspace(dir = paste0(tempdir(), "/createWorkspace_example"))'
in the example for the function createWorkspace().

* Reviewed the function examples in an attempt to
replace instances of \dontrun{} with \donttest{}. However,
some functions either require the presence of specific files
in the current working directory, or are intended to deploy 
examples/template files. As such, it is my interpretation 
that switching to \donttest{} in these cases would go 
against the "Please ensure that your functions do not 
modify (save or delete) the user's home filespace in your 
examples" policy. I.e. if the user invoked the function
examples without analysing the code first, they could 
unwillingly trigger the deployment of files in their 
working directory. Please let me know if I am incorrect.

Of the 21 functions exported by actel, the following seven
_only_ contain "dontrun" examples:

distancesMatrix(): Requires the output of transitionLayer to
be able to run. Since transtitionLayer() cannot operate without
a shape file (see below), the examples in distancesMatrix() 
would lead to an error.

exampleWorkspace(): This function is intended to deploy files
in the user file system.

explore(), migration() and residency(): These functions 
require the presence of specific files in the current 
directory to run. Successfully running the examples
for these function would require either deploying files in the
current directory or moving the R session back and forth
between the temporary directory and the original directory.

getTimes(): Requires the output of one of the three main 
functions (explore, migration or residency), which in turn
require the presence of specific files in the user file
system.

transitionLayer(): Requires the presence of a shape file to
be able to run. Running the example in the absence of a valid
file will lead to an error. This is not a file I can easily
generate in R's temporary directory.

* In parallel with the requested revisions, I removed a 
"plot(x, y)" command that had accidentally been left active
in the function advEfficiency(). This should not have 
any impact on the issues raised before.

## Package resubmission (2nd revision)

This is a resubmission. As requested by Jelena Saf,
In this version I have:

* Included a reference to a supporting paper in the package
description, and also included a "references" field in the
documentation for simpleCJS and dualArrayCJS.

* Included "on.exit()" calls immediately after any changes
imposed on the options() or par(), to ensure these changes are
reverted when the function ends.

* removed the argument 'path' from explore(), migration() and
residency() to remove the need to rely on work directory changes.

* set the 'report' argument default to FALSE, so that explore()
migration() and residency() only write to the file system by
explicit user request (i.e. if the user sets 'report' to TRUE).

* The function appendTo() now writes to R's temporary folder.
All auxiliary files required for the report are now saved in
R's temporary folder. Thank you very much for mentioning the
tempdir() function, it is really great!

* The user is now asked for confirmation before the results
are saved as an .RData file and the analysis log is saved as a
.txt file (the latter only applies if report = FALSE).

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
now performed inside R's temporary directory. This change has also
been applied for tests that do not run on CRAN.

* All installed.packages() calls were removed and the respective
functions were adapted to work with packageDescription()

* Examples were included in the documentation of all exported
functions. Some of these examples were wrapped with \dontrun{}
to avoid errors due to missing files and/or the need to deploy
files outside R's temporary directory without explicit permission.

* \value fields detailing the nature and content of the output
were included for all functions.

* Removed the startNote function as it is no longer relevant.

* Updated the package vignettes accordingly.

* In parallel with the requested revisions, I have upgraded the
function advEfficiency to a more elegant method. This should
not have any impact on the issues raised before.

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

On all tested platforms:

0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Hugo Flávio <hdmfla@aqua.dtu.dk>’

New submission
