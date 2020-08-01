## Main changes relevant to the CRAN check:

* The version was updated from 1.0.0 directly to 1.1.0 to
warn the users about changes in the input file structure.

* Compatibility with data.table 1.13.0 was implemented.

* Tests that involve data.table::fread() are now preceeded by:

oldtz <- Sys.getenv('TZ', unset = NA)
Sys.setenv(TZ = 'UTC')

which is reset at the end of each test file with:

if (is.na(oldtz)) Sys.unsetenv("TZ") else Sys.setenv(TZ = oldtz)

I copied these lines from the data.table test files, so
I assumed they are in agreement with CRAN policies.

* actel now makes use of the option "actel.debug" to output
detailed R objects into tempdir(). This option has to be 
manually set by the user to have any effect.

* The function stripCodeSpaces() has been deprecated and
substituted by extractSignals()

* New exported functions: extractCodeSpaces(), getSpeeds(),
getTimes(), preload(), stationName(). All new exported functions 
contain examples.

## Test environments

* Windows Server 2019 (on GitHub), R 4.0.2 and devel
* Mac OS (on GitHub), R 4.0.2
* Ubuntu 16.04 (on GitHub), R 4.0.2
* local Windows 10, R 4.0.2

## R CMD check results

On all tested platforms:

0 errors | 0 warnings | 0 notes
