Jump to [**installation instructions**](#installing-actel)

# actel<img src="vignettes/actel_logo.png" align="right" width="120" />

[![R-CMD-check](https://github.com/hugomflavio/actel/workflows/R-CMD-check/badge.svg)](https://github.com/hugomflavio/actel/actions)
[![Travis Build Status](https://travis-ci.com/hugomflavio/actel.svg?branch=dev)](https://travis-ci.org/hugomflavio/actel)
[![codecov](https://codecov.io/github/hugomflavio/actel/branch/dev/graphs/badge.svg)](https://codecov.io/github/hugomflavio/actel)<sup> of non-interactive code*</sup>

Read the [latest news here!](https://github.com/hugomflavio/actel/blob/master/NEWS.md#news)

## Overview

If you are using acoustic telemetry to track animals as they move inside a study area or as they migrate somewhere, actel is the package for you. 
By bringing together the study area configuration and the recorded detections, actel provides a systematic way of analysing fish migration and residency data.

<img src="vignettes/mb_arrays.svg" alt="drawing" width="870"/>

### Main functions:

**1. explore()**

 explore() allows you to quickly get a summary of your data. You can use explore() to get a general feel for the study results, and check if the input files are behaving as expected. It is also a good candidate if you just want to validate your detections for later use in other analyses.
 
**2. migration()**

  The migration() analysis runs the same initial checks as explore(), but on top of it, it analyses the fish behaviour. By selecting the arrays that lead to success, you can define whether or not your fish survived the migration. Additional plots help you find out if some fish has been acting odd. Multiple options allow you to tweak the analysis to fit your study perfectly.
 
**3. residency()**

  The residency() analysis runs the same initial checks as explore(), but, similarly to migration, explores particular points of the fish behaviour. If you want to know where your fish were in each day of the study, how many fish were in each section each day, and other residency-focused variables, this is the analysis you are looking for!

## Unlock actel's full potential

To truly learn how to operate actel, you must read the package vignettes. These have been arranged so that you can
prepare your analysis as you learn; quite soon you will get your first results!

Here are some examples: 

**Movement tables:**

|Array  | Detections|First station |Last station |First time          |Last time           |Time travelling |Time on array |
|:------|----------:|:-------------|:------------|:-------------------|:-------------------|:---------------|:-------------|
|River1 |         14|St.1          |St.2         |2019-05-15 10:30:00 |2019-05-15 13:00:00 |NA              |3:30          |
|River2 |          3|St.4          |St.4         |2019-05-15 13:50:00 |2019-05-15 14:40:00 |0:50            |0:50          |
|River3 |          8|St.5          |St.6         |2019-05-15 16:00:00 |2019-05-15 16:20:00 |1:20            |0:20          |
|Fjord2 |         21|St.10         |St.11        |2019-05-16 15:10:00 |2019-05-16 18:00:00 |22:50           |2:50          |
|Sea1   |          1|St.18         |St.18        |2019-05-18 09:45:00 |2019-05-18 09:45:00 |15:45           |0:00          |

**Detection graphics**

<img src="vignettes/R64K-4521.png" alt="drawing" width="430"/> <img src="vignettes/R64K-4526.png" alt="drawing" width="430"/>

**Times of arrival and summary information**

<img src="vignettes/times_River3.svg" alt="drawing" width="410"/> <img src="vignettes/LaTeX_example_survival.svg" alt="drawing" height="370"/>

**Array efficiency and fish progression**

<img src="vignettes/mb_efficiency.svg" alt="drawing" width="870"/>

**Individual residency**

<img src="vignettes/readme_individual_residency.png" alt="drawing" width="870"/>

**Global residency**

<img src="vignettes/readme_global_residency.png" alt="drawing" width="870"/>

## Installing actel 

Master version: 0.1.3

Dev version: 1.0.0

To install actel, you will need to have the remotes package installed.

    install.packages("remotes")
    library("remotes")
    
<span style="color:red">**Note:**</span> 

* <span style="color:red">actel 0.1.3 is currently outdated. actel 1.0.0 (currently in the development branch) is ready for deployment, but I am working on getting it published to CRAN.</span> 

To install actel 1.0.0 (currently in the develomnet branch), run the following line:

    remotes::install_github("hugomflavio/actel", ref = "dev", build_opts = c("--no-resave-data", "--no-manual"), build_vignettes = TRUE)

To install actel 0.1.3 (current master branch - known bugs present), run the following line:

    remotes::install_github("hugomflavio/actel", build_opts = c("--no-resave-data", "--no-manual"), build_vignettes = TRUE)

**Have a look at the manual:**

After installing, you should read the package vignettes (i.e. the manual), which can be found by running:

    browseVignettes("actel")

**Note:**

1. If the vignettes are not showing up with the command above, you can download them directly here: 
   * For actel 1.0.0: [**actel_vignettes.zip**](https://github.com/hugomflavio/actel/raw/dev/actel_vignettes.zip)
   * For actel 0.1.3: [**actel_vignettes.zip**](https://github.com/hugomflavio/actel/raw/master/actel_vignettes.zip)
1. If you are getting "pandoc document conversion" errors during the package installation, try installing the [newest version of pandoc](https://pandoc.org/installing.html), restarting R and trying again.


<sup>* interactive code (i.e. code that expects user input) cannot be tested automatically using codecov and, as such, was excluded from the codecov scope.</sup>
