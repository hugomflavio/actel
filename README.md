Jump to [**installation instructions**](#installing-actel)

# actel<img src="vignettes/actel_logo.png" align="right" width="120" />

[![R-CMD-check](https://github.com/hugomflavio/actel/workflows/R-CMD-check--as-cran/badge.svg)](https://github.com/hugomflavio/actel/actions)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/actel)](https://cran.r-project.org/package=actel)
[![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/actel)](https://cran.r-project.org/package=actel)

[![codecov](https://codecov.io/github/hugomflavio/actel/branch/master/graphs/badge.svg)](https://codecov.io/github/hugomflavio/actel)<sup> of non-interactive code*</sup>

Read the [latest news here!](https://github.com/hugomflavio/actel/blob/master/NEWS.md#news)

## Overview

If you are using acoustic telemetry to track animals as they move inside a study area or as they migrate somewhere, actel is the package for you. 
By bringing together the study area configuration and the recorded detections, actel provides a systematic way of analysing animal migration and residency data.

<img src="vignettes/mb_arrays.svg" alt="drawing" width="870"/>

### Main functions:

**1. explore()**

 explore() allows you to quickly get a summary of your data. You can use explore() to get a general feel for the study results, and check if the input files are behaving as expected. It is also a good candidate if you just want to validate your detections for later use in other analyses.
 
**2. migration()**

  The migration() analysis runs the same initial checks as explore(), but on top of it, it analyses the animal behaviour. By selecting the arrays that lead to success, you can define whether or not your animals survived the migration. Additional plots help you find out if some animal/tag has been acting odd. Multiple options allow you to tweak the analysis to fit your study perfectly.
 
**3. residency()**

  The residency() analysis runs the same initial checks as explore(), but, similarly to migration, explores particular points of the animal behaviour. If you want to know where your animals were in each day of the study, how many animals were in each section each day, and other residency-focused variables, this is the analysis you are looking for!

## Unlock actel's full potential

To truly learn how to operate actel, you must read the package vignettes. These have been arranged so that you can
prepare your analysis as you learn; quite soon you will get your first results!

Here are some examples: 

**Movement tables:**

|Array  | Detections|First station |Last station |First time          |Last time           |Time travelling |Time on array |
|:------|----------:|:-------------|:------------|:-------------------|:-------------------|---------------:|-------------:|
|River1 |         14|St.1          |St.2         |2019-05-15 10:30:00 |2019-05-15 13:00:00 |        25:20:14|       2:30:00|
|River2 |          3|St.4          |St.4         |2019-05-15 13:50:00 |2019-05-15 14:40:00 |         0:50:00|       0:50:00|
|River3 |          8|St.5          |St.6         |2019-05-15 16:00:00 |2019-05-15 16:20:00 |         1:20:00|       0:20:00|
|Fjord2 |         21|St.10         |St.11        |2019-05-16 15:10:00 |2019-05-16 18:00:00 |        22:50:00|       2:50:00|
|Sea1   |          1|St.18         |St.18        |2019-05-18 09:45:00 |2019-05-18 09:45:00 |        15:45:00|       0:00:00|

**Detection graphics**

<img src="vignettes/R64K-4521.png" alt="drawing" width="400"/> <img src="vignettes/R64K-4526.png" alt="drawing" width="400"/>

**Times of arrival and summary information**

<img src="vignettes/times_River3.svg" alt="drawing" width="410"/> <img src="vignettes/LaTeX_example_survival.svg" alt="drawing" height="370"/>

**Array efficiency and tag progression**

<img src="vignettes/mb_efficiency.svg" alt="drawing" width="870"/>

**Individual residency**

<img src="vignettes/readme_individual_residency.png" alt="drawing" width="870"/>

**Global residency**

<img src="vignettes/readme_global_residency.png" alt="drawing" width="870"/>

## Installing actel 

**CRAN version: 1.2.1**

actel is available on CRAN. To install the latest stable version, simply run:

    install.packages("actel")


**Development version**

If you would like to install the [latest updates](https://github.com/hugomflavio/actel/blob/master/NEWS.md#news) (which have not been integrated to CRAN yet), you can run the line below. Note that you need to have the package `remotes` installed!

    remotes::install_github("hugomflavio/actel", build_opts = c("--no-resave-data", "--no-manual"), build_vignettes = TRUE)


**Have a look at the manual:**

After installing, you should read the package vignettes (i.e. the manual), which can be found by running:

    browseVignettes("actel")

**Note:**

1. If you are getting "pandoc document conversion" errors during the package installation, try installing the [newest version of pandoc](https://pandoc.org/installing.html), restarting R and trying again.


<sup>* interactive code (i.e. code that expects user input) cannot be tested automatically using codecov and, as such, was excluded from the codecov scope.</sup>



## Papers using or relating to actel

**2022**

- Edwards, M. L., McCallister, M., Brewster, L. R., Bangley, C. W., Curtis, T. H., Ogburn, M. B., & Ajemian, M. J. (2022). Multi-year assessment of immature bull shark <i>Carcharhinus leucas</i> residency and activity spaces in an expansive estuarine nursery. Marine Ecology Progress Series, 695, 125–138. https://doi.org/10.3354/meps14113

- Hewitt, D. E., Niella, Y., Johnson, D. D., Suthers, I. M., & Taylor, M. D. (2022). Crabs Go With the Flow: Declining Conductivity and Cooler Temperatures Trigger Spawning Migrations for Female Giant Mud Crabs (<i>Scylla serrata</i>) in Subtropical Estuaries. Estuaries and Coasts. https://doi.org/10.1007/s12237-022-01061-1

- Hollins, J., Pettitt-Wade, H., Gallagher, C. P., Lea, E. V., Loseto, L. L., & Hussey, N. E. (2022). Distinct freshwater migratory pathways in Arctic char (<i>Salvelinus alpinus</i>) coincide with separate patterns of marine spatial habitat-use across a large coastal landscape. Canadian Journal of Fisheries and Aquatic Sciences. https://doi.org/10.1139/cjfas-2021-0291

- Thorstensen, M. J., Vandervelde, C. A., Bugg, W. S., Michaleski, S., Vo, L., Mackey, T. E., Lawrence, M. J., & Jeffries, K. M. (2022). Non-Lethal Sampling Supports Integrative Movement Research in Freshwater Fish. Frontiers in Genetics, 13. https://www.frontiersin.org/articles/10.3389/fgene.2022.795355


**2021**

- Flávio, H., & Baktoft, H. (2021). actel: Standardised analysis of acoustic telemetry data from animals moving through receiver arrays. Methods in Ecology and Evolution, 12(1), 196–203. https://doi.org/10.1111/2041-210X.13503

- Flávio, H., Caballero, P., Jepsen, N., & Aarestrup, K. (2021). Atlantic salmon living on the edge: Smolt behaviour and survival during seaward migration in River Minho. Ecology of Freshwater Fish, 30(1), 61–72. https://doi.org/10.1111/eff.12564

- Niella, Y., Smoothey, A. F., Taylor, M. D., Peddemors, V. M., & Harcourt, R. (2021). Environmental Drivers of Fine-Scale Predator and Prey Spatial Dynamics in Sydney Harbour, Australia, and Adjacent Coastal Waters. Estuaries and Coasts. https://doi.org/10.1007/s12237-021-01020-2

- Nordli, E. (2021). The behaviour of anadromous Arctic charr during their first marine migration [Master thesis, Arctic University of Norway]. https://munin.uit.no/handle/10037/21752

- Winter, E. R. (2021). The movement ecology of common bream _Abramis brama_ in a highly connected wetland using acoustic telemetry. [Doctoral thesis, Bournemouth University]. http://eprints.bournemouth.ac.uk/35141/

- Winter, E. R., Hindes, A. M., Lane, S., & Britton, J. R. (2021a). Movements of common bream _Abramis brama_ in a highly connected, lowland wetland reveal sub-populations with diverse migration strategies. Freshwater Biology, 66(7), 1410–1422. https://doi.org/10.1111/fwb.13726

- Winter, E. R., Hindes, A. M., Lane, S., & Britton, J. R. (2021b). Acoustic telemetry reveals strong spatial preferences and mixing during successive spawning periods in a partially migratory common bream population. Aquatic Sciences, 83(3), 52. https://doi.org/10.1007/s00027-021-00804-9


**2020**

- Flávio, H., Kennedy, R., Ensing, D., Jepsen, N., & Aarestrup, K. (2020). Marine mortality in the river? Atlantic salmon smolts under high predation pressure in the last kilometres of a river monitored for stock assessment. Fisheries Management and Ecology, 27(1), 92–101. https://doi.org/10.1111/fme.12405

- Niella, Y., Flávio, H., Smoothey, A. F., Aarestrup, K., Taylor, M. D., Peddemors, V. M., & Harcourt, R. (2020). Refined Shortest Paths (RSP): Incorporation of topography in space use estimation from node-based telemetry data. Methods in Ecology and Evolution, 11(12), 1733–1742. https://doi.org/10.1111/2041-210X.13484


Is your paper not here? Let me know!
