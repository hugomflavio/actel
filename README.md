Jump to [**installation instructions**](#installing-actel)

## Actel: Acoustic telemetry data sorting

If you are tracking animals during their migration using acoustic telemetry, actel is the package for you. 
By bringing together the study area configuration and the recorded detections, actel provides a systematic way of analysing fish migration data.

<img src="vignettes/mb_arrays.svg" alt="drawing" width="870"/>

### Main functions:

**1. explore()**

  explore() allows you to quickly get a summary of your data. You can use explore() to get
  a general feel for the study results, and check if the input files are behaving as expected
 
**2. migration()**

  The migration() analysis runs the same initial checks as explore(), but on top of it, it analysis
  the fish behaviour. By selecting the arrays that lead to success, you can define whether or not
  your fish survived the study. Multiple options allow you to tweak the analysis to fit your study perfectly.
 
**3. residency()**

  The residency() analysis is still under construction. Keep an eye out for updates!

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


## Installing actel

To install actel, you will need to have the devtools package installed.

    install.packages("devtools")
    library("devtools")
    
Now you can either install actel sourcing remotely or locally.

**a) install directly from the repository:**

    devtools::install_github("hugomflavio/actel", build_opts = c("--no-resave-data", "--no-manual"), ref = "devel")

**b) install locally:**

1. Download actel's source files here: [**actel-devel.zip**](https://github.com/hugomflavio/actel/archive/devel.zip)
1. Unzip the file and move your R session into the newly created folder
1. Run: 

        devtools::install("actel-devel", build_vignettes = TRUE)

After installing, you should read the package vignettes, which can be found by running:

    browseVignettes('actel')

<span style="color:red">**Note:**</span> 

1. There have been some reports that the **vignettes are not being installed** through option A. If you installed actel remotely and the vignettes are not showing up, **try using the second installation method**.
1. If you are getting "pandoc document conversion" errors during the package installation, try installing the [newest version of pandoc](https://pandoc.org/installing.html), restarting R and trying again.

