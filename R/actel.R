#' actel: Acoustic Telemetry Data Analysis
#'
#' actel is designed for studies where animals tagged with acoustic tags are expected
#' to move through receiver arrays. actel combines the advantages of automatic sorting and checking
#' of animal movements with the possibility for user intervention on tags that deviate from expected
#' behaviour. The three analysis functions: \code{\link{explore}}, \code{\link{migration}}
#' and \code{\link{residency}}, allow the users to analyse their data in a systematic way,
#' making it easy to compare results from different studies.
#'
#' # Input structure
#'
#' To be able to work with actel, you must prepare your data in a specific
#' format. To learn more about this, you need to have a look at the package
#' vignettes, which can be found by running \code{browseVignettes('actel')}. If
#' this function returns "No vignettes found", you can alternatively find
#' the vignettes online: \url{https://CRAN.R-project.org/package=actel}
#'
#'
#' # Example dataset
#'
#' If this is the first time you are using actel, you can try running it on an
#' example dataset using the \code{\link{exampleWorkspace}} function. This function
#' deploys a set of example files following the structure described in the package
#' vignettes. Namely:
#' \itemize{
#'   \item biometrics.csv
#'   \item deployments.csv
#'   \item spatial.csv
#'   \item detections/ (a folder with .csv files)
#' }
#'
#' Once the example dataset is created, \code{\link{exampleWorkspace}} also provides
#' you with example code to run an \code{\link{explore}} analysis.
#'
#' Note that you can also run the \code{\link{migration}} and \code{\link{residency}}
#' analyses on the example dataset, e.g.:
#'
#' \code{results <- migration(tz = 'Europe/Copenhagen', report = TRUE)}
#'
#' or
#'
#' \code{results <- residency(tz = 'Europe/Copenhagen', report = TRUE)}
#'
#' Note: Running these lines with \code{report = TRUE} will open an analysis report on your web browser.
#'
#'
#' # Main functions
#'
#' The actel package provides three main analyses:
#' \code{\link{explore}}, \code{\link{migration}} and \code{\link{residency}}
#'
#' ## explore
#'
#' \code{\link{explore}} allows you to quickly get a summary of your data. You
#' can use \code{\link{explore}} to get a general feel for the study results,
#' and check if the input files are behaving as expected. It is also a good
#' candidate if you just want to validate your detections for later use in other
#' analyses.
#'
#'
#' ## migration
#'
#' The \code{\link{migration}} analysis runs the same initial checks as
#' \code{\link{explore}}, but on top of it, it analyses the animal behaviour.
#' By selecting the arrays that lead to success, you can define whether or not
#' your animals survived the migration. Additional plots help you find out if some
#' animals/tags has been acting odd. Multiple options allow you to tweak the
#' analysis to fit your study perfectly.
#'
#'
#' ## residency
#'
#' The \code{\link{residency}} analysis runs the same initial checks as
#' \code{\link{explore}}, but, similarly to \code{\link{migration}}, explores
#' particular points of the animal behaviour. If you want to know where your animals
#' were in each day of the study, how many animals were in each section each day,
#' and other residency-focused variables, this is the analysis you are looking
#' for!
#'
#'
#' @seealso \code{\link{explore}}, \code{\link{migration}}, \code{\link{residency}},
#' \code{\link{exampleWorkspace}}, \code{\link{blankWorkspace}}
#'
#' @importFrom circular sd.circular mean.circular var.circular
#' @import stats
#' @import utils
#' @import graphics
#' @import data.table
#'
#' @keywords internal
#'
#' @docType package
#' @name actel
"_PACKAGE"