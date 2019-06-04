#' Example spatial data
#'
#' A dataset containing the positions of the deployed ALS and release site.
#'
#' @format A data frame with 18 rows and 6 variables:
#' \describe{
#'   \item{Station.Name}{The name of the ALS or release site}
#'   \item{Receiver}{The ALS deployed (leave empty if the row is a release site)}
#'   \item{Latitude}{The latitude of the ALS or release site}
#'   \item{Longitude}{The longitude of the ALS or release site}
#'   \item{Group}{The Array to which the ALS belongs, or the first ALS array downstream of the release site.}
#'   \item{Type}{The type of spatial object (must be either Hydrophone or Release)}
#' }
#' @source Data collected by the authors.
"example.spatial"
