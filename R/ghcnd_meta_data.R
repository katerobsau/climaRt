#' Data frame that contains the station meta data
#' for the Global Historical Climatology Network Daily (GHCND). The most up
#' to date version of this meta data can be returned from function
#' \code{rnoaa::ghncd_stations}. However, we have also stored a copy
#' as a dataset current as of 10/07/2019 to save time.
#'
#' A dataset contains basic information about the station and the relevant
#' climate attributes to temperature and precipitation
#'
#' @format A data frame with 680632 rows and 11 variables:
#' \describe{
#'   \item{id}{Station ID}
#'   \item{latitude}{Station latitude}
#'   \item{longitude}{Station longitude}
#'   \item{elevation}{Station elevation}
#'   ...
#' }
#' @source \url{https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt}
"ghcnd_meta_data"
