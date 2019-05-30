#' AG195: elephant trajectory
#'
#' A dataframe containing the clean regularized relocation points for
#'  African elephant (\emph{Loxodonta africana}) AG195. All points are from
#'  2010 and were collected by Getz et al. x and y coordinates are projected in
#'  WGS 84 / UTM zone 33S (espg:32733). The fix rate for this individual is 1 fix
#'  every 20 minutes. Columns are as follows:
#'
#' @format A data frame with 53940 rows and 10 variables:
#' \describe{
#'   \item{x}{x coordinate}
#'   \item{y}{y coordinate}
#'   \item{date}{timestamp of relocation}
#'   \item{id}{animal id}
#'   \item{real}{logical indicating empirical or interpolated position}
#' }
"AG195"

#' AG195_raw: raw elephant trajectory
#'
#' A dataframe containing the raw irregular relocation points for
#'  African elephant (\emph{Loxodonta africana}) AG195. All points are from
#'  2010 and were collected by Getz et al. Coordinates reflect the WGS84 geographic
#'  coordinate system. The fix rate for this individual alternates
#'  between every 1 and 19 minutes.
#'
#' @format A data frame with 51964 rows and 4 variables:
#' \describe{
#'   \item{longitude}{x coordinate}
#'   \item{latitude}{y coordinate}
#'   \item{datetime}{timestamp of relocation}
#'   \item{id}{animal id}
#' }
"AG195_raw"

#' AG268: elephant trajectory
#'
#' A dataframe containing the clean regularized relocation points for
#'  African elephant (\emph{Loxodonta africana}) AG195. All points are from
#'  2010 and were collected by Getz et al. x and y coordinates are projected in
#'  WGS 84 / UTM zone 33S (espg:32733). The fix rate for this individual is 1 fix
#'  every 15 minutes. Columns are as follows:
#'
#' @format A data frame with 53940 rows and 10 variables:
#' \describe{
#'   \item{x}{x coordinate}
#'   \item{y}{y coordinate}
#'   \item{date}{timestamp of relocation}
#'   \item{id}{animal id}
#'   \item{real}{logical indicating empirical or interpolated position}
#' }
"AG268"
