#' The \code{stmove} package
#'
#' See the README on \href{https://github.com/dpseidel/stmove#readme}{GitHub}
#'
#' @docType package
#' @name stmove
#' @importFrom sp CRS
#' @importFrom lubridate hour yday
#' @importFrom dplyr %>% summarise group_by sym
#' @importFrom graphics par plot
#' @importFrom stats cor dist na.omit sd KalmanSmooth
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

## quiets concerns of R CMD check re: unnamed global variables created in stats functions, in lieu of NULL hack
## https://bit.ly/2SWkx93
globalVariables(c("rel.angle", "interval_start"))
