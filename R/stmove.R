#' The \code{stmove} package
#'
#' See the README on \href{https://github.com/dpseidel/stmove#readme}{GitHub}
#'
#' @docType package
#' @name stmove
#' @importFrom sp CRS
#' @importFrom lubridate hour yday
#' @importFrom dplyr %>% summarise group_by sym .data
#' @importFrom graphics par plot hist
#' @importFrom stats cor dist na.omit sd KalmanSmooth
#' @importFrom adehabitatLT dl ld
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1") utils::globalVariables(c("."))
