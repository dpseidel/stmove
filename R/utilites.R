#' Regularize a near-regular trajectory
#'
#' @param x a dataframe containing columns x, y, date representing relocations in space and time.
#' @param dt the expected time lag between relocations
#' @param units a character string indicating the time units for dt and tol
#' @param tol the tolerance, i.e. the imprecision in the timing of data collection
#'
#' @export
#' @importFrom lubridate round_date
#' @seealso `adehabitatLT::setNA` `adehabitatLT::sett0` `adehabitatLT::subsample`
regularize <- function(x, dt, units = "min", tol = dt / 10) {
  traj <- adehabitatLT::dl(x)
  ref <- lubridate::round_date(summary(traj)$date.begin[1], unit = paste(dt, units))
  traj <- adehabitatLT::setNA(traj, date.ref = ref, dt = dt, units = units)
  traj <- adehabitatLT::sett0(traj, date.ref = ref, dt = dt, units = units)

  return(traj)
}
