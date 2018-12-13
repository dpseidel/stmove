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


#' Build telemetry objects using projected data
#'
#' A helper function to create telemetry objects required by `ctmm`
#'
#' @param df a dataframe containing columns x, y, date representing relocations in space and time.
#' @param proj4 a character string indicating the proj.4 definition of the
#' coordinate reference system defining the relocations
#' @importFrom move move
#'
create_telemetry <- function(df, proj4) {

  # build telemetry objects using projected data
  suppressWarnings(mv <- move::move(
    x = df$x,
    y = df$y,
    time = df$date,
    data = df,
    animal = df$id,
    proj = CRS(proj4)
  ) %>%
    as.telemetry(., projection = CRS(proj4)))

  mv@info$identity <- df$id[1]

  return(mv)
}
