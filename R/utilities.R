#' Regularize a near-regular trajectory
#'
#' @param df a dataframe containing columns x, y, date representing relocations in space and time.
#' @param dt the expected time lag between relocations
#' @param units a character string indicating the time units for dt and tol
#' @param tol the tolerance, i.e. the imprecision in the timing of data collection
#' @param ref a datetime from which to start the fixes. Default `ref=NULL` will
#' calculate ref value by rounding the first timestamp in `df`
#'
#' @export
#' @seealso `adehabitatLT::setNA` `adehabitatLT::sett0` `adehabitatLT::subsample`
regularize <- function(df, dt, units = "min", tol = dt / 10, ref = NULL) {
  traj <- adehabitatLT::dl(df)

  if (is.null(ref)) {
    ref <- lubridate::round_date(summary(traj)$date.begin[1], unit = paste(dt, units))
  }

  traj <- adehabitatLT::setNA(traj, date.ref = ref, dt = dt, units = units)
  traj <- adehabitatLT::sett0(traj, date.ref = ref, dt = dt, units = units)

  return(adehabitatLT::ld(traj))
}


#' Build telemetry objects using projected data
#'
#' A helper function to create telemetry objects required by `ctmm`
#'
#' @param df a dataframe containing columns x, y, date representing relocations in space and time.
#' @param proj4 a character string indicating the proj.4 definition of the
#' coordinate reference system defining the relocations
create_telemetry <- function(df, proj4) {

  # build telemetry objects using projected data
  suppressWarnings(mv <- ctmm::as.telemetry(
    move::move(
      x = df$x,
      y = df$y,
      time = df$date,
      data = df,
      animal = df$id,
      proj = CRS(proj4)
    ),
    projection = CRS(proj4)
  ))

  mv@info$identity <- df$id[1]

  return(mv)
}

#' Kalman Smoothing with Auto ARIMA
#'
#' Apply Kalman smoothing to interpolate missing values in trajectory.
#'
#' @param df a dataframe containing columns x, y, date representing relocations in space and time.
#' @details The ARIMA model fitting used in this optimized for rapid estimation of models for multiple time series using conditional sums of squares (the final model still uses maximum likelihood estimation) and using step-wise model selection.
#' @seealso \link[forecast]{auto.arima} \link{KalmanSmooth}
#' @export
kalman <- function(df) {
  if (!requireNamespace(c("forecast"), quietly = TRUE)) {
    stop("Package forecast must be installed for kalman smoothing. Please install it.",
      call. = FALSE
    )
  }

  # Replace NA values in longitude with Kalman Smoothed estimates
  lon <- df$x
  fit <- forecast::auto.arima(df$x)
  kr <- KalmanSmooth(df$x, fit$model)
  id.na <- which(is.na(df$x))
  num <- ncol(kr$smooth)
  for (j in id.na) {
    lon[j] <- kr$smooth[j, num]
  }
  df$x <- lon

  # Replace NA values in latitude with Kalman Smoothed estimates
  lat <- df$y
  fit <- forecast::auto.arima(df$y)
  kr <- KalmanSmooth(df$y, fit$model)
  id.na <- which(is.na(df$y))
  num <- ncol(kr$smooth)
  for (j in id.na) {
    lat[j] <- kr$smooth[j, num]
  }
  df$y <- lat

  return(df)
}

# More performant modifyList without recursion
modify_list <- function(old, new) {
  for (i in names(new)) old[[i]] <- new[[i]]
  old
}
