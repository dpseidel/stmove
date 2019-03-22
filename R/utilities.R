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
#' @seealso \link[adehabitatLT]{setNA} \link[adehabitatLT]{sett0} \link[adehabitatLT]{subsample}
regularize <- function(df, dt, units = "min", tol = dt / 10, ref = NULL) {
  traj <- adehabitatLT::dl(df)

  if (is.null(ref)) {
    ref <- lubridate::round_date(summary(traj)$date.begin[1], unit = paste(dt, units))
  }

  traj <- adehabitatLT::setNA(traj, date.ref = ref, dt = dt, units = units)
  traj <- adehabitatLT::sett0(traj, date.ref = ref, dt = dt, units = units)

  adehabitatLT::ld(traj) %>% dplyr::select(names(df))
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

#' Kalman Smoothing with Structural Time Series
#'
#' Interpolate missing values in a trajectory.
#'
#' @param df a dataframe containing columns x, y, date representing relocations in space and time.
#' @param warn logical, should warnings be issued if interpolation is above 5\%?
#' @return a dataframe with additional binary column `real` flagging those points whose positions were interpolated.
#' @details The replacement points are generated using a structural time series model fitted by maximum likelihood.
#' @seealso \link[imputeTS]{na.kalman}
#' @export
kalman <- function(df, warn = TRUE) {
  if (!requireNamespace(c("imputeTS"), quietly = TRUE)) {
    stop("Package imputeTS must be installed for kalman smoothing. Please install it.",
      call. = FALSE
    )
  }

  df$real <- !is.na(df$x)

  temp_df <- df
  tryCatch ({
    # Replace NA values in longitude with Kalman Smoothed estimates
    temp_df$x <- imputeTS::na.kalman(df$x, model = "StructTS", smooth = TRUE)
    # Replace NA values in latitude with Kalman Smoothed estimates
    temp_df$y <- imputeTS::na.kalman(df$y, model = "StructTS", smooth = TRUE)
  }, warning = function(w) {
    print("Running warning protocol; Kalman smoothing failed to converge \n
          using structural time series modeling. Reverting to auto ARIMA approach")
    # Replace NA values in longitude using auto.arima approach
    temp_df$x <- imputeTS::na.kalman(df$x, model = "auto.arima", smooth = TRUE)
    # Replace NA values in latitude using auto.arima approach
    temp_df$y <- imputeTS::na.kalman(df$y, model = "auto.arima", smooth = TRUE)
  }, error = function(e) {
    print("Running error protocol; Kalman smoothing failed to converge \n
          using structural time series modeling. Reverting to auto ARIMA approach")
    # Replace NA values in longitude using auto.arima approach
    temp_df$x <- imputeTS::na.kalman(df$x, model = "auto.arima", smooth = TRUE)
    # Replace NA values in latitude using auto.arima approach
    temp_df$y <- imputeTS::na.kalman(df$y, model = "auto.arima", smooth = TRUE)
  }, finally = {
    df$x <- temp_df$x
    df_y <- temp_df$y
  })
  
  if (warn == TRUE) {
    if (sum(!df$real) / nrow(df) > .05) {
      warning(paste(
        "kalman interpolated", round(sum(!df$real) / nrow(df), 2) * 100,
        "% of values along your trajectory. We advise interpolating less than 5 %."
      ), call. = FALSE)
    }
  }

  return(df)
}

#' Check dataframes conform to stmove style
#'
#' An (internal) helper function to check dataframes and provide helpful errors
#'
#' @param df a dataframe
#' @export
#' @keywords internal
df_check <- function(df) {
  if (any(!(c("x", "y", "date") %in% names(df)))) {
    stop("stmove expects dataframes with columns 'x', 'y', and 'date', please include them.",
         call. = FALSE
    )
  }

  if (diff(range(diff(df$date))) != 0 || any(is.na(df))) {
    stop("stmove expects dataframes with regular intervals and no missing values, consider
          using `regularize()` and `kalman()` to regularize your data.",
         call. = FALSE
    )
  }
}

# More performant modifyList without recursion
modify_list <- function(old, new) {
  for (i in names(new)) old[[i]] <- new[[i]]
  old
}
