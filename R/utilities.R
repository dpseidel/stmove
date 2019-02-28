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

#' Kalman Smoothing with Auto ARIMA
#'
#' Apply Kalman smoothing to interpolate missing values in trajectory.
#'
#' @param df a dataframe containing columns x, y, date representing relocations in space and time.
#' @return a dataframe with additional binary column `real` flagging those points whose positions were interpolated.
#' @details The replacement points are generated using a structural model fitted by maximum likelihood.
#' @seealso \link[imputeTS]{na.kalman}
#' @export
kalman <- function(df) {
    if (!requireNamespace(c("forecast"), quietly = TRUE)) {
        stop("Package forecast must be installed for kalman smoothing. Please install it.",
             call. = FALSE
        )
    }
    
    df$real <- !is.na(df$x)
    
    # Replace NA values in longitude with Kalman Smoothed estimates
    x_replace <- na.kalman(df$x, model='StructTS', smooth=TRUE)
    id.na <- which(is.na(df$x))
    for (j in id.na) {
        df$x[j] <- x_replace[j]
    }
    
    # Replace NA values in latitude with Kalman Smoothed estimates
    y_replace <- na.kalman(df$y, model='StructTS', smooth=TRUE)
    id.na <- which(is.na(df$y))
    for (j in id.na) {
        df$y[j] <- y_replace[j]
    }
    
    return(df)
}

# More performant modifyList without recursion
modify_list <- function(old, new) {
  for (i in names(new)) old[[i]] <- new[[i]]
  old
}
