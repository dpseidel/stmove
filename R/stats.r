#' Rolling Summary Statistics
#' @details some detailled discussion of how auto and cross correlation are calculated and
#' na handling
#' @param x a dataframe containing columns "x", "y", and "date
#' @param na.approx a logical indicating whether or not the user wants to interpolate NAs, see details
#' @export
#'
rolling_stats <- function(x, na.approx = F) {
  traj <- dl(x)[[1]]
  dt <- traj$dt[1] # assuming completely regular trajectory, this is safe.
  n_fix_hr <- 60 * 60 / dt

  # decide sliding time window:
  # if dt is greater than or equal to an hr, sliding window should be 6 hr rather than 3.
  if (dt >= 60 * 60) {
    win <- 6
  } else {
    win <- 3
  }

  # if user wants to do light NA interpolation for rolling correlations
  if (na.approx == T) {
    dist_fill <- zoo::na.approx(traj$dist) # drops trailing NA, interpolates others.
    ang_fill <- zoo::na.approx(traj$rel.angle)
  }


  dplyr::mutate(traj,
    mean_dist = RcppRoll::roll_meanr(x = dist, n = n_fix_hr * win),
    sd_dist = RcppRoll::roll_sdr(x = dist, n = n_fix_hr * win),

    # the source on these runCor functions are written in Fortran
    # provide significant speed improvements but don't handle NAs well.
    acf_dist = c(runCor(x = dist_fill[-length(dist_fill)], y = dist_fill[-1], n = n_fix_hr * win), NA, NA),
    mean_ang = RcppRoll::roll_meanr(rel.angle, n = n_fix_hr * win),
    sd_ang = RcppRoll::roll_sdr(rel.angle, n = n_fix_hr * win),

    acf_ang = c(NA, TTR::runCor(x = ang_fill[-length(ang_fill)], y = ang_fill[-1], n = n_fix_hr * win), NA, NA),
    ccf = c(NA, TTR::runCor(x = dist_fill[-1], y = ang_fill, n = n_fix_hr * win), NA)
  )
}

#' Interval Summary Statistics
#' @export
#' @param x a dataframe containing columns "x", "y", and "date
#' @param type a character string indicating the intervals to calculate, one of
#' "diurnal", "lunar", or "seasonal".
#' @param seas a named numeric vector including the start date of each season
#' indicated by the Julian day (see `lubridate::yday` for easy conversion).
#' Required if `type == "seasonal"`

interval_stats <- function(x, type = "diurnal", seas = NULL) {
  # Consider adapting this to do multiple ids at once or for full ltraj compatability

  traj <- dl(x)[[1]]

  # jumping time window - 12hr - diurnal cycle
  if (type == "diurnal") {
    # for diurnal stats
    traj$interval_start <- lubridate::floor_date(x = traj$date, "12 hours")
  }

  # lunar cycle
  if (type == "lunar") {
    if (!requireNamespace("lunar", quietly = TRUE)) {
      stop("Package lunar must be installed to calculate summary statistics for lunar cycles. Please install it.",
        call. = FALSE
      )
    }
    # may want to allow user specification of "shift" parameter to adjust exactly to local time.

    traj$phase <- lunar::lunar.phase(traj$date, name = T)
    # the following need specific testing, and to be cleaned up!
    full <- unique(lubridate::floor_date(traj[traj$phase == "Full", "date"], "1 day"))[
      c(TRUE, (diff(unique(lubridate::floor_date(traj[traj$phase == "Full", "date"], "1 day"))) != 1))
    ]
    new <- unique(lubridate::floor_date(traj[traj$phase == "New", "date"], "1 day"))[
      c(TRUE, (diff(unique(lubridate::floor_date(traj[traj$phase == "New", "date"], "1 day"))) != 1))
    ]
    interval_starts <- lubridate::yday(c(traj$date[1], sort(c(full, new)), traj$date[nrow(traj)]))

    traj$interval_start <- cut(lubridate::yday(traj$date),
      breaks = unique(interval_starts), # needs testing
      right = F, include.lowest = T
    )
  }

  if (type == "seasonal") {
    if (is.null(seas)) {
      stop("To calculate basic statistics seasonally, you must provide season start dates using the `seas` arg",
        call. = FALSE
      )
    }

    interval_starts <- yday(c(traj$date[1], seas, traj$date[nrow(traj)]))

    traj$interval_start <- cut(yday(traj$date),
      breaks = interval_starts,
      right = F, include.lowest = T
    )

    # adapt this to take names too??
  }


  # TODO add some tidy eval so that the group by can include `phase` when type == "lunar"
  traj %>%
    group_by(interval_start) %>%
    summarise(
      mean_dist = mean(dist, na.rm = T),
      sd_dist = sd(dist, na.rm = T),
      acf_dist = cor(dist, c(dist[-1], NA), "complete.obs"),
      mean_ang = mean(rel.angle, na.rm = T),
      sd_ang = sd(rel.angle, na.rm = T),
      acf_ang = cor(rel.angle, c(rel.angle[-1], NA), "complete.obs"),
      ccf = cor(dist, rel.angle, "complete.obs")
    )

  # For future reference:
  # .colMeans provides a nice way to do means this without tidyverse
  # mean_dist = .colMeans(dist, 12 * n_fixes_hr, length(traj$dist) / (12 * n_fixes_hr), na.rm = T)
  # sd will require more manipulation
}



#### Notes
# if we wanted to be really flexible we could build this as an S3 method that could accept
# either ltraj and dataframes and handle them appropriately.
# if we do so we could pitch our package as an extension of adehabitat etc.

# we'll want to mimic the "next plot" functionality of plot(fit) etc.
# likely will need to make an S3 class method for this.
