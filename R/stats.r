#' Rolling Summary Statistics
#' @details some detailled discussion of how auto and cross correlation are calculated and
#' na handling
#' @param x a dataframe containing columns "x", "y", and "date
#' @export
#'
rolling_stats <- function(x) {
  traj <- dl(x)[[1]][-nrow(x), ]
  dt <- traj$dt[1] # assuming completely regular trajectory, this is safe.
  n_fix_hr <- 60 * 60 / dt

  # becasue ltraj puts NA in rel.angle when they have not moved.
  traj$rel.angle[is.na(traj$rel.angle)] <- 0

  # decide sliding time window:
  # if dt is greater than or equal to an hr, sliding window should be 6 hr rather than 3.
  if (dt >= 60 * 60) {
    win <- 6
  } else {
    win <- 3
  }

  dplyr::mutate(traj,
    mean_dist = RcppRoll::roll_meanr(x = dist, n = n_fix_hr * win),
    sd_dist = RcppRoll::roll_sdr(x = dist, n = n_fix_hr * win),

    # the source on these runCor functions are written in Fortran
    # provide significant speed improvements but don't handle NAs well.
    acf_dist = c(runCor(x = dist[-length(dist)], y = dist[-1], n = n_fix_hr * win), NA),
    mean_ang = RcppRoll::roll_meanr(rel.angle, n = n_fix_hr * win),
    sd_ang = RcppRoll::roll_sdr(rel.angle, n = n_fix_hr * win),

    acf_ang = c(TTR::runCor(x = rel.angle[-length(rel.angle)], y = rel.angle[-1], n = n_fix_hr * win), NA),
    ccf = TTR::runCor(x = dist, y = rel.angle, n = n_fix_hr * win)
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
    traj$TOD <- ifelse(hour(traj$interval_start) == 0, "0:00-12:00", "12:00-24:00")
    quocol <- sym("TOD")
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
    # odd order of yday and c to avoid automatic switching to local tz
    interval_starts <- sort(c(yday(traj$date[1]), yday(full), yday(new), yday(traj$date[nrow(traj)])))

    traj$phase <- ifelse(traj$phase %in% c("Full", "Waning"), "Full-Waning", "New-Waxing")
    traj$interval_start <- cut(lubridate::yday(traj$date),
      breaks = unique(interval_starts), # needs testing
      right = F, include.lowest = T
    )

    quocol <- sym("phase")
  }

  if (type == "seasonal") {
    if (is.null(seas)) {
      stop("To calculate basic statistics seasonally, you must provide season start dates using the `seas` arg",
        call. = FALSE
      )
    }

    interval_starts <- unique(c(yday(traj$date[1]), seas, yday(traj$date[nrow(traj)])))

    traj$interval_start <- cut(yday(traj$date),
      breaks = interval_starts,
      right = F, include.lowest = T
    )

    traj$seas <- NA

    for (i in 1:length(seas)) {
      traj$seas[yday(traj$date) == seas[i]] <- names(seas)[i]
    }

    traj <- fill(traj, seas, .direction = "down")
    traj$seas[is.na(traj$seas)] <- names(seas)[length(seas)]

    quocol <- sym("seas")
  }


  # TODO add some tidy eval so that the group by can include `phase` when type == "lunar"
  traj %>%
    group_by(interval_start, !!quocol) %>%
    summarise(
      mean_dist = mean(dist, na.rm = T),
      sd_dist = sd(dist, na.rm = T),
      acf_dist = cor(dist, c(dist[-1], NA), "p"),
      mean_ang = mean(rel.angle, na.rm = T),
      sd_ang = sd(rel.angle, na.rm = T),
      acf_ang = cor(rel.angle, c(rel.angle[-1], NA), "p"),
      ccf = cor(dist, rel.angle, "p")
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
