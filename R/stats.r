#' Rolling Summary Statistics
#' @details some detailed discussion of how auto and cross correlation are calculated and
#' na handling
#' @param df a dataframe containing columns "x", "y", and "date
#' @param n_roll numeric, number of fixes (e.g. width of the window) over which to
#' calculate rolling statistics. If NULL, the default is to roll over 3 hours for fix-rates
#' of 1 hour or less, and over 6 hours for fix rates occuring at intervals greater than 1 hour.
#' If your fix rate is every 4 hours or greater and n_roll is not set, the function
#' will warn you to set `n_roll`.
#' @seealso \link[RcppRoll]{roll_mean} \link[RcppRoll]{roll_sdr} \link[TTR]{runCor}
#' @examples
#' roll <- rolling_stats(AG195)
#' @export
rolling_stats <- function(df, n_roll = NULL) {
  df_check(df)

  traj <- dl(df)[[1]][-nrow(df), ]
  dt <- traj$dt[1]
  n_fix_hr <- 60 * 60 / dt

  # add warning if the dt is too big to be used with rolling
  if (dt >= 4 * 60 * 60 && is.null(n_roll)) {
    warning("Your fix rate is 4 hours or greater, please set n_roll for informative results.", call. = F)
  }

  # becasue ltraj puts NA in rel.angle when they have not moved.
  traj$rel.angle[is.na(traj$rel.angle)] <- 0

  # decide sliding time window:
  # if dt is greater than or equal to an hr, sliding window should be 6 hr rather than 3.
  if (dt >= 60 * 60) {
    win <- 6
  } else {
    win <- 3
  }

  if (is.null(n_roll)) {
    n_roll <- round(n_fix_hr * win)
  }

  dplyr::mutate(traj,
    mean_dist = RcppRoll::roll_meanr(x = dist, n = n_roll),
    sd_dist = RcppRoll::roll_sdr(x = dist, n = n_roll),

    # the source on these runCor functions are written in Fortran
    # provide significant speed improvements but don't handle NAs well.
    acf_dist = c(TTR::runCor(x = dist[-length(dist)], y = dist[-1], n = n_roll), NA),
    mean_ang = RcppRoll::roll_meanr(.data$rel.angle, n = n_roll),
    sd_ang = RcppRoll::roll_sdr(.data$rel.angle, n = n_roll),

    acf_ang = c(TTR::runCor(x = .data$rel.angle[-length(.data$rel.angle)], y = .data$rel.angle[-1], n = n_roll), NA),
    ccf = TTR::runCor(x = dist, y = .data$rel.angle, n = n_roll)
  )
}

#' Interval Summary Statistics
#' @export
#' @param df a dataframe containing columns "x", "y", and "date"
#' @param type a character string indicating the intervals to calculate, one of
#' "diurnal", "lunar", or "seasonal".
#' @param seas a character vector including the start date of each season interval in the format
#' \%Y-\%m-\%d, e.g. "2015-01-01".  Required if `type == "seasonal"`.
#' @details Note: if your trajectory includes dates before the first date in your `seas` vector, they
#' will automatically be considered a separate season.
#' @importFrom lubridate floor_date days ymd
#' @importFrom utils tail
#' @seealso \link[lunar]{lunar.phase}
#' @examples
#' # diurnal is default
#' interval_stats(AG195)
#' interval_stats(AG195, type = "lunar")
#' # for seasonal, include y-m-d formatted `seas` vector
#' interval_stats(AG195, type = "seasonal", seas = c("2010-03-01", "2010-06-01", "2010-9-01"))
interval_stats <- function(df, type = "diurnal", seas = NULL) {
  # Consider adapting this to do multiple ids at once or for full ltraj compatability
  df_check(df)

  traj <- dl(df)[[1]]

  # jumping time window - 12hr - diurnal cycle
  if (type == "diurnal") {
    dt <- traj$dt[1]

    if (dt > 6 * 60 * 60) {
      warning("Diurnal statistics are only informative for dt < 6 hours", call. = F)
    }

    # for diurnal stats
    traj$interval_start <- lubridate::floor_date(x = traj$date, "12 hours")
    traj$TOD <- ifelse(hour(traj$interval_start) == 0, "0:00-12:00", "12:00-24:00")
    quocol <- sym("TOD")
  }

  # lunar cycle
  if (type == "lunar") {
    # may want to allow user specification of "shift" parameter to adjust exactly to local time.

    traj$phase <- lunar::lunar.phase(traj$date, name = T)

    # the following need specific testing, and to be cleaned up!
    # yeah it doesn't work when the trajectory crosses year end... 363-5 for instance. SAT676
    full <- unique(lubridate::floor_date(traj[traj$phase == "Full", "date"], "1 day"))[
      c(TRUE, (diff(unique(lubridate::floor_date(traj[traj$phase == "Full", "date"], "1 day"))) != 1))
    ]
    new <- unique(lubridate::floor_date(traj[traj$phase == "New", "date"], "1 day"))[
      c(TRUE, (diff(unique(lubridate::floor_date(traj[traj$phase == "New", "date"], "1 day"))) != 1))
    ]

    interval_starts <- c(floor_date(traj$date[1], "1 day"), sort(c(full, new)), floor_date(traj$date[nrow(traj)], "1 day"))

    if (length(unique(tail(interval_starts, 2))) == 1) {
      interval_starts[length(interval_starts)] <- tail(interval_starts, 1) + lubridate::days(1)
    }

    traj$phase <- ifelse(traj$phase %in% c("Full", "Waning"), "Full-Waning", "New-Waxing")
    traj$interval_start <- cut(floor_date(traj$date, "1 day"),
      breaks = unique(interval_starts), # needs testing
      right = F, include.lowest = T
    ) # not behaving when the last break is small.

    quocol <- sym("phase")
  }

  if (type == "seasonal") {
    if (is.null(seas)) {
      stop("To calculate basic statistics seasonally, you must provide season start dates using the `seas` arg",
        call. = FALSE
      )
    }

    if (any(is.na(suppressWarnings(ymd(seas))))) {
      stop("`seas` argument must be a character vector of dates with format year, month, day.",
        call. = FALSE
      )
    }


    if (traj$date[1] < ymd(seas[1])) {
      seas <- c(strftime(traj$date[1], "%Y-%m-%d"), seas)
    }

    if (traj$date[nrow(traj)] > ymd(seas[length(seas)])) {
      seas <- c(seas, strftime(traj$date[nrow(traj)] + lubridate::days(1), "%Y-%m-%d"))
    }

    traj$interval_start <- cut.POSIXt(traj$date,
      breaks = as.POSIXct(seas, tz = "UTC"),
      right = F, include.lowest = T
    )

    quocol <- sym("interval_start")
  }

  traj %>%
    group_by(.data$interval_start, !!quocol) %>%
    summarise(
      mean_dist = mean(dist, na.rm = T),
      sd_dist = sd(dist, na.rm = T),
      acf_dist = cor(dist, c(dist[-1], NA), "p"),
      mean_ang = mean(.data$rel.angle, na.rm = T),
      sd_ang = sd(.data$rel.angle, na.rm = T),
      acf_ang = cor(.data$rel.angle, c(.data$rel.angle[-1], NA), "p"),
      ccf = cor(dist, .data$rel.angle, "p")
    )
}
