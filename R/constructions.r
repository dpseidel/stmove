#' Home Range Construction by Two Methods
#'
#' @param df a dataframe containing columns x, y, date representing relocations in space and time.
#' @param type a character string indicating the type/s of constructions to build, "klocoh" or "akde"
#' @param proj4 a character string indicating the proj.4 definition of the
#' coordinate reference system defining the relocations
#'
construct <- function(df, type = c("klocoh", "akde"), proj4) {
  # we may want to consider allowing user specification of some tlocoh arguments.

  # placeholders -- there is certainly a better way.
  lhs <- NULL
  UD <- NULL


  if ("klocoh" %in% type && !requireNamespace("tlocoh", quietly = TRUE)) {
    stop("Package tlocoh must be installed to run klocoh.\n",
         "Please see `http://tlocoh.r-forge.r-project.org/#installation` for installation instructions.",
         call. = FALSE
    )
  }

  if ("akde" %in% type) {
    message("Be aware, fitting an akde can take several minutes.")
  }


  if (length(type) > 1) {
    par(mfrow = c(1, 2))
  }

  # k - LoCoh
  if ("klocoh" %in% tolower(type)) {
    dropNA <- na.omit(df)
    k <- round(sqrt(nrow(dropNA)))
    lxy <- tlocoh::xyt.lxy(
      xy = matrix(c(dropNA$x, dropNA$y), ncol = 2),
      dt = dropNA$date, id = dropNA$id,
      proj4string = sp::CRS(proj4)
    )

    # lxy.thin.bursts.... unnecessary?
    lxy <- tlocoh::lxy.nn.add(lxy, s = 0, k = k)
    lhs <- tlocoh::lxy.lhs(lxy, k = k, s = 0, iso.levels = c(0.25, 0.50, 0.95), iso.add = T)

    plot(lhs, iso = T)
  }

  # ctmm
  if ("akde" %in% tolower(type)) {
    telm <- create_telemetry(df,
      proj4 = proj4
    )
    m.ouf <- ctmm::ctmm.guess(telm, interactive = FALSE) # automated model guess
    M.OUF <- ctmm::ctmm.fit(telm, m.ouf) # this can take awhile...
    UD <- ctmm::akde(telm, M.OUF)
    plot(UD, level.UD = c(.25, .5, .95))
  }

  return(Filter(f = length, x = list(lhs, UD)))
}
