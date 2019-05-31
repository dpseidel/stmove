#' Build a report with basic spatio-temporal movement computations
#'
#' @inheritParams interval_stats
#' @inheritParams rolling_stats
#' @param path absolute file path for output .pdf, defaults to project working directory.
#' see also \link[here]{here}.
#' @param stats a character vector of stats to calculate, options include: "rolling",
#' "diurnal", "lunar", and "seasonal"
#' @param construct a character vector indicating which spacetime construction methods to use.
#' options include "klocoh" and "akde"
#' @param wavelet  a character vector indicating which variables to calculate wavelet analysis on,
#' options in "dist", "rel.angle", "acf_dist", "acf_ang", and "ccf".
#' @inheritParams construct
#'
#' @export
#' @examples
#' \donttest{
#' build_report(AG195, proj4 = "+proj=utm +zone=33 +south +datum=WGS84 +units=m +no_defs")
#' }
build_report <- function(df, path = here::here(), proj4, stats = c("rolling", "diurnal"),
                         construct = c("klocoh"), seas = NULL, wavelet = NULL) {
  df_check(df)

  params <- list(
    df = df, stats = stats, construct = construct,
    proj4 = proj4, seas = seas, wavelet = wavelet
  ) # needs to match params list in yaml

  # Knit the document, passing in the `params` list, and eval it in a
  # child of the global environment (this isolates the code in the document
  # from the code in this package).

  if (length(unique(df$id)) > 1) {
    # population report
    rmarkdown::render(system.file("reports", "pop.Rmd", package = "stmove"),
      output_file = file.path(path, "population.pdf"),
      params = list(df = df, proj4 = proj4),
      envir = new.env(parent = globalenv()),
      clean = TRUE
    )
    # individual reports
    ids <- unique(df$id)
    for (i in ids) {
      rmarkdown::render(system.file("reports", "report.Rmd", package = "stmove"),
        output_file = file.path(path, paste0("report_", i, ".pdf")),
        params = modify_list(params, list(df = dplyr::filter(df, .data$id == i))),
        envir = new.env(parent = globalenv()),
        clean = TRUE
      )
    }
  } else {
    rmarkdown::render(system.file("reports", "report.Rmd", package = "stmove"),
      output_file = file.path(path, "report.pdf"),
      params = params,
      envir = new.env(parent = globalenv()),
      clean = TRUE
    )
  }
}
