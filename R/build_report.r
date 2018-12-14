#' Build a report with basic spatio-temporal movement computations
#'
#' @inheritParams interval_stats
#' @inheritParams rolling_stats
#' @param file a file name and path.
#' @param stats a character vector of stats to calculate, options include: "rolling",
#' "diurnal", "lunar", and "sseasonal
#' @param construct a character vector indicating which spacetime construction methods to use.
#' options include "klocoh" and "akde"
#' @param wavelet a logical indicating whether or not to produce wavelet visualization
#' @inheritParams construct
#'
#' @export
#' @examples
#' \donttest{
#' build_report(AG195, proj4 = "+proj=utm +zone=33 +south +datum=WGS84 +units=m +no_defs")
#' }
build_report <- function(df, file = "report.pdf", stats = c("rolling", "diurnal"),
                         construct = c("klocoh"), proj4,
                         seas = NULL, wavelet = T) {
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("Package rmarkdown must be installed to build reports. Please install it.",
      call. = FALSE
    )
  }

  params <- list(
    df = df, stats = stats, construct = construct,
    proj4 = proj4, seas = seas, wavelet = wavelet
  ) # needs to match params list in yaml

  # Knit the document, passing in the `params` list, and eval it in a
  # child of the global environment (this isolates the code in the document
  # from the code in this package).
  rmarkdown::render("report.Rmd",
    output_file = file,
    params = params,
    envir = new.env(parent = globalenv())
  )
}
