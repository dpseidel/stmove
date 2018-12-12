#' Build a report with basic spatio-temporal movement computations
#'
#' @inheritParams interval_stats
#' @param file a file name and path.
#' @param stats
#'
#' @export
build_report <- function(file, df, stats = c("rolling", "diurnal"),
                         construct = c("klocoh"), proj4,
                         na.approx = T, seas = NULL) {
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("Package rmarkdown must be installed to build reports. Please install it.",
      call. = FALSE
    )
  }

  params <- list(
    df = df, stats = stats, na.approx = na.approx,
    construct = construct, proj4 = proj4
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
