#' Build a report with basic spatio-temporal movement computations
#'
#' @param df a dataframe containing columns x, y, date representing relocations in space and time.
#' @param file a file name and path.
#'
#' @export
build_report <- function(file, df) {
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("Package rmarkdown must be installed to build reports. Please install it.",
      call. = FALSE
    )
  }

  params <- list(df = df) # needs to match params list in yaml

  # Knit the document, passing in the `params` list, and eval it in a
  # child of the global environment (this isolates the code in the document
  # from the code in this package).
  rmarkdown::render("report.Rmd",
    output_file = file,
    params = params,
    envir = new.env(parent = globalenv())
  )
}
