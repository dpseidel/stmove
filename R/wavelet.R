#' Wavelet analyses
#'
#' Build morelet wavelet diagrams for step size, turning angle, auto and cross correlation
#' coefficients through time.
#'
#' @param df a dataframe with columns: x, y, date
#' @param stats a character vector indicating which variables to calculate wavelet analysis on,
#' options in "dist", "rel.angle", "acf_dist", "acf_ang", and "ccf". By default, analysis is run on all 5.
#' @param plot a logical indicating whether or not to return a histogram of the distribution
#' @param ... Arguments passed to \link[dplR]{wavelet.plot}. Only relevant when `plot = T`.
#' @export
#' @examples
#' \donttest{
#' wavelet(AG195)
#' wavelet(AG195, c("dist", "angle"))
#' }
#' 
wavelet <- function(df, stats = c("dist", "rel.angle", "acf_dist", "acf_ang", "ccf"), plot = T, ...) {
  x <- rolling_stats(df)

  wave <- list()
  for (i in stats) {
    wave[[i]] <- dplR::morlet(na.omit(x[[i]])) # na.omit to remove leading 0's on acf and ccf
    if (plot == T) {
      dplR::wavelet.plot(wave[[i]], crn.lab = gettext(i), ...)
    }
  }

  return(wave)
}
