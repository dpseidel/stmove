#' Wavelet analyses
#'
#'
#' @param df a dataframe with columns: x, y, date, and id (optional)
#' @param plot a logical indicating whether or not to return a histogram of the distribution
#' @param na.approx a logical indicating whether NA values should be interpolated using to `zoo::na.approx`
#' @export
#' @examples
#' \donttest{
#' dist_wavelet(AG195)
#' }
#' 
#' 
#' # Build morelet wavelet diagram based on step sizes over the time series
dist_wavelet <- function(df, plot = T, na.approx = T) {
  traj <- adehabitatLT::dl(df)
  dt <- traj[[1]]$dt[1]
  dist <- traj[[1]]$dist

  if (na.approx == T) {
    dist <- zoo::na.approx(dist)
  }

  dist.wave <- dplR::morlet(dist)

  if (plot == T) {
    dplR::wavelet.plot(dist.wave, crn.lab = gettext("Step Size"), useRaster = NA)
  }
}
