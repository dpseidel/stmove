#' Wavelet analyses
#'
#'
#' @param x a dataframe with columns: x, y, date, and id (optional)
#' @param plot a logical indicating whether or not to return a histogram of the distribution
#' @export
#' @importFrom adehabitatLT dl
#' @importFrom biwavelet morelet
#' @importFrom biwavelet wavelet.plot
#' @examples
#' \donttest{
#' ss_dist(AG195)
#' ta_dist(AG195)
#' }
#' 

# Build morelet wavelet diagram based on step sizes over the time series
dist_wavelet <- function(x, plot = T) {
    
    traj <- adehabitatLT::dl(x)
    dt <- traj[[1]]$dt[1]
    dist <- traj[[1]]$dist
    
    dist.wave <- morelet(dist)
    
    if (plot == T) {
        wavelet.plot(dist.wave, na.rm=TRUE, crn.lab = gettext("Step Size"))
    }
}