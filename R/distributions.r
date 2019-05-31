#' Basic Distributions
#'
#' The first step to any movement analysis is to extract the step length and turning angle
#' distributions of trajectory. These functions will calculate step length and relative
#' turning angles for a given trajectory and plot associated histograms.
#'
#' @param x a dataframe with columns: x, y, date, and id (optional)
#' @param plot a logical indicating whether or not to return a histogram of the distribution
#' @return a numeric vector and (optionally) a plot of the turning angle or step length distribution
#' @importFrom ggplot2 geom_histogram labs theme_minimal
#' @export
#' @examples
#' \donttest{
#' ss <- ss_dist(AG195)
#' ta <- ta_dist(AG195)
#' }
#'
ss_dist <- function(x, plot = T) {

  # we could make these calculations ourselves without pulling in adehabitat
  # we should benchmark what is faster.

  # may want to add error handling for irregular data
  traj <- adehabitatLT::dl(x)
  dt <- traj[[1]]$dt[1]
  dist <- traj[[1]]$dist

  if (plot == T) {
    df = as.data.frame(dist)

    p = ggplot() + geom_histogram(aes(x = dist), bins = 30) +
      labs(
      x = "Step Length",
      title = paste0("Step Length Distribution \n dt = ", dt, " secs")
    ) + theme_minimal()

    print(p)
  }

  return(dist)
}


#' @export
#' @rdname ss_dist
ta_dist <- function(x, plot = T) {

  # include option for rel.angle v. abs.angle?

  traj <- adehabitatLT::dl(x)
  dt <- traj[[1]]$dt[1]
  ang <- traj[[1]]$rel.angle


  if (plot == T) {
    df = as.data.frame(ang)
    p = ggplot(df) + geom_histogram(aes(
      x = ang), binwidth = .1) + labs(
      x = "Turning Angle",
      title = paste0("Turning Angle Distribution \n dt = ", dt, " secs")
    ) + theme_minimal()
    print(p)
  }

  return(ang)
}
