#' Basic Distributions
#'
#' The first step to any movement analysis is to extract the step length and turning angle
#' distributions of trajectory. These functions will calculate step length and relative
#' turning angles for a given trajectory and plot associated histograms.
#'
#' @param x a dataframe with columns: x, y, date, and id (optional)
#' @param plot a logical indicating whether or not to return a histogram of the distribution
#' @return a numeric vector and (optionally) a plot of the turning angle or step length distribution
#' @export
#' @examples
#' \donttest{
#' ss_dist(AG195)
#' ta_dist(AG195)
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
    hist(
      x = dist,
      xlab = "Step Length",
      main = paste0("Step Length Distribution \n dt = ", dt, " secs")
    )
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
    hist(
      x = ang,
      xlab = "Turning Angle",
      main = paste0("(Relative) Turning Angle Distribution \n dt = ", dt, " secs")
    )
  }

  return(ang)
}
