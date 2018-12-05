#' Basic Distributions
#'
#' @param x a dataframe with columns: x, y, date, and id (optional)
#' @return an ltraj object and a plot of the turning angle or step length distribution
#' @export
#' @importFrom adehabitatLT dl ld
#' @importFrom graphics hist
#' @examples
#' \donttest{
#' ss_dist(AG195)
#' ta_dist(AG195)
#' }
#'
ss_dist <- function(x) {

  # do we want to handle multiple animals at once. What will that mean for plots?
  # do we want to return a vector? a data frame? an ltraj?

  # may want to add error handling for irregular data
  traj <- adehabitatLT::dl(x)

  hist(
    x = traj[[1]]$dist,
    xlab = "Step Length",
    main = paste0("Step Length Distribution \n dt = ", traj[[1]]$dt[1], " secs")
  )
  return(traj)
}


ta_dist <- function(x) {

  # include option for rel.angle v. abs.angle?

  traj <- adehabitatLT::dl(x)

  hist(
    x = traj[[1]]$abs.angle,
    xlab = "Turning Angle",
    main = paste0("(Absolute) Turning Angle Distribution \n dt = ", traj[[1]]$dt[1], " secs")
  )

  return(traj)
}
