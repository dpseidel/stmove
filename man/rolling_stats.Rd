% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stats.r
\name{rolling_stats}
\alias{rolling_stats}
\title{Rolling Summary Statistics}
\usage{
rolling_stats(df, n_roll = NULL)
}
\arguments{
\item{df}{a dataframe containing columns "x", "y", and "date}

\item{n_roll}{numeric, number of fixes (e.g. width of the window) over which to
calculate rolling statistics. If NULL, the default is to roll over 3 hours for fix-rates
of 1 hour or less, and over 6 hours for fix rates occuring at intervals greater than 1 hour.
If your fix rate is every 4 hours or greater and n_roll is not set, the function
will warn you to set \code{n_roll}.}
}
\description{
Rolling Summary Statistics
}
\details{
some detailed discussion of how auto and cross correlation are calculated and
na handling
}
\examples{
roll <- rolling_stats(AG195)
}
\seealso{
\link[RcppRoll]{roll_mean} \link[RcppRoll]{roll_sdr} \link[TTR]{runCor}
}
