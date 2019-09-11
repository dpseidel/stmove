
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Travis Build
Status](https://travis-ci.com/dpseidel/stmove.svg?token=ZVrezsGfh5uSAe6FpgAU&branch=master)](https://travis-ci.com/dpseidel/stmove)
[![Codecov test
coverage](https://codecov.io/gh/dpseidel/stmove/branch/master/graph/badge.svg?token=A1gUYaWSSY)](https://codecov.io/gh/dpseidel/stmove)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![CRAN
status](https://www.r-pkg.org/badges/version/stmove)](https://cran.r-project.org/package=stmove)

# stmove

The goal of stmove is to make more accessible and transparent standard
spatio-temporal approaches to interpreting movement data before getting
into more challenging aspects of deconstructing movement trajectories.
Analyses in this package expect “clean, regular, movement data time
series” which consists of a sequence of points `(x, y, time)` where
`time = 0, 1, 2, 3,..., T` and all missing points have been interpolated
and filled in.

For a detailed review of stmove’s motivation and functionality, please
see [our preprint available on
Biorxiv](http://biorxiv.org/cgi/content/short/758987v1).

## Installation

stmove is in active development and not yet available on CRAN. To
download the current version of the package use the following code:

``` r
# install.packages("remotes")
remotes::install_github("dpseidel/stmove")
```

If you encounter bugs or have features you would like to see
incorporated in future version of stmove, please open an issue.

## Usage

The primary function of this package is `build_report` which, given a
clean regularized trajectory, will deliver a .Rmd and .pdf report
including the results of the following computations.

### Basic path distributions:

1.  Generate step size time series \(S={(t,s(t)) | over [0,T]}\) and
    plot step-size histogram
2.  Generate turning angle time series \(A={(t,a(t)) | over [0,T]}\) and
    plot turning angle distribution

### Basic path statistics:

1.  If data is sub hourly, plot running means, standard deviations, and
    auto correlations for s(t) and a(t), plus cross-correlation
    s(t)-a(t) (basic stats collection: BSC) using a 3 hour (or 6 hour if
    data is only hourly) “sliding time window” (STW)
2.  Generate and plot 12-hourly BSC using a 12 hour “jumping time
    window” (JTW)
3.  Generate and plot 14/15 day BSC following a new-full-new moon
    sequence using a JTW
4.  Generate and plot seasonal BSC for how ever many seasons are
    available using a JTW

### Basic path visualizations.

1.  Plot the trajectory over space
2.  Generate a wavelet plot that allows us to visualize possible
    periodic components in s(t) and a(t), auto and cross correlation
    coefficients.

### Basic space constructions:

1.  Construct 25%, 50% and 95% home range isopleths (optionally by
    season) using two methods:
    1.  `k-LoCoH` hull sets
    2.  autocorrelated utilization distribution analysis implemented
        with `ctmm::akde`

Once these are done, one can then pursue various kinds of analysis that
address questions of interest (e.g., GLM models of location and
landscape, HMM modeling, step section analysis), but this package
strives to set a standard for what is minimally needed before embarking
on such analyses.
