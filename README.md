
<!-- README.md is generated from README.Rmd. Please edit that file -->

# stmove

The goal of `stmove` is to make more transparent some basic spatial
temporal approaches to interpreting movement data before getting into
more challenging aspects of deconstructing movement trajectories.
Analyses in this package expect “clean, regular, movement data time
series” which consists of a sequence of points (x,y,time) where
time=0,1,2,3,….,T and all missing points have been interpolated and
filled in.

The primary function of this package is `build_report` which, given a
clean regularized trajectory, will deliver a .Rmd and .pdf report
including the results of the following computations.

## PROPOSED MINIMAL SET OF COMPUTATIONS

### The basic distributions:

1.  Generate Step size time series S={(t,s(t)) | over \[0,T\]} and plot
    step-size histogram
2.  Generate turning angle time series A={(t,a(t)) | over \[0,T\]} and
    plot turning angle distribution

### The basic stats:

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

### The basic visualizations.

1.  Plot the trajectory over space
2.  Generate a wavelet plot that allows us to visualize possible
    periodic components in s(t) and a(t), auto and cross correlation
    coefficients.

### The basic constructions:

1.  Construct 25%, 50% and 95% home range isopleths by season using two
    methods:

<!-- end list -->

  - 1 general space-time constructor using `t-locoh` hull sets
  - 1 autocorrelated utilization distribution analysis modeled after
    `ctmm::akde`

Once these are done, one can then pursue various kinds of analysis that
address questions of interest (e.g., GLM models of location and
landscape, HMM modeling, step section analysis), but this package
strives to set a standard for what is minimally needed before embarking
on such analyses.

## Installation

stmove is in active development. You can install the current version of
stmove with:

``` r
devtools::install_github("dpseidel/stmove")
```
