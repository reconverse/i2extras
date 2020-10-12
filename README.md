
<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/incidence2)](https://CRAN.R-project.org/package=incidence2plus)
[![R build
status](https://github.com/reconhub/incidence2plus/workflows/R-CMD-check/badge.svg)](https://github.com/reconhub/incidence2plus/actions)
[![Codecov test
coverage](https://codecov.io/gh/reconhub/incidence2plus/branch/master/graph/badge.svg)](https://codecov.io/gh/reconhub/incidence2plus?branch=master)
<!-- badges: end -->

<br> **<span style="color: red;">Disclaimer</span>**

This package is a work in progress. Please reach out to the authors
before using.

# Scope

*incidence2* adds additional functionality to the
[incidence2](https://github.com/reconhub/incidence2) package.

# What does it do?

The main features of the package include:

  - **`fit_curve`** and **`growth_rate`**: fit a trend (poisson /
    negative binomial) to an `incidence()` object and calculate the
    associated growth rate.

  - **`add_rolling_average`**: add a rolling average to an `incidence()`
    object.

  - **`bootstrap()`**: generates a bootstrapped `incidence()` object by
    re-sampling, with replacement, the original dates of events.

  - **`find_peak()`**: locates the peak time of the epicurve.

  - **`estimate_peak()`**: uses bootstrap to estimate the peak time (and
    related confidence interval) of a partially observed outbreak. We
    can use **`tidy()`** to generate nicer looking output.

## Installing the package

Once it is released on [CRAN](https://CRAN.R-project.org), you will be
able to install the stable version of the package with:

``` r
install.packages("incidence2plus")
```

The development version can be installed from
[GitHub](https://github.com/) with:

``` r
if (!require(remotes)) {
  install.packages("remotes")
}
remotes::install_github("reconhub/incidence2plus", build_vignettes = TRUE)
```

# Resources

## Vignettes

An overview of *incidence2plus* is provided in the included vignettes:
\* `vignette("peak_estimation", package = "incidence2plus")`

## Getting help online

Bug reports and feature requests should be posted on *github* using the
[*issue* system](https://github.com/reconhub/incidence2plus/issues). All
other questions should be posted on the **RECON** slack channel see
<https://www.repidemicsconsortium.org/forum/> for details on how to
join.
