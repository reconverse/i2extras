
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

  - **`rolling_average`** and **`remove_rolling`**: add or remove a
    rolling average to/from an `incidence()` object.

  - **`bootstrap()`**: generates a bootstrapped `incidence()` object by
    re-sampling, with replacement, the original dates of events.

  - **`find_peak()`**: locates the peak time of the epicurve.

  - **`estimate_peak()`**: uses bootstrap to estimate the peak time (and
    related confidence interval) of a partially observed outbreak. We
    can use **`tidy()`** to generate nicer looking output.
