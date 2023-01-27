# i2extras (development version)

* Most updates due to changes in upstream incidence2 and trending packages.

* `find_peak()` is now a wrapper around the `keep_peaks()` function from 
  the underlying incidence2 package.

* `add_rolling_average()` is no longer a generic function and will now error
  if not called on an incidence2 object. Internally it now makes use of
  `data.table::frollmean()`.
  
* `estimate_peak()` can now (optionally) return multiple peaks.
  

# i2extras 0.1.2

* patch release due to changes in the upstream incidence2 package.

# i2extras 0.1.0

* updated to work with the latest version (1.0.0) of
[incidence2](https://CRAN.R-project.org/package=incidence2)

* `growth_rate()` now has an `include_warnings` parameter to match plot
functionality.

* Added `flag_low_counts()` function to highlight low counts below a specified
threshold.

# i2extras 0.0.2

* Initial release
