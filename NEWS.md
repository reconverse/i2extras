# i2extras (development version)

* Most updates due to changes in upstream incidence2 and trending packages.

* `find_peak()` is now a wrapper around the `keep_peaks()` function from 
  the underlying incidence2 package.

* `add_rolling_average()` is no longer a generic function and will now error
  if not called on an incidence2 object. It now returns the original input with
  additional rolling average columns but does not change the underlying objects
  class. Internally the code has been refactored to make use of
  `data.table::frollmean()`.
  
* Due to the changes to `add_rolling_average()` the plot method for
  incidence2_rolling objects has been removed.
  
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
