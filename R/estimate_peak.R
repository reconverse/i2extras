#' Estimate the peak date of an incidence curve using bootstrap
#'
#' This function can be used to estimate the peak of an epidemic curve stored as
#' [incidence2::incidence] object, using bootstrap. See [bootstrap()] for more
#' information on the resampling.
#'
#' @author Thibaut Jombart \email{thibautjombart@@gmail.com}, with inputs on
#'   caveats from Michael Höhle.
#'
#' @details Input dates are resampled with replacement to form bootstrapped
#'   datasets; the peak is reported for each, resulting in a distribution of
#'   peak times. When there are ties for peak incidence, only the first date is
#'   reported.
#'
#' Note that the bootstrapping approach used for estimating the peak time makes
#' the following assumptions:
#'
#' - the total number of event is known (no uncertainty on total incidence)
#' - dates with no events (zero incidence) will never be in bootstrapped
#'   datasets
#' - the reporting is assumed to be constant over time, i.e. every case is
#'   equally likely to be reported
#'
#' @param x An [incidence2::incidence] object.
#'
#' @param n The number of bootstrap datasets to be generated; defaults to 100.
#'
#' @param alpha The type 1 error chosen for the confidence interval; defaults to
#'   0.05.
#'
#' @param progress Should a progress bar be displayed (default = TRUE)
#'
#' @return A tibble with the the following columns:
#' 
#' - `observed_date`: the date of peak incidence of the original dataset.
#' - `observed_count`: the peak incidence of the original dataset.
#' - `estimated`: the mean peak time of the bootstrap datasets.
#' - `lower_ci/upper_ci`: the confidence interval based on bootstrap datasets.
#' - `peaks`: a nested tibble containing the the peak times of the bootstrapped
#'   datasets.
#'
#' @seealso [bootstrap()] for the bootstrapping underlying this
#'   approach and [find_peak()] to find the peak in a single
#'   [incidence2::incidence] object.
#'
#' @examples
#' if (requireNamespace("outbreaks", quietly = TRUE) &&
#'     requireNamespace("incidence2", quietly = TRUE)) {
#'   withAutoprint( {
#'     # load data and create incidence
#'     data(fluH7N9_china_2013, package = "outbreaks")
#'     i <- incidence2::incidence(fluH7N9_china_2013, date_index = date_of_onset)
#'     i
#'
#'     # one simple bootstrap
#'     x <- bootstrap(i)
#'     x
#'
#'     # find 95% CI for peak time using bootstrap
#'     peak_data <- estimate_peak(i)
#'     peak_data
#'     summary(peak_data$peaks)
#'   })
#' }
#'
#' @export
estimate_peak <- function(x, n = 100, alpha = 0.05, progress = TRUE) {
  if (!inherits(x, "incidence2")) {
    stop("x is not an incidence2 object")
  }

  group_vars <- incidence2::get_group_names(x)
  
  if (!is.null(group_vars)) {
    f_groups <- lapply(suppressMessages(x[group_vars]), factor, exclude = NULL)
    split_x <- split(x, f_groups, sep = "-")
  } else {
    split_x = list(x)
  }


  out <- suppressMessages(
    lapply(
      1:length(split_x),
      function(i) {
        dat <- split_x[[i]]
        bootstrap_peak(incidence2::regroup(dat),
                       n = n,
                       alpha = alpha,
                       iteration = i,
                       num_iterations = length(split_x),
                       progress = progress)
      }
    )
  )
   
  out <- dplyr::bind_rows(out)
  if (!is.null(names(split_x))) {
    groupings <- as.data.frame(do.call(rbind, strsplit(names(split_x),"-")))
    names(groupings) <- names(f_groups)
    out <- dplyr::bind_cols(tibble::as_tibble(groupings), out)
  }
  out

}
# -------------------------------------------------------------------------


bootstrap_peak <- function(x, n = 100, alpha = 0.05, iteration = 1,
                           num_iterations = 1, progress = FALSE) {

  date_var <- incidence2::get_dates_name(x)
  count_var <- incidence2::get_counts_name(x)

  ## use it to find CI for epidemic peak
  observed <- find_peak(x)

  ## peaks on 'n' bootstrap samples
  if (progress) {
    if (num_iterations > 1) {
      msg <- sprintf("Group %s of %s; Estimating peaks from bootstrap samples:\n",
                     iteration,
                     num_iterations)
    } else {
      msg <- "Estimating peaks from bootstrap samples:\n"
    }

    message(msg)
    pb <- utils::txtProgressBar(min = 0, max = n, style = 1)
    peak_boot <- lapply(seq_len(n),
                        function(i) {
                          res <- find_peak(bootstrap(x))
                          utils::setTxtProgressBar(pb, i)
                          res
                        }
    )
    cat("\n\n")
  } else {
    peak_boot <- lapply(1:n, function(i) find_peak(bootstrap(x)))
  }


  ## convert to vector without losing Date class
  peak_boot <- dplyr::bind_rows(peak_boot)

  # store relevant stats
  estimated <- mean(peak_boot[[date_var]])
  QUANTILE <-
    if (inherits(peak_boot[[date_var]], c("Date", "POSIX"))) {
      quantile_Date
    } else {
      stats::quantile
    }
  ci <- QUANTILE(peak_boot[[date_var]], c(alpha / 2, 1 - alpha / 2))
  
  tibble::tibble(
      {{date_var}} := observed[[date_var]],
      observed_count = observed[[count_var]],
      estimated_date = estimated,
      lower_ci = ci[1],
      upper_ci = ci[2],
      peaks = list(peak_boot)
  )
}



# -------------------------------------------------------------------------
# quantiles for Date objects
quantile_Date <- function(x, ...) {
  if (!inherits(x, "Date")) {
    stop("'x' is not a 'Date' object")
  }

  first_date <- min(x, na.rm = TRUE)
  x_num <- as.numeric(x - min(x))
  out <- stats::quantile(x_num, ...)
  first_date + out
}
# -------------------------------------------------------------------------
