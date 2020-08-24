#' Bootstrap incidence time series
#'
#' This function can be used to bootstrap [incidence2::incidence] objects.
#' Bootstrapping is done by sampling with replacement the original input dates.
#' See `details` for
#' more information on how this is implemented.
#'
#' @author Thibaut Jombart, Tim Taylor
#'
#' @details As original data are not stored in [incidence2::incidence] objects,
#'   the bootstrapping is achieved by multinomial sampling of date bins weighted
#'   by their relative incidence.
#'
#' @param x An [incidence2::incidence] object.
#'
#' @param randomise_groups A `logical` indicating whether groups should be
#'   randomised as well in the resampling procedure; respective group sizes will
#'   be preserved, but this can be used to remove any group-specific temporal
#'   dynamics. If `FALSE` (default), data are resampled within groups.
#'
#' @return An `incidence` object.
#'
#' @seealso [find_peak()] to use estimate peak date using bootstrap
#'
#' @examples
#' if (requireNamespace("outbreaks", quietly = TRUE) &&
#'     requireNamespace("incidence2", quietly = TRUE)) {
#'   withAutoprint( {
#'     data(fluH7N9_china_2013, package = "outbreaks")
#'     i <- incidence2::incidence(fluH7N9_china_2013, date_index = date_of_onset)
#'     i
#'
#'     x <- bootstrap(i)
#'     x
#'    })
#' }
#' @importFrom dplyr select grouped_df summarise n all_of
#' @importFrom rlang :=
#' @export
bootstrap <- function(x, randomise_groups = FALSE) {

  if (!inherits(x, "incidence2")) stop("x is not an incidence2 object")

  count_var <- incidence2::get_counts_name(x)
  group_vars <- incidence2::get_group_names(x)
  date_var <- incidence2::get_dates_name(x)


  tbl <- suppressMessages(select(x, !all_of(count_var)))
  tbl <- dplyr::slice_sample(tbl,
                             n = sum(x[[count_var]]),
                             weight_by = x[[count_var]],
                             replace = TRUE)


  tbl <- grouped_df(tbl, c(date_var, group_vars))
  tbl <- summarise(tbl, {{count_var}} := n(), .groups = "drop")

  if (randomise_groups) {
    for (gr in group_vars) {
      tbl[[gr]] <- sample_(tbl[[gr]])
    }
  }

  # create subclass of tibble
  tbl <- tibble::new_tibble(tbl,
                            groups = group_vars,
                            date = date_var,
                            count = count_var,
                            interval = incidence2::get_interval(x),
                            cumulative = attr(x, "cumulative"),
                            nrow = nrow(tbl),
                            class = "incidence2"
  )
  tibble::validate_tibble(tbl)
}
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
# A fix for the behaviour of `sample` when first argument is of length 1.
sample_ <- function(x, ...) {
  x[sample.int(length(x), ...)]
}
# -------------------------------------------------------------------------


