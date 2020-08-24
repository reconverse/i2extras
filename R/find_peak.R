#' Find the peak date of an incidence curve
#'
#' This function can be used to find the peak of an epidemic curve stored as an
#' [incidence2::incidence] object.
#'
#' @author Thibaut Jombart \email{thibautjombart@@gmail.com}, Zhian N. Kamvar
#'   \email{zkamvar@@gmail.com}
#'
#' @md
#'
#' @param x An [incidence2::incidence] object.
#' @param regroup If `TRUE` (default), any groups will be regrouped before
#'   finding a peak. If `FALSE`, separate peaks will be found for each group.
#'
#' @return The date of the (first) highest incidence in the data.
#'
#' @seealso [estimate_peak()] for bootstrap estimates of the peak time
#'
#' @examples
#' if (requireNamespace("outbreaks", quietly = TRUE) &&
#'     requireNamespace("incidence2", quietly = TRUE)) {
#'
#'   withAutoprint( {
#'     # load data and create incidence
#'     data(fluH7N9_china_2013, package = "outbreaks")
#'     i <- incidence2::incidence(fluH7N9_china_2013, date_index = date_of_onset)
#'     i
#'
#'     find_peak(i)
#'   })
#' }
#' @importFrom rlang .data
#' @export
find_peak <- function(x, regroup = TRUE) {
  if (!inherits(x, "incidence2")) {
    stop(sprintf("`%s` is not an incidence object", deparse(substitute(x))))
  }

  count_var <- incidence2::get_counts_name(x)
  group_vars <- incidence2::get_group_names(x)

  if ((length(group_vars) > 0) && regroup) {
    msg <- paste("`%s` is stratified by groups",
                 "regrouping groups before finding peaks",
                 sep = "\n")
    message(sprintf(msg, deparse(substitute(x))))
    x <- regroup(x)

  } else if (length(group_vars) > 0) {
    x <- dplyr::grouped_df(x, group_vars)
  }

  res <- dplyr::slice_max(x, .data[[count_var]], n = 1, with_ties = FALSE)
  dplyr::ungroup(res)
}
