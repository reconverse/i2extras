#' Find the peak date of an incidence curve
#'
#' This function can be used to find the peak of an epidemic curve stored as an
#' [incidence2::incidence] object.
#'
#' @author Tim Taylor
#'
#' @param x An [incidence2::incidence] object.
#'
#' @return A tibble containing the date of the (first) highest incidence in the
#'   data along with the count. If `x` is grouped object then the output will
#'   have the peak calculated for each grouping.
#'
#' @seealso [estimate_peak()] for bootstrap estimates of the peak time.
#'
#' @examples
#' if (requireNamespace("outbreaks", quietly = TRUE)) {
#'
#'   # load data and create incidence
#'   data(fluH7N9_china_2013, package = "outbreaks")
#'   i <- incidence2::incidence(fluH7N9_china_2013, date_index = date_of_onset)
#'   find_peak(i)
#'
#' }
#'
#' @import data.table
#' @importFrom utils head
#' @export
find_peak <- function(x) {

  if (!inherits(x, "incidence2")) {
    stop(sprintf("`%s` is not an incidence object", deparse(substitute(x))))
  }

  # get relevant column names
  date_var <- incidence2::get_dates_name(x)
  count_vars <- incidence2::get_counts_name(x)
  group_vars <- incidence2::get_group_names(x)

  # convert to data.table
  dat <- as.data.table(x)

  # if more than one count variable melt to one column and add to grouping variables
  if (length(count_vars) > 1) {
    dat <- melt(
      dat,
      id.vars = c(group_vars, date_var),
      measure.vars = count_vars,
      na.rm = TRUE,
      variable.name = "count_variable",
      value.name = "count",
      verbose=FALSE
    )
    group_vars <- c(group_vars, "count_variable")
    count_vars <- "count"
  }

  # order in ascending order of groups and then descending order of counts
  setorderv(dat, cols = c(group_vars, count_vars), order = c(rep(1, length(group_vars)), -1), na.last = TRUE)

  # If no groups just return the ordered dat else the head value for each group
  if (is.null(group_vars)) {
    out <- dat[1]
  } else {
    out <- dat[, head(.SD, 1), by = group_vars]
  }

  # convert to tibble
  new_bare_tibble(out)
}
