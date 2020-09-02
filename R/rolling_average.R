#' Add a rolling average
#'
#' [rolling_average()] adds a rolling average to an [incidence2::incidence()]
#'   object.  If `x` is a grouped this will be a [dplyr::rowwise()] type object.
#'   If x is not grouped this will be a subclass of tibble.
#'
#' @param x An [incidence2::incidence] object.
#' @param before how many prior dates to group with.
#' @param ... Not currently used.
#'
#' @note If groups are present the average will be calculated across each
#' grouping, therefore care is required when plotting.
#'
#' @return An object of class `incidence2_rolling`.
#'
#' @examples
#' if (requireNamespace("outbreaks", quietly = TRUE) &&
#'     requireNamespace("incidence2", quietly = TRUE)) {
#'   data(ebola_sim_clean, package = "outbreaks")
#'   dat <- ebola_sim_clean$linelist
#'
#'   inci <- incidence2::incidence(dat,
#'                     date_index = date_of_onset,
#'                     interval = "week",
#'                     last_date = "2014-10-05",
#'                     groups = gender)
#'
#'   ra <- rolling_average(inci, before = 2)
#'   plot(ra, color = "white")
#'
#'
#'
#'   inci2 <- incidence2::regroup(inci)
#'   ra2 <- rolling_average(inci2, before = 2)
#'   plot(ra, color = "white")
#'
#' }
#'
#' @rdname rolling_average
#' @export
rolling_average <- function(x, ...) {
  UseMethod("rolling_average")
}

#' @rdname rolling_average
#' @aliases rolling_average.default
#' @export
rolling_average.default <- function(x, ...) {
  stop(sprintf("Not implemented for class %s",
               paste(class(x), collapse = ", ")))
}


#' @rdname rolling_average
#' @aliases rolling_average.incidence2
#' @export
rolling_average.incidence2 <- function(x, before = 2, ...) {
  ellipsis::check_dots_empty()
  group_vars <- incidence2::get_group_names(x)
  count_var <- incidence2::get_counts_name(x)
  date_var <- incidence2::get_dates_name(x)

  if (!is.null(group_vars)) {
    out <- dplyr::grouped_df(x, group_vars)
    out <- dplyr::summarise(
      out,
      rolling_average = list(ra(dplyr::cur_data(), date_var, count_var, before)),
      .groups = "drop"
    )

  } else {
    out <- ra(dat = x, date = date_var, count = count_var, before = before)
  }

  # create subclass of tibble
  out <- tibble::new_tibble(out,
                            groups = group_vars,
                            date = date_var,
                            count = count_var,
                            interval = incidence2::get_interval(x),
                            cumulative = attr(x, "cumulative"),
                            rolling_average = "rolling_average",
                            nrow = nrow(out),
                            class = "incidence2_rolling")

  attr(out, "date_group") <- attr(x, "date_group")
  tibble::validate_tibble(out)
}


ra <- function(dat, date, count, before = 2) {
  dat <- dplyr::arrange(dat, .data[[date]])
  dat <- dplyr::mutate(
    dat,
    rolling_average = slider::slide_dbl(
      .data[[count]],
      mean,
      .before = before,
      .complete = TRUE))
  dat
}


