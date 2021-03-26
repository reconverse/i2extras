#' Add a rolling average
#'
#' [add_rolling_average()] adds a rolling average to an [incidence2::incidence()]
#'   object.  If `x` is a grouped this will be a [dplyr::rowwise()] type object.
#'   If x is not grouped this will be a subclass of tibble.
#'
#' @param x An [incidence2::incidence] object.
#' @param before how many prior dates to group the current observation with.
#'   Default is 2 days.
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
#'
#'   data(ebola_sim_clean, package = "outbreaks")
#'   dat <- ebola_sim_clean$linelist
#'   dat <- subset(dat, date_of_onset <= as.Date("2014-10-05"))
#'
#'   inci <- incidence2::incidence(dat,
#'                     date_index = date_of_onset,
#'                     interval = "week",
#'                     groups = gender)
#'
#'   ra <- add_rolling_average(inci, before = 2)
#'   plot(ra, color = "white")
#'
#'   inci2 <- incidence2::regroup(inci)
#'   ra2 <- add_rolling_average(inci2, before = 2)
#'   plot(ra, color = "white")
#'
#' }
#'
#' @rdname add_rolling_average
#' @export
add_rolling_average <- function(x, ...) {
  UseMethod("add_rolling_average")
}

#' @rdname add_rolling_average
#' @aliases add_rolling_average.default
#' @export
add_rolling_average.default <- function(x, ...) {
  stop(sprintf("Not implemented for class %s",
               paste(class(x), collapse = ", ")))
}


#' @rdname add_rolling_average
#' @aliases add_rolling_average.incidence2
#' @importFrom rlang .data
#' @export
add_rolling_average.incidence2 <- function(x, before = 2, ...) {

  ellipsis::check_dots_empty()

  # get variable names
  group_vars <- incidence2::get_group_names(x)
  count_vars <- incidence2::get_counts_name(x)
  date_var <- incidence2::get_dates_name(x)

  # nest by count_variable and group_vars
  grouping_variables <- c(count_vars, group_vars)
  out <- pivot_longer(x, cols = count_vars, names_to = "count_variable", values_to = "count")
  out <- dplyr::grouped_df(out, c("count_variable", group_vars))

  # add rolling average
  out <- dplyr::summarise(
    out,
    rolling_average = list(
      rolling_average(
        dplyr::cur_data(),
        date_var,
        "count",
        before + 1
      )
    ),
    .groups = "keep"
  )

  # create subclass of tibble
  out <- tibble::new_tibble(out,
                            groups = group_vars,
                            date = date_var,
                            count_variable = "count_variable",
                            counts = count_vars,
                            interval = incidence2::get_interval(x),
                            cumulative = attr(x, "cumulative"),
                            rolling_average = "rolling_average",
                            nrow = nrow(out),
                            class = "incidence2_rolling")
  tibble::validate_tibble(out)
}


rolling_average <- function(dat, date, count, width = 3) {
  dat <- dplyr::arrange(dat, .data[[date]])
  dat <- dplyr::mutate(
    dat,
    rolling_average = as.vector(
      {
        tmp <- .data[[count]]
        filter <- rep(1 / width, width)
        if (length(tmp) < length(filter)) {
          rep(NA, length(tmp))
        } else {
          stats::filter(tmp, filter, sides = 1)
        }
      }
    )
  )
  dat
}
