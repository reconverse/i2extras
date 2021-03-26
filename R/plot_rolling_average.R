#' Plot a rolling average incidence object
#'
#' @param x An `incidence2_ra` object created by [add_rolling_average()].
#' @param cnt The count variable to print.  If `NULL` defaults to the first
#'   value from `attr(x, "counts")`.
#' @param ... Additional arguments to be passed to
#'   [incidence2::plot.incidence2()] or [incidence2::facet_plot()].
#'
#' @return An incidence plot with the addition of a rolling average.  This will
#'   be facetted if the object is grouped.
#'
#' @importFrom rlang sym
#' @export
plot.incidence2_rolling <- function(x, cnt = NULL, ...) {

  # pull out the variables we need
  ra <- attr(x, "rolling_average")
  group_vars <- attr(x, "groups")
  date_var <- attr(x, "date")
  count_vars <- attr(x, "counts")
  count_variable <- attr(x, "count_variable")
  interval <- attr(x, "interval")
  cumulative <- attr(x, "cumulative")

  # filter to desired count or default to first one
  if (is.null(cnt)) {
    cnt <- count_vars[1]
  }
  if (!cnt %in% count_vars) {
    stop(
      sprintf("`%s` is not a valid count variable", deparse(substitute(cnt)))
    )
  }
  x <- x[x[[count_variable]] == cnt, ]

  # unnest and widen data
  dat <- unnest(x, cols = !!sym(ra))
  dat <- pivot_wider(dat, names_from = count_variable, values_from = "count")

  # create an incidence object to help with plotting
  dat <- minimal_incidence(
    dat,
    groups = group_vars,
    date = date_var,
    counts = cnt,
    interval = interval,
    cumulative = cumulative
  )

  # create single or facet_plot as appropriate
  if (is.null(group_vars)) {
    graph <- plot(dat, ...)
  } else {
    graph <- incidence2::facet_plot(dat, ...)
  }

  # Adjustment due to how dates are plotted (centred)
  if (inherits(dat[[date_var]], "period")) {
    shift <- mean(incidence2::get_interval(dat, integer = TRUE))/2
  } else {
    shift <-0
  }

  graph +
    ggplot2::geom_point(mapping = ggplot2::aes(x = !!sym(date_var) + shift,
                                               y = !!sym(ra)),
                        position = ggplot2::position_stack(),
                        size = 0.5) +
    ggplot2::geom_line(mapping = ggplot2::aes(x = !!sym(date_var) + shift,
                                              y = !!sym(ra)),
                       position = ggplot2::position_stack())
}
