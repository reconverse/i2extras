#' Plot a rolling average incidence object
#'
#' @param x An `incidence2_ra` object created by [add_rolling_average()].
#' @param ... Additional arguments to be passed to
#'   [incidence2::plot.incidence2()] or [incidence2::facet_plot()].
#'
#' @return An incidence plot with the addition of a rolling average.  This will
#'   be facetted if the object is grouped.
#'
#' @importFrom rlang sym
#' @export
plot.incidence2_ra <- function(x, ...) {

  ra <- attr(x, "rolling_average")
  groups <- incidence2::get_group_names(x)
  date_var <- incidence2::get_dates_name(x)
  shift <- mean(incidence2::get_interval(x, integer = TRUE))/2

  if (is.null(groups)) {
    graph <- plot(x, ...)
  } else {
    graph <- incidence2::facet_plot(x, ...)
  }

  graph +
    ggplot2::geom_point(mapping = ggplot2::aes(x = !!sym(date_var) + shift,
                                               y = !!sym(ra)),
                        data = x,
                        position = ggplot2::position_stack()) +
    ggplot2::geom_line(mapping = ggplot2::aes(x = !!sym(date_var) + shift,
                                              y = !!sym(ra)),
                       data = x,
                       position = ggplot2::position_stack())
}
