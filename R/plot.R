#' Add a rolling average to an incidence plot
#'
#' @param graph An incidence plot.
#' @param x An `incidence2_ra` object created by [add_rolling_average()].
#'
#' @return An incidence plot with the addition of a rolling average.
#'
#' @importFrom rlang sym
#' @export
plot_rolling_average <- function(graph, x) {

  if (!inherits(x, "incidence2_ra")) {
    stop("x must be an object with class `incidence_ra`")
  }

  ra <- attr(x, "rolling_average")
  if (!is.null(ra)) {
    shift <- mean(incidence2::get_interval(x, integer = TRUE))/2

    date_var <- incidence2::get_dates_name(x)
    graph <-
      graph +
      ggplot2::geom_point(mapping = ggplot2::aes(x = !!sym(date_var) + shift, y = !!sym(ra)),
                          data = x,
                          position = ggplot2::position_stack()) +
      ggplot2::geom_line(mapping = ggplot2::aes(x = !!sym(date_var) + shift, y = !!sym(ra)),
                         data = x,
                         position = ggplot2::position_stack())
  }

  graph

}




