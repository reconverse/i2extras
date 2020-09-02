#' Plot a rolling average incidence object
#'
#' @param x An `incidence2_fit` object created by [fit()].
#' @param ... Additional arguments to be passed to
#'   [incidence2::plot.incidence2()] or [incidence2::facet_plot()].
#'
#' @return An incidence plot with the addition of a fitted curve.  This will
#'   be facetted if the object is grouped.
#'
#' @importFrom rlang sym
#' @export
plot.incidence2_fit <- function(x, ...) {

  grouped_fit <- attr(x, "group_fit")
  x$model <- NULL
  dat <- tidyr::unnest(x, tidyr::everything())
  dat <- dplyr::ungroup(dat)

  if (grouped_fit) {
    dat <- copy_over_attributes(from = x, to = dat)
    class(dat) <- c("incidence2", class(dat))
    graph <- incidence2::facet_plot(dat, ...)
  } else {
    dat <- x
    dat <- copy_over_attributes(from = x, to = dat)
    class(dat) <- c("incidence2", class(dat))
    graph <- plot(dat, ...)
  }

  date_var <- incidence2::get_dates_name(dat)
  shift <- mean(incidence2::get_interval(dat, integer = TRUE))/2

  col_model <- "#BBB67E"

  graph +
    ggplot2::geom_point(
      data = dat,
      mapping = ggplot2::aes(x = !!sym(date_var) + shift,
                             y = .data$fit)) +
    ggplot2::geom_ribbon(
      data = dat,
      ggplot2::aes(x = !!sym(date_var) + shift,
                   ymin = .data$lower,
                   ymax = .data$upper),
      alpha = 0.4,
      fill = col_model)
}


copy_over_attributes <- function(from, to) {
  attr(to, "groups") <- attr(from, "groups2")
  attr(to, "date") <- attr(from, "date")
  attr(to, "count") <- attr(from, "count")
  attr(to, "interval") <- attr(from, "interval")
  attr(to, "cumulative") <- attr(from, "cumulative")
  attr(to, "date_group") <- attr(from, "date_group")
  to
}
