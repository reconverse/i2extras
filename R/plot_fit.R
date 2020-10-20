#' Plot a fitted epicurve
#'
#' @param x An `incidence2_fit` object created by [fit()].
#' @param include_warnings Include results in plot that triggered warnings but
#'   not errors.  Defaults to `FALSE`.
#' @param ci Plot confidence intervals (defaults to TRUE).
#' @param pi Plot prediction intervals (defaults to FALSE).
#' @param ... Additional arguments to be passed to
#'   [incidence2::plot.incidence2()] or [incidence2::facet_plot()].
#'
#' @return An incidence plot with the addition of a fitted curve.  This will
#'   be facetted if the object is grouped.
#'
#' @importFrom rlang sym
#' @export
plot.incidence2_fit <- function(x, include_warnings = FALSE, 
                                ci = TRUE, pi = FALSE, ...) {

  
  group_vars <- attr(x, "groups")
  date_var <- attr(x, "date")
  count_var <- attr(x, "count")
  interval <- attr(x, "interval")
  cumulative <- attr(x, "cumulative")
  date_group <- attr(x, "date_group")

  x <- is_ok(x, include_warnings = include_warnings)
  x$model <- NULL

  dat <- tidyr::unnest(x, tidyr::everything())
  dat <- minimal_incidence(
    dat,
    groups = group_vars,
    date = date_var,
    count = count_var,
    interval = interval,
    cumulative = cumulative,
    date_group = date_group
  )

  if (is.null(group_vars)) {
    graph <- plot(dat, ...)
  } else {
    graph <- incidence2::facet_plot(dat, ...)
  }


  shift <- mean(incidence2::get_interval(dat, integer = TRUE))/2
  col_model <- "#BBB67E"

  graph <- graph +
    ggplot2::geom_point(
      mapping = ggplot2::aes(x = !!sym(date_var) + shift, y = .data$estimate),
      size = 0.5)
      
  if (ci) {
    graph <- graph +
      ggplot2::geom_ribbon(ggplot2::aes(x = !!sym(date_var) + shift,
                                      ymin = .data$lower_ci,
                                      ymax = .data$upper_ci),
                           alpha = 0.5,
                           fill = col_model)
  }

  if (pi) {
    graph <- graph +
      ggplot2::geom_ribbon(ggplot2::aes(x = !!sym(date_var) + shift,
                                      ymin = .data$lower_pi,
                                      ymax = .data$upper_pi),
                           alpha = 0.3,
                           fill = col_model)
  }

  graph
     
    
}
