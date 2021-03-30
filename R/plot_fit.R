#' Plot a fitted epicurve
#'
#' @author Tim Taylor
#'
#' @param x An `incidence2_fit` object created by [fit_curve()].
#' @param cnt The count variable to print.  If `NULL` defaults to the first
#'   value from `attr(x, "counts")`.
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
#' @importFrom tidyr pivot_wider unnest
#' @export
plot.incidence2_fit <- function(x, cnt = NULL, include_warnings = FALSE,
                                ci = TRUE, pi = FALSE, ...) {


  # pull out the variables we need
  group_vars <- attr(x, "groups")
  date_var <- attr(x, "date")
  count_variable <- attr(x, "count_variable")
  counts <- attr(x, "counts")
  interval <- attr(x, "interval")
  cumulative <- attr(x, "cumulative")
  data_var <- attr(x, "data")
  model_var <- attr(x, "model")

  # filter to desired count or default to first one
  if (is.null(cnt)) {
    cnt <- counts[1]
  }
  if (!cnt %in% counts) {
    stop(
      sprintf("`%s` is not a valid count variable", deparse(substitute(cnt)))
    )
  }
  x <- x[x[[count_variable]] == cnt, ]

  # filter results to exclude errors and, optionally, warnings
  x <- is_ok(x, include_warnings = include_warnings)

  # remove unnecesary columns
  x[[model_var]] <- NULL
  x[[data_var]] <- NULL

  # unnest and widen data
  dat <- unnest(x, tidyr::everything())
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

  # ribbon colour
  col_model <- "#BBB67E"

  # add estimate (prediction) to plot
  graph <- graph +
    ggplot2::geom_line(
      mapping = ggplot2::aes(x = !!sym(date_var) + shift, y = .data$estimate)
    )

  # add confidence intervals to plot
  if (ci) {
    graph <- graph +
      ggplot2::geom_ribbon(ggplot2::aes(x = !!sym(date_var) + shift,
                                      ymin = .data$lower_ci,
                                      ymax = .data$upper_ci),
                           alpha = 0.5,
                           fill = col_model)
  }

  # add prediction intervals to plot
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
