#' Fit a regression model
#'
#' @param dat An [incidence2::incidence] object.
#' @param model The regression model to fit (can be "negbin" or "poisson").
#' @param alpha Value of alpha used to calculate confidence intervals; defaults
#'   to 0.05 which corresponds to a 95% confidence interval.
#' @param ... Not currently used.
#'
#' @return
#'   - `rolling_average` An [incidence2::incidence] like object with additional
#'     fitted values and associated confidence intervals.
#'
#'
#' @rdname fit
#' @export
fit <- function(dat, model, ...) {
  UseMethod("fit")
}

#' @rdname fit
#' @aliases fit.default
#' @export
fit.default <- function(dat, model, ...) {
  stop(sprintf("Not implemented for class %s",
               paste(class(dat), collapse = ", ")))
}


#' @rdname fit
#' @aliases fit.incidence2
#' @export
fit.incidence2 <- function(dat, model = c("negbin", "poisson"), alpha = 0.05, ...) {

  ellipsis::check_dots_empty()
  model <- match.arg(model)
  groups <- incidence2::get_group_names(dat)
  dates <- incidence2::get_dates_name(dat)
  count <- incidence2::get_counts_name(dat)
  fmla <- stats::as.formula(paste(count, "~", dates))

  out <- incidence2::as_tibble(dat)
  if (is.null(groups)) {
    model <- switch(
      model,
      negbin = MASS::glm.nb(fmla, data = out),
      poisson = stats::glm(fmla, data = out, family = stats::poisson),
      stop('Invalid model. Please use one of "negbin" or "poisson".'))
    out <- fitted_values(out, model, alpha)
    attr(out, "group_fit") <- FALSE
  } else {
    out <- dplyr::nest_by(grouped_df(out, groups))
    out <- dplyr::mutate(
      out,
      model = list(
        switch(
          model,
          negbin = MASS::glm.nb(fmla, data = .data$data),
          poisson = stats::glm(fmla, data = .data$data, family = stats::poisson),
          stop('Invalid model. Please use one of "negbin" or "poisson".')
        )),
      fitted = list(
        fitted_values(.data$data, model, alpha)
      ))
    attr(out, "group_fit") <- TRUE
    out$data <- NULL
  }


  # copy over original attributes
  attr(out, "groups2") <- incidence2::get_group_names(dat)
  attr(out, "date") <- incidence2::get_dates_name(dat)
  attr(out, "count") <- incidence2::get_counts_name(dat)
  attr(out, "interval") <- incidence2::get_interval(dat)
  attr(out, "cumulative") <- attr(dat, "cumulative")
  attr(out, "date_group") <- incidence2::get_date_group_names(dat)

  class(out) <- c("incidence2_fit", class(out))
  out
}

fitted_values <- function(x, fit, alpha) {
  inverse_link <- stats::family(fit)$linkinv
  pred <- stats::predict(fit, se.fit = TRUE)

  x$fit <- inverse_link(pred$fit)
  x$lower <- inverse_link(pred$fit + stats::qnorm(alpha/2) * pred$se.fit)
  x$upper <- inverse_link(pred$fit + stats::qnorm(1 - alpha/2) * pred$se.fit)
  x
}
