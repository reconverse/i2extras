#' Fit an epi curve
#'
#' @author Tim Taylor
#'
#' @param x An [incidence2::incidence] object.
#' @param model The regression model to fit (can be "poisson" or "negbin").
#' @param alpha Value of alpha used to calculate confidence intervals; defaults
#'   to 0.05 which corresponds to a 95% confidence interval.
#' @param ... Additional arguments to pass to [stats::glm()] for
#'   `model = "poisson"` or [MASS::glm.nb()] for `model = "negbin"`.
#'
#' @return An object of class `incidence2_fit`.
#'
#' @importFrom dplyr nest_by
#' @importFrom tidyr pivot_longer
#' @export
fit_curve <- function(x, model, ...) {
  UseMethod("fit_curve")
}

#' @rdname fit_curve
#' @aliases fit_curve.default
#' @export
fit_curve.default <- function(x, model, ...) {
  not_implemented(x)
}


#' @rdname fit_curve
#' @aliases fit_curve.incidence2 incidence2_fit
#' @export
fit_curve.incidence2 <- function(x,
                                 model = c("poisson", "negbin"),
                                 alpha = 0.05,
                                 ...) {

  # ensure model is poisson or negbin
  model <- match.arg(model, several.ok = TRUE)

  # get other variable names
  group_vars <- incidence2::get_group_names(x)
  count_vars <- incidence2::get_counts_name(x)
  date_var <- incidence2::get_dates_name(x)

  # fix for global variable warning
  dat <- NULL

  # ensure model is of length one or the same length as counts_var
  if (length(model) == 1) model <- rep(model, length(count_vars))
  if (length(model) != length(count_vars)) {
    stop("model must be of length 1 or the same length as `count_var`")
  }
  model <- setNames(model, count_vars)

  # melt data for convenience
  out <- tidyr::pivot_longer(
    x,
    cols = count_vars,
    values_drop_na = TRUE,
    names_to = "count_variable",
    values_to = "count"
  )

  # nest by count_variable and group_vars
  grouping_variables <- c("count_variable", group_vars)
  out <- dplyr::nest_by(grouped_df(out, grouping_variables), .key = "data")

  # perform fitting and capture any warnings / errors
  fiterr <- mapply(
    function(dat, cnt) {
      mdl <- model[[cnt]]
      fmla <- stats::as.formula(paste("count", "~", date_var))
      trending_model <- switch(
        mdl,
        negbin = trending::glm_nb_model(fmla, ...),
        poisson = trending::glm_model(fmla, family = "poisson", ...),
        stop('Invalid model. Please use one of "negbin" or "poisson".')
      )
      trending::fit(trending_model, dat, as_tibble = TRUE)
    },
    out$data,
    count_vars,
    SIMPLIFY = FALSE
  )

  # perform prediction and capture any warnings / errors
  prederr <- lapply(fiterr, stats::predict, as_tibble = TRUE)
  prederr <- do.call(rbind, prederr)
  fiterr <- do.call(rbind, fiterr)

  # add columns to output
  out$model <- fiterr[[1]]
  out$estimates <- prederr[[1]]
  out$fitting_warning <- fiterr[[2]]
  out$fitting_error <- fiterr[[3]]
  out$prediction_warning <- prederr[[2]]
  out$prediction_error <- prederr[[3]]
  warning_vars <- c("fitting_warning", "prediction_warning")
  error_vars <- c("fitting_error", "prediction_error")

  # TODO - review which attributes are actually necessary
  # output a subclass of tibble
  out <- tibble::new_tibble(out,
                            groups = group_vars,
                            date = date_var,
                            count_variable = "count_variable",
                            counts = count_vars,
                            data = "data",
                            interval = incidence2::get_interval(x),
                            cumulative = attr(dat, "cumulative"),
                            model = "model",
                            fitted = "estimates",
                            warning_vars = warning_vars,
                            error_vars = error_vars,
                            nrow = nrow(out),
                            class = "incidence2_fit")
  tibble::validate_tibble(out)
}
