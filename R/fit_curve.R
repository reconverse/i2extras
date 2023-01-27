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
fit_curve.incidence2 <- function(
        x,
        model = c("poisson", "negbin"),
        alpha = 0.05,
        ...
) {

    # ensure model is poisson or negbin
    model <- match.arg(model)

    # get other variable names
    group_vars <- get_group_names(x)
    count_var <- get_count_variable_name(x)
    count_val <- get_count_value_name(x)
    date_var <- get_date_index_name(x)

    # fix for global variable warning
    dat <- NULL

    # nest by count_variable and group_vars
    grouping_variables <- c("count_variable", group_vars)
    out <- dplyr::nest_by(grouped_df(x, grouping_variables), .key = "data")

    # perform fitting and capture any warnings / errors
    fiterr <- lapply(
        out$data,
        function(dat, cnt) {
            fmla <- stats::as.formula(paste(count_val, "~", date_var))
            trending_model <- switch(
                model,
                negbin = trending::glm_nb_model(fmla, ...),
                poisson = trending::glm_model(fmla, family = "poisson", ...),
                stop('Invalid model. Please use one of "negbin" or "poisson".')
            )
            trending::fit(trending_model, dat, as_tibble = TRUE)
        }
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
                              count_variable = count_var,
                              counts = count_val,
                              data = "data",
                              model = "model",
                              fitted = "estimates",
                              warning_vars = warning_vars,
                              error_vars = error_vars,
                              nrow = nrow(out),
                              class = "incidence2_fit")
    tibble::validate_tibble(out)
}
