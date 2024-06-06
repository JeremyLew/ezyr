#' Format p-value
#'
#' `format_p_value` converts p-values in the raw numeric form
#' to a string for presenting in tables.
#'
#' @param p_value (numeric or numeric vector) p-value
#'
#' @return (character or character vector) formatted p-value
#' @export
#'
#' @examples
#' format_p_value(0.0001)
#' format_p_value(0.0086)
#' format_p_value(0.0097)
#' format_p_value(0.0456)
#' format_p_value(0.0495)
#'
#' # Rounding follows IEEE 754 standard so rounding of 5 may not be up
#' format_p_value(0.0085)
#' format_p_value(0.055)
#'
format_p_value <- function(p_value) {
  dplyr::case_when(
    is.na(p_value) ~ NA_character_,
    p_value < 0.001 ~ "<0.001",
    p_value < 0.01 & as.numeric(sprintf("%.3f", p_value)) < 0.01 ~ sprintf("%.3f", p_value),
    p_value < 0.01 & as.numeric(sprintf("%.3f", p_value)) == 0.01 ~ sprintf("%.2f", p_value),
    p_value < 0.05 & as.numeric(sprintf("%.2f", p_value)) >= 0.05 & signif(p_value, digits = 2) < 0.05 ~ format.pval(p_value, digits = 2),
    p_value < 0.05 & as.numeric(sprintf("%.2f", p_value)) >= 0.05 ~ format.pval(p_value, digits = 3),
    TRUE ~ sprintf("%.2f", p_value)
  )
}


#' Get presentation template for a generalised linear model
#'
#' `get_regression_presentation_template` creates a data.frame template that
#' lays out the variables in your model e.g. levels of a factor variable.
#'
#' `get_regression_presentation_template` has been incorporated into [tabulate_glm_result],
#' to create a table of your GLM results for presentation in an Rmarkdown document.
#'
#' @param model (glm object) A [stats::glm] model object
#'
#' @return (data.frame) A template for you to left join unto to present your regression results
#' @export
#'
#' @examples
#' library(dplyr)
#' if (require("mlbench") & require("forcats")) {
#'   data(BostonHousing2, package = "mlbench")
#'
#'   glm(medv ~ crim + zn + indus + town + rm + age + dis,
#'     data = BostonHousing2 %>%
#'       filter(town %in% c(
#'         "Newton", "Boston South Boston", "Boston Roxbury",
#'         "Somerville", "Boston Savin Hill", "Cambridge"
#'       )) %>%
#'       dplyr::mutate_at("town", forcats::fct_drop),
#'     family = gaussian(link = "identity")
#'   ) %>%
#'     get_regression_presentation_template()
#' }
#'
get_regression_presentation_template <- function(model) {
  variable_name <- c()
  var0 <- c()
  var <- c()

  for (term in labels(stats::terms(model$formula))) {
    term_ <- model$data[[term]]
    vlevels <- levels(model$data[[term]])
    var0 <- c(var0, "header")

    # Main variable names
    variable_name <- c(
      variable_name,
      ifelse(!is.null(labelled::var_label(term_)),
        labelled::var_label(term_), term
      )
    )

    # Categorical variables
    if (!is.null(vlevels)) {
      var <- c(var, "")
      for (i in 1:length(vlevels)) {
        variable_name <- c(variable_name, vlevels[i])
        var0 <- c(var0, ifelse(i == 1, "Ref", ""))
        var <- c(var, ifelse(i > 1, paste(term, vlevels[i], sep = ""), ""))
      }
    }
    # Continuous variables
    else {
      var <- c(var, term)
    }
  }

  return(
    data.frame(variable_name, var0, var) %>%
      dplyr::mutate(dplyr::across(.cols = c(var0, var), .fns = ~ replace(.x, .x == "", NA)))
  )
}


#' Tabulate GLM result
#'
#' `tabulate_glm_result` tabulates your GLM results for presentation
#' in an Rmarkdown document.
#'
#' @param model (glm object) A [stats::glm] model object
#' @param conf_lvl (numeric) Confidence level. The default value of 0.95 for 95% confidence level.
#'
#' By default, summary.glm function in R computes p-values using the Wald method.
#' To be consistent, we compute confidence intervals using the Wald method via [stats::confint.default].
#' See discussion in this [post](https://stats.stackexchange.com/questions/144603/why-do-my-p-values-differ-between-logistic-regression-output-chi-squared-test)
#' @param exponentiate (logical) TRUE if beta-coefficients are to be exponentiated and expressed as odds ratios.
#' If TRUE, confidence intervals will also be exponentiated correspondingly
#' @param num_dp (numeric) Number of decimal places to present beta coefficients/odds ratios and confidence intervals
#'
#' @return (data.frame) Table of your GLM results
#' @export
#'
#' @examples
#' library(dplyr)
#' if (require("mlbench") & require("forcats")) {
#'   data(BostonHousing2, package = "mlbench")
#'
#'   glm(medv ~ crim + town + rm + age + dis,
#'     data = BostonHousing2 %>%
#'       filter(town %in% c(
#'         "Newton", "Boston South Boston", "Boston Roxbury",
#'         "Somerville", "Boston Savin Hill", "Cambridge"
#'       )) %>%
#'       dplyr::mutate_at("town", forcats::fct_drop),
#'     family = gaussian(link = "identity")
#'   ) %>%
#'     tabulate_glm_result()
#' }
#'
tabulate_glm_result <- function(model,
                                conf_lvl = 0.95,
                                exponentiate = FALSE,
                                num_dp = 2) {
  exp_CI_ubound <- exp_CI_lbound <- CI_lbound <- CI_ubound <- exp_beta <- estimate <- NULL

  # Compute confidence intervals
  CI <- stats::confint.default(model, level = conf_lvl) %>%
    as.data.frame() %>%
    stats::setNames(c("CI_lbound", "CI_ubound"))

  # check that beta coefficients are within confidence intervals
  assertthat::assert_that(all(stats::coef(model) >= CI$CI_lbound))
  assertthat::assert_that(all(stats::coef(model) <= CI$CI_ubound))

  if (exponentiate) {
    results <- cbind(data.frame(stats::coef(summary(model))) %>% rename_colnames(), CI) %>%
      dplyr::rename(beta = estimate, p_value = dplyr::matches("^pr")) %>%
      dplyr::mutate(dplyr::across(
        .cols = c("beta", "CI_lbound", "CI_ubound"),
        .fns = exp,
        .names = "exp_{.col}"
      )) %>%
      dplyr::mutate(!!stringr::str_glue("exp(Beta) (exp({conf_lvl * 100}% CI))") :=
        sprintf(stringr::str_glue("%.{num_dp}f (%.{num_dp}f to %.{num_dp}f)"), exp_beta, exp_CI_lbound, exp_CI_ubound))
  } else {
    results <- cbind(data.frame(stats::coef(summary(model))) %>% rename_colnames(), CI) %>%
      dplyr::rename(beta = estimate, p_value = dplyr::matches("^pr")) %>%
      dplyr::mutate(!!stringr::str_glue("Beta ({conf_lvl * 100}% CI)") :=
        sprintf(stringr::str_glue("%.{num_dp}f (%.{num_dp}f to %.{num_dp}f)"), beta, CI_lbound, CI_ubound))
  }

  # format p-value
  results <- results %>% dplyr::mutate(dplyr::across(
    .cols = "p_value",
    .fns = format_p_value,
    .names = "p_value_"
  ))

  # join to template
  get_regression_presentation_template(model) %>%
    safe_left_join(results %>% tibble::rownames_to_column("var"),
      by = "var"
    )
}
