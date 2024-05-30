#' Format p-value
#'
#' `format_p_value` converts p-values in the raw numeric form
#' to a string for presenting in tables
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
#' This is useful for creating a table of your regression results in an Rmarkdown document.
#'
#' @param model (glm object) A glm model object from the stats package
#'
#' @return (data.frame) A template for you to left join unto to present your regression results
#' @export
#'
#' @examples
#' library(dplyr)
#' library(mlbench)
#' data("BostonHousing2")
#'
#' glm(medv ~ crim + zn + indus + town + rm + age + dis,
#'   data = BostonHousing2 %>%
#'     filter(town %in% c(
#'       "Newton", "Boston South Boston", "Boston Roxbury",
#'       "Somerville", "Boston Savin Hill", "Cambridge"
#'     )) %>%
#'     mutate_at("town", forcats::fct_drop),
#'   family = gaussian(link = "identity")
#' ) %>%
#'   get_regression_presentation_template()
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
    variable_name <- c(variable_name,
                       ifelse(!is.null(labelled::var_label(term_)),
                              labelled::var_label(term_), term))

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
