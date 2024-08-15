#' Do multicollinearity test
#'
#' `do_multicollinearity_test` is a wrapper function that prints the results from
#' [car::vif] and [mctest::imcdiag]
#'
#' @param model (glm object) A [stats::glm] model object
#' @param vif_threshold (numeric) The threshold above which is suggestive of the
#' presence of multicollinearity. Typical values are 5 or 10. The default value is 10.
#'
#' @return None
#' @export
#'
#' @examples
#' if (require("mlbench")) {
#'   # Load data -----------------------------------------------------------------
#'   data("PimaIndiansDiabetes2", package = "mlbench")
#'
#'   # Fit a logistic regression model -------------------------------------------
#'   model <- glm(diabetes ~ age + pregnant + glucose + pressure + mass + pedigree,
#'     data = PimaIndiansDiabetes2,
#'     family = binomial(link = "logit")
#'   )
#'
#'   # Run the test for multicollinearity ----------------------------------------
#'   model %>% do_multicollinearity_test()
#' }
#'
do_multicollinearity_test <- function(model, vif_threshold = 10) {
  # Fox, J. and Monette, G. (1992) Generalized collinearity diagnostics.JASA,87, 178–183
  cat("\n")
  cat(paste(rep("=", 80), collapse = ""), "\n")
  cat("Generalized variance inflation factors", "\n")
  cat(paste(rep("=", 80), collapse = ""), "\n")
  print(car::vif(model))

  # ============================================================================
  # mctest package
  # 1. https://cran.r-project.org/web/packages/mctest/mctest.pdf
  # 2. https://journal.r-project.org/archive/2016-2/imdadullah-aslam-altaf.pdf
  # 3. If multicollinearity is present, at least one eigenvalue of Gram matrix is 0
  # ============================================================================
  # If glm is a logistic regression model with a factor type dependent variable,
  # convert the dependent variable to numeric i.e. 0 and 1
  dependent_var <- all.vars(model$formula)[1]
  if (inherits(model, "glm") &
    (model$family$family == "binomial") & (model$family$link == "logit") &
    inherits(model$data[[dependent_var]], "factor") &
    (length(levels(model$data[[dependent_var]])) == 2)) {
    model <- stats::glm(model$formula,
      data = model$data %>%
        dplyr::mutate(dplyr::across(
          .cols = dependent_var,
          .fns = ~ as.numeric(.x) - 1
        )),
      family = model$family
    )
  }
  cat("\n")
  cat(paste(rep("=", 80), collapse = ""), "\n")
  cat("Individual variable multicollinearity diagnostic", "\n")
  cat(paste(rep("=", 80), collapse = ""), "\n")
  print(mctest::imcdiag(model, method = "VIF", vif = vif_threshold))
}
