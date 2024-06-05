#' Plot a histogram from a MatchIt object
#'
#' `plot_propensity_score_histogram` plots a mirrored histogram which
#' enables visual inspection of the distribution of propensity scores
#' in the treatment vs control group.
#'
#' @param matchit_obj (matchit object) A matchit object from the MatchIt R package
#' @param trt_control_var (quosure) variable that stores the treatment/control grouping
#' @param trt_label (character) value for treatment group in trt_control_var (e.g. video_consult_grp)
#' @param control_label (character) value for control group in trt_control_var (e.g. face_to_face_grp)
#' @param is_after_matching (logical) TRUE for after-matching plot, FALSE for before-matching plot
#' @param num_bins (numeric) number of bins of the histogram
#'
#' @return ggplot object
#' @export
#'
#' @examples
#' library(MatchIt)
#'
#' MatchIt::matchit(
#'   treat ~ age + educ + race + married + nodegree + re78,
#'   data = lalonde %>% dplyr::mutate(trt_control = ifelse(treat == 1, "trt", "control")),
#'   method = "nearest",
#'   ratio = 2
#' ) %>%
#'   plot_propensity_score_histogram(trt_control, "trt", "control",
#'     is_after_matching = TRUE, num_bins = 30
#'   )
plot_propensity_score_histogram <- function(matchit_obj, trt_control_var,
                                            trt_label, control_label,
                                            is_after_matching = TRUE,
                                            num_bins = 30) {
  distance <- count <- NULL
  all_data <- matchit_obj$model$data %>% dplyr::mutate(distance = matchit_obj$model$fitted.values)
  matched_data <- MatchIt::match.data(matchit_obj)

  if (is_after_matching) {
    plot_data <- matched_data
    title_label <- "After Matching"
  } else {
    plot_data <- all_data
    title_label <- "Before Matching"
  }

  trt_grp <- plot_data %>%
    dplyr::filter({{ trt_control_var }} == trt_label) %>%
    dplyr::select(distance) %>%
    unlist()

  control_grp <- plot_data %>%
    dplyr::filter({{ trt_control_var }} == control_label) %>%
    dplyr::select(distance) %>%
    unlist()

  fills <- c("#D55E00", "#0072B2") %>% magrittr::set_names(c(trt_label, control_label))
  plot <- ggplot2::ggplot() +
    ggplot2::geom_histogram(
      data = data.frame(control_grp),
      ggplot2::aes(x = control_grp, y = ggplot2::after_stat(count / sum(count) * 100), fill = control_label),
      binwidth = diff(range(control_grp)) / num_bins
    ) +
    ggplot2::geom_histogram(
      data = data.frame(trt_grp),
      ggplot2::aes(x = trt_grp, y = -ggplot2::after_stat(count / sum(count) * 100), fill = trt_label),
      binwidth = diff(range(trt_grp)) / num_bins
    ) +
    ggplot2::labs(x = "Propensity score", y = "Percent (%)", title = title_label, fill = "Legend") +
    ggplot2::scale_fill_manual(values = fills)

  plot
}
