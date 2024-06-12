#' Rename column names
#'
#' `rename_colnames` cleans up column names of your raw data frame with
#' [make.names], replaces whitespaces with a separator of your choice
#' and converts to lower case.
#'
#' @param df (data.frame) Your data in the form of a data frame
#' @param separator (character) Separator to replace whitespaces
#' @param add_labels (logical) TRUE if you want original
#' column names to be used as labels, FALSE otherwise
#'
#' @return (data.frame) Your data with cleaned column names
#' @export
#'
#' @examples
#' if (require("tibble")) {
#'   print("Original data")
#'   data <- tibble::tibble(
#'     "Column A!!!" = sample(1:100, 5, replace = TRUE),
#'     "Column - 2;" = runif(5, min = 0, max = 1),
#'     "Column: (3)" = sample(letters, 5, replace = TRUE),
#'     "<Column 4...>" = sample(c(TRUE, FALSE), 5, replace = TRUE)
#'   )
#'   data %>% print()
#'
#'   print("Data after renaming columns")
#'   data %>%
#'     rename_colnames() %>%
#'     print()
#' }
#'
rename_colnames <- function(df, separator = "_", add_labels = TRUE) {
  # clean column names
  column_names <- make.names(colnames(df)) %>%
    stringr::str_replace_all("[.]{2,}", ".") %>%
    stringr::str_replace_all("[.]$", "") %>%
    stringr::str_replace_all("[.]", separator) %>%
    stringr::str_to_lower()

  # check for duplicated column names
  assertthat::assert_that(!any(duplicated(column_names)), msg = "Duplicated column names present")

  # add labels
  if (add_labels) {
    labels <- df %>%
      colnames() %>%
      purrr::set_names()
    labelled::var_label(df) <- labels
  }

  # rename column names
  colnames(df) <- column_names
  df
}


#' Safe left join
#'
#' `safe_left_join` is a wrapper around [dplyr::left_join] that does an additional
#' check for additional rows joined unto the left data frame.
#'
#' @param left_df (data.frame) Left data frame to left join to
#' @param right_df (data.frame) Right data frame to join unto the left data frame
#' @param ... (ellipsis) Extra arguments to pass into [dplyr::left_join]
#'
#' @return (data.frame) left_df, the left data frame
#' @export
#'
#' @examples
#' # Left table ----------------------------------------------------------------
#' left_table <- data.frame(
#'   ID = sprintf(
#'     "%s%s",
#'     sample(1000:2000, 5, replace = TRUE),
#'     sample(letters, 5, replace = TRUE)
#'   ),
#'   column_b = runif(5, min = 0, max = 1),
#'   column_c = sample(letters, 5, replace = TRUE)
#' )
#' print(left_table)
#'
#' # Right table ---------------------------------------------------------------
#' right_table <- data.frame(
#'   ID = left_table$ID,
#'   column_d = runif(5, min = 10, max = 50)
#' )
#' print(right_table)
#'
#' # Left table has same row count after join ----------------------------------
#' left_table %>%
#'   safe_left_join(right_table, by = "ID") %>%
#'   print()
#'
#' # Right table with duplicated ID --------------------------------------------
#' right_table2 <- rbind(right_table, data.frame(
#'   ID = right_table[3:4, "ID"],
#'   column_d = runif(2, min = 10, max = 50)
#' ))
#' print(right_table2)
#'
#' # Left table has more rows after join ---------------------------------------
#' try(
#'   left_table %>%
#'     safe_left_join(right_table2, by = "ID") %>%
#'     print()
#' )
safe_left_join <- function(left_df, right_df, ...) {
  PRE_JOIN_ROW_CNT <- nrow(left_df)
  left_df <- left_df %>% dplyr::left_join(right_df, ...)
  assertthat::assert_that(
    assertthat::are_equal(
      nrow(left_df), PRE_JOIN_ROW_CNT
    ),
    msg = "Row count after join not equals row count before join"
  )
  left_df
}


#' Label prefixed or suffixed dataframe columns
#'
#' `label_ix_columns` adds labels to column(s) of your data.frame which have the
#' particular prefix/suffix you specified, by copying the labels from
#' corresponding columns without the prefix/suffix.
#' The labels are added with [labelled::var_label()].
#'
#' @param df (data.frame) Your data in the form of a data frame
#' @param pattern (character) regex pattern to identify your prefix/suffix
#' @param suffix (logical) TRUE if the pattern is a suffix.
#' FALSE if the pattern is a prefix
#'
#' @return (data.frame) Your data with new labels added
#' @export
#'
#' @examples
#' library(labelled)
#'
#' # Example of suffix ---------------------------------------------------------
#' df <- data.frame(
#'   sex = sample(1:2, 5, replace = TRUE),
#'   race = sample(1:4, 5, replace = TRUE)
#' )
#' df$sex.factor <- c("Female", "Male")[df$sex]
#' df$race.factor <- c("Chinese", "Malay", "Indian", "Others")[df$race]
#'
#' # Set labels
#' labels <- list(
#'   sex = "Sex",
#'   race = "Race"
#' )
#' var_label(df) <- labels
#'
#' # Get labels after label_ix_columns()
#' var_label(label_ix_columns(df, ".factor", suffix = TRUE))
#'
#' # Example of prefix ---------------------------------------------------------
#' df <- data.frame(
#'   sex = sample(1:2, 5, replace = TRUE),
#'   race = sample(1:4, 5, replace = TRUE)
#' )
#' df$fct_sex <- c("Female", "Male")[df$sex]
#' df$fct_race <- c("Chinese", "Malay", "Indian", "Others")[df$race]
#'
#' # Set labels
#' labels <- list(
#'   sex = "Sex",
#'   race = "Race"
#' )
#' var_label(df) <- labels
#'
#' # Get labels after label_ix_columns()
#' var_label(label_ix_columns(df, "fct_", suffix = FALSE))
#'
label_ix_columns <- function(df, pattern, suffix = TRUE) {
  if (suffix) {
    pattern_ix <- stringr::str_glue("{pattern}$")
  } else { # prefix
    pattern_ix <- stringr::str_glue("^{pattern}")
  }

  for (col in colnames(dplyr::select(df, -dplyr::matches(pattern_ix)))) {
    if (suffix) {
      col_ix <- stringr::str_glue("{col}{pattern}")
    } else { # prefix
      col_ix <- stringr::str_glue("{pattern}{col}")
    }
    if ((col_ix %in% colnames(df)) & !is.null(labelled::var_label(df[[col]]))) {
      labelled::var_label(df[[col_ix]]) <- labelled::var_label(df[[col]])
    }
  }
  df
}
