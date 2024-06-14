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


#' Extract latest reading in specified window period of observation
#'
#' Suppose that you have the following, `extract_reading_from_interval` will
#' extract the latest laboratory reading for each patient within the respective
#' window periods to be considered as the "baseline" reading, "3-month" reading etc.
#' 1. a set of longitudinal data e.g. laboratory readings of patients
#' 1. a set of parameters that define the window period of observation for
#'    considering readings that would constitute as "baseline" reading,
#'    "3-month" reading etc. for example:
#'     - _baseline_: T-180 days to T
#'     - _3-month_: T+31 days to T+90 days
#'     - where T is the index date which could be the enrolment date or
#'       date of first visit
#'
#' @param df_master (data.frame) Master dataframe to join the readings unto.
#' _df_master_ must minimally contain the following 2 columns:
#' - A column of IDs to identify each patient
#' - A column of index dates (represented by T as described above) for each patient
#' @param df_master_fields (list) A named list to identify the _ID_ and
#' _index date_ columns in _df_master_
#' - It takes on this format: `list(ID = <your_ID_column>, index_date = <your_index_date_column>)`
#' - For example: `list(ID = "patient_id", index_date = "enrolment_date")`
#' @param df_long (data.frame) Longitudinal data e.g. laboratory readings
#' @param df_long_fields (list) A named list to identify the _reading date_ and _reading(s)_ in _df_long_
#' - It takes on this format: `list(reading_date = <your_reading_date_column>, reading = <your_reading_column(s)>)`
#'     - _reading_date_ (character) The name of the dataframe column that contains the dates of the readings
#'     - _reading_ (character or character vector) The name(s) of the dataframe column(s) that contains the readings
#' - For example: `list(reading_date = "calendar_date", reading = c("bp_systolic", "bp_diastolic"))`
#' @param params (list) A named list where the names are the timepoints and the
#' values are numeric vectors that represent `c(lower_bound, upper_bound)`
#' - For example: `params <- list(baseline = c(-180, 0), "3mth" = c(31, 90), "6mth" = c(120, 180))`
#'
#' @return (data.frame) _df_master_ with longitudinal readings joined unto
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' # Generate fake patient data for illustration -------------------------------
#' generate_fake_patient_data <- function(id) {
#'   data.frame(
#'     list(
#'       patient_id = id,
#'       enrolment_date = Sys.Date() + runif(1, min = -730, max = 0),
#'       bp_systolic = sample(30:160, 9, replace = FALSE),
#'       bp_diastolic = sample(10:100, 9, replace = FALSE),
#'       hba1c = sample(4:14, 9, replace = FALSE)
#'     ),
#'     time = c(rep("baseline", 3), rep("6mth", 3), rep("12mth", 3))
#'   ) %>%
#'     mutate(visit_date = case_when(
#'       time == "baseline" ~ enrolment_date + sample(-180:0, 3, replace = FALSE),
#'       time == "6mth" ~ enrolment_date + sample(100:180, 3, replace = FALSE),
#'       time == "12mth" ~ enrolment_date + sample(240:360, 3, replace = FALSE)
#'     ))
#' }
#'
#' df_long <- rbind(
#'   generate_fake_patient_data("P01"),
#'   generate_fake_patient_data("P02"),
#'   generate_fake_patient_data("P03")
#' ) %>%
#'   dplyr::arrange(patient_id, visit_date)
#'
#' # Master dataframe
#' df_master <- df_long %>% distinct(patient_id, enrolment_date)
#' print(df_master)
#'
#' # Longitudinal data of readings
#' df_long <- df_long %>% select(-enrolment_date)
#' print(df_long)
#'
#' # Extract readings ----------------------------------------------------------
#' df_master %>%
#'   # Blood pressure
#'   extract_reading_from_interval(
#'     list(
#'       ID = "patient_id",
#'       index_date = "enrolment_date"
#'     ),
#'     df_long,
#'     list(
#'       reading_date = "visit_date",
#'       reading = c("bp_systolic", "bp_diastolic")
#'     ),
#'     list(
#'       baseline = c(-180, 0),
#'       "6mth" = c(100, 180),
#'       "12mth" = c(240, 360)
#'     )
#'   ) %>%
#'   # HbA1c
#'   extract_reading_from_interval(
#'     list(
#'       ID = "patient_id",
#'       index_date = "enrolment_date"
#'     ),
#'     df_long,
#'     list(
#'       reading_date = "visit_date",
#'       reading = "hba1c"
#'     ),
#'     list(
#'       baseline = c(-180, 0),
#'       "6mth" = c(100, 180),
#'       "12mth" = c(240, 360)
#'     )
#'   ) %>%
#'   print()
#'
extract_reading_from_interval <- function(df_master,
                                          df_master_fields,
                                          df_long,
                                          df_long_fields,
                                          params) {
  days_diff <- NULL

  # check that df_master_fields have <ID> and <index_date>
  for (field in c("ID", "index_date")) {
    assertthat::assert_that(field %in% names(df_master_fields),
                            msg = stringr::str_glue("<{field}> missing from df_master_fields"))
  }

  # check that df_long_fields have <reading_date> and <reading>
  for (field in c("reading_date", "reading")) {
    assertthat::assert_that(field %in% names(df_long_fields),
                            msg = stringr::str_glue("<{field}> missing from df_long_fields"))
  }

  # check that fields specified in df_master_fields are found in df_master
  for (field in df_master_fields) {
    assertthat::assert_that(field %in% colnames(df_master),
      msg = stringr::str_glue("{field} specified in df_master_fields not found in df_master")
    )
  }

  # check that fields specified in df_long_fields are found in df_long
  for (field in unlist(df_long_fields)) {
    assertthat::assert_that(field %in% colnames(df_long),
      msg = stringr::str_glue("{field} specified in df_long_fields not found in df_long")
    )
  }

  # check that df_long has <ID> field
  assertthat::assert_that(df_master_fields$ID %in% colnames(df_long),
    msg = stringr::str_glue("{df_master_fields$ID} not found in df_long")
  )

  # validate lower & upper bounds of params
  for (timepoint in names(params)) {
    assertthat::assert_that(params[[timepoint]][1] <= params[[timepoint]][2],
      msg = stringr::str_glue("{timepoint} lower bound must be less than or equal to {timepoint} upper bound")
    )
  }

  # convert <reading_date> to lubridate's Date datatype (if not already)
  if (!lubridate::is.Date(df_long[[df_long_fields$reading_date]])) {
    df_long <- df_long %>% dplyr::mutate(dplyr::across(
      .cols = df_long_fields$reading_date,
      .fns = lubridate::as_date
    ))
    message(stringr::str_glue("df_long <{df_long_fields$reading_date}> converted to lubridate Date type"))
  }

  # if <index_date> exists in df_long, drop it and join from df_master
  if (df_master_fields$index_date %in% colnames(df_long)) {
    df_long <- df_long %>% dplyr::select(-df_master_fields$index_date) 
    message(stringr::str_glue("<{df_master_fields$index_date}> is dropped from df_long,
                               instead it will be taken from df_master"))
  }

  # join <index_date> from df_master onto df_long
  df_long <- df_long %>%
    dplyr::left_join(
      df_master %>%
        dplyr::select(df_master_fields$ID, df_master_fields$index_date),
      by = df_master_fields$ID,
      relationship = "many-to-many"
    ) %>%
    dplyr::arrange(.data[[df_master_fields$ID]], .data[[df_long_fields$reading_date]]) %>%
    dplyr::mutate(days_diff = .data[[df_long_fields$reading_date]] - .data[[df_master_fields$index_date]])

  # join readings
  for (timepoint in names(params)) {
    df_master <- df_master %>%
      safe_left_join(
        df_long %>%
          dplyr::filter(data.table::between(days_diff, params[[timepoint]][1], params[[timepoint]][2])) %>%
          dplyr::group_by(.data[[df_master_fields$ID]], .data[[df_master_fields$index_date]]) %>%
          dplyr::slice_max(days_diff, n = 1, with_ties = FALSE) %>%
          dplyr::select(df_master_fields$ID, df_master_fields$index_date, df_long_fields$reading_date, df_long_fields$reading) %>%
          dplyr::rename(!!stringr::str_glue("{df_long_fields$reading[1]}_date_{timepoint}") := !!rlang::sym(df_long_fields$reading_date)) %>%
          dplyr::rename_with(
            .fn = ~ stringr::str_glue("{.x}_{timepoint}"),
            .cols = dplyr::all_of(df_long_fields$reading)
          ),
        by = c(df_master_fields$ID, df_master_fields$index_date)
      )
  }

  df_master
}