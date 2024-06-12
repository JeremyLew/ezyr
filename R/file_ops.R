#' Export to excel
#'
#' `export_to_excel` enables you to export your data out to an excel workbook.
#'
#' @param output_table (data.frame) Data that you want to export to excel
#' @param workbook (character) File path of excel workbook
#' - If the workbook does not exist, it will first be created
#' - If the workbook exists, data will be exported to the specified sheet
#' @param sheet_name (character) Name of excel sheet to export data to
#' - Defaults to system time at execution if none provided
#'
#' @return None
#' @export
#'
#' @examples
#' # Load data
#' data(BostonHousing2, package = "mlbench")
#'
#' # Export data to an excel file in temp directory
#' output_filepath_1 <- file.path(tempdir(), "file1.xlsx")
#' export_to_excel(BostonHousing2, output_filepath_1, sheet_name = "my_sheet")
#' print(list.files(tempdir(), pattern = "\\.(xlsx|xls)$"))
#'
#' # Delete the excel file in temp directory
#' unlink(output_filepath_1)
#' print(list.files(tempdir(), pattern = "\\.(xlsx|xls)$"))
#'
export_to_excel <- function(output_table,
                            workbook,
                            sheet_name = format(Sys.time(), "%d%b%Y_%H%M")) {
  if (!file.exists(workbook)) {
    wb <- openxlsx::createWorkbook()
  } else {
    wb <- openxlsx::loadWorkbook(workbook)
  }
  openxlsx::addWorksheet(wb, sheet_name)
  openxlsx::writeData(wb, sheet = sheet_name, output_table)
  openxlsx::saveWorkbook(wb, file = workbook, overwrite = TRUE)
}


#' Export dataframe labels to excel
#'
#' `export_dataframe_labels` exports the variable labels of a dataframe, which
#' are created by [labelled::var_label], to an excel sheet.
#'
#' @param data (data.frame) Your dataframe whose variable labels you want to export to excel
#' @param workbook (character) File path of excel workbook
#' @param sheet_name (character) Name of excel sheet to export labels to
#' - Defaults to system time at execution if none provided
#'
#' @return None
#' @export
#'
#' @examples
#' library(labelled)
#'
#' df <- data.frame(
#'   column_a = sample(letters, 3),
#'   column_b = runif(3, 1, 10)
#' )
#' labels <- list(
#'   column_a = "This is column A",
#'   column_b = "This is column B"
#' )
#' var_label(df) <- labels
#'
#' # Export column labels to an excel file in temp directory
#' output_filepath_1 <- file.path(tempdir(), "file1.xlsx")
#' export_dataframe_labels(df, output_filepath_1, sheet_name = "my_sheet")
#' print(list.files(tempdir(), pattern = "\\.(xlsx|xls)$"))
#'
#' # Delete the excel file in temp directory
#' unlink(output_filepath_1)
#' print(list.files(tempdir(), pattern = "\\.(xlsx|xls)$"))
#'
export_dataframe_labels <- function(data,
                                    workbook,
                                    sheet_name = format(Sys.time(), "%d%b%Y_%H%M")) {
  data.frame(labels = labelled::var_label(data, unlist = T)) %>%
    tibble::rownames_to_column(var = "columns") %>%
    export_to_excel(workbook, sheet_name = sheet_name)
}


#' Ask for passkey
#'
#' `ask_for_key` asks for a passkey which will be used to create a sodium
#' symmetric key (using [cyphr::key_sodium]) which can then be used to encrypt
#' your data using [cyphr::encrypt].
#'
#' @param key (character) A passkey for creating a sodium key. Defaults to NULL,
#' for which the user will be prompted for a passkey at runtime via
#' [rstudioapi::askForPassword]
#'
#' @return (cyphr_key) sodium key for encrypting data with [cyphr::encrypt]
#' @export
#'
#' @examples
#' try(
#'   if (!exists("key")) {
#'     key <- ask_for_key()
#'     print(key)
#'   }
#' )
#'
ask_for_key <- function(key = NULL) {
  if (!is.null(key)) {
    cyphr::key_sodium(sodium::hash(charToRaw(key)))
  } else {
    cyphr::key_sodium(sodium::hash(charToRaw(rstudioapi::askForPassword())))
  }
}


#' Save dataframe to an encrypted RDS file
#'
#' `ezyr_save_rds` saves your dataframe to an RDS file and encrypts it with
#' a sodium symmetric key.
#'
#' @param df (data.frame) A dataframe which you want to save to rds format
#' @param filepath (character) Filepath to save your RDS file to
#' @param key (cyphr_key) Sodium key obtained from invoking [ask_for_key]
#'
#' @return (data.frame) Original input dataframe
#' @export
#'
#' @examples
#' library(diffdf)
#'
#' # Create random dataframe for illustration purpose
#' set.seed(123)
#' df <- data.frame(
#'   col1 = sample(letters, 10, replace = TRUE), # Random characters
#'   col2 = round(runif(10, min = 1, max = 100), 0), # Random integers between 1 and 100
#'   col3 = runif(10, min = 0, max = 1) # Random floats between 0 and 1
#' )
#'
#' # Get a sodium key
#' key <- ask_for_key("my_password")
#'
#' # Save RDS file in temp directory
#' filepath <- file.path(tempdir(), "file1.rds")
#' ezyr_save_rds(df, filepath, key)
#' print(list.files(tempdir(), pattern = "\\.rds$"))
#'
#' # Read the RDS file back
#' df_readback <- ezyr_read_rds(filepath, key)
#'
#' # Compare the read back RDS file with original
#' diffdf(base = df, compare = df_readback)
#'
#' # Delete the saved RDS file in temp directory
#' unlink(filepath)
#' print(list.files(tempdir(), pattern = "\\.rds$"))
#'
ezyr_save_rds <- function(df, filepath, key) {
  cyphr::encrypt(saveRDS(df, filepath), key)
  invisible(df)
}


#' Read encrypted RDS file
#'
#' `ezyr_read_rds` reads data from an encrypted RDS file.
#'
#' @param filepath (character) Filepath to encrypted RDS file
#' @param key (cyphr_key) Sodium key obtained from invoking [ask_for_key]
#'
#' @return Decrypted data from the encrypted RDS file
#' @export
#'
#' @examples
#' library(diffdf)
#'
#' # Create random dataframe for illustration purpose
#' set.seed(123)
#' df <- data.frame(
#'   col1 = sample(letters, 10, replace = TRUE), # Random characters
#'   col2 = round(runif(10, min = 1, max = 100), 0), # Random integers between 1 and 100
#'   col3 = runif(10, min = 0, max = 1) # Random floats between 0 and 1
#' )
#'
#' # Get a sodium key
#' key <- ask_for_key("my_password")
#'
#' # Save RDS file in temp directory
#' filepath <- file.path(tempdir(), "file1.rds")
#' ezyr_save_rds(df, filepath, key)
#' print(list.files(tempdir(), pattern = "\\.rds$"))
#'
#' # Read the RDS file back
#' df_readback <- ezyr_read_rds(filepath, key)
#'
#' # Compare the read back RDS file with original
#' diffdf(base = df, compare = df_readback)
#'
#' # Delete the saved RDS file in temp directory
#' unlink(filepath)
#' print(list.files(tempdir(), pattern = "\\.rds$"))
#'
ezyr_read_rds <- function(filepath, key) {
  cyphr::decrypt(readRDS(filepath), key)
}
