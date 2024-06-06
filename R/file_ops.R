#' Write to excel
#'
#' `write_to_excel` enables you to write your data out to an excel workbook.
#'
#' @param output_table (data.frame) Data that you want to write to excel
#' @param workbook (character) File path of excel workbook
#' - If the workbook does not exist, it will be created
#' - If the workbook exists, data will be written to the specified sheet
#' @param sheet_name (character) Name of excel sheet to write data to
#' - Defaults to system time at execution if none provided
#'
#' @return None
#' @export
#'
#' @examples
#' # Load data
#' data(BostonHousing2, package = "mlbench")
#'
#' # Write data to excel file in temp directory
#' output_file_path_1 <- file.path(tempdir(), "file1.xlsx")
#' write_to_excel(BostonHousing2, output_file_path_1, sheet_name = "my_sheet")
#' print(list.files(tempdir(), pattern = "\\.(xlsx|xls)$"))
#' 
#' # Clean up file in temp directory
#' unlink(output_file_path_1)
#' print(list.files(tempdir(), pattern = "\\.(xlsx|xls)$"))
#'
write_to_excel <- function(output_table,
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

