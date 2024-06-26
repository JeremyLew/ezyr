% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/file_ops.R
\name{export_dataframe_labels}
\alias{export_dataframe_labels}
\title{Export dataframe labels to excel}
\usage{
export_dataframe_labels(
  data,
  workbook,
  sheet_name = format(Sys.time(), "\%d\%b\%Y_\%H\%M")
)
}
\arguments{
\item{data}{(data.frame) Your dataframe whose variable labels you want to export to excel}

\item{workbook}{(character) File path of excel workbook}

\item{sheet_name}{(character) Name of excel sheet to export labels to
\itemize{
\item Defaults to system time at execution if none provided
}}
}
\value{
None
}
\description{
\code{export_dataframe_labels} exports the variable labels of a dataframe, which
are created by \link[labelled:var_label]{labelled::var_label}, to an excel sheet.
}
\examples{
library(labelled)

df <- data.frame(
  column_a = sample(letters, 3),
  column_b = runif(3, 1, 10)
)
labels <- list(
  column_a = "This is column A",
  column_b = "This is column B"
)
var_label(df) <- labels

# Export column labels to an excel file in temp directory
output_filepath_1 <- file.path(tempdir(), "file1.xlsx")
export_dataframe_labels(df, output_filepath_1, sheet_name = "my_sheet")
print(list.files(tempdir(), pattern = "\\\\.(xlsx|xls)$"))

# Delete the excel file in temp directory
unlink(output_filepath_1)
print(list.files(tempdir(), pattern = "\\\\.(xlsx|xls)$"))

}
