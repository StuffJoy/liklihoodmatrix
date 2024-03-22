#' Sort Columns in Excel Sheets
#'
#' Sorts all columns (except the first) in each sheet of an Excel file in ascending order and writes the sorted data to a new Excel file.
#'
#' @param file_path Path to the input Excel file.
#' @param output_file Path to the output Excel file where the sorted data will be saved.
#'
#' @return None. The sorted data is written to the output Excel file.
#'
#' @examples
#' # Sort columns in input.xlsx and save the sorted data to output.xlsx
#' sort_columns("input.xlsx", "output.xlsx")
#'
#' @import readxl
#' @import writexl
#' @export
# function that will sort all columns (except the first) by ASC order
sort_excel_columns <- function(file_path, output_file) {

  # Get sheet names
  sheet_names <- readxl::excel_sheets(file_path)

  # Create an empty list to store data frames for each sheet
  data_list <- list()

  for (sheet_name in sheet_names) {
    # Read data from the current sheet
    data <- readxl::read_xlsx(file_path, sheet = sheet_name)

    # Identify the order of columns except the first column
    column_order <- c(1, order(names(data)[-1]) + 1)

    # Organize columns alphabetically except the first column
    data <- data[, column_order]

    # Store the organized data in the list
    data_list[[sheet_name]] <- data
  }

  # Write data frames to the Excel file
  writexl::write_xlsx(data_list, output_file)

  cat("Columns (except the first column) have been organized alphabetically for all sheets and saved to", output_file, "\n")
}
