# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

#https://www.youtube.com/watch?v=kQ5QkN4Kx4Q

# function that will sort all columns (except the first) by ASC order
sort_columns <- function(file_path, output_file) {

  # Get sheet names
  sheet_names <- excel_sheets(file_path)

  # Create an empty list to store data frames for each sheet
  data_list <- list()

  for (sheet_name in sheet_names) {
    # Read data from the current sheet
    data <- read_xlsx(file_path, sheet = sheet_name)

    # Identify the order of columns except the first column
    column_order <- c(1, order(names(data)[-1]) + 1)

    # Organize columns alphabetically except the first column
    data <- data[, column_order]

    # Store the organized data in the list
    data_list[[sheet_name]] <- data
  }

  # Write data frames to the Excel file
  write_xlsx(data_list, output_file)

  cat("Columns (except the first column) have been organized alphabetically for all sheets and saved to", output_file, "\n")
}
