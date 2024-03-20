#' Remove Missing Data
#'
#' Remove columns or rows with missing data exceeding a specified threshold percentage.
#'
#' @param data The input dataset.
#' @param type The type of missing data removal. Choose between "column" or "row".
#' @param threshold The threshold percentage of missing data above which columns or rows will be removed.
#'
#' @return The modified dataset with missing data removed.
#'
#' @examples
#' # Remove columns with missing data exceeding 20%
#' new_data <- remove_missing_data(data, "column", 0.2)
#'
#' # Remove rows with missing data exceeding 30%
#' new_data <- remove_missing_data(data, "row", 0.3)
#'
#' @export
remove_missing_data <- function(data, type, threshold) {
  if (type == "column") {
    # Calculate the number of missing values in each column
    missing_values <- colMeans(is.na(data))

    # Identify columns where the percentage of missing values exceeds the threshold
    columns_to_remove <- names(missing_values[missing_values >= threshold])

    # Remove identified columns from the dataset
    data <- data[, !names(data) %in% columns_to_remove]
  } else if (type == "row") {

    # Calculate the percentage of missing values for each row, excluding the first column
    missing_percentage <- apply(data[, -1], 1, function(row) {  # Exclude the first column
      sum(is.na(row)) / length(row)
    })

    # Identify rows where the percentage of missing values is less than or equal to the threshold
    rows_to_keep <- missing_percentage <= threshold-0.01

    # Subset the data to keep only the desired rows
    data <- data[rows_to_keep, ]

  } else {
    stop("Invalid type. Choose 'column' or 'row'.")
  }

  # Return the modified dataset
  return(data)
}
