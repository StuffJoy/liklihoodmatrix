remove_missing_data <- function(data, type, threshold_percentage) {
  if (type == "column") {
    # Calculate the number of missing values in each column
    missing_values <- colMeans(is.na(data))

    # Identify columns where the percentage of missing values exceeds the threshold
    columns_to_remove <- names(missing_values[missing_values > threshold_percentage])

    # Remove identified columns from the dataset
    data <- data[, !names(data) %in% columns_to_remove]
  } else if (type == "row") {
    # Calculate the number of missing values in each row
    missing_values <- rowMeans(is.na(data))

    # Identify rows where the percentage of missing values exceeds the threshold
    rows_to_remove <- which(missing_values > threshold_percentage)

    # Remove identified rows from the dataset
    data <- data[-rows_to_remove, ]
  } else {
    stop("Invalid type. Choose 'column' or 'row'.")
  }

  # Return the modified dataset
  return(data)
}

# Define a function to remove columns with missing data exceeding a certain threshold
remove_columns_with_missing_data <- function(data, threshold_percentage) {
  # Calculate the number of missing values in each column
  missing_values <- colMeans(is.na(data))

  # Identify columns where the percentage of missing values exceeds the threshold
  columns_to_remove <- names(missing_values[missing_values > threshold_percentage])

  # Remove identified columns from the dataset
  data <- data[, !names(data) %in% columns_to_remove]

  # Return the modified dataset
  return(data)
}

# Define a function to remove rows with missing data exceeding a certain threshold
remove_rows_with_missing_data <- function(data, threshold_percentage) {
  # Calculate the number of missing values in each row
  missing_values <- rowMeans(is.na(data))

  # Identify rows where the percentage of missing values exceeds the threshold
  rows_to_remove <- which(missing_values > threshold_percentage)

  # Remove identified rows from the dataset
  data <- data[-rows_to_remove, ]

  # Return the modified dataset
  return(data)
}
