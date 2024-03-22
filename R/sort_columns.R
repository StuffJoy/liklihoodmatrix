#' Sort Columns in data set
#'
#' Sorts all columns (except the first) in each sheet of an Excel file in ascending order and writes the sorted data to a new Excel file.
#'
#' @param unordered_data Data that needs to be ordered
#'
#' @return Data ordered in ASC order
#'
#' @examples
#' # Sort columns in a given data set
#' sort_columns(dataset)
#'
#' @export
# function that will sort all columns (except the first) by ASC order
sort_columns <- function(unordered_data) {

  ordered_data <- unordered_data[, c(colnames(unordered_data[1]), sort(setdiff(names(unordered_data), colnames(unordered_data[1]))))]
  return(ordered_data)
}
