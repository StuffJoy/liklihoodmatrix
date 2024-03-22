#' Harmonize Columns Between Two Data Frames
#'
#' Adjusts the column structure of the target data frame to match the reference
#' data frame, except for the first column which is retained from both. This is
#' useful when preparing datasets for comparative analysis, ensuring they have
#' the same set of columns (except for the first column which can be an identifier).
#'
#' @param reference_dataset A data frame that serves as the reference for column names.
#' @param target_dataset A data frame whose columns will be adjusted to match the reference, except for the first column.
#'
#' @return A data frame that is a modified version of `target_dataset` containing only
#' the columns found in both `target_dataset` (excluding the first column) and `reference_dataset`,
#' plus the first column of `target_dataset`.
#'
#' @examples
#' # Reference_df_columns: ID, Name, Age
#' # target_df_columns: Identifier, Name, Age, Gender
#' # The function call would look like this:
#' harmonized_df <- harmonize_columns(Reference_df, target_df)
#' # The resulting harmonized_df would have columns: Identifier, Name, Age
#'
#' @export
harmonize_columns <- function(reference_dataset, target_dataset) {

  # Get the names of the columns excluding the first column
  reference_dataset_columns <- names(reference_dataset)[-1]
  target_dataset_columns <- names(target_dataset)[-1]

  # Find the common columns, excluding those not in reference_dataset
  common_columns <- intersect(reference_dataset_columns, target_dataset_columns)

  # Retain the first column of target_dataset and the common columns only
  target_dataset_harmonized <- target_dataset[, c(names(target_dataset)[1], common_columns)]

  # Returning the modified version of target_dataset
  return(target_dataset_harmonized)
}
