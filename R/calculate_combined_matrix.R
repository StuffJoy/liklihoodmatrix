#' Calculates both the Liklihood and Uncertainty (Missing Data) Matrix Between Datasets
#'
#' This function calculates both the likelihood matrix, comparing genetic markers
#' between blood extracted from mosquitoes and human samples, and uncertainty matrix, evaluating the presence of missing (NA) values in genetic markers
#' between two datasets. The function returns a matrix where each cell has two values, the first showing liklihood, and second uncertainty in parenthesis
#'
#' @param buccal_swabs A data frame containing human genetic data
#' @param human_profiles A data frame containing human blood extracted from mosquitoes
#' @param result_type Denotes whether you want the results to be the genetic distance (1), or percentage match (2)
#' @param tolerance Denotes an acceptable tolerance range for values
#' @return A matrix representing the likelihood of genetic marker matching between the samples in each mosquito and human.
#' @import progress
#' @export
#' @examples
#' # Example usage:
#' data("buccal_swabs")
#' data("human_profiles")
#' calculate_combined_matrix(buccal_swabs, human_profiles,1,0)
calculate_combined_matrix <- function(buccal_swabs, human_profiles, result_type, tolerance = 0) {
  tolerance_decimal <- tolerance / 100

  # Extract human and mosquito IDs
  human_ids <- buccal_swabs[[1]]
  mosquito_ids <- human_profiles[[1]]
  genetic_markers <- colnames(buccal_swabs)[-1]  # Exclude the first column (human IDs)

  # Initialize an empty matrix for the combined data
  combined_matrix <- matrix("", nrow = length(human_ids), ncol = length(mosquito_ids))

  # Initialize progress bar
  pb <- progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
                         total = length(mosquito_ids) * length(human_ids), clear = FALSE, width = 100)

  # Iterate through each mosquito and calculate combined data for each human
  for (i in 1:length(mosquito_ids)) {
    for (j in 1:length(human_ids)) {
      matches <- 0
      missing_counts <- 0

      # Iterate through each genetic marker
      for (k in 1:length(genetic_markers)) {
        human_marker_value <- buccal_swabs[j, k + 1]  # Accessing the current marker
        mosquito_marker_value <- human_profiles[i, k + 1]

        # Calculate matches considering tolerance
        if (!is.na(human_marker_value) && !is.na(mosquito_marker_value)) {
          difference <- abs(human_marker_value - mosquito_marker_value) / human_marker_value
          if (difference <= tolerance_decimal) {
            matches <- matches + 1
          }
        }

        # Count missing values
        if (is.na(human_marker_value) || is.na(mosquito_marker_value)) {
          missing_counts <- missing_counts + 1
        }
      }

      # Determine the matching percentage or distance
      if(result_type == 1) {
        match_value <- 1 - (matches / length(genetic_markers))
      } else if (result_type == 2) {
        match_value <- (matches / length(genetic_markers)) * 100
      } else {
        stop("Invalid type. Use '1' for genetic distance or '2' for percentage match.")
      }

      # Determine the percentage of missing data
      uncertainty_percentage <- (missing_counts / length(genetic_markers)) * 100

      # Format the combined data for the cell
      combined_matrix[j, i] <- sprintf("%.2f (%.2f%%)", match_value, uncertainty_percentage)

      # Update progress bar
      pb$tick()
    }
  }

  colnames(combined_matrix) <- mosquito_ids
  rownames(combined_matrix) <- human_ids

  return(combined_matrix)
}
