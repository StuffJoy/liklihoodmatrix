#' Calculate Uncertainty (Missing Data) Matrix Between Datasets
#'
#' This function calculates an uncertainty matrix by evaluating the presence of missing (NA) values in genetic markers
#' between two datasets: one representing human genetic data from buccal swabs and the other representing genetic data
#' extracted from mosquitoes. The function returns a matrix where each element represents the percentage of missing data
#' for the corresponding pair of human and mosquito samples.
#'
#' @param buccal_swabs A data frame containing human genetic data from buccal swabs.
#' @param human_profiles A data frame containing genetic data extracted from mosquitoes, presumably after biting humans.
#' @return A matrix where rows correspond to human samples and columns correspond to mosquito samples.
#'         Each cell in the matrix shows the percentage of genetic markers that have missing data in either or both
#'         of the paired samples.
#' @examples
#' # Example usage:
#' data("buccal_swabs")  # Load buccal swab data
#' data("human_profiles")  # Load mosquito extracted genetic data
#' missing_data_matrix <- calculate_uncertainty_matrix(buccal_swabs, human_profiles)
#'
#' @export
calculate_uncertainty_matrix <- function(buccal_swabs, human_profiles) {
  # Extract human IDs and genetic marker names
  human_ids <- buccal_swabs[[1]]
  genetic_markers <- colnames(buccal_swabs)[-1]  # Exclude the first column (human IDs)

  # Extract mosquito IDs
  mosquito_ids <- human_profiles[[1]]

  # Initialize an empty matrix for the missing data percentage
  uncertainty_matrix <- matrix(0, nrow = length(human_ids), ncol = length(mosquito_ids))

  # Iterate through each mosquito and calculate missing data percentage for each human
  for (i in 1:length(mosquito_ids)) {
    for (j in 1:length(human_ids)) {
      # Initialize counts
      missing_counts <- 0

      # Iterate through each genetic marker for comparison
      for (k in 1:length(genetic_markers)) {
        human_marker_value <- buccal_swabs[j, k + 1]  # +1 to skip the first column (IDs), accessing the current marker
        mosquito_marker_value <- human_profiles[i, k + 1]  # +1 to skip the first column (IDs)

        # Check for NA values in either dataset
        if(is.na(human_marker_value) | is.na(mosquito_marker_value)) {
          missing_counts <- missing_counts + 1
        }
      }

      # Calculate the percentage of missing data for each human-mosquito pair
      uncertainty_percentage <- (missing_counts / length(genetic_markers)) * 100

      # Update missing data matrix
      uncertainty_matrix[j, i] <- uncertainty_percentage
    }
  }

  colnames(uncertainty_matrix) <- mosquito_ids
  rownames(uncertainty_matrix) <- human_ids

  return(uncertainty_matrix)
}
