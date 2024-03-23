#' Calculate likelihood matrix based on genetic marker matching
#'
#' This function calculates the likelihood matrix by comparing genetic markers
#' between blood extracted from mosquitoes and human samples.
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
#' calculate_new_likelihood_matrix(buccal_swabs, human_profiles,1,0)


calculate_likelihood_matrix <- function(buccal_swabs, human_profiles, result_type, tolerance = 0) {
  # Convert tolerance percentage to a decimal for calculation
  tolerance_decimal <- tolerance / 100

  # Extract human IDs and genetic marker names
  human_ids <- buccal_swabs[[1]]
  genetic_markers <- colnames(buccal_swabs)[-1]  # Exclude the first column (human IDs)

  # Extract mosquito IDs
  mosquito_ids <- human_profiles[[1]]

  # Initialize an empty matrix for the likelihood
  likelihood_matrix <- matrix(0, nrow = length(human_ids), ncol = length(mosquito_ids))

  # Total iterations for progress bar
  total_iterations <- length(mosquito_ids) * length(human_ids)

  # Initialize progress bar
  pb <- progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
                         total = total_iterations, clear = FALSE, width = 100)

  # Iterate through each mosquito and calculate likelihood for each human
  for (i in 1:length(mosquito_ids)) {
    for (j in 1:length(human_ids)) {
      # Initialize the count of matches for the current human-mosquito pair
      matches <- 0

      # Iterate through each genetic marker for comparison
      for (k in 1:length(genetic_markers)) {
        human_marker_value <- buccal_swabs[j, k + 1]  # +1 to skip the first column (IDs), accessing the current marker
        mosquito_marker_value <- human_profiles[i, k + 1]  # +1 to skip the first column (IDs)

        # Replace NA values with 0
        mosquito_marker_value[is.na(mosquito_marker_value)] <- 0

        # Calculate the difference and check if it's within the tolerance
        if(!is.na(human_marker_value)) {
          difference <- abs(human_marker_value - mosquito_marker_value) / human_marker_value
          if (difference <= tolerance_decimal) {
            matches <- matches + 1
          }
        }
      }

      # Calculate the percentage of match for each human
      if(result_type == "1") {
        match_percentage <- 1 - (matches / length(genetic_markers))
      } else if (result_type == "2") {
        match_percentage <- matches / length(genetic_markers) * 100
      } else {
        stop("Invalid type. For: genetic_distance, use '1', percentage match, use '2'")
      }

      # Update likelihood matrix
      likelihood_matrix[j, i] <- match_percentage

      # Update progress bar
      pb$tick()
    }
  }

  colnames(likelihood_matrix) <- mosquito_ids
  rownames(likelihood_matrix) <- human_ids

  return(likelihood_matrix)
}
