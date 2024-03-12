# Modified function that replaces %in% matching with cell by cell matching
calculate_likelihood_matrix <- function(buccal_swabs, human_profiles) {
  # Extract human IDs and genetic marker names
  human_ids <- buccal_swabs[[1]]
  genetic_markers <- colnames(buccal_swabs)[-1]  # Exclude the first column (human IDs)

  # Extract mosquito IDs and genetic markers
  mosquito_ids <- human_profiles[[1]]
  mosquito_genetic_markers <- human_profiles[,-1]  # Exclude the first column (mosquito IDs)

  # Initialize an empty matrix for the likelihood
  likelihood_matrix <- matrix(0, nrow = length(human_ids), ncol = length(mosquito_ids))

  # Iterate through each mosquito and calculate likelihood for each human
  for (i in 1:length(mosquito_ids)) {
    for (j in 1:length(human_ids)) {
      human_genetic_markers <- buccal_swabs[j, -1]  # Exclude the first column (human IDs)
      mosquito_genetic_markers_row <- mosquito_genetic_markers[i, , drop = FALSE]

      # Calculate the genetic distance something something, need to ask Fede the specific name
      match_percentage <- 1 - sum(sapply(1:length(genetic_markers), function(k) identical(mosquito_genetic_markers_row[k], human_genetic_markers[k]))) / length(genetic_markers)

      # script below uses a more straight forward percentage match method
      #match_percentage <- sum(sapply(1:length(genetic_markers), function(k) identical(mosquito_genetic_markers_row[k], human_genetic_markers[k]))) / length(genetic_markers) * 100

      likelihood_matrix[j, i] <- match_percentage
    }
  }

  colnames(likelihood_matrix) <- mosquito_ids
  rownames(likelihood_matrix) <- human_ids

  return(likelihood_matrix)
}
