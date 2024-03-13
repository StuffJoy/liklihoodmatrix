#' Calculate likelihood matrix based on genetic marker matching
#'
#' This function calculates the likelihood matrix by comparing genetic markers
#' between blood extracted from mosquitoes and human samples.
#'
#' @param volunteer_samples A data frame containing human genetic data
#' @param human_profiles A data frame containing human blood extrated from mosquitoes
#' @return A matrix representing the likelihood of genetic marker matching between the samples in each mosquito and human.
#' @export
#' @examples
#' # Example usage:
#' data("buccal_swabs")
#' data("human_profiles")
#' calculate_new_likelihood_matrix(buccal_swabs, human_profiles)


# Modified function that replaces %in% matching with cell by cell matching
calculate_likelihood_matrix <- function(volunteer_samples, human_samples_in_mosquito) {

  # Extracts the first column of the volunteer_samples data
  volunteer_samples_row_labels <- volunteer_samples[[1]]

  # Extracts the column names of the volunteer_samples data (excluding the first column)
  volunteer_samples_column_labels <- colnames(volunteer_samples)[-1]

  # Extracts the first column of the human_samples_in_mosquito data
  human_in_mosquito_row_labels <- human_samples_in_mosquito[[1]]

  # Extracts the column names of the human_samples_in_mosquito data (excluding the first column)
  human_in_mosquito_column_labels <- human_samples_in_mosquito[,-1]


  # Initialize an empty matrix for the likelihood
  likelihood_matrix <- matrix(0, nrow = length(volunteer_samples_row_labels), ncol = length(human_in_mosquito_row_labels))

  # Iterate through each mosquito and calculate likelihood for each human
  for (i in 1:length(human_in_mosquito_row_labels)) { # Loop that iterates over each mosquito in the human_samples_in_mosquito data frame
    for (j in 1:length(volunteer_samples_row_labels)) { # Nested loop iterates over each person in the volunteer_samples data frame
      human_genetic_markers <- volunteer_samples[j, -1]  # Extracts the genetic markers for each volunteer (excluding first column
      human_in_mosquito_selected_row <- human_in_mosquito_column_labels[i, , drop = FALSE] # Extracts a single row of human genetic markers found in each mosquito

      # (Need to ask Fede if this description is accurate) Calculates the genetic distance between the genetic markers of samples found in the current mosquito against the samples of the volunteer
      match_percentage <- 1 - sum(sapply(1:length(volunteer_samples_column_labels), function(k) identical(human_in_mosquito_selected_row[k], human_genetic_markers[k]))) / length(volunteer_samples_column_labels)

      # script below uses a more straight forward percentage match method. Higher the percentage, closer the match
      # match_percentage <- sum(sapply(1:length(volunteer_samples_column_labels), function(k) identical(human_in_mosquito_selected_row[k], human_genetic_markers[k]))) / length(volunteer_samples_column_labels) * 100

      # Assigns calculated match percentage
      likelihood_matrix[j, i] <- match_percentage
    }
  }

  # assigns column and row names
  colnames(likelihood_matrix) <- human_in_mosquito_row_labels
  rownames(likelihood_matrix) <- volunteer_samples_row_labels

  return(likelihood_matrix)
}


# Modified function that replaces %in% matching with cell by cell matching
calculate_likelihood_matrix_generic <- function(matrix1, matrix2) {

  # Extracts the first column of the volunteer_samples data
  matrix1_row_labels <- matrix1[[1]]

  # Extracts the column names of the volunteer_samples data (excluding the first column)
  matrix1_column_labels <- colnames(matrix1)[-1]

  # Extracts the first column of the human_samples_in_mosquito data
  matrix2_row_labels <- matrix2[[1]]

  # Extracts the column names of the human_samples_in_mosquito data (excluding the first column)
  matrix2_column_labels <- matrix2[,-1]


  # Initialize an empty matrix for the likelihood
  likelihood_matrix <- matrix(0, nrow = length(matrix1_row_labels), ncol = length(matrix2_row_labels))

  # Iterate through each mosquito and calculate likelihood for each human
  for (i in 1:length(matrix2_row_labels)) {
    for (j in 1:length(matrix1_row_labels)) {
      matrix1_data <- matrix1[j, -1]  # Exclude the first column (human IDs)
      matrix2_selected_row <- matrix2_column_labels[i, , drop = FALSE]

      # Calculate the genetic distance something something, need to ask Fede the specific name
      match_percentage <- 1 - sum(sapply(1:length(matrix2_row_labels), function(k) identical(matrix2_selected_row[k], matrix1_data[k]))) / length(matrix1_column_labels)

      # script below uses a more straight forward percentage match method
      match_percentage <- sum(sapply(1:length(matrix2_row_labels), function(k) identical(matrix2_selected_row[k], matrix1_data[k]))) / length(matrix1_column_labels) * 100

      likelihood_matrix[j, i] <- match_percentage
    }
  }

  colnames(likelihood_matrix) <- human_in_mosquito_row_labels
  rownames(likelihood_matrix) <- volunteer_samples_row_labels

  return(likelihood_matrix)
}

