# This script generates a full-Sample Connectedness Table. The sample is April 29, 2016 through Jan 01, 2024, and the predictive horizon is 7 days. 
# The ij-th entry of the upper-left 21 × 21 zone submatrix gives the ij-th pairwise directional connectedness; i.e., the percent of 7-day-ahead 
# forecast error variance of zone i due to shocks from zone j. The rightmost (FROM) column gives total directional connectedness (from); i.e., 
# row sums (from all others to i). The bottom (TO) row gives total directional connectedness (to); i.e., column sums (to all others from j). 
# The bottommost (NET) row gives the difference in total directional connectedness (to–from). The bottom-right element is total connectedness.

####--------------------------------------------------------Remark---------------------------------------------------####

# Generalized FEVD
library(frequencyConnectedness)
library(xlsx)

# Set working directory
setwd("...")

# Calculate TO, FROM, NET and TOTAL elements
remove_diagonal <- function(matrix) {
  size <- nrow(matrix)
  for (i in 1:size) {
    matrix[i, i] <- 0
  }
  return(matrix)
}

extend_matrix <- function(matrix) {
  # Check if the matrix is square
  if (nrow(matrix) != ncol(matrix)) {
    cat("The passed matrix is not square\n")
    return(NULL)
  }
  
  # Extend the matrix by one row and one column
  new_matrix <- matrix(0, nrow(matrix) + 2, ncol(matrix) + 1)
  rownames(new_matrix) <- c(rownames(matrix), "TO", "NET")
  colnames(new_matrix) <- c(colnames(matrix), "FROM")
  
  # Copy the original matrix into the upper left corner of the new matrix
  new_matrix[1:nrow(matrix), 1:ncol(matrix)] <- matrix
  
  # Calculate and fill values for the new "FROM" column
  from_column <- rowSums(matrix)
  new_matrix[, ncol(new_matrix)] <- c(from_column, 0, 0)  # Last entry remains 0
  
  # Calculate and fill values for the new "TO" row
  to_row <- c(colSums(matrix), 0)  # Last entry remains 0
  new_matrix[nrow(new_matrix)-1, ] <- to_row
  
  # Calculate and fill values for the new "NET" row
  for(i in 1:ncol(new_matrix)){
    new_matrix[nrow(new_matrix),i] = to_row[i]-from_column[i]
  }
  
  # Fill the bottom-right entry with the mean of "TO"
  new_matrix[nrow(new_matrix)-1, ncol(new_matrix)] <- mean(to_row)
  
  return(new_matrix)
}

# Restore the diagonal elements of matrix2 into matrix1
restore_diagonal = function(matrix1, matrix2){
  n = min(nrow(matrix1), nrow(matrix2))
  for(i in 1:n){
    matrix1[i,i] = matrix2[i,i]
  }
  return(matrix1)
}

# Construct forecast variance decomposition matrix
connectedness_table = function(df, var_estimation, h){
  matrix = genFEVD(var_estimation, n.ahead = h)
  rownames(matrix) <- colnames(df)
  colnames(matrix) <- colnames(df)
  tab1 = remove_diagonal(matrix)
  tab2 = extend_matrix(tab1)
  tab3 = restore_diagonal(tab2, matrix)
  return(tab3)
}

# Application on Full Sample
con_tab = connectedness_table(aggregation_diff_scaled_df, result$results_list[[1]], 7)
# Save the GFEVD
write.xlsx(con_tab, file = "Full sample connectedness table, h=7.xlsx")
