#This script aims to analyze the stationarity of variables in a rolling window fashion,
#identifying windows where at least one variable exhibits non-stationary behavior based
#on ADF and KPSS tests.


####-----------------------------------------------Remark---------------------------------------------------####
# Load necessary library
library(tseries)

# Function to perform stationarity tests (ADF and KPSS) on each column of a dataframe
stationarity_tests <- function(df) {
  results <- data.frame(Variable = character(), ADF = character(), KPSS = character(), stringsAsFactors = FALSE)
  
  # Loop through each column in the dataframe
  for (col in colnames(as.data.frame(df))) {
    # Perform Augmented Dickey-Fuller (ADF) test
    adf_result <- adf.test(df[, col])  
    # Perform Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test
    kpss_result <- kpss.test(df[, col])  
    
    # Determine stationarity based on p-values
    adf_stationary <- ifelse(adf_result$p.value < 0.05, "Stationary", "Non-Stationary")
    kpss_stationary <- ifelse(kpss_result$p.value > 0.05, "Stationary", "Non-Stationary")
    
    # Append results to the dataframe
    results <- rbind(results, data.frame(Variable = col, ADF = adf_stationary, KPSS = kpss_stationary))
  }
  
  return(results)
}

# Main code starts here

# Number of rows in the dataframe
num_rows <- nrow(aggregation_diff_scaled_df)
# Convert dataframe to matrix
Y <- as.matrix(aggregation_diff_scaled_df)
# Define the size of the rolling window
window_size <- 100
# Initialize empty list for results
results <- list()

# Iterate over rolling windows
for (i in 1:(num_rows - window_size + 1)) {
  # Create the rolling window of data
  Y_window <- Y[i:(i + window_size - 1), ]
  # Perform stationarity tests on the windowed data and store the results
  results[[i]] <- stationarity_tests(Y_window)
}

# Identify windows containing non-stationary variables
contain_non_stationarity <- list()
for (i in 1:length(results)){  
  # Check if either ADF or KPSS tests indicate non-stationarity
  if(any(grepl("Non-Stationary", results[[i]]$ADF)) | any(grepl("Non-Stationary", results[[i]]$KPSS))){
    # Print the index of the window containing non-stationary variables
    print(i)
    # Store the results of the non-stationary window
    contain_non_stationarity[[i]] <- results[[i]] 
  }
}

# Filter out NULL entries (windows without non-stationary variables)
contain_non_stationarity <- Filter(Negate(is.null), contain_non_stationarity)

# Return the final list of windows containing non-stationary variables
contain_non_stationarity
length(contain_non_stationarity)  #---> 136 from 2704 windows contain at least one non-stationary series
