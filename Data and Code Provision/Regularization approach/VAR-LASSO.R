# Note that since we utilize a single penalty parameter
# for all model coefficients, it is required that all series
# included be on the same scale; hence, we assume that the
# series are standardized to each have a zero mean and unit
# variance, prior to estimation

####--------------------------------------------------------Remark---------------------------------------------------####

# Clear objects from the workspace
rm(list=ls())

# Set working directory
setwd("D:/Education/Study (KIT)/Semester/Semester 9/Bachelorarbeit/Data/Final")

# Load libraries
library(BigVAR)
library(expm)
library(data.table)
library(dplyr)

#----------------------------------------------------------------------------------------------------------------------------------#
# Implement BigVAR method
analyze_VAR <- function(Y, max_p, h, recursive) {
  # Initialize list to store results
  results_list <- list()
  criteria_list <- list()  # List to store information criteria
  
  # Loop through different p-values
  for (p_value in 1:max_p) {
    
    # Model parameters
    K <- ncol(Y)
    T <- nrow(Y)
    
    # Model estimation
    mod1 <- constructModel(Y, p = p_value, "Basic", gran = c(200, 15), h = h, cv = "Rolling", verbose = FALSE, IC = TRUE, T1 = floor(1*nrow(Y)/3), T2 = floor(2*nrow(Y)/3), recursive = recursive,
                           model.controls = list(intercept = TRUE)) # Note that the intercept is fit separately and not subject to regularization
    current_results <- cv.BigVAR(mod1)
    
    # Insert results into the list
    results_list[[p_value]] <- current_results
    
    # Calculate residual covariance matrix
    res_cov <- crossprod(current_results@resids[1:T]) / T
    
    # Calculate information criteria: AIC, BIC, HQ
    AIC_value <- log(det(res_cov)) + 2 * (p_value * K^2) / T
    BIC_value <- log(det(res_cov)) + log(T) * (p_value * K^2) / T
    HQ_value <- log(det(res_cov)) + 2 * log(log(T)) * (p_value * K^2) / T
    MSFE <- mean(current_results@OOSMSFE)
    
    # Output the results
    cat(paste("Results for p =", p_value, ":\n"))
    cat("AIC:", AIC_value, "\n")
    cat("BIC:", BIC_value, "\n")
    cat("HQ:", HQ_value, "\n")
    cat("MSFE:", MSFE, "\n")
    cat("\n")
    
    # Insert information criteria results into the list
    criteria_list[[p_value]] <- list(AIC = AIC_value, BIC = BIC_value, HQ = HQ_value)
    
    # Display or save plot
    plot(SparsityPlot.BigVAR.results(results_list[[p_value]]))
  }
  
  # Return results as a list of lists
  return(list(results_list = results_list, criteria_list = criteria_list))
}
#----------------------------------------------------------------------------------------------------------------------------------#
# Read and format data
aggregation_diff_scaled_df <- as.data.frame(read.csv("aggregation_diff_scaled_df.csv", row.names = 1))
#----------------------------------------------------------------------------------------------------------------------------------#
# Call the BigVAR method (entire period)
Y0 <- as.matrix(aggregation_diff_scaled_df)
max_p_value <- 3
forecast_horizon <- 30
result <- analyze_VAR(Y0, max_p_value, forecast_horizon, FALSE)
