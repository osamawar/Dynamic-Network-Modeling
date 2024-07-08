# A distinct change in the dynamics of all time series is evident in June 2021.
# The dataset is divided into two parts.
# The two new datasets are processed in the same manner as the initial dataset,
# and a network is derived from each.
####-----------------------------------------------Remark---------------------------------------------------####

# Clear objects from the workspace
rm(list=ls())

# Set working directory
setwd("...")

# Load libraries
library(BigVAR)
library(expm)
library(data.table)
library(dplyr)

#-------------------------------------------------------------------------------------------------------------#
# Implementing the BigVAR method
analyze_VAR <- function(Y, max_p, h, recursive) {
  # Initialize list to store results
  results_list <- list()
  criteria_list <- list()  # List to store information criteria
  
  # Loop through different p values
  for (p_value in 1:max_p) {
    
    # Model parameters
    K <- ncol(Y)
    T <- nrow(Y)
    
    # Model estimation
    mod1 <- constructModel(Y, p = p_value, "Basic", gran = c(200, 15), h = h, cv = "Rolling", verbose = FALSE, IC = TRUE, T1 = floor(1*nrow(Y)/3), T2 = floor(2*nrow(Y)/3), recursive = recursive
                           , model.controls = list(intercept = TRUE))#Note that the intercept is fit separately and not subject to regularization
    current_results <- cv.BigVAR(mod1)
    
    # Insert results into list
    results_list[[p_value]] <- current_results
    
    # Calculate residual covariance matrix
    res_cov <- crossprod(current_results@resids[1:T]) / T
    
    # Calculate information criteria AIC, BIC, HQ
    AIC_value <- log(det(res_cov)) + 2 * (p_value * K^2) / T
    BIC_value <- log(det(res_cov)) + log(T) * (p_value * K^2) / T
    HQ_value <- log(det(res_cov)) + 2 * log(log(T)) * (p_value * K^2) / T
    MSFE <- mean(current_results@OOSMSFE)
    
    # Print results
    cat(paste("Results for p =", p_value, ":\n"))
    cat("AIC:", AIC_value, "\n")
    cat("BIC:", BIC_value, "\n")
    cat("HQ:", HQ_value, "\n")
    cat("MSFE:", MSFE, "\n")
    cat("\n")
    
    # Insert information criteria results into list
    criteria_list[[p_value]] <- list(AIC = AIC_value, BIC = BIC_value, HQ = HQ_value)
    
    # Display or save plot
    plot(SparsityPlot.BigVAR.results(results_list[[p_value]]))
  }
  
  # Return results as a list of lists
  return(list(results_list = results_list, criteria_list = criteria_list))
}
#----------------------------------------------------------------------------------------------------------------------------------#
# Method to check stationarity
stationarity_tests <- function(df) {
  results <- data.frame(Variable = character(), ADF = character(), KPSS = character(), stringsAsFactors = FALSE)
  
  for (col in colnames(as.data.frame(df))) {
    adf_result <- adf.test(df[, col])  
    kpss_result <- kpss.test(df[, col])  
    
    adf_stationary <- ifelse(adf_result$p.value < 0.05, "Stationary", "Non-Stationary")
    kpss_stationary <- ifelse(kpss_result$p.value > 0.05, "Stationary", "Non-Stationary")
    
    results <- rbind(results, data.frame(Variable = col, ADF = adf_stationary, KPSS = kpss_stationary))
  }
  
  return(results)
}

#----------------------------------------------------------------------------------------------------------------------------------#
# Read and format data
aggregation_diff_df <- as.data.frame(read.csv("aggregation_diff_df.csv", row.names = 1))
#----------------------------------------------------------------------------------------------------------------------------------#
# Separate data based on change in the dynamics ("2021-06-01")
date_threshold <- as.Date("2021-06-01")
part_one <- scale(aggregation_diff_df[as.Date(row.names(aggregation_diff_df)) <= date_threshold, ])
part_two <- scale(aggregation_diff_df[as.Date(row.names(aggregation_diff_df)) >= date_threshold + 1, ])
#stationarity_tests(part_one) # check
#stationarity_tests(part_two) # check
#----------------------------------------------------------------------------------------------------------------------------------#
# Call BigVAR method for part_one
Y1 <- as.matrix(part_one)
max_p_value <- 3
forecast_horizon <- 30
part1 <- analyze_VAR(Y1, max_p_value, forecast_horizon, FALSE)
# Calculate variance decomposition matrix for part1
con_tab_part1 = connectedness_table(part_one, part1$results_list[[1]], 7)
write.xlsx(con_tab_part1, file = "The static network part_one, h=7.xlsx")

#----------------------------------------------------------------------------------------------------------------------------------#
# Call BigVAR method for part_two
Y2 <- as.matrix(part_two)
max_p_value <- 3
forecast_horizon <- 30
part2 <- analyze_VAR(Y2, max_p_value, forecast_horizon, FALSE)
# Calculate variance decomposition matrix for part2
con_tab_part2 = connectedness_table(part_two, part2$results_list[[1]], 7)
write.xlsx(con_tab_part2, file = "The static network part_two, h=7.xlsx")

#----------------------------------------------------------------------------------------------------------------------------------#
