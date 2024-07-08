# Dynamic characterization of the European Day-Ahead market:
# rolling estimation with a 100-day window, alongside repeated cross-validation of the
# penalty parameter λ and model selection within each interval
####-----------------------------------------------Remark---------------------------------------------------####


# Loading libraries
library(ggplot2)
library(dplyr)
library(frequencyConnectedness)
library(BigVAR)
library(expm)
library(data.table)
library(xlsx)


# Setting Working Directory
setwd("...")

# Implementing BigVAR method
rolling_analyze_VAR <- function(Y, max_p, h, recursive) {
  # Initializing list to store results
  results_list <- list()
  criteria_list <- list()  # List to store information criteria
  
  # Loop through different p-values
  for (p_value in 1:max_p) {
    
    # Model parameters
    K <- ncol(Y)
    T <- nrow(Y)
    
    # Model estimation
    mod1 <- constructModel(Y, p = p_value, "Basic", gran = c(150, 10), h = h, cv = "Rolling", verbose = FALSE, IC = FALSE, T1 = floor(1*nrow(Y)/3), T2 = floor(2*nrow(Y)/3) , recursive = recursive
                           , model.controls = list(intercept = TRUE))
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
    
    
    # Insert information criteria results into list
    criteria_list[[p_value]] <- list(AIC = AIC_value, BIC = BIC_value, HQ = HQ_value)
  }
  
  # Return results as a list of lists
  return(list(results_list = results_list, criteria_list = criteria_list))
}

# Implementing Best_Model method
select_best_model <- function(analyze_result, ic_criterion) {
  # Extract results and information criteria from analysis result
  results_list <- analyze_result$results_list
  criteria_list <- analyze_result$criteria_list
  
  # Determine minimum value of the chosen information criterion
  min_value <- Inf
  best_model <- NULL
  
  # Loop through results and select model with minimum IC values
  for (p_value in seq_along(criteria_list)) {
    ic_value <- criteria_list[[p_value]][[ic_criterion]]
    if (ic_value < min_value) {
      min_value <- ic_value
      best_model <- results_list[[p_value]]
    }
  }
  
  # Return the best model
  return(best_model)
}


perform_rolling_analysis <- function(Y, window_size, max_p, forecast_horizon, n.ahead) {
  results_list <- list()
  connectedness_results <- list()
  values <- list()
  
  num_rows <- nrow(Y)
  num_cols <- ncol(Y)
  
  for (i in 1:(num_rows - window_size + 1)) {
    # Define the rolling window
    Y_window <- Y[i:(i + window_size - 1), ]
    
    # Perform VAR analysis and store best model
    var_results <- rolling_analyze_VAR(as.matrix(Y_window), max_p, forecast_horizon, recursive = FALSE)
    best_model = select_best_model(var_results, "HQ")
    results_list[[i]] <- best_model
    
    # Perform network analysis
    connectedness_table <- connectedness_table(Y, best_model, n.ahead)
    connectedness_results[[i]] <- connectedness_table
    
    # Store the date of the last entry in the network analysis result
    last_date <- rownames(Y_window)[nrow(Y_window)]
    last_entry <- connectedness_table[22, 22]
    values[[i]] <- list(date = last_date, entry = last_entry)
  }
  
  # Return the results
  return(list(results = results_list, connectedness = connectedness_results, values = values))
}

dynamic <- perform_rolling_analysis(aggregation_diff_scaled_df, window_size = 100, max_p = 2, forecast_horizon = 14, n.ahead = 7)

# Generate Plots
# Convert the list into a data frame
df_daynamic <- do.call(rbind, lapply(dynamic$values, data.frame))

# Convert the date into date format
df_daynamic$date <- as.Date(df_daynamic$date)


library(ggplot2)

ggplot(df_daynamic, aes(x = date, y = entry)) +
  geom_line(color = "darkblue") +
  #geom_vline(xintercept = as.numeric(as.Date("2016-10-23")), linetype = "dashed", linewidth = 0,8) +
  geom_vline(xintercept = as.numeric(as.Date("2017-11-23")), linetype = "dashed", linewidth = 0,8) +
  geom_vline(xintercept = as.numeric(as.Date("2018-09-08")), linetype = "dashed", linewidth = 0,8) +
  #geom_vline(xintercept = as.numeric(as.Date("2019-06-09")), linetype = "dashed", linewidth = 0,8) +
  geom_vline(xintercept = as.numeric(as.Date("2020-10-05")), linetype = "dashed", linewidth = 0,8) +
  geom_vline(xintercept = as.numeric(as.Date("2021-07-26")), linetype = "dashed", linewidth = 0,8) +
  geom_vline(xintercept = as.numeric(as.Date("2022-06-17")), linetype = "dashed", linewidth = 0,8) +
  geom_vline(xintercept = as.numeric(as.Date("2022-09-15")), linetype = "dashed", linewidth = 0,8) +
  geom_vline(data = data.frame(x = seq(as.Date("2016-06-01"), max(df_daynamic$date), by = "1 month")),
             aes(xintercept = as.numeric(x)), color = "grey", linetype = "dotted") +
  #annotate("text", x = as.Date("2016-10-23"), y = Inf, label = " A", vjust = 1, hjust = 0) +
  annotate("text", x = as.Date("2017-11-23"), y = Inf, label = " A", vjust = 1, hjust = 0) +
  annotate("text", x = as.Date("2018-09-08"), y = Inf, label = " B", vjust = 1, hjust = 0) +
  #annotate("text", x = as.Date("2019-06-09"), y = Inf, label = " D", vjust = 1, hjust = 0) +
  annotate("text", x = as.Date("2020-10-05"), y = Inf, label = " C", vjust = 1, hjust = 0) +
  annotate("text", x = as.Date("2021-07-26"), y = Inf, label = " D", vjust = 1, hjust = 0) +
  annotate("text", x = as.Date("2022-06-17"), y = Inf, label = " E", vjust = 1, hjust = -1) +
  labs(x = " ",y = "Total Connectedness", title = "") +
  scale_x_date(date_breaks = "4 months", date_labels = "%m.%y")
