#This script proceeds to check for seasonality in the transformed time series using the Friedman Rank test,
#a nonparametric statistical test.
####--------------------------------------------------------Remark-----------------------------------------####

# Clear objects from the workspace
rm(list=ls())

# Set working directory
setwd("...")

# Load libraries
library(BigVAR)
library(expm)
library(data.table)
library(dplyr)
library(seastests)
library(xts)

####-------------------------------------------------------------------------------------------------------####
# Read Data
aggregation_df <- as.data.frame(read.csv("aggregation_df.csv", row.names = 1))
aggregation_diff_df <- as.data.frame(read.csv("aggregation_diff_df.csv", row.names = 1))

# Transform time series into xts
start_date <- as.Date("2016-04-29")
end_date <- as.Date("2024-01-01")
dates <- seq(start_date, end_date, by = "day")

# Transform aggregation_df into xts
xts_list <- lapply(names(aggregation_df), function(colname) {
  xts(aggregation_df[[colname]], order.by = dates)
})

# Transform aggregation_diff_df into xts
xts_diff_list <- lapply(names(aggregation_diff_df), function(colname) {
  xts(aggregation_diff_df[[colname]], order.by = seq(as.Date("2016-04-30"), end_date, by = "day"))
})

# Assign names to the columns
names(xts_list) <- names(aggregation_df)
names(xts_diff_list) <- names(aggregation_diff_df)

# Check for seasonality in xts_list

# Friedman Rank test (nonparametric test)
for (x in names(xts_list)) {
  print(x)
  print(fried(xts_list[[x]], freq = freq_xts(xts_list[[x]]), diff = FALSE, residuals = FALSE, autoarima = FALSE))
  print("---------------------------------------")
}

# Check for seasonality in xts_diff_list

# Friedman Rank test
for (x in names(xts_diff_list)) {
  print(x)
  print(fried(xts_diff_list[[x]], freq = freq_xts(xts_diff_list[[x]]), diff = FALSE, residuals = FALSE, autoarima = FALSE))
  print("---------------------------------------")
}
