# Shared Utility Functions for DAACS Math QA Pipelines

# Function to calculate the mean
calculate_mean <- function(values) {
  return(mean(values, na.rm = TRUE))
}

# Function to calculate the median
calculate_median <- function(values) {
  return(median(values, na.rm = TRUE))
}

# Function to calculate standard deviation
calculate_sd <- function(values) {
  return(sd(values, na.rm = TRUE))
}

# Function to validate input values
validate_values <- function(values) {
  if (any(is.na(values))) {
    stop("Values contain NA.")
  }
}

# Function to normalize values
normalize_values <- function(values) {
  return((values - min(values)) / (max(values) - min(values)))
}

# Additional utility functions can be added as needed...
