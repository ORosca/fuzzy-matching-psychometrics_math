# Standardizing Legacy UMGC1 + UA2 Wide Datasets

# Load required libraries
library(dplyr)
library(tidyr)

# Define function to clean the dataset
clean_data <- function(data) {
  # Standardize column names
  data <- data %>% 
    rename_with(tolower) %>% 
    rename_with(~ gsub("[\s\-]+", "_", .))

  # Handle missing values
  data <- data %>% 
    mutate(across(everything(), ~ replace_na(., 0)))

  # Additional cleaning steps can be added here

  return(data)
}

# Load dataset
# dataset <- read.csv("path/to/your/dataset.csv")

# Clean dataset
# cleaned_dataset <- clean_data(dataset)

# Save cleaned dataset
# write.csv(cleaned_dataset, "path/to/your/cleaned_dataset.csv", row.names = FALSE)