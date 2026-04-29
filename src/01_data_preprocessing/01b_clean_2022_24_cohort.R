# R Pipeline Code for Harmonizing and Filtering UA23 + UMGC Datasets from 2022-2024

# Load necessary libraries
library(dplyr)
library(tidyr)

# Function to harmonize datasets
harmonize_datasets <- function(dataset1, dataset2) {
  combined_data <- bind_rows(dataset1, dataset2)
  return(combined_data)
}

# Function to filter datasets based on year
filter_by_year <- function(data, start_year, end_year) {
  filtered_data <- data %>% filter(year >= start_year & year <= end_year)
  return(filtered_data)
}

# Load datasets
ua23_data <- read.csv('path/to/ua23_data.csv')
umgc_data <- read.csv('path/to/umgc_data.csv')

# Harmonize datasets
harmonized_data <- harmonize_datasets(ua23_data, umgc_data)

# Filter datasets for 2022-2024
final_data <- filter_by_year(harmonized_data, 2022, 2024)

# Save final dataset
write.csv(final_data, 'path/to/final_harmonized_filtered_data.csv', row.names = FALSE)