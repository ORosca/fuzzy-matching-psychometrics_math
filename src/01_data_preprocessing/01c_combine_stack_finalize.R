# Load necessary libraries
library(dplyr)
library(tidyr)
library(readr)

# Function to combine math wide files
combine_math_files <- function(file1, file2) {
  data1 <- read_csv(file1)
  data2 <- read_csv(file2)
  combined_data <- bind_rows(data1, data2)
  return(combined_data)
}

# Stacking the 2022 and 2022-24 math wide files
math_2022 <- "path/to/2022_math_wide_file.csv"
math_2022_24 <- "path/to/2022-24_math_wide_file.csv"
final_data <- combine_math_files(math_2022, math_2022_24)

# Running missingness diagnostics
missingness_analysis <- final_data %>% summarize(across(everything(), ~ sum(is.na(.))))

# Detecting and handling duplicate students
final_data <- final_data %>%
  group_by(student_id) %>%
  mutate(duplicate = n() > 1) %>%
  ungroup() %>%
  filter(!(duplicate & demographic_alignment))

# Patching for UMGC/UMGC1 rows
final_data <- final_data %>% mutate(school = ifelse(school == "UMGC1", "UMGC", school))

# Dropping uninformative rows
final_data <- final_data %>% filter(complete.cases(.))

# Creating final summary tables
summary_table <- final_data %>% group_by(demographics) %>% summarize(mean_score = mean(score, na.rm = TRUE))

# Output results
write_csv(summary_table, "path/to/final_summary_table.csv")