# R Code for Item Selection Summaries

# Load necessary libraries
library(dplyr)
library(tidyr)

# Sample data frame (replace this with actual data)
data <- data.frame(
    domain = rep(c('Domain1', 'Domain2'), each = 10),
    difficulty = rep(c('Easy', 'Medium', 'Hard'), length.out = 20),
    score = rnorm(20)
)

# Generate summary tables by domain and difficulty

# Summary by Domain
domain_summary <- data %>%
    group_by(domain) %>%
    summarise(
        mean_score = mean(score),
        sd_score = sd(score),
        n = n()
    )

# Summary by Difficulty
difficulty_summary <- data %>%
    group_by(difficulty) %>%
    summarise(
        mean_score = mean(score),
        sd_score = sd(score),
        n = n()
    )

# Summary by Domain and Difficulty
combined_summary <- data %>%
    group_by(domain, difficulty) %>%
    summarise(
        mean_score = mean(score),
        sd_score = sd(score),
        n = n()
    )

# Display the summaries
print(domain_summary)
print(difficulty_summary)
print(combined_summary)