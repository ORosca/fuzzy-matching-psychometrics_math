# math_summary_table_for_item_selection.R
# ============================================================
# Create six summary tables for item selection in the planned
# math study by counting eligible items across six math domains
# and four difficulty categories under response-count thresholds.
#
# Purpose:
# 1. Read item-level response-count summaries from
#    item_sample_size_by_demo.csv.
# 2. Derive each item's math domain from the first lowercase
#    letter in QID:
#       g = geometry
#       l = lines_and_functions
#       n = number_and_calculation
#       s = statistics
#       v = variables_and_equations
#       w = word_problems
# 3. Derive each item's difficulty category from QID and group
#    items into EASY, MEDIUM, HARD, and HARD_P.
# 4. Build six domain-by-difficulty tables for the Summary Table
#    for Item Selection:
#       - IRT 2PL Fit: 300 responses
#       - IRT 2PL Fit: 150 responses
#       - Multivariable DIF: 200 per group
#       - Multivariable DIF: 50 per group
#       - Factor Level: 100 per level
#       - Factor Level: 40 per level
# 5. Save the six tables to an Excel workbook and a CSV file
#    for use in the planned paper.
# ============================================================

library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(openxlsx)

project_dir <- "C:/Users/orosc/OneDrive/Math"

output_dir <- file.path(project_dir, "math_v2_umgc_ua_22_24_combined_qa_outputs")

item_file <- file.path(project_dir, "math_v2_umgc_ua_22_24_combined_qa_outputs",
                       "item_sample_size_by_demo.csv")
item_sample_size_by_demo <- read_csv(item_file, show_col_types = FALSE)

# ----------------------------
# helpers
# ----------------------------

difficulty4_from_qid <- function(x) {
  case_when(
    str_detect(x, "[A-Za-z]M(_p)?$") ~ "MEDIUM",
    str_detect(x, "[A-Za-z]E(_p)?$") ~ "EASY",
    str_detect(x, "[A-Za-z]H_p$")    ~ "HARD_P",
    str_detect(x, "[A-Za-z]H$")      ~ "HARD",
    TRUE ~ NA_character_
  )
}

domain_from_qid <- function(x) {
  letter <- str_match(x, "^Q\\d{3}([glnsvw])")[, 2]
  
  case_when(
    letter == "g" ~ "geometry",
    letter == "l" ~ "lines_and_functions",
    letter == "n" ~ "number_and_calculation",
    letter == "s" ~ "statistics",
    letter == "v" ~ "variables_and_equations",
    letter == "w" ~ "word_problems",
    TRUE ~ NA_character_
  )
}

domain_order <- c(
  "geometry",
  "lines_and_functions",
  "number_and_calculation",
  "statistics",
  "variables_and_equations",
  "word_problems"
)

difficulty_order <- c("EASY", "MEDIUM", "HARD", "HARD_P")

# ----------------------------
# build item lookup from QID
# ----------------------------

qid_lookup <- item_sample_size_by_demo %>%
  distinct(QID) %>%
  mutate(
    domain = domain_from_qid(QID),
    difficulty = difficulty4_from_qid(QID)
  )

# ----------------------------
# threshold metrics
# ----------------------------

# IRT overall:
# use the largest complete demographic-partition total per item
overall_n <- item_sample_size_by_demo %>%
  group_by(QID, demographic) %>%
  summarise(n_responded = sum(n_responded), .groups = "drop") %>%
  group_by(QID) %>%
  summarise(overall_n = max(n_responded), .groups = "drop")

# Multivariable DIF:
# require threshold in each gender and military group
min_gender_military_n <- item_sample_size_by_demo %>%
  filter(
    demographic %in% c("gender", "military"),
    !is.na(group_value),
    group_value != "NA",
    group_value != ""
  ) %>%
  group_by(QID) %>%
  summarise(min_gender_military_n = min(n_responded), .groups = "drop")

# Factor level:
# require threshold in every non-missing factor level
min_any_factor_level_n <- item_sample_size_by_demo %>%
  filter(
    !is.na(group_value),
    group_value != "NA",
    group_value != ""
  ) %>%
  group_by(QID) %>%
  summarise(min_any_factor_level_n = min(n_responded), .groups = "drop")

summary_df <- qid_lookup %>%
  left_join(overall_n, by = "QID") %>%
  left_join(min_gender_military_n, by = "QID") %>%
  left_join(min_any_factor_level_n, by = "QID")

# ----------------------------
# function to make one table
# ----------------------------

make_domain_difficulty_table <- function(df, metric_col, threshold) {
  out <- df %>%
    filter(.data[[metric_col]] >= threshold) %>%
    count(domain, difficulty, name = "n_items") %>%
    pivot_wider(
      names_from = difficulty,
      values_from = n_items,
      values_fill = 0
    )
  
  out <- tibble(domain = domain_order) %>%
    left_join(out, by = "domain")
  
  for (nm in difficulty_order) {
    if (!nm %in% names(out)) out[[nm]] <- 0L
  }
  
  out %>%
    select(domain, all_of(difficulty_order)) %>%
    mutate(
      Total = EASY + MEDIUM + HARD + HARD_P
    )
}

# ----------------------------
# six tables
# ----------------------------

tbl_irt_300 <- make_domain_difficulty_table(summary_df, "overall_n", 300)
tbl_irt_150 <- make_domain_difficulty_table(summary_df, "overall_n", 150)

tbl_dif_200 <- make_domain_difficulty_table(summary_df, "min_gender_military_n", 200)
tbl_dif_50  <- make_domain_difficulty_table(summary_df, "min_gender_military_n", 50)

tbl_factor_100 <- make_domain_difficulty_table(summary_df, "min_any_factor_level_n", 100)
tbl_factor_40  <- make_domain_difficulty_table(summary_df, "min_any_factor_level_n", 40)

# ----------------------------
# inspect in console
# ----------------------------

tbl_irt_300
tbl_irt_150
tbl_dif_200
tbl_dif_50
tbl_factor_100
tbl_factor_40

# ----------------------------
# save to Excel
# ----------------------------

out_xlsx <- file.path(project_dir, "Summary_Tables_for_Item_Selection_math.xlsx")

wb <- createWorkbook()

addWorksheet(wb, "README")
writeData(
  wb, "README",
  data.frame(
    Note = c(
      "File used: item_sample_size_by_demo.csv",
      "Domain was derived from the first lowercase letter in QID: g, l, n, s, v, w.",
      "Mapping used: g=geometry; l=lines_and_functions; n=number_and_calculation; s=statistics; v=variables_and_equations; w=word_problems.",
      "MEDIUM_P was counted as MEDIUM; EASY_P was counted as EASY; HARD and HARD_P were kept separate.",
      "IRT tables: overall item responses threshold.",
      "DIF tables: minimum item responses across gender and military groups threshold.",
      "Factor-level tables: minimum item responses across all non-missing factor levels threshold."
    )
  )
)

sheet_write <- function(wb, sheet, df, subtitle) {
  addWorksheet(wb, sheet)
  writeData(wb, sheet, subtitle, startRow = 1, startCol = 1)
  writeData(wb, sheet, df, startRow = 3, startCol = 1, withFilter = FALSE)
  
  hs <- createStyle(
    fontColour = "#FFFFFF",
    fgFill = "#1F4E78",
    halign = "center",
    textDecoration = "bold",
    border = "Bottom"
  )
  body <- createStyle(border = "Bottom")
  note <- createStyle(textDecoration = "italic")
  
  addStyle(wb, sheet, note, rows = 1, cols = 1, gridExpand = TRUE)
  addStyle(wb, sheet, hs, rows = 3, cols = 1:ncol(df), gridExpand = TRUE)
  addStyle(wb, sheet, body, rows = 4:(nrow(df) + 3), cols = 1:ncol(df), gridExpand = TRUE)
  
  setColWidths(wb, sheet, cols = 1:ncol(df), widths = c(28, 10, 10, 10, 10, 10))
  freezePane(wb, sheet, firstActiveRow = 4)
}

sheet_write(
  wb, "IRT_Frequentist_300", tbl_irt_300,
  "IRT 2PL Fit: minimum recommended frequentist threshold = 300 responses"
)
sheet_write(
  wb, "IRT_Bayesian_150", tbl_irt_150,
  "IRT 2PL Fit: minimum with Bayesian stabilization = 150 responses"
)

sheet_write(
  wb, "DIF_Frequentist_200", tbl_dif_200,
  "Multivariable DIF: minimum recommended frequentist threshold = 200 per gender/military group"
)
sheet_write(
  wb, "DIF_Bayesian_50", tbl_dif_50,
  "Multivariable DIF: minimum with Bayesian stabilization = 50 per gender/military group"
)

sheet_write(
  wb, "FactorLevel_Freq_100", tbl_factor_100,
  "Factor level: minimum recommended frequentist threshold = 100 per factor level"
)
sheet_write(
  wb, "FactorLevel_Bayes_40", tbl_factor_40,
  "Factor level: minimum with Bayesian stabilization = 40 per factor level"
)

saveWorkbook(wb, out_xlsx, overwrite = TRUE)

# a small check for items that were not included in the tables
summary_df %>%
  filter(is.na(domain) | is.na(difficulty))

# CSV bundle
bind_rows(
  tbl_irt_300 %>% mutate(table_name = "IRT_Frequentist_300"),
  tbl_irt_150 %>% mutate(table_name = "IRT_Bayesian_150"),
  tbl_dif_200 %>% mutate(table_name = "DIF_Frequentist_200"),
  tbl_dif_50  %>% mutate(table_name = "DIF_Bayesian_50"),
  tbl_factor_100 %>% mutate(table_name = "FactorLevel_Freq_100"),
  tbl_factor_40  %>% mutate(table_name = "FactorLevel_Bayes_40")
) %>%
  write_csv(file.path(output_dir, "Summary_Table_for_Item_Selection_math.csv"))