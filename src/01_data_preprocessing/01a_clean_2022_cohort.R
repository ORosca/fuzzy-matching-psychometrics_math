# math_v2_umgc1ua2-anSamp2-2022_qa_pipeline.R
# ============================================================
# QA pipeline for the already-wide UMGC1 + UA2 AnSamp2 math file
#
# Purpose:
# 1. Load the already-wide 2022 UMGC1 + UA2 math dataset
# 2. Rename item columns from legacy qid_ua2 names to final QIDs
# 3. Standardize key demographic and metadata variables
# 4. Create clean UMGC1, UA2, and combined wide datasets
# 5. Run item- and dataset-level QA summaries
#
# Key conventions:
# - age_d24: TCAUS if age < 24, AUS if age >= 24
# - ethnicity: White / Asian / Black / Hispanic / Other
# - pell: No / Yes
# - military: No / Yes
# - transfer: continuous transferred credits
# - mathTime: seconds
#
# Unit of analysis in final outputs:
# one row per student (global_id)
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(readxl)
  library(stringr)
  library(tidyr)
  library(purrr)
  library(tibble)
  library(ggplot2)
})

# ============================================================
# Section 1. Configuration
# ============================================================

project_dir <- "C:/Users/orosc/OneDrive/Math"
source(file.path(project_dir, "utils_math_pipeline.R"))

input_rds <- file.path(project_dir, "math.items_AnSamp2_umgc1ua2.rds")
input_csv <- NULL

mapping_xlsx <- file.path(project_dir, "MappingQID_math.xlsx")

output_dir <- file.path(
  project_dir,
  "math_v2_umgc1ua2-anSamp2-2022_qa_outputs"
)
final_output_dir <- file.path(output_dir, "final_clean_datasets")

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(final_output_dir, showWarnings = FALSE, recursive = TRUE)

WAVE_YEAR <- 2022L
ADULT_AGE_CUTOFF <- 24L

stopifnot(file.exists(input_rds) || (!is.null(input_csv) && file.exists(input_csv)))
stopifnot(file.exists(mapping_xlsx))

# ============================================================
# Section 2. Standardize the wide dataset
# ============================================================
standardize_wide_math <- 
  function(df, wave = WAVE_YEAR, adult_age_cutoff = ADULT_AGE_CUTOFF, deduplicate = TRUE) {
  assert_has_cols(df, c("DAACS_ID", "college"), "wide math data")

  df %>%
    mutate(
      college = tolower(as.character(college)),
      college = case_when(
        college %in% c("umgc1", "umgc") ~ "umgc1",
        college %in% c("ua2", "ua") ~ "ua2",
        TRUE ~ college
      ),

      DAACS_ID = normalize_daacs_id(DAACS_ID),

      # restore original UA2 DAACS_ID by subtracting 10000
      DAACS_ID_num = suppressWarnings(as.numeric(DAACS_ID)),
      DAACS_ID_num = case_when(
        college == "ua2" & !is.na(DAACS_ID_num) & DAACS_ID_num >= 10000 ~ DAACS_ID_num - 10000,
        TRUE ~ DAACS_ID_num
      ),
      DAACS_ID = if_else(
        !is.na(DAACS_ID_num),
        sprintf("%08d", as.integer(DAACS_ID_num)),
        DAACS_ID
      ),

      wave = as.integer(wave),
      global_id = make_global_id(college, wave, DAACS_ID),

      age = if ("Age" %in% names(.)) suppressWarnings(as.numeric(Age)) else NA_real_,
      age_d24 = case_when(
        is.na(age) ~ NA_character_,
        age < adult_age_cutoff ~ "TCAUS",
        age >= adult_age_cutoff ~ "AUS",
        TRUE ~ NA_character_
      ),
      age_d24 = factor(age_d24, levels = c("TCAUS", "AUS")),

      gender = if ("gender" %in% names(.)) as.character(gender) else NA_character_,
      gender = case_when(
        gender %in% c("Male", "Female") ~ gender,
        TRUE ~ NA_character_
      ),

      ethnicity = if ("ethnicity" %in% names(.)) recode_ethnicity_common(ethnicity) else NA_character_,
      ethnicity = factor(
        ethnicity,
        levels = c("White", "Asian", "Black", "Hispanic", "Other")
      ),

      military = if ("Military" %in% names(.)) recode_yes_no(Military) else NA_character_,
      military = factor(military, levels = c("No", "Yes")),

      pell = if ("Pell" %in% names(.)) recode_yes_no(Pell) else NA_character_,
      pell = factor(pell, levels = c("No", "Yes")),

      # continuous transferred credits for consistency with later waves
      transfer = if ("credits_transferred" %in% names(.)) {
        suppressWarnings(as.numeric(credits_transferred))
      } else {
        NA_real_
      },

      credits_transferred = if ("credits_transferred" %in% names(.)) {
        suppressWarnings(as.numeric(credits_transferred))
      } else {
        NA_real_
      },

      mathCompletionDate = if ("mathCompletionDate" %in% names(.)) {
        as.POSIXct(mathCompletionDate)
      } else {
        as.POSIXct(NA)
      },

      # keep mathTime explicitly in seconds
      mathTime = if ("mathTime" %in% names(.)) {
        suppressWarnings(as.integer(mathTime))
      } else {
        NA_integer_
      }
    ) %>%
    
  select(
    DAACS_ID, global_id, college, wave,
    age, age_d24, gender, ethnicity, military, pell, transfer,
    everything(),
    -DAACS_ID_num,
    -any_of(c("Age", "Age_d24", "Military", "Pell"))
  ) %>%
    reorder_item_columns() %>%
    { if (deduplicate) distinct(., global_id, .keep_all = TRUE) else . }
  }

# ============================================================
# Section 3. QA functions
# ============================================================

run_qa_wide <- function(df, dataset_name, output_dir) {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  dupes <- qa_duplicates(df)
  ident <- qa_identical_response_patterns(df)
  nondisc <- qa_non_discriminative_items(df)
  ranges <- qa_value_ranges(df)
  item_table <- qa_item_counts(df)
  diff_summary <- qa_response_counts_by_difficulty(item_table)
  sample_size_report <- make_item_sample_size_report(df)
  math_time_summary <- qa_check_math_time_seconds(df, dataset_name)

  out <- list(
    dataset_name = dataset_name,
    n_students = nrow(df),
    n_items = length(get_item_cols(df)),
    duplicates = dupes,
    identical_patterns = ident,
    non_discriminative_items = nondisc,
    item_ranges = ranges,
    item_response_counts = item_table,
    response_counts_by_difficulty = diff_summary,
    sample_size_overall = sample_size_report$overall_counts,
    sample_size_by_demo = sample_size_report$by_demo_counts,
    math_time_summary = math_time_summary
  )

  saveRDS(out, file.path(output_dir, paste0(dataset_name, "_qa_results.rds")))
  write.csv(ranges, file.path(output_dir, paste0(dataset_name, "_item_ranges.csv")), row.names = FALSE)
  write.csv(item_table, file.path(output_dir, paste0(dataset_name, "_item_counts.csv")), row.names = FALSE)
  write.csv(
    sample_size_report$overall_counts,
    file.path(output_dir, paste0(dataset_name, "_sample_size_overall.csv")),
    row.names = FALSE
  )
  write.csv(
    sample_size_report$by_demo_counts,
    file.path(output_dir, paste0(dataset_name, "_sample_size_by_demo.csv")),
    row.names = FALSE
  )
  
  write.csv(
    math_time_summary,
    file.path(output_dir, paste0(dataset_name, "_math_time_summary.csv")),
    row.names = FALSE
  )
  
  if (!is.null(diff_summary)) {
    write.csv(
      diff_summary,
      file.path(output_dir, paste0(dataset_name, "_counts_by_difficulty.csv")),
      row.names = FALSE
    )
  }
  plot_response_count_density(
    item_table,
    out_file = file.path(output_dir, paste0(dataset_name, "_density_counts.png")),
    title = paste0(dataset_name, ": Distribution of Item Response Counts")
  )
  
  out
}

# ============================================================
# Section 4. Load, clean, split, and save
# ============================================================

wide_raw <- load_wide_input(input_rds, input_csv)
qid_map <- load_qid_mapping(mapping_xlsx)

wide_renamed <- rename_item_columns_to_final_qid(wide_raw, qid_map)
wide_std <- standardize_wide_math(
  wide_renamed,
  wave = WAVE_YEAR,
  deduplicate = FALSE
)

pre_dedup_duplicates <- qa_duplicates(wide_std)

wide_dedup <- wide_std %>%
  distinct(global_id, .keep_all = TRUE)

speed_filter <- filter_speedy_math_students(
  wide_dedup,
  min_items = 18L,
  min_seconds = 180L
)

wide_clean <- speed_filter$data

saveRDS(
  speed_filter,
  file.path(output_dir, "speedy_math_filter_results.rds")
)

write.csv(
  speed_filter$summary,
  file.path(output_dir, "speedy_math_filter_summary.csv"),
  row.names = FALSE
)

write.csv(
  speed_filter$flagged_cases,
  file.path(output_dir, "speedy_math_flagged_cases.csv"),
  row.names = FALSE
)

saveRDS(
  pre_dedup_duplicates,
  file.path(output_dir, "pre_dedup_duplicate_summary.rds")
)

write.csv(
  pre_dedup_duplicates$summary,
  file.path(output_dir, "pre_dedup_duplicate_summary.csv"),
  row.names = FALSE
)

write.csv(
  pre_dedup_duplicates$duplicate_values,
  file.path(output_dir, "pre_dedup_duplicate_values.csv"),
  row.names = FALSE
)

math_umgc1_anSamp2_wide <- wide_clean %>%
  filter(college == "umgc1") %>%
  distinct(global_id, .keep_all = TRUE)

math_ua2_anSamp2_wide <- wide_clean %>%
  filter(college == "ua2") %>%
  distinct(global_id, .keep_all = TRUE)

math_umgc1ua2_anSamp2_wide <- wide_clean %>%
  distinct(global_id, .keep_all = TRUE)

save_both(math_umgc1_anSamp2_wide, final_output_dir, "math_umgc1_anSamp2_wide")
save_both(math_ua2_anSamp2_wide, final_output_dir, "math_ua2_anSamp2_wide")
save_both(math_umgc1ua2_anSamp2_wide, final_output_dir, "math_umgc1ua2_anSamp2_wide")

# ============================================================
# Section 5. Run QA on each clean dataset
# ============================================================

qa_umgc1_anSamp2 <- run_qa_wide(
  df = math_umgc1_anSamp2_wide,
  dataset_name = "math_umgc1_anSamp2_wide",
  output_dir = file.path(output_dir, "qa_umgc1_anSamp2")
)

qa_ua2_anSamp2 <- run_qa_wide(
  df = math_ua2_anSamp2_wide,
  dataset_name = "math_ua2_anSamp2_wide",
  output_dir = file.path(output_dir, "qa_ua2_anSamp2")
)

qa_umgc1ua2_combined <- run_qa_wide(
  df = math_umgc1ua2_anSamp2_wide,
  dataset_name = "math_umgc1ua2_anSamp2_wide",
  output_dir = file.path(output_dir, "qa_umgc1ua2_combined")
)

# ============================================================
# Section 6. Final checks and console summary
# ============================================================

stopifnot(sum(duplicated(math_umgc1_anSamp2_wide$global_id)) == 0)
stopifnot(sum(duplicated(math_ua2_anSamp2_wide$global_id)) == 0)
stopifnot(sum(duplicated(math_umgc1ua2_anSamp2_wide$global_id)) == 0)

stopifnot(length(get_item_cols(math_umgc1_anSamp2_wide)) > 0)
stopifnot(length(get_item_cols(math_ua2_anSamp2_wide)) > 0)
stopifnot(length(get_item_cols(math_umgc1ua2_anSamp2_wide)) > 0)

cat("\nSaved clean datasets to:\n", final_output_dir, "\n")

cat("\nRows:\n")
cat("UMGC1: ", nrow(math_umgc1_anSamp2_wide), "\n")
cat("UA2:   ", nrow(math_ua2_anSamp2_wide), "\n")
cat("Combined: ", nrow(math_umgc1ua2_anSamp2_wide), "\n")

cat("\nDuplicate global_id counts:\n")
cat("UMGC1: ", qa_umgc1_anSamp2$duplicates$duplicate_global_id_n, "\n")
cat("UA2:   ", qa_ua2_anSamp2$duplicates$duplicate_global_id_n, "\n")
cat("Combined: ", qa_umgc1ua2_combined$duplicates$duplicate_global_id_n, "\n")

