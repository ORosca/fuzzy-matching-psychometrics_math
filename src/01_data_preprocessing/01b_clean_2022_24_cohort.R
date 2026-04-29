# math_v2_ua23umgc-2022-24_qa_pipeline.R
# ============================================================
# QA pipeline for UA23 + UMGC math datasets (2022-2024)
#
# Purpose:
# 1. Load institution-level and item-level raw files
# 2. Standardize institution-level demographics and identifiers
# 3. Standardize item-level response files and attach final QIDs
# 4. Exclude students with fewer than 18 answered items
# 5. Derive student-level mathCompletionDate and mathTime
# 6. Build clean long and wide math datasets for UA23, UMGC, and combined
# 7. Export QA summaries and cleaned outputs
#
# Key conventions:
# - age_d24: TCAUS if age < 24, AUS if age >= 24
# - ethnicity: White / Asian / Black / Hispanic / Other
# - pell: No / Yes
# - military: No / Yes
# - transfer: continuous transferred credits
# - mathTime: seconds
#
# Unit of analysis:
# - long files: one row per student-item response
# - wide files: one row per student (global_id)
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(readxl)
  library(stringr)
  library(tibble)
  library(lubridate)
  library(ggplot2)
})

# ============================================================
# Section 0. Configuration
# ============================================================

project_dir <- "C:/Users/orosc/OneDrive/Math"
source(file.path(project_dir, "utils_math_pipeline.R"))

map_file <- file.path(project_dir, "MappingQID_math.xlsx")

institution_ua23_file <- file.path("D:/DData24/institution.ua23.treat.rds")
institution_umgc_file <- file.path("D:/DData24/institution.umgc.treat.rds")

items_ua23_file <- file.path("D:/DData24/math.item.results.ua23.treat.rds")
items_umgc_file <- file.path("D:/DData24/math.item.results.umgc.treat.rds")

output_dir <- file.path(project_dir, "math_v2_ua23umgc-2022-24_qa_outputs")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

MIN_ITEMS <- 18L
UA23_WAVE <- 2023L
ADULT_AGE_CUTOFF <- 24L

stopifnot(file.exists(map_file))
stopifnot(file.exists(institution_ua23_file))
stopifnot(file.exists(institution_umgc_file))
stopifnot(file.exists(items_ua23_file))
stopifnot(file.exists(items_umgc_file))

# ============================================================
# Section 1. Load files
# ============================================================

map <- read_excel(map_file)

institution.ua23.treat <- readRDS(institution_ua23_file)
institution.umgc.treat <- readRDS(institution_umgc_file)

math.item.results.ua23.treat <- readRDS(items_ua23_file)
math.item.results.umgc.treat <- readRDS(items_umgc_file)

# ============================================================
# Section 2. Final QID lookup
# ============================================================

assert_has_cols(map, c("question_id", "QID"), "map")

qid_lookup <- map %>%
  transmute(
    question_id = as.character(question_id),
    QID = as.character(QID),
    assigned_difficulty = as.character(assigned_difficulty),
    map_domain = as.character(domain),
    map_question = as.character(question)
  ) %>%
  distinct(question_id, .keep_all = TRUE)

# ============================================================
# Section 3. Standardize institution-level data
# ============================================================

standardize_institution_umgc <- function(df, adult_age_cutoff = ADULT_AGE_CUTOFF) {
  df %>%
    mutate(
      DAACS_ID = normalize_daacs_id(DAACS_ID),
      wave = as.integer(lubridate::year(FYE_EndDate)),
      global_id = make_global_id("umgc", wave, DAACS_ID),
      college = "umgc",
      
      age = suppressWarnings(as.numeric(Age)),
      age_d24 = case_when(
        is.na(age) ~ NA_character_,
        age < adult_age_cutoff ~ "TCAUS",
        age >= adult_age_cutoff ~ "AUS",
        TRUE ~ NA_character_
      ),
      age_d24 = factor(age_d24, levels = c("TCAUS", "AUS")),
      
      gender = case_when(
        as.character(Gender) %in% c("Male", "Female") ~ as.character(Gender),
        TRUE ~ NA_character_
      ),
      
      ethnicity = recode_ethnicity_common(Ethnicity),
      ethnicity = factor(
        ethnicity,
        levels = c("White", "Asian", "Black", "Hispanic", "Other")
      ),
      
      military = recode_yes_no(MilitaryStudent),
      military = factor(military, levels = c("No", "Yes")),
      
      pell = recode_yes_no(PELL),
      pell = factor(pell, levels = c("No", "Yes")),
      
      transfer = suppressWarnings(as.numeric(Transfer_Credits)),
      
      treat = as.character(Treat),
      fye_course = as.character(FYE_Course),
      fye_semester = as.character(FYE_Semester)
    )
}

standardize_institution_ua23 <- function(df, wave = UA23_WAVE, adult_age_cutoff = ADULT_AGE_CUTOFF) {
  df %>%
    mutate(
      DAACS_ID = normalize_daacs_id(DAACS_ID),
      wave = as.integer(wave),
      global_id = make_global_id("ua23", wave, DAACS_ID),
      college = "ua23",
      
      age = suppressWarnings(as.numeric(Age)),
      age_d24 = case_when(
        is.na(age) ~ NA_character_,
        age < adult_age_cutoff ~ "TCAUS",
        age >= adult_age_cutoff ~ "AUS",
        TRUE ~ NA_character_
      ),
      age_d24 = factor(age_d24, levels = c("TCAUS", "AUS")),
      
      gender = case_when(
        as.character(Gender) %in% c("Male", "Female") ~ as.character(Gender),
        TRUE ~ NA_character_
      ),
      
      ethnicity = recode_ethnicity_common(Ethnicity),
      ethnicity = factor(
        ethnicity,
        levels = c("White", "Asian", "Black", "Hispanic", "Other")
      ),
      
      military = case_when(
        is.na(Military) ~ NA_character_,
        Military ~ "Yes",
        !Military ~ "No"
      ),
      military = factor(military, levels = c("No", "Yes")),
      
      pell = case_when(
        is.na(PELL) ~ NA_character_,
        PELL ~ "Yes",
        !PELL ~ "No"
      ),
      pell = factor(pell, levels = c("No", "Yes")),
      
      transfer = suppressWarnings(as.numeric(Transfer_Credits)),
      
      treat = as.character(Treat),
      fye_course = as.character(FYE_Course),
      fye_semester = as.character(FYE_Semester)
    )
}

institution_ua23_std <- standardize_institution_ua23(institution.ua23.treat, wave = UA23_WAVE)
institution_umgc_std <- standardize_institution_umgc(institution.umgc.treat)

# ============================================================
# Section 4. Standardize item-level response files
# ============================================================

standardize_item_results_raw <- function(df, college_name, institution_std, qid_lookup) {
  assert_has_cols(df, c("DAACS_ID", "question_id", "block", "domainId", "score", "startDate"), college_name)
  assert_has_cols(institution_std, c("DAACS_ID", "wave"), paste0(college_name, "_institution_std"))
  
  wave_lookup <- institution_std %>%
    select(DAACS_ID, wave) %>%
    distinct(DAACS_ID, .keep_all = TRUE)
  
  completion_var <- if ("completeDate" %in% names(df)) "completeDate" else "completionDate"
  
  df %>%
    mutate(
      DAACS_ID = normalize_daacs_id(DAACS_ID),
      question_id = as.character(question_id),
      block = as.integer(block),
      domainId = as.character(domainId),
      score = as.integer(score),
      college = college_name,
      startDate = as.POSIXct(startDate),
      completion_dt = as.POSIXct(.data[[completion_var]])
    ) %>%
    left_join(wave_lookup, by = "DAACS_ID") %>%
    mutate(
      global_id = make_global_id(college_name, wave, DAACS_ID)
    ) %>%
    left_join(qid_lookup, by = "question_id")
}

items_ua23_std <- standardize_item_results_raw(
  df = math.item.results.ua23.treat,
  college_name = "ua23",
  institution_std = institution_ua23_std,
  qid_lookup = qid_lookup
)

items_umgc_std <- standardize_item_results_raw(
  df = math.item.results.umgc.treat,
  college_name = "umgc",
  institution_std = institution_umgc_std,
  qid_lookup = qid_lookup
)

# ============================================================
# Section 5. Exclude students with fewer than 18 answered items
# ============================================================

items_ua23_std <- filter_min_items(items_ua23_std, min_items = MIN_ITEMS)
items_umgc_std <- filter_min_items(items_umgc_std, min_items = MIN_ITEMS)

# ============================================================
# Section 6. Derive student-level timing
# ============================================================

make_student_math_timing <- function(items_std) {
  items_std %>%
    group_by(global_id) %>%
    summarise(
      mathCompletionDate = suppressWarnings(max(completion_dt, na.rm = TRUE)),
      mathStartDate = suppressWarnings(min(startDate, na.rm = TRUE)),
      mathTime = as.integer(difftime(mathCompletionDate, mathStartDate, units = "secs")),
      .groups = "drop"
    ) %>%
    mutate(
      mathCompletionDate = if_else(
        is.infinite(as.numeric(mathCompletionDate)),
        as.POSIXct(NA),
        mathCompletionDate
      ),
      mathStartDate = if_else(
        is.infinite(as.numeric(mathStartDate)),
        as.POSIXct(NA),
        mathStartDate
      ),
      mathTime = if_else(
        is.na(mathCompletionDate) | is.na(mathStartDate),
        NA_integer_,
        mathTime
      )
    ) %>%
    select(global_id, mathCompletionDate, mathTime)
}

ua23_timing <- make_student_math_timing(items_ua23_std)
umgc_timing <- make_student_math_timing(items_umgc_std)

institution_ua23_std <- institution_ua23_std %>%
  left_join(ua23_timing, by = "global_id")

institution_umgc_std <- institution_umgc_std %>%
  left_join(umgc_timing, by = "global_id")

qa_check_math_time_seconds(institution_ua23_std, "institution_ua23_std")
qa_check_math_time_seconds(institution_umgc_std, "institution_umgc_std")

# final long files
math_items_ua23_long <- items_ua23_std
math_items_umgc_long <- items_umgc_std

# ============================================================
# Section 7. Keep only institution rows with retained math data
# ============================================================

institution_ua23_std <- institution_ua23_std %>%
  semi_join(math_items_ua23_long %>% distinct(global_id), by = "global_id")

institution_umgc_std <- institution_umgc_std %>%
  semi_join(math_items_umgc_long %>% distinct(global_id), by = "global_id")

# ============================================================
# Section 8. Build wide student-level math files
# ============================================================

select_std_student_block <- function(df) {
  df %>%
    select(
      DAACS_ID, global_id, college, wave,
      age, age_d24, gender, ethnicity, military, pell, transfer,
      treat, fye_course, fye_semester,
      any_of(c("mathCompletionDate", "mathTime"))
    ) %>%
    distinct(global_id, .keep_all = TRUE)
}

make_math_wide <- function(items_std, institution_std) {
  item_wide <- items_std %>%
    filter(!is.na(QID)) %>%
    select(global_id, QID, score) %>%
    distinct(global_id, QID, .keep_all = TRUE) %>%
    pivot_wider(
      names_from = QID,
      values_from = score
    )
  
  qcols <- order_qid_cols(names(item_wide))
  
  item_wide <- item_wide %>%
    select(global_id, all_of(qcols))
  
  institution_std_keep <- institution_std %>%
    semi_join(items_std %>% distinct(global_id), by = "global_id")
  
  select_std_student_block(institution_std_keep) %>%
    left_join(item_wide, by = "global_id")
}

math_ua23_wide <- make_math_wide(math_items_ua23_long, institution_ua23_std)
math_umgc_wide <- make_math_wide(math_items_umgc_long, institution_umgc_std)

math_ua23umgc_wide <- bind_rows(
  math_ua23_wide,
  math_umgc_wide
) %>%
  distinct(global_id, .keep_all = TRUE)

# ============================================================
# Section 9. QA checks
# ============================================================

stopifnot(sum(duplicated(math_ua23_wide$global_id)) == 0)
stopifnot(sum(duplicated(math_umgc_wide$global_id)) == 0)
stopifnot(sum(duplicated(math_ua23umgc_wide$global_id)) == 0)

stopifnot(sum(count_answered_items(math_ua23_wide) < MIN_ITEMS) == 0)
stopifnot(sum(count_answered_items(math_umgc_wide) < MIN_ITEMS) == 0)

stopifnot(length(get_item_cols(math_ua23_wide)) == 174)
stopifnot(length(get_item_cols(math_umgc_wide)) == 174)
stopifnot(length(get_item_cols(math_ua23umgc_wide)) == 174)

qa_ua23umgc_summary <- list(
  institution_ua23_n = nrow(institution_ua23_std),
  institution_umgc_n = nrow(institution_umgc_std),
  
  math_items_ua23_long_n = nrow(math_items_ua23_long),
  math_items_umgc_long_n = nrow(math_items_umgc_long),
  
  ua23_unique_question_ids = n_distinct(math_items_ua23_long$question_id),
  umgc_unique_question_ids = n_distinct(math_items_umgc_long$question_id),
  
  ua23_unique_qids = n_distinct(math_items_ua23_long$QID, na.rm = TRUE),
  umgc_unique_qids = n_distinct(math_items_umgc_long$QID, na.rm = TRUE),
  
  ua23_missing_qid_rows = sum(is.na(math_items_ua23_long$QID)),
  umgc_missing_qid_rows = sum(is.na(math_items_umgc_long$QID)),
  
  math_ua23_wide_n = nrow(math_ua23_wide),
  math_umgc_wide_n = nrow(math_umgc_wide),
  math_ua23umgc_wide_n = nrow(math_ua23umgc_wide)
)

print(qa_ua23umgc_summary)

ua23_duplicate_item_keys <- math_items_ua23_long %>%
  count(global_id, question_id) %>%
  filter(n > 1)

umgc_duplicate_item_keys <- math_items_umgc_long %>%
  count(global_id, question_id) %>%
  filter(n > 1)

# ============================================================
# Section 10. Save outputs
# ============================================================

save_both(institution_ua23_std, output_dir, "institution_ua23_std")
save_both(institution_umgc_std, output_dir, "institution_umgc_std")

save_both(math_items_ua23_long, output_dir, "math_items_ua23_long")
save_both(math_items_umgc_long, output_dir, "math_items_umgc_long")

save_both(math_ua23_wide, output_dir, "math_ua23_wide")
save_both(math_umgc_wide, output_dir, "math_umgc_wide")
save_both(math_ua23umgc_wide, output_dir, "math_ua23umgc_wide")

saveRDS(qa_ua23umgc_summary, file.path(output_dir, "qa_ua23umgc_summary.rds"))

write.csv(
  tibble(metric = names(qa_ua23umgc_summary), value = unlist(qa_ua23umgc_summary)),
  file.path(output_dir, "qa_summary.csv"),
  row.names = FALSE
)

write.csv(
  ua23_duplicate_item_keys,
  file.path(output_dir, "ua23_duplicate_item_keys.csv"),
  row.names = FALSE
)

write.csv(
  umgc_duplicate_item_keys,
  file.path(output_dir, "umgc_duplicate_item_keys.csv"),
  row.names = FALSE
)