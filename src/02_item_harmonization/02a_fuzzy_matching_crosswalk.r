# math_umgc_ua23_questionid_to_qid_crosswalk

# Purpose:
# Create a reproducible question_id -> QID crosswalk for UMGC and UA23
# math item-result datasets using exact and fuzzy text matching.
#
# Inputs:
# - MappingQID_math.xlsx
# - math.assessments.umgc.rds
# - math.assessments.ua23.rds
# - math.item.results.umgc.treat.rds
# - math.item.results.ua23.treat.rds
#
# Outputs:
# - item-level question_id -> QID lookup tables
# - row-level math.items files with QID assigned
# - fuzzy candidate review files
# - diagnostics files

# ============================================================
# Crosswalk: question_id -> QID
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(readxl)
  library(tidyr)
  library(purrr)
  library(stringdist)
  library(readr)
})

# ------------------------------------------------------------
# Section 1. Load mapping and assessment tables
# ------------------------------------------------------------

project_dir <- "C:/Users/orosc/OneDrive/Math"

map <- read_excel(file.path(project_dir, "MappingQID_math.xlsx"))
math.assessments.umgc <- readRDS(file.path(project_dir, "math.assessments.umgc.rds"))
math.assessments.ua23 <- readRDS(file.path(project_dir, "math.assessments.ua23.rds"))
math.item.results.ua23.treat <- readRDS(file.path(project_dir, "math.item.results.ua23.treat.rds"))
math.item.results.umgc.treat <- readRDS(file.path(project_dir, "math.item.results.umgc.treat.rds"))

n_distinct(math.assessments.umgc$assessment_id)
n_distinct(math.assessments.ua23$assessment_id)
n_distinct(math.item.results.ua23.treat$assessment_id)
n_distinct(math.item.results.umgc.treat$assessment_id)

n_distinct(math.assessments.umgc$question_id)
n_distinct(math.assessments.ua23$question_id)
n_distinct(math.item.results.ua23.treat$question_id)
n_distinct(math.item.results.umgc.treat$question_id)

# ------------------------------------------------------------
# Section 1A. Standardize join keys in source files
# ------------------------------------------------------------

math.item.results.ua23.treat_std <- math.item.results.ua23.treat %>%
  mutate(
    question_id = as.character(question_id),
    domainId    = as.character(domainId),
    block       = as.integer(block)
  )

math.item.results.umgc.treat_std <- math.item.results.umgc.treat %>%
  mutate(
    question_id = as.character(question_id),
    domainId    = as.character(domainId),
    block       = as.integer(block)
  )

math.assessments.ua23_std <- math.assessments.ua23 %>%
  mutate(
    question_id = as.character(question_id),
    domainId    = as.character(domainId),
    block       = as.integer(block)
  )

math.assessments.umgc_std <- math.assessments.umgc %>%
  mutate(
    question_id = as.character(question_id),
    domainId    = as.character(domainId),
    block       = as.integer(block)
  )

# ------------------------------------------------------------
# Section 1B. Keep only needed columns from assessment files
# ------------------------------------------------------------

assess_keep_ua23 <- math.assessments.ua23_std %>%
  select(
    block, question_id, domainId,
    difficulty, stem, question, a, b, c, d
  ) %>%
  distinct(block, question_id, domainId, .keep_all = TRUE)

assess_keep_umgc <- math.assessments.umgc_std %>%
  select(
    block, question_id, domainId,
    difficulty, stem, question, a, b, c, d
  ) %>%
  distinct(block, question_id, domainId, .keep_all = TRUE)

# ------------------------------------------------------------
# Section 1C. Keep only needed columns from item-results files
# ------------------------------------------------------------

item_keep_ua23 <- math.item.results.ua23.treat_std %>%
  select(
    block, question_id, domainId,
    startDate, completeDate, score, DAACS_ID, Group, Course, Semester
  )

item_keep_umgc <- math.item.results.umgc.treat_std %>%
  select(
    block, question_id, domainId,
    startDate, completionDate, score, DAACS_ID
  )

# ------------------------------------------------------------
# Section 1D. Merge item-results with assessment text/content
# Keep only rows with matching block + question_id + domainId
# ------------------------------------------------------------

# Keep only rows with matching question_id + domainId
math.items.ua23.treat <- item_keep_ua23 %>%
  inner_join(
    assess_keep_ua23,
    by = c("question_id", "domainId")
  )

math.items.umgc.treat <- item_keep_umgc %>%
  inner_join(
    assess_keep_umgc,
    by = c("question_id", "domainId")
  )

# ------------------------------------------------------------
# Section 1E. Diagnostics
# ------------------------------------------------------------

nrow(math.item.results.ua23.treat_std)
nrow(math.items.ua23.treat)

nrow(math.item.results.umgc.treat_std)
nrow(math.items.umgc.treat)

# unmatched item-results rows
ua23_unmatched <- anti_join(
  item_keep_ua23,
  assess_keep_ua23,
  by = c("question_id", "domainId")
)

umgc_unmatched <- anti_join(
  item_keep_umgc,
  assess_keep_umgc,
  by = c("question_id", "domainId")
)

nrow(ua23_unmatched)
nrow(umgc_unmatched)

# ------------------------------------------------------------
# Section 1F. Save one combined enriched file per college
# ------------------------------------------------------------

saveRDS(
  math.items.ua23.treat,
  file.path(project_dir, "math.items.ua23.treat.rds")
)

saveRDS(
  math.items.umgc.treat,
  file.path(project_dir, "math.items.umgc.treat.rds")
)

write.csv(
  math.items.ua23.treat,
  file.path(project_dir, "math.items.ua23.treat.csv"),
  row.names = FALSE
)

write.csv(
  math.items.umgc.treat,
  file.path(project_dir, "math.items.umgc.treat.csv"),
  row.names = FALSE
)

# ------------------------------------------------------------
# Section 2. Prepare map text for QID matching
# ------------------------------------------------------------

normalize_text_light <- function(x) {
  x %>%
    as.character() %>%
    str_replace_all("<[^>]+>", " ") %>%          # remove HTML tags
    str_replace_all("&nbsp;|&#160;", " ") %>%
    str_replace_all("\\\\\\(|\\\\\\)|\\\\\\[|\\\\\\]", " ") %>%  # latex wrappers
    str_replace_all("\\$\\$", " ") %>%
    str_replace_all("\\\\bullet", " ") %>%
    str_replace_all("\\\\cdot", " ") %>%
    str_replace_all("⁡", "") %>%                 # invisible operator mark
    str_replace_all("[\r\n\t]+", " ") %>%
    str_squish() %>%
    str_to_lower()
}

normalize_text_heavy <- function(x) {
  x %>%
    normalize_text_light() %>%
    str_replace_all("[^[:alnum:]]+", " ") %>%
    str_squish()
}

map_qid <- map %>%
  transmute(
    QID = as.character(QID),
    map_question = as.character(question),
    map_domain = as.character(domain),
    map_difficulty = as.character(assigned_difficulty),
    map_q_light = normalize_text_light(question),
    map_q_heavy = normalize_text_heavy(question)
  ) %>%
  distinct(QID, .keep_all = TRUE)

# ------------------------------------------------------------
# Section 3. Prepare math.items files for matching
# ------------------------------------------------------------

prep_math_items_for_match <- function(df, source_name) {
  df %>%
    mutate(
      source = source_name,
      stem = as.character(stem),
      question = as.character(question),
      domainId = as.character(domainId),
      difficulty = as.character(difficulty),
      stem_light = normalize_text_light(stem),
      stem_heavy = normalize_text_heavy(stem),
      question_light = normalize_text_light(question),
      question_heavy = normalize_text_heavy(question)
    )
}

# Diagnostics

math.items.ua23.items <- math.items.ua23.treat %>%
  distinct(question_id, .keep_all = TRUE)

math.items.umgc.items <- math.items.umgc.treat %>%
  distinct(question_id, .keep_all = TRUE)

math.items.ua23.match <- prep_math_items_for_match(math.items.ua23.items, "ua23")
math.items.umgc.match <- prep_math_items_for_match(math.items.umgc.items, "umgc")

# For these three prompts:
# which of the following is equivalent to the expression below? → 8 times
# what is the value of the expression below? → 3 times
# which of the following is closest to the volume of the cone? → 2 times
# So text alone cannot identify the item. That is expected for math items with generic stems.

math.items.ua23.match %>%
  count(stem_light, sort = TRUE) %>%
  filter(n > 1)

math.items.umgc.match %>%
  count(stem_light, sort = TRUE) %>%
  filter(n > 1)

map_qid %>%
  count(map_q_light, sort = TRUE) %>%
  filter(n > 1)

# ------------------------------------------------------------
# Section 4. Exact matching
# First try full question text, then stem + domain + difficulty
# ------------------------------------------------------------

exact_match_on_question <- function(df_items) {
  df_items %>%
    left_join(
      map_qid %>%
        select(QID, map_question, map_domain, map_difficulty, map_q_light),
      by = c("question_light" = "map_q_light")
    ) %>%
    mutate(match_type = if_else(!is.na(QID), "exact_question", NA_character_))
}

exact_match_on_stem_domain_diff <- function(df_items) {
  df_items %>%
    left_join(
      map_qid_exact_unique %>%
        select(QID, map_question, map_domain, map_difficulty, map_q_light),
      by = c(
        "stem_light" = "map_q_light",
        "domainId" = "map_domain",
        "difficulty" = "map_difficulty"
      )
    ) %>%
    mutate(match_type = if_else(!is.na(QID), "exact_stem_domain_difficulty", NA_character_))
}

map_qid_exact_unique <- map_qid %>%
  count(map_q_light, map_domain, map_difficulty) %>%
  filter(n == 1) %>%
  select(map_q_light, map_domain, map_difficulty) %>%
  inner_join(
    map_qid,
    by = c("map_q_light", "map_domain", "map_difficulty")
  )

# first pass: exact on question
ua23_exact_q <- exact_match_on_question(math.items.ua23.match)
umgc_exact_q <- exact_match_on_question(math.items.umgc.match)

# unresolved after question match
ua23_need_stem <- ua23_exact_q %>%
  filter(is.na(QID)) %>%
  select(names(math.items.ua23.match))

umgc_need_stem <- umgc_exact_q %>%
  filter(is.na(QID)) %>%
  select(names(math.items.umgc.match))

# second pass: exact on stem + domain + difficulty
ua23_exact_s <- exact_match_on_stem_domain_diff(ua23_need_stem)
umgc_exact_s <- exact_match_on_stem_domain_diff(umgc_need_stem)

ua23_exact <- bind_rows(
  ua23_exact_q %>% filter(!is.na(QID)),
  ua23_exact_s %>% filter(!is.na(QID))
) %>%
  distinct(question_id, .keep_all = TRUE)

umgc_exact <- bind_rows(
  umgc_exact_q %>% filter(!is.na(QID)),
  umgc_exact_s %>% filter(!is.na(QID))
) %>%
  distinct(question_id, .keep_all = TRUE)

ua23_unresolved <- math.items.ua23.match %>%
  anti_join(ua23_exact %>% select(question_id), by = "question_id")

umgc_unresolved <- math.items.umgc.match %>%
  anti_join(umgc_exact %>% select(question_id), by = "question_id")

# ------------------------------------------------------------
# Section 5. Fuzzy matching for unresolved rows
# ------------------------------------------------------------

fuzzy_match_items <- function(df_unresolved) {
  df_unresolved %>%
    mutate(row_id = row_number()) %>%
    select(
      row_id, question_id, domainId, difficulty, stem, question,
      stem_heavy, question_heavy
    ) %>%
    tidyr::crossing(
      map_qid %>%
        transmute(
          QID,
          map_question,
          map_domain,
          map_difficulty,
          map_q_heavy
        )
    ) %>%
    mutate(
      dist_stem = stringdist::stringdist(stem_heavy, map_q_heavy, method = "jw"),
      dist_question = stringdist::stringdist(question_heavy, map_q_heavy, method = "jw"),
      best_dist = pmin(dist_stem, dist_question, na.rm = TRUE),
      domain_match = domainId == map_domain,
      diff_match = difficulty == map_difficulty
    ) %>%
    arrange(row_id, best_dist, desc(domain_match), desc(diff_match)) %>%
    group_by(row_id) %>%
    mutate(candidate_rank = row_number()) %>%
    slice_head(n = 5) %>%
    ungroup()
}

ua23_fuzzy_candidates <- fuzzy_match_items(ua23_unresolved)
umgc_fuzzy_candidates <- fuzzy_match_items(umgc_unresolved)

# Conservative auto-accept rule:
# accept only the top fuzzy candidate with Jaro-Winkler distance <= 0.08
# and matching domain

ua23_best_fuzzy <- ua23_fuzzy_candidates %>%
  filter(candidate_rank == 1) %>%
  mutate(
    match_type = "fuzzy_best",
    fuzzy_accept = best_dist <= 0.08 & domain_match
  )

umgc_best_fuzzy <- umgc_fuzzy_candidates %>%
  filter(candidate_rank == 1) %>%
  mutate(
    match_type = "fuzzy_best",
    fuzzy_accept = best_dist <= 0.08 & domain_match
  )

ua23_resolved_fuzzy <- ua23_best_fuzzy %>%
  filter(fuzzy_accept) %>%
  select(question_id, QID, map_question, map_domain, map_difficulty, best_dist, match_type)

umgc_resolved_fuzzy <- umgc_best_fuzzy %>%
  filter(fuzzy_accept) %>%
  select(question_id, QID, map_question, map_domain, map_difficulty, best_dist, match_type)

# ------------------------------------------------------------
# Section 6. Build final QID-assigned math.items files
# ------------------------------------------------------------

# exact matches: keep only lookup-relevant columns
math.items.ua23_exact_qid <- ua23_exact %>%
  filter(!is.na(QID)) %>%
  select(question_id, QID, map_question, map_domain, map_difficulty, match_type) %>%
  distinct(question_id, .keep_all = TRUE)

math.items.umgc_exact_qid <- umgc_exact %>%
  filter(!is.na(QID)) %>%
  select(question_id, QID, map_question, map_domain, map_difficulty, match_type) %>%
  distinct(question_id, .keep_all = TRUE)

# accepted fuzzy matches
math.items.ua23_fuzzy_qid <- ua23_resolved_fuzzy %>%
  distinct(question_id, .keep_all = TRUE)

math.items.umgc_fuzzy_qid <- umgc_resolved_fuzzy %>%
  distinct(question_id, .keep_all = TRUE)

# final item-level lookup: one row per question_id
math.items.ua23_qid_lookup <- bind_rows(
  math.items.ua23_exact_qid,
  math.items.ua23_fuzzy_qid
) %>%
  distinct(question_id, .keep_all = TRUE)

math.items.umgc_qid_lookup <- bind_rows(
  math.items.umgc_exact_qid,
  math.items.umgc_fuzzy_qid
) %>%
  distinct(question_id, .keep_all = TRUE)

# attach QID lookup back to the full row-level math.items files
math.items.ua23_qid <- math.items.ua23.treat %>%
  left_join(math.items.ua23_qid_lookup, by = "question_id")

math.items.umgc_qid <- math.items.umgc.treat %>%
  left_join(math.items.umgc_qid_lookup, by = "question_id")

# ------------------------------------------------------------
# Section 7. Diagnostics
# ------------------------------------------------------------

diagnostics <- list(
  ua23_item_inventory_n = nrow(math.items.ua23.items),
  ua23_lookup_n = nrow(math.items.ua23_qid_lookup),
  umgc_item_inventory_n = nrow(math.items.umgc.items),
  umgc_lookup_n = nrow(math.items.umgc_qid_lookup),
  
  ua23_lookup_matched_n = sum(!is.na(math.items.ua23_qid_lookup$QID)),
  ua23_lookup_unmatched_n = sum(is.na(math.items.ua23_qid_lookup$QID)),
  umgc_lookup_matched_n = sum(!is.na(math.items.umgc_qid_lookup$QID)),
  umgc_lookup_unmatched_n = sum(is.na(math.items.umgc_qid_lookup$QID)),
  
  ua23_rowlevel_n = nrow(math.items.ua23.treat),
  ua23_rowlevel_qid_n = nrow(math.items.ua23_qid),
  umgc_rowlevel_n = nrow(math.items.umgc.treat),
  umgc_rowlevel_qid_n = nrow(math.items.umgc_qid)
)

ua23_duplicate_question_ids <- math.items.ua23_qid_lookup %>%
  count(question_id) %>%
  filter(n > 1)

umgc_duplicate_question_ids <- math.items.umgc_qid_lookup %>%
  count(question_id) %>%
  filter(n > 1)

ua23_still_unmatched <- math.items.ua23_qid %>%
  filter(is.na(QID)) %>%
  distinct(question_id)

umgc_still_unmatched <- math.items.umgc_qid %>%
  filter(is.na(QID)) %>%
  distinct(question_id)

diagnostics$ua23_rowlevel_unmatched_question_ids <- nrow(ua23_still_unmatched)
diagnostics$umgc_rowlevel_unmatched_question_ids <- nrow(umgc_still_unmatched)

print(diagnostics)

# ------------------------------------------------------------
# Section 8. Save outputs
# ------------------------------------------------------------

saveRDS(
  math.items.ua23_qid_lookup,
  file.path(project_dir, "math.items.ua23_qid_lookup.rds")
)

saveRDS(
  math.items.umgc_qid_lookup,
  file.path(project_dir, "math.items.umgc_qid_lookup.rds")
)

saveRDS(
  math.items.ua23_qid,
  file.path(project_dir, "math.items.ua23_qid.rds")
)

saveRDS(
  math.items.umgc_qid,
  file.path(project_dir, "math.items.umgc_qid.rds")
)


write.csv(
  math.items.ua23_qid_lookup,
  file.path(project_dir, "math.items.ua23_qid_lookup.csv"),
  row.names = FALSE
)

write.csv(
  math.items.umgc_qid_lookup,
  file.path(project_dir, "math.items.umgc_qid_lookup.csv"),
  row.names = FALSE
)

write.csv(
  math.items.ua23_qid,
  file.path(project_dir, "math.items.ua23_qid.csv"),
  row.names = FALSE
)

write.csv(
  math.items.umgc_qid,
  file.path(project_dir, "math.items.umgc_qid.csv"),
  row.names = FALSE
)

write.csv(
  ua23_fuzzy_candidates,
  file.path(project_dir, "math.items.ua23_fuzzy_candidates.csv"),
  row.names = FALSE
)

write.csv(
  umgc_fuzzy_candidates,
  file.path(project_dir, "math.items.umgc_fuzzy_candidates.csv"),
  row.names = FALSE
)

saveRDS(
  diagnostics,
  file.path(project_dir, "math_items_qid_crosswalk_diagnostics.rds")
)

write.csv(
  tibble::enframe(diagnostics, name = "metric", value = "value"),
  file.path(project_dir, "math_items_qid_crosswalk_diagnostics.csv"),
  row.names = FALSE
)

write.csv(
  ua23_duplicate_question_ids,
  file.path(project_dir, "math.items.ua23_duplicate_question_ids.csv"),
  row.names = FALSE
)

write.csv(
  umgc_duplicate_question_ids,
  file.path(project_dir, "math.items.umgc_duplicate_question_ids.csv"),
  row.names = FALSE
)

write.csv(
  ua23_still_unmatched,
  file.path(project_dir, "math.items.ua23_still_unmatched.csv"),
  row.names = FALSE
)

write.csv(
  umgc_still_unmatched,
  file.path(project_dir, "math.items.umgc_still_unmatched.csv"),
  row.names = FALSE
)