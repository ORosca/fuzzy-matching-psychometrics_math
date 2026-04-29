# utils_math_pipeline.R
# ============================================================
# Shared utility functions for DAACS math QA pipelines
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(readxl)
  library(tidyr)
  library(stringr)
  library(tibble)
  library(purrr)
  library(ggplot2)
})

# ------------------------------------------------------------
# Core helpers
# ------------------------------------------------------------

`%||%` <- function(a, b) if (!is.null(a)) a else b

assert_has_cols <- function(df, cols, df_name = "data") {
  missing <- setdiff(cols, names(df))
  if (length(missing) > 0) {
    stop(
      sprintf("[%s] missing required columns: %s", df_name, paste(missing, collapse = ", ")),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

save_both <- function(df, out_dir, file_stem) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  saveRDS(df, file.path(out_dir, paste0(file_stem, ".rds")))
  write.csv(
    df,
    file.path(out_dir, paste0(file_stem, ".csv")),
    row.names = FALSE,
    na = ""
  )
}

load_wide_input <- function(input_rds, input_csv = NULL) {
  if (file.exists(input_rds)) {
    readRDS(input_rds)
  } else if (!is.null(input_csv) && file.exists(input_csv)) {
    read_csv(input_csv, show_col_types = FALSE)
  } else {
    stop("No input file found. Check input_rds / input_csv paths.", call. = FALSE)
  }
}

normalize_daacs_id <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x[x == ""] <- NA_character_
  x <- sub("\\.0+$", "", x)
  x
}

make_global_id <- function(college_id, wave, daacs_id_raw) {
  paste(college_id, as.character(wave), daacs_id_raw, sep = "_")
}

# ------------------------------------------------------------
# Item-column helpers
# ------------------------------------------------------------

get_item_cols <- function(df) {
  names(df)[grepl("^Q\\d{3}", names(df))]
}

order_qid_cols <- function(x) {
  qcols <- x[grepl("^Q\\d{3}", x)]
  qcols[order(substr(qcols, 1, 4), qcols)]
}

reorder_item_columns <- function(df) {
  item_cols <- order_qid_cols(names(df))
  non_item_cols <- names(df)[!names(df) %in% item_cols]
  df %>% select(all_of(non_item_cols), all_of(item_cols))
}

coerce_factors_to_char <- function(df) {
  df %>% mutate(across(where(is.factor), as.character))
}

coerce_item_cols <- function(df) {
  item_cols <- get_item_cols(df)
  if (length(item_cols) > 0) {
    df <- df %>%
      mutate(across(all_of(item_cols), ~ suppressWarnings(as.integer(.x))))
  }
  df
}

align_to_master_cols <- function(df, master_cols) {
  missing_cols <- setdiff(master_cols, names(df))
  if (length(missing_cols) > 0) {
    for (nm in missing_cols) {
      df[[nm]] <- NA
    }
  }
  df %>% select(all_of(master_cols))
}

count_answered_items <- function(df) {
  item_cols <- get_item_cols(df)
  rowSums(!is.na(df[item_cols]))
}

# ------------------------------------------------------------
# Shared recodes
# ------------------------------------------------------------

recode_ethnicity_common <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  
  case_when(
    is.na(x) | x == "" ~ NA_character_,
    str_detect(x, regex("^White", ignore_case = TRUE)) ~ "White",
    str_detect(x, regex("Asian", ignore_case = TRUE)) ~ "Asian",
    str_detect(x, regex("Black|African", ignore_case = TRUE)) ~ "Black",
    str_detect(x, regex("Hispanic|Latino", ignore_case = TRUE)) ~ "Hispanic",
    TRUE ~ "Other"
  )
}

recode_yes_no <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  
  case_when(
    x %in% c("Yes", "Y", "1", "TRUE", "True") ~ "Yes",
    x %in% c("No", "N", "0", "FALSE", "False") ~ "No",
    TRUE ~ NA_character_
  )
}

# ------------------------------------------------------------
# QID mapping helpers
# ------------------------------------------------------------

load_qid_mapping <- function(mapping_xlsx) {
  map <- readxl::read_excel(mapping_xlsx)
  
  if (all(c("qid_ua2", "QID") %in% names(map))) {
    map %>%
      transmute(
        qid_ua2 = as.character(qid_ua2),
        QID = as.character(QID)
      ) %>%
      distinct(qid_ua2, .keep_all = TRUE)
  } else if (all(c("question_id", "QID") %in% names(map))) {
    map %>%
      transmute(
        question_id = as.character(question_id),
        QID = as.character(QID),
        assigned_difficulty = if ("assigned_difficulty" %in% names(map)) as.character(assigned_difficulty) else NA_character_,
        map_domain = if ("domain" %in% names(map)) as.character(domain) else NA_character_,
        map_question = if ("question" %in% names(map)) as.character(question) else NA_character_
      ) %>%
      distinct(question_id, .keep_all = TRUE)
  } else {
    stop(
      "Mapping file must contain either (qid_ua2, QID) or (question_id, QID).",
      call. = FALSE
    )
  }
}

rename_item_columns_to_final_qid <- function(df, mapping_df) {
  item_cols <- get_item_cols(df)
  
  if (!all(c("qid_ua2", "QID") %in% names(mapping_df))) {
    stop("mapping_df must contain qid_ua2 and QID for item-column renaming.", call. = FALSE)
  }
  
  mapping_use <- mapping_df %>%
    filter(qid_ua2 %in% item_cols)
  
  rename_vec <- setNames(mapping_use$qid_ua2, mapping_use$QID)
  
  df %>% rename(!!!rename_vec)
}

extract_assigned_difficulty_from_qid <- function(qid) {
  case_when(
    str_detect(qid, "H_p$") ~ "HARD_P",
    str_detect(qid, "H$") ~ "HARD",
    str_detect(qid, "M(_p)?$") ~ "MEDIUM",
    str_detect(qid, "E(_p)?$") ~ "EASY",
    TRUE ~ NA_character_
  )
}

# ------------------------------------------------------------
# Filtering helpers
# ------------------------------------------------------------

filter_min_items <- function(items_std, min_items = 18L) {
  keep_ids <- items_std %>%
    group_by(global_id) %>%
    summarise(
      n_answered = sum(!is.na(score)),
      .groups = "drop"
    ) %>%
    filter(n_answered >= min_items)
  
  items_std %>%
    semi_join(keep_ids, by = "global_id")
}

filter_speedy_math_students <- function(
    df,
    min_items = 18L,
    min_seconds = 180L   # 3 minutes
) {
  assert_has_cols(df, c("global_id", "mathTime"), "math wide data")
  
  out <- df %>%
    mutate(
      n_answered = count_answered_items(.),
      mathTime_num = suppressWarnings(as.numeric(mathTime)),
      flagged_speedy = !is.na(mathTime_num) &
        n_answered >= min_items &
        mathTime_num < min_seconds
    )
  
  audit <- out %>%
    transmute(
      global_id,
      DAACS_ID = if ("DAACS_ID" %in% names(.)) DAACS_ID else NA_character_,
      college = if ("college" %in% names(.)) college else NA_character_,
      wave = if ("wave" %in% names(.)) wave else NA_integer_,
      n_answered,
      mathTime = mathTime_num,
      flagged_speedy
    )
  
  filtered_df <- out %>%
    filter(!flagged_speedy) %>%
    select(-n_answered, -mathTime_num, -flagged_speedy)
  
  summary <- tibble::tibble(
    metric = c(
      "n_total",
      "n_with_nonmissing_time",
      "n_with_18plus_answered",
      "n_flagged_speedy",
      "n_retained"
    ),
    value = c(
      nrow(df),
      sum(!is.na(audit$mathTime)),
      sum(audit$n_answered >= min_items, na.rm = TRUE),
      sum(audit$flagged_speedy, na.rm = TRUE),
      nrow(filtered_df)
    )
  )
  
  list(
    data = filtered_df,
    summary = summary,
    flagged_cases = audit %>% filter(flagged_speedy),
    full_audit = audit
  )
}

# ------------------------------------------------------------
# QA helpers
# ------------------------------------------------------------

qa_check_math_time_seconds <- function(df, df_name = "data", min_reasonable_seconds = 180L) {
  if (!"mathTime" %in% names(df)) {
    message(sprintf("[%s] mathTime column not found.", df_name))
    return(invisible(NULL))
  }
  
  x <- suppressWarnings(as.numeric(df$mathTime))
  
  out <- tibble(
    dataset = df_name,
    n = sum(!is.na(x)),
    min = suppressWarnings(min(x, na.rm = TRUE)),
    q1 = suppressWarnings(as.numeric(quantile(x, 0.25, na.rm = TRUE))),
    median = suppressWarnings(median(x, na.rm = TRUE)),
    mean = suppressWarnings(mean(x, na.rm = TRUE)),
    q3 = suppressWarnings(as.numeric(quantile(x, 0.75, na.rm = TRUE))),
    max = suppressWarnings(max(x, na.rm = TRUE)),
    n_missing = sum(is.na(x)),
    n_nonpositive = sum(!is.na(x) & x <= 0),
    n_under_1_min = sum(!is.na(x) & x < 60),
    n_under_2_min = sum(!is.na(x) & x < 120),
    n_under_3_min = sum(!is.na(x) & x < min_reasonable_seconds),
    n_over_8_hours = sum(!is.na(x) & x > 8 * 60 * 60)
  )
  
  print(out)
  invisible(out)
}

qa_duplicates <- function(df) {
  dup_daacs <- unique(df$DAACS_ID[duplicated(df$DAACS_ID)])
  dup_global <- unique(df$global_id[duplicated(df$global_id)])
  
  list(
    summary = data.frame(
      metric = c("duplicate_DAACS_ID_n", "duplicate_global_id_n"),
      value = c(length(dup_daacs), length(dup_global))
    ),
    duplicate_values = data.frame(
      duplicate_DAACS_ID_values = c(dup_daacs, rep(NA, max(0, length(dup_global) - length(dup_daacs)))),
      duplicate_global_id_values = c(dup_global, rep(NA, max(0, length(dup_daacs) - length(dup_global))))
    )
  )
}

qa_identical_response_patterns <- function(df) {
  item_cols <- get_item_cols(df)
  mat <- df %>% select(all_of(item_cols)) %>% as.data.frame()
  
  dup_any <- duplicated(mat) | duplicated(mat, fromLast = TRUE)
  
  list(
    identical_patterns_n = sum(dup_any),
    identical_ids = df$global_id[dup_any]
  )
}

qa_non_discriminative_items <- function(df) {
  item_cols <- get_item_cols(df)
  
  item_cols[sapply(df[item_cols], function(x) {
    ux <- unique(x[!is.na(x)])
    length(ux) <= 1
  })]
}

qa_value_ranges <- function(df) {
  item_cols <- get_item_cols(df)
  
  tibble(
    QID = item_cols,
    min = sapply(df[item_cols], function(x) suppressWarnings(min(x, na.rm = TRUE))),
    max = sapply(df[item_cols], function(x) suppressWarnings(max(x, na.rm = TRUE))),
    n = sapply(df[item_cols], function(x) sum(!is.na(x)))
  ) %>%
    arrange(QID)
}

qa_item_counts <- function(df) {
  item_cols <- get_item_cols(df)
  
  tibble(
    QID = item_cols,
    Count = sapply(df[item_cols], function(x) sum(!is.na(x)))
  ) %>%
    arrange(QID)
}

qa_response_counts_by_difficulty <- function(item_table) {
  item_table %>%
    mutate(
      assigned_difficulty = extract_assigned_difficulty_from_qid(QID)
    ) %>%
    group_by(assigned_difficulty) %>%
    summarise(
      items = n(),
      min_count = min(Count, na.rm = TRUE),
      median_count = median(Count, na.rm = TRUE),
      max_count = max(Count, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(assigned_difficulty)
}

plot_response_count_density <- function(
    item_table,
    out_file,
    title = "Item response count distribution"
) {
  plot_df <- item_table %>%
    mutate(
      assigned_difficulty = extract_assigned_difficulty_from_qid(QID),
      assigned_difficulty = factor(
        assigned_difficulty,
        levels = c("EASY", "MEDIUM", "HARD", "HARD_P")
      )
    ) %>%
    filter(!is.na(assigned_difficulty))
  
  if (nrow(plot_df) == 0) {
    stop("No items remained after assigning difficulty. Check QID patterns and difficulty extraction.")
  }
  
  min_count <- min(plot_df$Count, na.rm = TRUE)
  
  p <- ggplot(
    plot_df,
    aes(x = Count, fill = assigned_difficulty)
  ) +
    geom_density(alpha = 0.30, color = "black", adjust = 1.1, linewidth = 0.8, na.rm = TRUE) +
    geom_vline(
      xintercept = min_count,
      linetype = "dashed",
      linewidth = 0.7,
      color = "black"
    ) +
    labs(
      title = title,
      subtitle = paste0("Min item response count: ", min_count),
      x = "Response Count",
      y = "Density",
      fill = "Difficulty"
    ) +
    scale_fill_manual(
      values = c(
        "EASY" = "#E8A4A4",
        "MEDIUM" = "#9DB7E8",
        "HARD" = "#8FD19E",
        "HARD_P" = "#CDB4DB"
      ),
      drop = FALSE
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "right",
      panel.grid.minor = element_blank()
    )
  
  ggsave(out_file, p, width = 12, height = 8, dpi = 300)
  invisible(p)
}
# ------------------------------------------------------------
# Item-level sample size summaries
# ------------------------------------------------------------

make_item_sample_size_report <- function(df) {
  item_cols <- get_item_cols(df)
  demo_vars <- c("age_d24", "gender", "ethnicity", "military", "pell")
  demo_vars <- intersect(demo_vars, names(df))
  
  long_df <- df %>%
    select(global_id, all_of(demo_vars), all_of(item_cols)) %>%
    pivot_longer(
      cols = all_of(item_cols),
      names_to = "QID",
      values_to = "score"
    ) %>%
    mutate(
      assigned_difficulty = extract_assigned_difficulty_from_qid(QID)
    )
  
  overall_counts <- long_df %>%
    filter(!is.na(score)) %>%
    group_by(QID) %>%
    summarise(
      assigned_difficulty = first(assigned_difficulty),
      n_responded = n(),
      n_wrong = sum(score == 0, na.rm = TRUE),
      n_correct = sum(score == 1, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(QID)
  
  by_demo_counts <- purrr::map_dfr(demo_vars, function(v) {
    long_df %>%
      filter(!is.na(score)) %>%
      mutate(group_value = as.character(.data[[v]])) %>%
      group_by(QID, demographic = v, group_value) %>%
      summarise(
        assigned_difficulty = first(assigned_difficulty),
        n_responded = n(),
        n_wrong = sum(score == 0, na.rm = TRUE),
        n_correct = sum(score == 1, na.rm = TRUE),
        .groups = "drop"
      )
  })
  
  list(
    overall_counts = overall_counts,
    by_demo_counts = by_demo_counts
  )
}

# ------------------------------------------------------------
# Missingness diagnostics
# ------------------------------------------------------------

run_missingness_diagnostics <- function(
    df,
    stage_name = "data",
    vars_miss = c("college", "wave", "age", "gender", "ethnicity", "military", "pell", "transfer"),
    group_vars = c("college", "wave"),
    save_plots = FALSE,
    output_dir = NULL,
    file_prefix = NULL
) {
  vars_miss <- intersect(vars_miss, names(df))
  group_vars <- intersect(group_vars, names(df))
  vars_to_summarise <- setdiff(vars_miss, group_vars)
  
  if (length(vars_miss) == 0) {
    stop("None of the requested vars_miss are present in df.")
  }
  
  if (isTRUE(save_plots) && is.null(output_dir)) {
    stop("If save_plots = TRUE, provide output_dir.")
  }
  
  if (is.null(file_prefix)) {
    file_prefix <- gsub("[^A-Za-z0-9_]+", "_", stage_name)
  }
  
  is_missing_mixed <- function(x) {
    x_chr <- trimws(as.character(x))
    is.na(x) | x_chr %in% c("", "NA", "<NA>", "NULL")
  }
  
  miss_table <- tibble(
    variable = vars_miss,
    n_missing = sapply(vars_miss, function(v) sum(is_missing_mixed(df[[v]])))
  )
  
  miss_df <- df %>%
    mutate(
      row_id = row_number(),
      across(all_of(vars_miss), as.character)
    ) %>%
    select(row_id, all_of(vars_miss)) %>%
    pivot_longer(
      cols = all_of(vars_miss),
      names_to = "variable",
      values_to = "value"
    ) %>%
    mutate(
      value = trimws(value),
      missing = is.na(value) | value %in% c("", "NA", "<NA>", "NULL")
    )
  
  miss_counts_long <- miss_df %>%
    count(variable, missing)
  
  row_order <- miss_df %>%
    group_by(row_id) %>%
    summarise(
      n_missing = sum(missing),
      wave_missing = any(variable == "wave" & missing),
      pell_missing = any(variable == "pell" & missing),
      gender_missing = any(variable == "gender" & missing),
      .groups = "drop"
    ) %>%
    arrange(desc(wave_missing), desc(pell_missing), desc(gender_missing), desc(n_missing), row_id) %>%
    mutate(row_order = row_number())
  
  miss_df_plot <- miss_df %>%
    left_join(row_order, by = "row_id") %>%
    group_by(row_id) %>%
    mutate(any_missing_row = any(missing)) %>%
    ungroup() %>%
    filter(any_missing_row) %>%
    mutate(row_order_f = factor(row_order, levels = rev(sort(unique(row_order)))))
  
  p_heatmap_rows <- ggplot(miss_df_plot, aes(x = variable, y = row_order_f, fill = missing)) +
    geom_raster() +
    scale_fill_manual(values = c("FALSE" = "grey85", "TRUE" = "black")) +
    labs(
      title = paste0("Missing-value heat map: ", stage_name),
      x = "Variable",
      y = "Ordered row",
      fill = "Missing"
    ) +
    theme_minimal() +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )
  
  p_missing_only <- miss_df_plot %>%
    filter(missing) %>%
    ggplot(aes(x = variable, y = row_order_f)) +
    geom_point(shape = 15, size = 1.8, color = "black") +
    labs(
      title = paste0("Missing cells only: ", stage_name),
      x = "Variable",
      y = "Ordered row"
    ) +
    theme_minimal() +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )
  
  if (length(group_vars) > 0 && length(vars_to_summarise) > 0) {
    miss_summary <- df %>%
      mutate(across(all_of(c(group_vars, vars_to_summarise)), as.character)) %>%
      group_by(across(all_of(group_vars))) %>%
      summarise(
        across(
          all_of(vars_to_summarise),
          ~ mean(is.na(.) | trimws(.) %in% c("", "NA", "<NA>", "NULL")),
          .names = "{.col}"
        ),
        .groups = "drop"
      ) %>%
      pivot_longer(
        cols = all_of(vars_to_summarise),
        names_to = "variable",
        values_to = "prop_missing"
      ) %>%
      mutate(group_label = as.character(interaction(!!!syms(group_vars), drop = TRUE)))
    
    p_grouped <- ggplot(miss_summary, aes(x = variable, y = group_label, fill = prop_missing)) +
      geom_tile() +
      labs(
        title = paste0("Proportion missing by group: ", stage_name),
        x = "Variable",
        y = paste(group_vars, collapse = " / "),
        fill = "Proportion missing"
      ) +
      theme_minimal()
  } else {
    miss_summary <- NULL
    p_grouped <- NULL
  }
  
  if (isTRUE(save_plots)) {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    
    ggsave(
      file.path(output_dir, paste0(file_prefix, "_missing_heatmap_rows.png")),
      p_heatmap_rows, width = 8, height = 10, dpi = 300
    )
    
    ggsave(
      file.path(output_dir, paste0(file_prefix, "_missing_cells_only.png")),
      p_missing_only, width = 8, height = 10, dpi = 300
    )
    
    if (!is.null(p_grouped)) {
      ggsave(
        file.path(output_dir, paste0(file_prefix, "_missing_grouped_heatmap.png")),
        p_grouped, width = 8, height = 5, dpi = 300
      )
    }
    
    write.csv(
      miss_table,
      file.path(output_dir, paste0(file_prefix, "_missing_table.csv")),
      row.names = FALSE
    )
  }
  
  invisible(list(
    miss_table = miss_table,
    miss_df = miss_df,
    miss_counts_long = miss_counts_long,
    row_order = row_order,
    miss_df_plot = miss_df_plot,
    miss_summary = miss_summary,
    p_heatmap_rows = p_heatmap_rows,
    p_missing_only = p_missing_only,
    p_grouped = p_grouped
  ))
}