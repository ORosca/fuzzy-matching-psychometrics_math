# fuzzy-matching-psychometrics_math
This repository provides a reproducible, modular R workflow for the operational psychometrics of the DAACS mathematics assessment. It transforms raw, multi-wave institutional data into analysis-ready formats, resolves inconsistent legacy item identifiers using fuzzy string-distance matching (Jaro-Winkler/Levenshtein), and conducts Differential Item Functioning (DIF) analyses to evaluate item fairness across diverse student subgroups. The pipeline spans data cleaning, item harmonization, and sample-size reporting for downstream IRT and Bayesian modeling.
# DAACS Mathematics Assessment: Operational Psychometrics & Data Engineering

This repository contains an end-to-end, reproducible R workflow for the data processing, item harmonization, and fairness testing of the DAACS mathematics assessment. 

The pipeline is designed to transform raw, multi-wave institutional data into analysis-ready formats, bridge messy item identifiers using fuzzy matching, and conduct rigorous Differential Item Functioning (DIF) analyses. These steps form the operational backbone for evaluating the validity and fairness of the math assessment across diverse student subgroups.

## Key Highlights

* **Reproducible Data Engineering:** Automated ingestion, standardization, and deduplication of complex, multi-wave educational datasets across institutions.
* **Fuzzy Item Harmonization:** Programmatic crosswalking of inconsistent legacy item identifiers using string distance algorithms (Jaro-Winkler/Levenshtein) to ensure longitudinal item tracking.
* **Automated Fairness Testing:** A modular, scalable pipeline for multivariable logistic regression DIF testing (evaluating both uniform and non-uniform DIF) utilizing bias-reduced logistic regression (`brglm2`).
* **Quality Assurance & Reporting:** Built-in missingness diagnostics, response-time filters, and cross-tabulated sample-size reporting for frequentist and Bayesian modeling thresholds.

## Repository Structure

    fuzzy-matching-psychometrics_math/
    ├── README.md
    ├── R/
    │   └── utils_math_pipeline.R                 # Shared helper functions (QA, missingness, recoding)
    ├── src/
    │   ├── 01_data_preprocessing/
    │   │   ├── 01a_clean_2022_cohort.R           # Standardizes legacy UMGC1 + UA2 wide datasets
    │   │   ├── 01b_clean_2022_24_cohort.R        # Harmonizes and filters long/wide UA23 + UMGC datasets
    │   │   └── 01c_combine_stack_finalize.R      # Stacks cohorts, resolves duplicates, and outputs final analytic sample
    │   ├── 02_item_harmonization/
    │   │   └── 02a_fuzzy_matching_crosswalk.R    # Generates question_id -> QID crosswalks via string distance matching
    │   ├── 03_reporting/
    │       └── 03a_item_selection_summaries.R    # Generates sample size threshold tables for IRT and DIF

## Workflow Overview

1. **Preprocessing (`01_data_preprocessing/`):** Raw math data from multiple years is ingested and standardized. Student demographic profiles are recoded, duplicate score patterns are heuristically patched, and non-valid attempts are filtered out to create a pristine analytic sample.
2. **Item Harmonization (`02_item_harmonization/`):** When exact item ID keys fail across cohorts, the pipeline utilizes fuzzy text matching on the item stems to generate candidate matches, securing a unified `QID` (e.g., `Q025gH_p`) for every item across all datasets.
3. **Reporting (`04_reporting/`):** Evaluates item counts across mathematical domains (geometry, statistics, word problems, etc.) and difficulty levels to ensure sub-samples meet the rigorous requirements for downstream modeling.

## Requirements

* **R (>= 4.1.0)**
* **Core Packages:** `dplyr`, `tidyr`, `readr`, `readxl`, `stringr`, `purrr`, `tibble`, `ggplot2`
* **Text Processing:** `stringdist`
* **Psychometrics/Stats:** `brglm2`
* **Reporting:** `openxlsx`

## Author
**Oxana Rosca, PhD** *Educational Psychology*
