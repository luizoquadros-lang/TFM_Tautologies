# Author: Luiz Otavio de Quadros
# Project: TFM — Main analysis
# Description: Computes mean ratings by dimension, category and condition.
# Results correspond to Section 5.2 (Results by dimension).

# =========================================================
# 1. LOAD LIBRARIES
library(dplyr)
library(tidyr)
library(purrr)

# =========================================================
# 2. LOAD DATASET
dataset <- read.csv("dataset_TFM_final.csv")

# =========================================================
# 3. DEFINE DIMENSIONS. 1–8 correspond to rating scales
dimensions <- 1:8

# =========================================================
# 4. FUNCTION: Compute mean ratings for each dimension:
compute_means <- function(d) {
  
  # Filter relevant data (exclude CAT = 0)
  data_dim <- dataset %>%
    filter(DIMEN == d, CAT != 0)
  
  # Mean by category and condition
  mean_by_cat_cond <- data_dim %>%
    group_by(CAT, COND) %>%
    summarise(
      mean_rating = mean(RATING),
      .groups = "drop"
    )
  
  # Overall mean by category
  mean_overall <- data_dim %>%
    group_by(CAT) %>%
    summarise(
      mean_rating = mean(RATING),
      .groups = "drop"
    ) %>%
    mutate(COND = "Overall")
  
  # Combine both results
  means_combined <- bind_rows(mean_by_cat_cond, mean_overall) %>%
    mutate(DIMEN = d)
  
  # Convert to wide format
  final_table <- means_combined %>%
    pivot_wider(
      names_from = COND,
      values_from = mean_rating
    ) %>%
    arrange(CAT)
  
  return(final_table)
}

# =========================================================
# 5. APPLY FUNCTION TO ALL DIMENSIONS
all_mean_results <- map_dfr(dimensions, compute_means)

# =========================================================
# 6. ORDER FINAL OUTPUT
all_mean_results <- all_mean_results %>%
  select(DIMEN, CAT, everything()) %>%
  arrange(DIMEN, CAT)

# =========================================================
# 7. DISPLAY RESULTS
print(all_mean_results, n = 24)

# DONE
