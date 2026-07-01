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
# 3. DEFINE DIMENSIONS. 1–8 correspond to rating scales. Use this to loop the function.
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

#########################################################

#Plots by Hypothesis

library(tidyverse)

#Hypothesis 1

# Step 1: Filter valid data
h1_data <- dataset %>%
  filter(
    CAT %in% c(1, 2, 3),
    DIMEN %in% c(1, 2)
  ) %>%
  mutate(RATING = as.numeric(RATING))

# Step 2: Compute means
h1_summary <- h1_data %>%
  group_by(CAT, DIMEN) %>%
  summarise(mean_rating = mean(RATING, na.rm = TRUE), .groups = "drop")

# Step 3: Relabel variables
h1_summary <- h1_summary %>%
  mutate(
    CAT = factor(CAT,
                 levels = c(1, 2, 3),
                 labels = c("Category 1", "Category 2", "Category 3")),
    DIMEN = factor(DIMEN,
                   levels = c(1, 2),
                   labels = c("D1 - Inevitability", "D2 - Lack of change"))
  )

# Step 4: Plot
ggplot(h1_summary, aes(x = CAT, y = mean_rating, fill = DIMEN)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_text(aes(label = round(mean_rating, 2)),
            position = position_dodge(width = 0.8),
            vjust = -0.3, size = 4) +
  coord_cartesian(ylim = c(1, 7)) +
  scale_y_continuous(breaks = 1:7) +
  
   scale_fill_manual(values = c(
    "D1 - Inevitability" = "#1f77b4",  
    "D2 - Lack of change" = "#ff7f0e"  
  )) +
  
  labs(
    title = "H1 – Mean Ratings by Category",
    x = "",
    y = "Mean Rating (1–7)",
    fill = "Dimension"
  ) +
  theme_minimal()
#=====================================================
#Hypothesis 2

# Step 1: Filter valid data
h2_data <- dataset %>%
  filter(
    CAT %in% c(1, 2, 3),
    DIMEN %in% c(3, 4)
  ) %>%
  mutate(RATING = as.numeric(RATING))

# Step 2: Compute means
h2_summary <- h2_data %>%
  group_by(CAT, DIMEN) %>%
  summarise(mean_rating = mean(RATING, na.rm = TRUE), .groups = "drop")

# Step 3: Relabel variables
h2_summary <- h2_summary %>%
  mutate(
    CAT = factor(CAT,
                 levels = c(1, 2, 3),
                 labels = c("Category 1", "Category 2", "Category 3")),
    DIMEN = factor(DIMEN,
                   levels = c(3, 4),
                   labels = c("D3 - Tolerance", "D4 - Normal behaviour"))
  )

# Step 4: Plot
ggplot(h2_summary, aes(x = CAT, y = mean_rating, fill = DIMEN)) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_text(aes(label = round(mean_rating, 2)),
            position = position_dodge(width = 0.8),
            vjust = -0.3, size = 4) +
  coord_cartesian(ylim = c(1, 7)) +
  scale_y_continuous(breaks = 1:7) +
  
  scale_fill_manual(values = c(
    "D3 - Tolerance" = "#1f77b4",
    "D4 - Normal behaviour" = "#ff7f0e"
  )) +
  
  labs(
    title = "H2 – Mean Ratings by Category",
    x = "",
    y = "Mean Rating (1–7)",
    fill = "Dimension"
  ) +
  theme_minimal()

#==========================================================
#Hypothesis 3

# Step 1: Filter valid data
h3_data <- dataset %>%
  filter(
    CAT %in% c(1, 2, 3),
    DIMEN %in% c(5, 6)
  ) %>%
  mutate(RATING = as.numeric(RATING))

# Step 2: Compute means
h3_summary <- h3_data %>%
  group_by(CAT, DIMEN) %>%
  summarise(mean_rating = mean(RATING, na.rm = TRUE), .groups = "drop")

# Step 3: Relabel variables
h3_summary <- h3_summary %>%
  mutate(
    CAT = factor(CAT,
                 levels = c(1, 2, 3),
                 labels = c("Category 1", "Category 2", "Category 3")),
    DIMEN = factor(DIMEN,
                   levels = c(5, 6),
                   labels = c("D5 - Rules", "D6 - Obligation"))
  )

# Step 4: Plot
ggplot(h3_summary, aes(x = CAT, y = mean_rating, fill = DIMEN)) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_text(aes(label = round(mean_rating, 2)),
            position = position_dodge(width = 0.8),
            vjust = -0.3, size = 4) +
  coord_cartesian(ylim = c(1, 7)) +
  scale_y_continuous(breaks = 1:7) +
  
  scale_fill_manual(values = c(
    "D5 - Rules" = "#1f77b4",
    "D6 - Obligation" = "#ff7f0e"
  )) +
  
  labs(
    title = "H3 – Mean Ratings by Category",
    x = "",
    y = "Mean Rating (1–7)",
    fill = "Dimension"
  ) +
  theme_minimal()

#=======================================================
#Hypothesis 4

library(tidyverse)

# Step 1: Filter only relevant combinations
h4_filtered <- dataset %>%
  filter(
    (CAT == 1 & DIMEN %in% c(1,2)) |
      (CAT == 2 & DIMEN %in% c(3,4)) |
      (CAT == 3 & DIMEN %in% c(5,6))
  ) %>%
  mutate(RATING = as.numeric(RATING))

# Step 2: Compute mean
h4_summary <- h4_filtered %>%
  group_by(CAT, DIMEN, COND) %>%
  summarise(
    mean_rating = mean(RATING, na.rm = TRUE),
    .groups = "drop"
  )

# Step 3: Labels
h4_summary <- h4_summary %>%
  mutate(
    CAT = factor(CAT,
                 levels = c(1,2,3),
                 labels = c("Category 1",
                            "Category 2",
                            "Category 3")),
    
    DIMEN = case_when(
      DIMEN == 1 ~ "D1",
      DIMEN == 2 ~ "D2",
      DIMEN == 3 ~ "D3",
      DIMEN == 4 ~ "D4",
      DIMEN == 5 ~ "D5",
      DIMEN == 6 ~ "D6"
    ),
    
    COND = factor(COND)
  )

# Step 4: Plot
ggplot(h4_summary, aes(x = DIMEN, y = mean_rating, fill = COND)) +
  
  geom_col(position = position_dodge(0.8)) +
  
  # Values on top
  geom_text(aes(label = round(mean_rating, 1)),
            position = position_dodge(0.8),
            vjust = -0.3,
            size = 4) +
  
  coord_cartesian(ylim = c(1, 7)) +
  scale_y_continuous(breaks = 1:7) +
  
  facet_wrap(~CAT, scales = "free_x") +
  
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e")) +
  
  labs(
    title = "H4 – Effect of Condition (T vs NT)",
    x = "Dimension",
    y = "Mean Rating (1–7)",
    fill = "Condition"
  ) +
  
  theme_minimal()

#===================================================
#Hypothesis 5 

library(tidyverse)

# Step 1: Filter correct data
h5_data <- dataset %>%
  filter(
    CAT %in% c(1, 2, 3),
    DIMEN %in% c(7, 8)     
  ) %>%
  mutate(RATING = as.numeric(RATING))

# Step 2: Compute mean
h5_summary <- h5_data %>%
  group_by(CAT, COND, DIMEN) %>%
  summarise(
    mean_rating = mean(RATING, na.rm = TRUE),
    .groups = "drop"
  )

# Step 3: Labels
h5_summary <- h5_summary %>%
  mutate(
    CAT = factor(CAT,
                 levels = c(1,2,3),
                 labels = c("Category 1", "Category 2", "Category 3")),
    
    DIMEN = factor(DIMEN,
                   levels = c(7, 8),
                   labels = c("D7", "D8")),
    
    COND = factor(COND)
  )

# Step 4: Plot
ggplot(h5_summary, aes(x = DIMEN, y = mean_rating, fill = COND)) +
  
  geom_col(position = position_dodge(0.8)) +
  
  geom_text(aes(label = round(mean_rating, 1)),
            position = position_dodge(0.9),
            vjust = -0.3,
            size = 4) +
  
  coord_cartesian(ylim = c(1, 7)) +
  scale_y_continuous(breaks = 1:7) +
  
  facet_wrap(~CAT) +   #category separation
  
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e")) +
  
  labs(
    title = "H5 – T vs NT in Naturalness and Familiarity",
    x = "Dimension",
    y = "Mean Rating (1–7)",
    fill = "Condition"
  ) +
  
  theme_minimal()

#done
