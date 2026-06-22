#TFM main analysis

#Load the dataset
dataset <- read.csv("dataset_TFM_final.csv")

#=================================
#this is the code for the 5.2 Results by dimension 
#=================================

library(dplyr)
library(tidyr)
library(purrr)

# Define the dimensions to analyze (1–8)
dimensions <- 1:8

# Function to compute means for a given dimension
compute_means <- function(d) {
  
  # Step 1: Mean by category and condition
  mean_by_cat_cond <- dataset %>%
    filter(DIMEN == d, CAT != 0) %>%
    group_by(DIMEN, CAT, COND) %>%
    summarise(mean_rating = mean(RATING), .groups = "drop")
  
  # Step 2: Overall mean by category
  mean_overall <- dataset %>%
    filter(DIMEN == d, CAT != 0) %>%
    group_by(DIMEN, CAT) %>%
    summarise(mean_rating = mean(RATING), .groups = "drop") %>%
    mutate(COND = "Overall")
  
  # Step 3: Combine
  means_combined <- bind_rows(mean_by_cat_cond, mean_overall)
  
  # Step 4: Pivot to wide format
  final_table <- means_combined %>%
    pivot_wider(names_from = COND, values_from = mean_rating)
  
  return(final_table)
}

# Apply function to all dimensions and combine results
all_mean_results <- map_dfr(dimensions, compute_means)

# Display final table
print(n=24, all_mean_results)
write.csv2(all_mean_results, "all_mean_results.csv", row.names = FALSE)



