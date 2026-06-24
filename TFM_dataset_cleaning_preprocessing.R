# Author: Luiz Otavio de Quadros
# Project: TFM — Dataset cleaning and preprocessing
# Description:This script processes raw questionnaire data collected 
# from two versions of the experiment (A and B).

# =========================================================
# 1. LOAD LIBRARIES
library(dplyr)
library(tidyr)
library(stringr)
library(readr)

# =========================================================
# 2. LOAD DATA
A <- read_csv("Version_A_original.csv")
B <- read_csv("Version_B_original.csv")

# =========================================================
# 3. STANDARDIZE DATA TYPES
# Convert all fields to character to avoid type mismatch issues
# when merging datasets with slightly different formats
A <- A %>% mutate(across(everything(), as.character))
B <- B %>% mutate(across(everything(), as.character))

# =========================================================
# 4. MERGE DATASETS
# Combine both datasets into a single dataframe
wide <- bind_rows(A, B)

# =========================================================
# 5. ADD VERSION LABEL
# Create a variable indicating the source version (A or B)
wide$VERSION <- c(rep("A", nrow(A)), rep("B", nrow(B)))

# =========================================================
# 6. CLEAN VARIABLES
# Standardize key demographic and metadata variables

# Detect the frequency column dynamically
freq_col <- names(wide)[str_detect(names(wide), regex("frequency|often", ignore_case = TRUE))][1]

# Extract frequency values
freq_values <- wide[[freq_col]]

wide <- wide %>%
  mutate(
    # AGE: remove text and convert to numeric
    AGE = str_replace(`Age:`, " years old", "") %>% as.numeric(),
    
    # EDUCATION: simplify category labels
    EDUC = recode(`Education level:`,
                  "Master/PhD" = "MA/PhD",
                  "Undergraduate" = "Undergrad",
                  "High school" = "HS"
    ),
    
    # GENDER: keep original values
    GENDER = `Gender:`,
    
    # FREQUENCY: normalize text and convert to numeric scale
    FREQ = freq_values %>%
      str_trim() %>%        # remove leading/trailing spaces
      str_to_lower() %>%    # standardize text
      recode(
        "never" = "1",
        "rarely" = "2",
        "sometimes" = "3",
        "often" = "4",
        "very often" = "5"
      ) %>%
      as.numeric()
  )

# =========================================================
# 7. CREATE PARTICIPANT IDENTIFIERS
# V_ID: participant ID within each version
# ID: global sequential ID across full dataset
wide <- wide %>%
  group_by(VERSION) %>%
  mutate(
    V_ID = row_number()
  ) %>%
  ungroup() %>%
  mutate(
    ID = row_number()
  )

# =========================================================
# 8. DETECT START OF RESPONSE COLUMNS
# Identify where questionnaire response data begins
start_col <- which(str_detect(names(wide), regex("1", ignore_case = TRUE)))[1]

# =========================================================
# 9. EXPERIMENTAL MAPPING
# Defines the structure of the experiment:

# =========================================================
map <- data.frame(
  QUESTION = 1:16, # Question number
  DIALOG = c(13,1,4,6,9,14,11,3,8,15,7,10,16,12,2,5), # Dialogue mapping
  CAT = c(0,1,1,2,3,0,3,1,2,0,2,3,0,3,1,2), # Category
  A_COND = c("F","T","NT","NT","T","F","T","T","NT","F","T","NT","F","NT","NT","T"), # Condition per version A
  B_COND = c("F","NT","T","T","NT","F","NT","NT","T","F","NT","T","F","T","T","NT") # Condition per version B
)

# =========================================================
# 10. BUILD LONG FORMAT DATASET
# Convert wide-format responses into long-format observations
# Each row represents one rating or response

rows <- list()

for (i in 1:nrow(wide)) {
  
  row <- wide[i, ]
  
  for (q in 1:16) {
    
    # Calculate column position for each question
    base <- start_col + (q - 1) * 9
    
    map_row <- map %>% filter(QUESTION == q)
    
    # Assign condition depending on version
    cond <- ifelse(row$VERSION == "A", map_row$A_COND, map_row$B_COND)
    
    for (d in 1:9) {
      
      # Extract value safely
      val <- if ((base + d - 1) <= ncol(row)) {
        row[[base + d - 1]]
      } else {
        NA
      }
      
      # Create long-format row
      rows[[length(rows) + 1]] <- data.frame(
        ID = row$ID,
        V_ID = row$V_ID,
        VERSION = row$VERSION,
        AGE = row$AGE,
        GENDER = row$GENDER,
        EDUC = row$EDUC,
        FREQ = row$FREQ,
        QUESTION = q,
        DIALOG = map_row$DIALOG,
        CAT = map_row$CAT,
        COND = cond,
        DIMEN = d,
        RATING = ifelse(d != 9, as.numeric(val), 0),
        TEXT = ifelse(d == 9, as.character(val), "")
      )
    }
  }
}

# Combine all rows into final dataset
long <- bind_rows(rows)

# =========================================================
# 11. FINAL CLEANING
# Ensure consistency across all variables
long <- long %>%
  mutate(
    # Replace missing ratings with 0
    RATING = replace_na(RATING, 0),
    
    # Normalize text fields
    TEXT = ifelse(is.na(TEXT) | TEXT == "", "", TEXT),
    TEXT = str_trim(TEXT),
    TEXT = str_replace_all(TEXT, "\r|\n", "")
  )

# =========================================================
# 12. ORDER COLUMNS
# Arrange variables in a consistent and interpretable order
long <- long %>%
  select(
    ID, V_ID, VERSION, AGE, GENDER, EDUC, FREQ,
    QUESTION, DIALOG, CAT, COND, DIMEN, RATING, TEXT
  )

# =========================================================
# 13. SORT DATA
# Ensure systematic ordering of rows
long <- long %>%
  arrange(ID, QUESTION, DIMEN)

# =========================================================
# 14. QUALITY CHECKS
# Simple checks to validate structure and data integrity

print(paste("Total rows:", nrow(long)))

print("Rating distribution:")
print(table(long$RATING))

print(paste("Missing TEXT values:", sum(is.na(long$TEXT))))

print(paste("Number of participants:", length(unique(long$V_ID))))

# Confirm correct number of columns
stopifnot(ncol(long) == 14)

# =========================================================
# 15. SAVE FINAL DATASET
# Export dataset for analysis
write_csv2(long, "dataset_TFM_final.csv")


