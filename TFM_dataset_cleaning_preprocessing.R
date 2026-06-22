#load data first
dataset <- read.csv("dataset_TFM_final.csv")

#cleaning codes++++++++++++++++++



#===============================================
#traduzir para ingles.
#Detection of poor responses

# =========================================================
# 1. PREPARAR DADOS
# =========================================================
data <- dataset %>%
  filter(DIMEN <= 8, CAT != 0)   # apenas ratings válidos

# =========================================================
# 2. CRITÉRIOS DE DETECÇÃO
# =========================================================

# --- CRITÉRIO 1: baixa variação ---
low_variation <- data %>%
  group_by(ID) %>%
  summarise(sd_rating = sd(RATING, na.rm = TRUE)) %>%
  filter(sd_rating < 0.5)

# --- CRITÉRIO 2: respostas extremas ---
extreme_responses <- data %>%
  group_by(ID) %>%
  summarise(prop_extreme = mean(RATING %in% c(1,7))) %>%
  filter(prop_extreme > 0.8)

# --- CRITÉRIO 3: consistência teórica ---
logic_check <- data %>%
  filter(CAT == 3, DIMEN %in% c(5,6)) %>%
  group_by(ID) %>%
  summarise(mean_obligation = mean(RATING)) %>%
  filter(mean_obligation < 3)

# --- CRITÉRIO 4: outliers ---
outliers <- data %>%
  group_by(DIALOG) %>%
  mutate(z = scale(RATING)) %>%
  ungroup() %>%
  filter(abs(z) > 2.5)

outlier_ids <- outliers %>%
  count(ID) %>%
  filter(n >= 3)

# =========================================================
# 3. LISTA FINAL DE IDS SUSPEITOS
# =========================================================
suspect_ids <- unique(c(
  low_variation$ID,
  extreme_responses$ID,
  logic_check$ID,
  outlier_ids$ID
))

print("IDs suspeitos:")
print(suspect_ids)

# =========================================================
# 4. EXTRAIR DADOS DESTES PARTICIPANTES
# =========================================================
suspect_data <- dataset %>%
  filter(ID %in% suspect_ids) %>%
  arrange(ID, QUESTION, DIMEN)

# =========================================================
# 5. VISUALIZAÇÃO — PADRÕES DE RESPOSTA
# =========================================================
#change this plot style for another one, easier to view
ggplot(suspect_data %>% filter(DIMEN <= 8),
       aes(x = QUESTION, y = RATING, group = ID, color = factor(ID))) +
  geom_line() +
  facet_wrap(~ID) +
  theme_minimal() +
  labs(title = "Suspect patterns")


# =========================================================
# 6. ANALISAR RESPOSTAS TEXTUAIS
# =========================================================
suspect_data %>%
  filter(DIMEN == 9) %>%
  select(ID, DIALOG, TEXT) %>%
  arrange(ID) %>%
  View()
