# ============================================================
# Spearman Correlation Analysis
# Question: Is recency of genetics/genomics CME associated with
#           genetic literacy and related subscales in HCPs?
#
# Data source:
#   PAGES_I_HCPs_GLS DATA.xlsx
#   Sheet: Sheet1
#
# Variables:
#   CME recency                    - column O
#   self-rated genetic literacy     - column R
#   subjective knowledge            - column AM
#   applied knowledge               - column AN
#   numeracy                        - column AX
#   situational knowledge           - column BI
#   skills/comprehension            - column BW
#   objective knowledge             - column CP (fallback to CR if needed)
#
# Analysis:
#   Spearman correlations with Benjamini-Hochberg (BH/FDR)
#   adjustment across the seven outcome tests.
#   Sensitivity analysis retains "I do not remember" as the
#   lowest recency rank.
#
# Notes:
#   - The workbook has header rows; data begin on row 4.
#   - The script skips outcomes with too few usable observations.
#   - The script skips constant vectors, which cannot be correlated.
# ============================================================

library(readxl)
library(dplyr)
library(tibble)

# ------------------------------------------------------------
# 1. LOAD DATA
# ------------------------------------------------------------
file_path <- "insert"

# Data begin on row 4, so skip the first three header/metadata rows.
raw <- read_excel(
  path = file_path,
  sheet = "Sheet1",
  skip = 3,
  col_names = FALSE,
  .name_repair = "minimal"
)

# Helper to count finite numeric values.
count_finite <- function(x) sum(is.finite(as.numeric(x)))

# ------------------------------------------------------------
# 2. EXTRACT RELEVANT COLUMNS BY EXCEL POSITION
# ------------------------------------------------------------
# Excel positions (1-based):
#   O  = 15   CME recency
#   R  = 18   self-rating
#   AM = 39   subjective
#   AN = 40   applied
#   AX = 50   numeracy
#   BI = 61   situational
#   BW = 75   skills/comprehension
#   CP = 94   objective (raw score)
#   CR = 96   appears blank in this workbook; kept as fallback

get_col <- function(df, pos) {
  if (pos > ncol(df)) return(rep(NA, nrow(df)))
  df[[pos]]
}

objective_cp <- get_col(raw, 94)
objective_cr <- get_col(raw, 96)

# Use CR only if it contains at least one finite observation; otherwise use CP.
objective_use <- if (count_finite(objective_cr) > 0) objective_cr else objective_cp

analysis_df <- tibble(
  cme_recency = get_col(raw, 15),
  self_rating  = get_col(raw, 18),
  subjective   = get_col(raw, 39),
  applied      = get_col(raw, 40),
  numeracy     = get_col(raw, 50),
  situational  = get_col(raw, 61),
  skills       = get_col(raw, 75),
  objective    = objective_use
) %>%
  mutate(
    cme_recency = as.character(cme_recency),
    self_rating = as.numeric(self_rating),
    subjective  = as.numeric(subjective),
    applied     = as.numeric(applied),
    numeracy    = as.numeric(numeracy),
    situational = as.numeric(situational),
    skills      = as.numeric(skills),
    objective   = as.numeric(objective)
  )

# ------------------------------------------------------------
# 3. RECODE CME RECENCY
# ------------------------------------------------------------
# Higher values = more recent CME.
recode_cme <- function(x) {
  case_when(
    x == "Within the last 6 months"                                     ~ 5,
    x == "Between 6 months and 1 year ago"                              ~ 4,
    x == "1 to 5 years ago"                                             ~ 3,
    x == "Over 5 years ago"                                             ~ 2,
    x == "I have never taken a genetics or genomics course"             ~ 1,
    x == "I do not remember when I last took a genetics or genomics course" ~ 1,
    TRUE ~ NA_real_
  )
}

analysis_df <- analysis_df %>%
  mutate(cme_ranked = recode_cme(cme_recency))

# ------------------------------------------------------------
# 4. DIAGNOSTIC COUNTS
# ------------------------------------------------------------
outcomes <- c("self_rating", "subjective", "applied", "numeracy", "situational", "skills", "objective")

cat("\n=== FINITE COUNTS ===\n")
cat("Predictor (cme_ranked): ", count_finite(analysis_df$cme_ranked), "\n", sep = "")
for (nm in outcomes) {
  cat(sprintf("%-12s: %d\n", nm, count_finite(analysis_df[[nm]])))
}

# ------------------------------------------------------------
# 5. HELPER FUNCTIONS
# ------------------------------------------------------------
run_spearman_set <- function(data, predictor_col, outcomes, analysis_label, p_adjust = "BH") {
  out <- lapply(outcomes, function(outcome_name) {
    tmp <- data %>%
      select(predictor = all_of(predictor_col), outcome = all_of(outcome_name)) %>%
      mutate(
        predictor = as.numeric(predictor),
        outcome   = as.numeric(outcome)
      ) %>%
      filter(is.finite(predictor), is.finite(outcome))

    n_use <- nrow(tmp)

    # Skip if too few observations or constant vectors.
    if (n_use < 3 || length(unique(tmp$predictor)) < 2 || length(unique(tmp$outcome)) < 2) {
      warning(sprintf(
        "Skipping %s (%s): insufficient non-missing/non-constant observations (n = %d).",
        analysis_label, outcome_name, n_use
      ))
      return(tibble(
        analysis = analysis_label,
        outcome  = outcome_name,
        n        = n_use,
        rho      = NA_real_,
        p_value  = NA_real_
      ))
    }

    ct <- suppressWarnings(cor.test(
      tmp$predictor,
      tmp$outcome,
      method = "spearman",
      exact = FALSE
    ))

    tibble(
      analysis = analysis_label,
      outcome  = outcome_name,
      n        = n_use,
      rho      = unname(ct$estimate),
      p_value  = ct$p.value
    )
  }) %>%
    bind_rows()

  # Benjamini-Hochberg adjustment on non-missing p-values only.
  p_adj <- rep(NA_real_, nrow(out))
  ok <- !is.na(out$p_value)
  if (any(ok)) {
    p_adj[ok] <- p.adjust(out$p_value[ok], method = p_adjust)
  }

  out %>% mutate(p_adj = p_adj)
}

# ------------------------------------------------------------
# 6. PRIMARY ANALYSIS
#    Exclude "I do not remember"
# ------------------------------------------------------------
primary_data <- analysis_df %>%
  filter(cme_recency != "I do not remember when I last took a genetics or genomics course") %>%
  filter(!is.na(cme_ranked))

primary_results <- run_spearman_set(
  data = primary_data,
  predictor_col = "cme_ranked",
  outcomes = outcomes,
  analysis_label = "Primary (exclude I do not remember)",
  p_adjust = "BH"
)

# ------------------------------------------------------------
# 7. SENSITIVITY ANALYSIS
#    Treat "I do not remember" as the lowest rank
# ------------------------------------------------------------
sens_data <- analysis_df %>%
  filter(!is.na(cme_ranked))

sens_results <- run_spearman_set(
  data = sens_data,
  predictor_col = "cme_ranked",
  outcomes = outcomes,
  analysis_label = "Sensitivity (I do not remember = lowest rank)",
  p_adjust = "BH"
)

# ------------------------------------------------------------
# 8. COMBINE AND FORMAT
# ------------------------------------------------------------
all_results <- bind_rows(primary_results, sens_results) %>%
  mutate(
    rho      = round(rho, 3),
    p_value  = signif(p_value, 3),
    p_adj    = signif(p_adj, 3),
    signif_FDR = case_when(
      is.na(p_adj) ~ NA_character_,
      p_adj < 0.001 ~ "***",
      p_adj < 0.01  ~ "**",
      p_adj < 0.05  ~ "*",
      TRUE          ~ "ns"
    )
  ) %>%
  select(analysis, outcome, n, rho, p_value, p_adj, signif_FDR)

# ------------------------------------------------------------
# 9. PRINT RESULTS
# ------------------------------------------------------------
cat("\n=== PRIMARY ANALYSIS (exclude 'I do not remember') ===\n")
print(primary_results)

cat("\n=== SENSITIVITY ANALYSIS ('I do not remember' = lowest rank) ===\n")
print(sens_results)

cat("\n=== COMBINED RESULTS ===\n")
print(all_results)

# ------------------------------------------------------------
# 10. SAVE RESULTS
# ------------------------------------------------------------
output_file <- "insert"
write.csv(all_results, file = output_file, row.names = FALSE)
cat("\nResults written to:\n", output_file, "\n")
