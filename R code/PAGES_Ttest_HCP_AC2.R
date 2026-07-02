# ============================================================
# EAGL T-Score Analysis
# Compares AC vs HCP participant groups across 5 scales
# ============================================================

library(readxl)
library(dplyr)

# ---- File path ----
file_path <- "~/Desktop/EAGL T-score FINAL.xlsx"

# ============================================================
# HELPER FUNCTIONS
# ============================================================

# Column index to letter (for reference: P=16, BM=65)
col_range <- function(start_col, end_col) start_col:end_col

# Compute scale scores for a given sheet
compute_scales <- function(sheet_name, data_rows) {
  df <- read_excel(
    file_path,
    sheet    = sheet_name,
    col_names = TRUE,
    skip      = 1          # skip row 1; row 2 becomes header
  )
  
  # Keep only participant rows (adjust for 0-based indexing after skip+header)
  # After skip=1, row 2 of Excel = header row, row 3 of Excel = row 1 of df
  df <- df[data_rows, ]
  
  # Select columns P:BM (columns 16:65 in Excel = indices 16:65)
  # After read_excel with skip=1, column indices are preserved by position
  # Column A = position 1, so P = 16, BM = 65
  all_cols <- df[, 16:65]
  
  # Ensure all numeric
  all_cols <- mutate_all(all_cols, as.numeric)
  
  # ---- Scale Scoring ----
  # Subjective: cols P-W  → positions 1-8  → AVERAGE
  subjective <- rowMeans(all_cols[, 1:8],  na.rm = TRUE)
  
  # Applied:    cols Y-AF → positions 10-17 → SUM (col X = pos 9 is skipped)
  applied    <- rowSums(all_cols[, 10:17], na.rm = TRUE)
  
  # Situational: cols AH-AN → positions 19-25 → SUM (col AG = pos 18 skipped)
  situational <- rowSums(all_cols[, 19:25], na.rm = TRUE)
  
  # Skills:     cols AP-AU → positions 27-32 → SUM (col AO = pos 26 skipped)
  skills     <- rowSums(all_cols[, 27:32], na.rm = TRUE)
  
  # Objective:  cols AW-BM → positions 34-50 → SUM (col AV = pos 33 skipped)
  objective  <- rowSums(all_cols[, 34:50], na.rm = TRUE)
  
  data.frame(
    Subjective  = subjective,
    Applied     = applied,
    Situational = situational,
    Skills      = skills,
    Objective   = objective
  )
}

# Descriptive statistics for one group's scale data
desc_stats <- function(scale_df, group_name) {
  cat(sprintf("\n--- Descriptive Statistics: %s ---\n", group_name))
  for (scale in names(scale_df)) {
    x <- scale_df[[scale]]
    x <- x[!is.na(x)]
    cat(sprintf(
      "  %-12s | n = %d | Mean = %.3f | SD = %.3f\n",
      scale, length(x), mean(x), sd(x)
    ))
  }
}

# ============================================================
# LOAD DATA
# ============================================================

# Sheet 1 "AC":       rows 3-101 → after skip=1 & header row → df rows 1-99
# Sheet 2 "HCP":      rows 3-102 → df rows 1-100
ac_data  <- compute_scales("AC",  1:99)
hcp_data <- compute_scales("HCP", 1:100)

# ============================================================
# DESCRIPTIVE STATISTICS
# ============================================================

cat("============================================================\n")
cat("           EAGL T-Score Descriptive Statistics\n")
cat("============================================================\n")

desc_stats(ac_data,  "AC  (n=99)")
desc_stats(hcp_data, "HCP (n=100)")

# ============================================================
# T-TESTS: AC vs HCP for each scale
# ============================================================

cat("\n============================================================\n")
cat("       Independent Samples t-Tests: AC vs HCP\n")
cat("       (Welch's t-test; assumes unequal variances)\n")
cat("============================================================\n")

scales <- c("Subjective", "Applied", "Situational", "Skills", "Objective")

results <- lapply(scales, function(scale) {
  x <- ac_data[[scale]]
  y <- hcp_data[[scale]]
  
  # Welch's t-test (does not assume equal variances — appropriate default)
  tt <- t.test(y, x, var.equal = FALSE)
  
  # Cohen's d using pooled SD (Hedges' pooled SD formula)
  n1 <- sum(!is.na(y)); n2 <- sum(!is.na(x))
  m1 <- mean(y, na.rm=TRUE); m2 <- mean(x, na.rm=TRUE)
  s1 <- sd(y, na.rm=TRUE);   s2 <- sd(x, na.rm=TRUE)
  pooled_sd <- sqrt(((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 - 2))
  cohens_d  <- (m1 - m2) / pooled_sd
  effect_size_label <- ifelse(abs(cohens_d) >= 0.8, "Large",
                              ifelse(abs(cohens_d) >= 0.5, "Medium",
                                     ifelse(abs(cohens_d) >= 0.2, "Small", "Negligible")))
  
  cat(sprintf("\n--- %s Scale ---\n", scale))
  cat(sprintf("  AC  : M = %.3f, SD = %.3f, n = %d\n", m2, s2, n2))
  cat(sprintf("  HCP : M = %.3f, SD = %.3f, n = %d\n", m1, s1, n1))
  cat(sprintf("  t(%s) = %.3f, p = %.4f %s\n",
              format(round(tt$parameter, 2), nsmall=2),
              tt$statistic,
              tt$p.value,
              ifelse(tt$p.value < .001, "***",
                     ifelse(tt$p.value < .01, "**",
                            ifelse(tt$p.value < .05, "*", "ns")))))
  cat(sprintf("  95%% CI for difference: [%.3f, %.3f]\n",
              tt$conf.int[1], tt$conf.int[2]))
  cat(sprintf("  Cohen's d = %.3f (%s effect)\n", cohens_d, effect_size_label))
  
  # Return tidy result row
  data.frame(
    Scale        = scale,
    AC_M         = round(m2, 3),
    AC_SD        = round(s2, 3),
    AC_n         = n2,
    HCP_M        = round(m1, 3),
    HCP_SD       = round(s1, 3),
    HCP_n        = n1,
    t            = round(tt$statistic, 3),
    df           = round(tt$parameter, 2),
    p_value      = round(tt$p.value, 4),
    sig          = ifelse(tt$p.value < .001, "***",
                          ifelse(tt$p.value < .01, "**",
                                 ifelse(tt$p.value < .05, "*", "ns"))),
    CI_lower     = round(tt$conf.int[1], 3),
    CI_upper     = round(tt$conf.int[2], 3),
    Cohens_d     = round(cohens_d, 3),
    Effect_Size  = effect_size_label
  )
})

# Summary table
summary_table <- do.call(rbind, results)
rownames(summary_table) <- NULL

cat("\n============================================================\n")
cat("                     Summary Table\n")
cat("============================================================\n")
print(summary_table, row.names = FALSE)

cat("\nSignificance codes: *** p<.001 | ** p<.01 | * p<.05 | ns = not significant\n")
cat("Cohen's d effect size: |d| >= 0.8 = Large | >= 0.5 = Medium | >= 0.2 = Small | < 0.2 = Negligible\n")
cat("Note: Welch's t-test used (does not assume equal variances).\n")
cat("Note: Cohen's d calculated using pooled SD (Hedges' formula).\n")
