# ============================================================
# EAGL Score Descriptive Statistics
# ============================================================

library(readxl)
library(dplyr)

# ---- File path ----
file_path <- path.expand("~/Desktop/EAGL T-score re-do.xlsx")

# ---- Sheet definitions: name + exact cell ranges ----
# Data starts row 3; columns P:BK only
sheets <- list(
  list(name = "AC",                 range = "P3:BK101"),
  list(name = "HCP",                range = "P3:BK102"),
  list(name = "Gen Pop (EAGL val)", range = "P3:BK2710")
)

# ---- Column layout within the imported P:BK range ----
# P=1, Q=2, R=3, S=4, T=5, U=6, V=7, W=8  -> Subjective  cols 1:8   (MEAN)
# X=9  -> SKIP
# Y=10, Z=11, AA=12, AB=13, AC=14, AD=15, AE=16, AF=17 -> Applied cols 10:17 (SUM)
# AG=18 -> SKIP
# AH=19, AI=20, AJ=21, AK=22, AL=23, AM=24, AN=25 -> Situational cols 19:25 (SUM)
# AO=26 -> SKIP
# AP=27, AQ=28, AR=29, AS=30, AT=31, AU=32 -> Skills cols 27:32 (SUM)
# AV=33 -> SKIP
# AW=34, AX=35, AY=36, AZ=37, BA=38, BB=39, BC=40, BD=41,
# BE=42, BF=43, BG=44, BH=45, BI=46, BJ=47, BK=48 -> Objective cols 34:48 (SUM)

local_cols <- list(
  Subjective  = list(idx = 1:8,   method = "mean"),
  Applied     = list(idx = 10:17, method = "sum"),
  Situational = list(idx = 19:25, method = "sum"),
  Skills      = list(idx = 27:32, method = "sum"),
  Objective   = list(idx = 34:48, method = "sum")
)

# ---- Helper: compute scale score per participant ----
scale_score <- function(df, cols, method) {
  sub_df <- df[, cols, drop = FALSE]
  sub_df <- as.data.frame(lapply(sub_df, function(x) as.numeric(as.character(x))))
  if (method == "mean") rowMeans(sub_df, na.rm = TRUE)
  else                  rowSums(sub_df,  na.rm = TRUE)
}

# ---- Helper: descriptive statistics ----
desc_stats <- function(x, scale_name) {
  x     <- x[!is.na(x) & is.finite(x)]
  n     <- length(x)
  m     <- mean(x)
  sd    <- sd(x)
  sem   <- sd / sqrt(n)
  med   <- median(x)
  q1    <- quantile(x, 0.25)
  q3    <- quantile(x, 0.75)
  ci_lo <- m - qt(0.975, df = n - 1) * sem
  ci_hi <- m + qt(0.975, df = n - 1) * sem
  
  data.frame(
    Scale      = scale_name,
    N          = n,
    Mean       = round(m,     3),
    SD         = round(sd,    3),
    SEM        = round(sem,   3),
    CI_95_Low  = round(ci_lo, 3),
    CI_95_High = round(ci_hi, 3),
    Median     = round(med,   3),
    Q1         = round(q1,    3),
    Q3         = round(q3,    3),
    stringsAsFactors = FALSE
  )
}

# ---- Main loop ----
all_results <- list()

for (sheet in sheets) {
  
  cat("\nReading sheet:", sheet$name, "| Range:", sheet$range, "\n")
  
  # Use range= to precisely target only P:BK for the participant rows.
  # col_names = FALSE so row 3 is treated as data, not a header.
  data_raw <- read_excel(
    path      = file_path,
    sheet     = sheet$name,
    range     = sheet$range,
    col_names = FALSE,
    col_types = "numeric"   # force all to numeric; suppresses type-guessing issues
  )
  
  data_raw <- as.data.frame(data_raw)
  
  cat("  Rows read:", nrow(data_raw), "| Cols read:", ncol(data_raw), "\n")
  
  # Sanity check: we expect 48 columns (P through BK)
  if (ncol(data_raw) != 48) {
    warning("Expected 48 columns for sheet '", sheet$name,
            "' but got ", ncol(data_raw), ". Check your spreadsheet layout.")
  }
  
  # Compute scores for each scale
  scores <- data.frame(
    Subjective  = scale_score(data_raw, local_cols$Subjective$idx,  local_cols$Subjective$method),
    Applied     = scale_score(data_raw, local_cols$Applied$idx,     local_cols$Applied$method),
    Situational = scale_score(data_raw, local_cols$Situational$idx, local_cols$Situational$method),
    Skills      = scale_score(data_raw, local_cols$Skills$idx,      local_cols$Skills$method),
    Objective   = scale_score(data_raw, local_cols$Objective$idx,   local_cols$Objective$method)
  )
  
  # Report NA counts per scale
  cat("  NA counts per scale:\n")
  for (sc in names(scores)) {
    cat("   ", sc, ":", sum(is.na(scores[[sc]])), "NAs\n")
  }
  
  # Compute descriptive stats
  results <- bind_rows(lapply(names(scores), function(sc) desc_stats(scores[[sc]], sc)))
  results$Group <- sheet$name
  results <- results[, c("Group", setdiff(names(results), "Group"))]
  
  all_results[[sheet$name]] <- results
  
  cat("\n", strrep("=", 70), "\n")
  cat(" Group:", sheet$name, "\n")
  cat(strrep("=", 70), "\n")
  print(results, row.names = FALSE)
}

# ---- Combined table ----
combined <- bind_rows(all_results)

cat("\n", strrep("=", 70), "\n")
cat(" COMBINED RESULTS — ALL GROUPS\n")
cat(strrep("=", 70), "\n")
print(combined, row.names = FALSE)

# ---- Save to CSV ----
output_path <- path.expand("~/Desktop/EAGL_descriptive_stats.csv")
write.csv(combined, output_path, row.names = FALSE)
cat("\nResults saved to:", output_path, "\n")
