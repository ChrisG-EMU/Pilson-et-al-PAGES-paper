# EAGL T-score Analysis
# Welch t-tests comparing AC vs Gen Pop across 5 scales

library(readxl)

# ── File path ─────────────────────────────────────────────────────────────────
file_path <- "~/Desktop/EAGL T-score FINAL.xlsx"

# ── Read sheets using EXACT cell ranges ───────────────────────────────────────
# Columns P:BM = 50 columns  ← extended by 2 (BK→BM) for 17-item Objective
# AC:      rows 3–101  → exactly 99 rows  (P3:BM101)
# Gen Pop: rows 3–2710 → exactly 2708 rows (P3:BM2710)

read_block <- function(sheet_name, range_str) {
  raw <- read_excel(
    file_path,
    sheet     = sheet_name,
    range     = range_str,
    col_names = FALSE
  )
  as.data.frame(lapply(raw, function(col) suppressWarnings(as.numeric(col))))
}

cat("Reading AC sheet (P3:BM101)...\n")
ac_raw <- read_block("AC", "P3:BM101")

cat("Reading Gen Pop sheet (P3:BM2710)...\n")
gp_raw <- read_block("Gen Pop (EAGL val)", "P3:BM2710")

# ── Hard assertions: fail immediately if row counts are wrong ─────────────────
stopifnot("AC must have exactly 99 rows"        = nrow(ac_raw) == 99)
stopifnot("Gen Pop must have exactly 2708 rows" = nrow(gp_raw) == 2708)
cat("\n✓ Row counts confirmed: AC =", nrow(ac_raw), "| Gen Pop =", nrow(gp_raw), "\n")

# ── Column layout within the P:BM block (1-indexed) ──────────────────────────
# P=1  ... W=8           → Subjective  (cols  1–8,  mean)
# X=9  gap/ignored
# Y=10 ... AF=17         → Applied     (cols 10–17, sum)
# AG=18 gap/ignored
# AH=19 ... AN=25        → Situational (cols 19–25, sum)
# AO=26 gap/ignored
# AP=27 ... AU=32        → Skills      (cols 27–32, sum)
# AV=33 gap/ignored
# AW=34 ... BM=50        → Objective   (cols 34–50, sum)  ← 17 items

scales <- list(
  Subjective  = list(cols = 1:8,   method = "mean"),
  Applied     = list(cols = 10:17, method = "sum"),
  Situational = list(cols = 19:25, method = "sum"),
  Skills      = list(cols = 27:32, method = "sum"),
  Objective   = list(cols = 34:50, method = "sum")   # AW–BM (17 items)
)

# ── Score function ─────────────────────────────────────────────────────────────
compute_scores <- function(block) {
  scores <- data.frame(matrix(NA, nrow = nrow(block), ncol = length(scales)))
  colnames(scores) <- names(scales)
  for (sc in names(scales)) {
    cols <- block[, scales[[sc]]$cols, drop = FALSE]
    if (scales[[sc]]$method == "mean") {
      scores[[sc]] <- rowMeans(cols, na.rm = TRUE)
    } else {
      scores[[sc]] <- rowSums(cols, na.rm = TRUE)
    }
  }
  scores
}

ac_scores <- compute_scores(ac_raw)
gp_scores <- compute_scores(gp_raw)

# ── Sanity check: Gen Pop column means ────────────────────────────────────────
cat("\n── Sanity Check: Gen Pop Scale Means ──────────────────────────────────\n")
cat("Subjective  (expect ~5.57):   ", round(mean(gp_scores$Subjective,  na.rm = TRUE), 4), "\n")
cat("Applied     (expect ~6.964):  ", round(mean(gp_scores$Applied,     na.rm = TRUE), 4), "\n")
cat("Situational (expect ~5.568):  ", round(mean(gp_scores$Situational, na.rm = TRUE), 4), "\n")
cat("Skills      (expect ~5.407):  ", round(mean(gp_scores$Skills,      na.rm = TRUE), 4), "\n")
cat("Objective   (expect ~13.035): ", round(mean(gp_scores$Objective,   na.rm = TRUE), 4), "\n")

# ── Descriptive statistics ─────────────────────────────────────────────────────
desc_stats <- function(df, group_label) {
  do.call(rbind, lapply(names(scales), function(sc) {
    x <- df[[sc]][!is.na(df[[sc]])]
    data.frame(
      Group = group_label,
      Scale = sc,
      n     = length(x),
      Mean  = round(mean(x), 4),
      SD    = round(sd(x),   4)
    )
  }))
}

ac_desc <- desc_stats(ac_scores, "AC")
gp_desc <- desc_stats(gp_scores, "Gen Pop")

cat("\n── Descriptive Statistics ──────────────────────────────────────────────\n")
print(rbind(ac_desc, gp_desc), row.names = FALSE)

# ── Helper functions ───────────────────────────────────────────────────────────
sig_label <- function(p) {
  if      (p < 0.001) "***"
  else if (p < 0.01)  "**"
  else if (p < 0.05)  "*"
  else if (p < 0.10)  "."
  else                "ns"
}

# Cohen's d using pooled SD (appropriate for unequal group sizes)
cohens_d <- function(x, y) {
  nx <- length(x); ny <- length(y)
  pooled_sd <- sqrt(((nx - 1) * var(x) + (ny - 1) * var(y)) / (nx + ny - 2))
  (mean(x) - mean(y)) / pooled_sd
}

# ── Welch t-tests (var.equal = FALSE) ─────────────────────────────────────────
results <- do.call(rbind, lapply(names(scales), function(sc) {
  
  h <- ac_scores[[sc]][!is.na(ac_scores[[sc]])]
  g <- gp_scores[[sc]][!is.na(gp_scores[[sc]])]
  
  # Verify full N is present after NA removal
  if (length(h) != 99)   warning(sprintf("Scale '%s': AC has %d valid rows (expected 99)",      sc, length(h)))
  if (length(g) != 2708) warning(sprintf("Scale '%s': Gen Pop has %d valid rows (expected 2708)", sc, length(g)))
  
  tt <- t.test(h, g, var.equal = FALSE)  # Welch t-test
  d  <- cohens_d(h, g)
  
  data.frame(
    Scale       = sc,
    AC_n        = length(h),
    AC_Mean     = round(mean(h), 4),
    AC_SD       = round(sd(h),   4),
    GenPop_n    = length(g),
    GenPop_Mean = round(mean(g), 4),
    GenPop_SD   = round(sd(g),   4),
    t_statistic = round(tt$statistic,   4),
    df          = round(tt$parameter,   2),
    CI_95_Lower = round(tt$conf.int[1], 4),
    CI_95_Upper = round(tt$conf.int[2], 4),
    p_value     = signif(tt$p.value,    6),
    Sig         = sig_label(tt$p.value),
    Cohens_d    = round(d, 4),
    Effect_Size = ifelse(abs(d) < 0.2, "Negligible",
                         ifelse(abs(d) < 0.5, "Small",
                                ifelse(abs(d) < 0.8, "Medium", "Large")))
  )
}))

cat("\n── Summary Table: Welch t-tests (AC vs Gen Pop) ───────────────────────\n")
cat("Significance: *** p<0.001 | ** p<0.01 | * p<0.05 | . p<0.10 | ns\n\n")
print(results, row.names = FALSE)

# ── Export ─────────────────────────────────────────────────────────────────────
write.csv(results,
          "~/Desktop/EAGL_welch_ttest_results.csv", row.names = FALSE)
write.csv(rbind(ac_desc, gp_desc),
          "~/Desktop/EAGL_descriptive_stats.csv",   row.names = FALSE)
cat("\nResults saved to Desktop.\n")
