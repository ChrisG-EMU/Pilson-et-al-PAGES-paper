# EAGL T-score Analysis
# Welch t-tests comparing HCP vs Gen Pop across 5 scales

library(readxl)

# ── File path ─────────────────────────────────────────────────────────────────
file_path <- "~/Desktop/EAGL T-score FINAL.xlsx"

# ── Read sheets using EXACT cell ranges (avoids all row/col offset bugs) ──────
# Each sheet: columns P:BK, data starts row 3
# HCP:     rows 3–102  (100 participants)
# Gen Pop: rows 3–2710 (2708 participants)

read_block <- function(sheet_name, range_str) {
  raw <- read_excel(
    file_path,
    sheet     = sheet_name,
    range     = range_str,
    col_names = FALSE
  )
  # Convert everything to numeric (handles any stray text/NA)
  as.data.frame(lapply(raw, function(col) suppressWarnings(as.numeric(col))))
}

cat("Reading HCP sheet (P3:BM102)...\n")
hcp_raw <- read_block("HCP", "P3:BM102")

cat("Reading Gen Pop sheet (P3:BM2710)...\n")
gp_raw  <- read_block("Gen Pop (EAGL val)", "P3:BM2710")

# ── Column layout within the P:BK block (1-indexed within the block) ──────────
# P=1, Q=2, R=3, S=4, T=5, U=6, V=7, W=8          → Subjective  (cols  1–8)
# X=9 (gap/ignored)
# Y=10, Z=11, AA=12, AB=13, AC=14, AD=15, AE=16, AF=17 → Applied (cols 10–17)
# AG=18 (gap/ignored)
# AH=19, AI=20, AJ=21, AK=22, AL=23, AM=24, AN=25  → Situational (cols 19–25)
# AO=26 (gap/ignored)
# AP=27, AQ=28, AR=29, AS=30, AT=31, AU=32          → Skills      (cols 27–32)
# AV=33 (gap/ignored)
# AW=34, AX=35, AY=36, AZ=37, BA=38, BB=39, BC=40,
# BD=41, BE=42, BF=43, BG=44, BH=45, BI=46, BJ=47, BK=48, BL=49, BM=50 → Objective (cols 34–50)

scales <- list(
  Subjective  = list(cols = 1:8,   method = "mean"),
  Applied     = list(cols = 10:17, method = "sum"),
  Situational = list(cols = 19:25, method = "sum"),
  Skills      = list(cols = 27:32, method = "sum"),
  Objective   = list(cols = 34:50, method = "sum")
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

hcp_scores <- compute_scores(hcp_raw)
gp_scores  <- compute_scores(gp_raw)

# ── Quick sanity check ─────────────────────────────────────────────────────────
cat("\n── Sanity Check: Gen Pop Column Means ─────────────────────────────────\n")
cat("Subjective  (expect ~5.57):  ", round(mean(gp_scores$Subjective,  na.rm=TRUE), 4), "\n")
cat("Applied     (expect ~6.964): ", round(mean(gp_scores$Applied,     na.rm=TRUE), 4), "\n")
cat("Situational (expect ~5.568): ", round(mean(gp_scores$Situational, na.rm=TRUE), 4), "\n")
cat("Skills      (expect ~5.407): ", round(mean(gp_scores$Skills,      na.rm=TRUE), 4), "\n")
cat("Objective   (expect ~13.035):", round(mean(gp_scores$Objective,   na.rm=TRUE), 4), "\n")

# ── Descriptive statistics ─────────────────────────────────────────────────────
desc_stats <- function(df, group_label) {
  do.call(rbind, lapply(names(scales), function(sc) {
    x <- df[[sc]]
    x <- x[!is.na(x)]
    data.frame(
      Group = group_label,
      Scale = sc,
      n     = length(x),
      Mean  = round(mean(x), 4),
      SD    = round(sd(x),   4)
    )
  }))
}

hcp_desc <- desc_stats(hcp_scores, "HCP")
gp_desc  <- desc_stats(gp_scores,  "Gen Pop")

cat("\n── Descriptive Statistics ──────────────────────────────────────────────\n")
print(rbind(hcp_desc, gp_desc), row.names = FALSE)

# ── Welch t-tests (unequal variance) + Cohen's d + 95% CI ────────────────────
sig_label <- function(p) {
  if      (p < 0.001) "***"
  else if (p < 0.01)  "**"
  else if (p < 0.05)  "*"
  else if (p < 0.10)  "."
  else                "ns"
}

# Cohen's d for unequal groups using pooled SD
cohens_d <- function(x, y) {
  nx <- length(x); ny <- length(y)
  pooled_sd <- sqrt(((nx - 1) * var(x) + (ny - 1) * var(y)) / (nx + ny - 2))
  (mean(x) - mean(y)) / pooled_sd
}

results <- do.call(rbind, lapply(names(scales), function(sc) {
  h  <- hcp_scores[[sc]][!is.na(hcp_scores[[sc]])]
  g  <- gp_scores[[sc]][!is.na(gp_scores[[sc]])]
  tt <- t.test(h, g, var.equal = FALSE)
  d  <- cohens_d(h, g)
  data.frame(
    Scale       = sc,
    HCP_n       = length(h),
    HCP_Mean    = round(mean(h), 4),
    HCP_SD      = round(sd(h),   4),
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

cat("\n── Summary Table: Welch t-tests (HCP vs Gen Pop) ───────────────────────\n")
cat("Significance: *** p<0.001 | ** p<0.01 | * p<0.05 | . p<0.10 | ns = not significant\n\n")
print(results, row.names = FALSE)

# ── Export ─────────────────────────────────────────────────────────────────────
write.csv(results,             "~/Desktop/EAGL_welch_ttest_results.csv",  row.names = FALSE)
write.csv(rbind(hcp_desc, gp_desc), "~/Desktop/EAGL_descriptive_stats.csv", row.names = FALSE)
cat("\nResults saved to Desktop.\n")
