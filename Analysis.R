# =====================================================================
# User study analysis: AR repair assistance on HoloLens 2 (HRA vs. AI)
# Bachelor's thesis - between-subjects design
# =====================================================================
# Flow: packages -> load data -> factors -> reverse-coding ->
#       scale scores -> reliability -> descriptives -> group comparison -> plots
# ---------------------------------------------------------------------

## 0) Packages --------------------------------------------------------
packages <- c("tidyverse", "readxl", "psych", "effsize", "car", "rstatix", "janitor")

# Install every package that is not installed yet
installed_packages <- rownames(installed.packages())     # names of installed packages
missing_packages   <- packages[!(packages %in% installed_packages)]
if (length(missing_packages) > 0) install.packages(missing_packages)

# Load every package (character.only = TRUE lets library() take the name as a string)
for (package_name in packages) library(package_name, character.only = TRUE)

## 1) Working directory + load data -----------------------------------
# In RStudio: Session -> Set Working Directory -> To Source File Location
# or manually:  setwd(".../userStudy/R_Auswertung")

# IMPORTANT: read straight from the CURRENT Excel file.
# Note: if the file was saved as "Strict Open XML Spreadsheet", read_excel may
# fail. In that case, re-save it in Excel as "Excel Workbook (*.xlsx)"
# (NOT "Strict Open XML").
study_data <- readxl::read_excel("data/Data_Input.xlsx", sheet = 1)
study_data <- as.data.frame(study_data)   # tibble -> data.frame so the [, columns] logic below works

# in the dataset and in every output (plots, tables) the condition is labelled
# "HRA". This affects the condition and the forced-choice answer.
study_data$condition <- ifelse(study_data$condition == "HRC", "HRA", study_data$condition)
if ("guess_forced" %in% names(study_data)) {
  study_data$guess_forced <- ifelse(study_data$guess_forced == "HRC", "HRA", study_data$guess_forced)
}
# KI -> AI: the raw Excel value "KI" is labelled "AI" in the dataset/outputs.
study_data$condition <- ifelse(study_data$condition == "KI", "AI", study_data$condition)
if ("guess_forced" %in% names(study_data)) {
  study_data$guess_forced <- ifelse(study_data$guess_forced == "KI", "AI", study_data$guess_forced)
}

# Drop header/empty rows and invalid conditions.
study_data <- study_data %>%
  mutate(id = trimws(as.character(id))) %>%
  filter(!is.na(id), id != "", id != "id",
         condition %in% c("HRA", "AI"))

cat("Cases read in:", nrow(study_data), "\n")
print(table(study_data$condition))

## 2) Data types / factors --------------------------------------------
study_data$condition <- factor(study_data$condition, levels = c("HRA", "AI"))
study_data$gender    <- factor(study_data$gender)
study_data$ai_freq   <- factor(study_data$ai_freq, levels = 1:6, ordered = TRUE)

## 3) Reverse-coding ---------------------------------------------------
# Helper: scale_max is the scale maximum (e.g. a 7-point scale -> scale_max = 7)
reverse_code <- function(value, scale_max) (scale_max + 1) - value

# ATI (1-6): ati_3, ati_4 are reversed
study_data$ati_3r <- reverse_code(study_data$ati_3, 6)
study_data$ati_4r <- reverse_code(study_data$ati_4, 6)

# Trust TAI (1-5, 8 items): trust_6 reversed -> (5+1)-value
study_data$trust_6r <- reverse_code(study_data$trust_6, 5)

# System/AR (1-7): sys_wait_long reversed
study_data$sys_wait_long_r <- reverse_code(study_data$sys_wait_long, 7)

## 4) Scale scores (per-person means) ---------------------------------
row_means <- function(frame, columns) rowMeans(frame[, columns], na.rm = TRUE)

study_data$ATI     <- row_means(study_data, c("ati_1","ati_2","ati_3r","ati_4r"))         # 1-6
study_data$Trust   <- row_means(study_data, c("trust_1","trust_2","trust_3","trust_4",
                                              "trust_5","trust_6r","trust_7","trust_8"))  # 1-7
study_data$PerfExp <- row_means(study_data, c("pe_1","pe_2","pe_3","pe_4"))               # 1-7
study_data$BehInt  <- row_means(study_data, c("bi_1","bi_2","bi_3"))                      # 1-7
study_data$Cues    <- row_means(study_data, c("cue_speed","cue_wording","cue_tone",
                                              "cue_correct","cue_precision",
                                              "cue_understand","cue_errortype"))         # 1-7

# --- NASA-TLX (Raw TLX): mean of the 6 dimensions (0-100) ---
# Invert the performance dimension for overall load (high = high load)
study_data$tlx_perf_inv <- 100 - study_data$tlx_performance
study_data$TLX <- rowMeans(study_data[, c("tlx_mental","tlx_physical","tlx_temporal",
                                          "tlx_perf_inv","tlx_effort","tlx_frustration")],
                           na.rm = TRUE)

# --- SUS (0-100) ---
# odd items: value - 1 ; even items: 5 - value ; sum x 2.5
sus_odd_items  <- c("sus_1","sus_3","sus_5","sus_7","sus_9")
sus_even_items <- c("sus_2","sus_4","sus_6","sus_8","sus_10")
study_data$SUS <- (rowSums(study_data[, sus_odd_items] - 1, na.rm = TRUE) +
                   rowSums(5 - study_data[, sus_even_items], na.rm = TRUE)) * 2.5

# Same-polarity SUS items (high = better) for the reliability estimate.
# For each even item, add a new column "<item>_r" that holds the flipped value.
for (sus_item in sus_even_items) {
  study_data[[paste0(sus_item, "_r")]] <- 6 - study_data[[sus_item]]   # e.g. creates column "sus_2_r"
}
sus_items_keyed <- c(sus_odd_items, paste0(sus_even_items, "_r"))

## 5) Reliability (Cronbach's alpha) ----------------------------------
alpha_safe <- function(items) {
  tryCatch(psych::alpha(study_data[, items], warnings = FALSE)$total$raw_alpha,
           error = function(e) NA_real_)
}
reliability <- tibble(
  Scale = c("ATI","Trust","PerfExp","BehInt","Cues","SUS"),
  Alpha = c(
    alpha_safe(c("ati_1","ati_2","ati_3r","ati_4r")),
    alpha_safe(c("trust_1","trust_2","trust_3","trust_4","trust_5",
                 "trust_6r","trust_7","trust_8")),
    alpha_safe(c("pe_1","pe_2","pe_3","pe_4")),
    alpha_safe(c("bi_1","bi_2","bi_3")),
    alpha_safe(c("cue_speed","cue_wording","cue_tone","cue_correct",
                 "cue_precision","cue_understand","cue_errortype")),
    alpha_safe(sus_items_keyed)
  )
)
print(reliability)

## 6) Descriptive statistics per condition ----------------------------
# Non-parametric approach -> report median + IQR (Q1/Q3), per group with n
# (the n may be unequal). Mean/SD only as extra information.
# quantile()/IQR() use type = 7 (R default).
scale_variables <- c("SUS","TLX","Trust","PerfExp","BehInt","Cues","ATI",
                     "guess_slider")

descriptive <- study_data %>%
  select(condition, all_of(scale_variables)) %>%
  pivot_longer(-condition, names_to = "Variable", values_to = "Value") %>%
  mutate(Variable = factor(Variable, levels = scale_variables)) %>%
  group_by(Variable, condition) %>%
  summarise(
    n   = sum(!is.na(Value)),
    Md  = median(Value, na.rm = TRUE),
    Q1  = quantile(Value, 0.25, na.rm = TRUE, type = 7, names = FALSE),
    Q3  = quantile(Value, 0.75, na.rm = TRUE, type = 7, names = FALSE),
    IQR = IQR(Value, na.rm = TRUE, type = 7),
    M   = mean(Value, na.rm = TRUE),
    SD  = sd(Value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(across(c(Md, Q1, Q3, IQR, M, SD), ~round(.x, 2)),
         # ready-made column for the Chapter 7 table: "Median [Q1, Q3]"
         Median_IQR = sprintf("%.1f [%.1f, %.1f]", Md, Q1, Q3)) %>%
  arrange(Variable, condition)

print(as.data.frame(descriptive))

## 6b) Covariate balance check (HRA vs. AI) ---------------------------
# Are the groups comparable on background characteristics? (internal validity /
# success of the stratified assignment). Metric/ordinal: median [Q1, Q3] +
# Mann-Whitney; categorical: frequencies + Fisher's exact test. ATI comes from
# section 4. Non-significant p values suggest comparable groups.
fisher_p <- function(contingency_table) {
  p_value <- tryCatch(fisher.test(contingency_table)$p.value, error = function(e) NA_real_)
  if (is.na(p_value)) p_value <- tryCatch(
    fisher.test(contingency_table, simulate.p.value = TRUE, B = 1e4)$p.value,
    error = function(e) NA_real_)
  p_value
}
median_iqr <- function(values) sprintf("%.1f [%.1f, %.1f]", median(values),
                                       quantile(values, .25, names = FALSE, type = 7),
                                       quantile(values, .75, names = FALSE, type = 7))

metric_covariates <- c("age","exp_ar","exp_repair","ATI","ai_attitude","ai_trust_gen")

# Build one result row per covariate and collect them in a list, then stack.
balance_rows <- list()
for (covariate in metric_covariates) {
  if (!covariate %in% names(study_data)) next   # skip covariates that are not present

  hra_values <- study_data[[covariate]][study_data$condition == "HRA"]   # HRA values, drop NA
  hra_values <- hra_values[!is.na(hra_values)]
  ai_values  <- study_data[[covariate]][study_data$condition == "AI"]    # AI values, drop NA
  ai_values  <- ai_values[!is.na(ai_values)]

  p_value <- tryCatch(wilcox.test(hra_values, ai_values, exact = FALSE)$p.value,
                      error = function(e) NA_real_)
  balance_rows[[covariate]] <- tibble(Covariate = covariate, HRA = median_iqr(hra_values),
                                      AI = median_iqr(ai_values), p = round(p_value, 3))
}
balance <- bind_rows(balance_rows)           # combine all rows into one table
cat("\nCovariate balance (metric/ordinal, median [Q1, Q3]):\n")
print(as.data.frame(balance))

# Categorical covariates incl. stratum Woehler/private
for (covariate in c("gender","ai_freq","woehler_employee")) {
  if (!covariate %in% names(study_data)) next
  contingency_table <- table(study_data$condition, study_data[[covariate]])
  cat(sprintf("\n%s (Fisher p = %.3f):\n", covariate, fisher_p(contingency_table)))
  print(contingency_table)
}

## 7) Group comparison HRA vs. AI: Mann-Whitney + Cliff's d + Holm ----
# Procedure per plan (6.8): non-parametric (Mann-Whitney/wilcox.test),
# two-sided. For each test the effect size Cliff's delta with a bootstrap
# 95% CI (step 5). The Holm correction (step 6) runs ONLY over the
# confirmatory family H1-H5. (t-test/Shapiro are deliberately omitted.)

# Cliff's delta = P(HRA > AI) - P(HRA < AI); > 0 => HRA values tend to be higher.
# Compare every value in group_a with every value in group_b:
#   sign(value_a - value_b) = +1 if the group_a value is larger, -1 if smaller, 0 if equal.
# outer() builds the full group_a-by-group_b comparison matrix; the mean of all
# those +1 / -1 / 0 signs is exactly Cliff's delta.
cliff_delta <- function(group_a, group_b) {
  comparison_signs <- outer(group_a, group_b, function(value_a, value_b) sign(value_a - value_b))
  mean(comparison_signs)
}

# Bootstrap confidence interval for Cliff's delta: resample both groups with
# replacement n_boot times, recompute delta each time, and take the CI from the
# quantiles of those n_boot deltas.
boot_cliff_ci <- function(group_a, group_b, n_boot = 5000, conf_level = 0.95) {
  if (length(group_a) < 2 || length(group_b) < 2) return(c(NA_real_, NA_real_))

  deltas <- replicate(n_boot, {
    a_resampled <- sample(group_a, replace = TRUE)
    b_resampled <- sample(group_b, replace = TRUE)
    cliff_delta(a_resampled, b_resampled)
  })

  lower_prob <- (1 - conf_level) / 2     # 0.025 for conf_level = 0.95
  upper_prob <- 1 - lower_prob           # 0.975 for conf_level = 0.95
  unname(quantile(deltas, c(lower_prob, upper_prob), na.rm = TRUE))
}

# Effect label per Romano et al  for |delta|
cliff_label <- function(delta) {
  abs_delta <- abs(delta)
  if (is.na(abs_delta))          NA_character_
  else if (abs_delta < 0.147)    "negligible"
  else if (abs_delta < 0.33)     "small"
  else if (abs_delta < 0.474)    "medium"
  else                           "large"
}

# Mapping hypothesis -> variable -> family. H1/H2 are placeholders until the
# objective data (repair time from the logs, process quality from the rubric)
# have been merged into study_data.
hypotheses <- tribble(
  ~Hypothesis,                 ~Variable,               ~Family,
  "H1 Efficiency",             "log_reparaturzeit_min", "confirmatory",
  "H2 Effectiveness",          "guete_score",           "confirmatory",
  "H3 Usability",              "SUS",                   "confirmatory",
  "H4 Workload",               "TLX",                   "confirmatory",
  "H5 Trust",                  "Trust",                 "confirmatory",
  "H6b Perception",            "guess_slider",          "Blinding",
  "sec. Interaction steps",    "log_anzahl_anfragen",   "exploratory",
  "sec. Latency (mean)",       "log_latenz_mittel_s",   "exploratory",
  "expl. Performance exp.",    "PerfExp",               "exploratory",
  "expl. Behavioral int.",     "BehInt",                "exploratory",
  "expl. Cues",                "Cues",                  "exploratory"
)

test_hypothesis <- function(hypothesis, variable, family) {
  empty_row <- tibble(Hypothesis = hypothesis, Variable = variable, Family = family,
                      n_HRA = NA_integer_, n_AI = NA_integer_, Wilcox_p = NA_real_,
                      Cliff_d = NA_real_, CI_low = NA_real_, CI_high = NA_real_,
                      Effect = NA_character_, Note = "")
  if (!variable %in% names(study_data)) {
    empty_row$Note <- "Variable missing (not merged yet)"; return(empty_row)
  }
  hra_values <- study_data[[variable]][study_data$condition == "HRA"]
  hra_values <- hra_values[!is.na(hra_values)]
  ai_values  <- study_data[[variable]][study_data$condition == "AI"]
  ai_values  <- ai_values[!is.na(ai_values)]
  if (length(hra_values) < 2 || length(ai_values) < 2) {
    empty_row$Note <- "too few data"; return(empty_row)
  }

  p_value       <- tryCatch(wilcox.test(hra_values, ai_values, exact = FALSE, correct = TRUE)$p.value,
                            error = function(e) NA_real_)
  delta         <- cliff_delta(hra_values, ai_values)
  conf_interval <- boot_cliff_ci(hra_values, ai_values)
  tibble(Hypothesis = hypothesis, Variable = variable, Family = family,
         n_HRA = length(hra_values), n_AI = length(ai_values), Wilcox_p = round(p_value, 3),
         Cliff_d = round(delta, 2), CI_low = round(conf_interval[1], 2),
         CI_high = round(conf_interval[2], 2),
         Effect = cliff_label(delta), Note = "")
}

set.seed(2026)  # reproducible bootstrap CIs

# Run the test for each row of the hypotheses table and collect the result rows.
result_rows <- list()
for (row_index in seq_len(nrow(hypotheses))) {
  result_rows[[row_index]] <- test_hypothesis(hypotheses$Hypothesis[row_index],
                                              hypotheses$Variable[row_index],
                                              hypotheses$Family[row_index])
}
results <- bind_rows(result_rows)            # combine all rows into one table

# Step 6: Holm correction ONLY over the confirmatory family (H1-H5).
# n = 5 (pre-specified family size). All 5 p values are now available.
is_confirmatory <- results$Family == "confirmatory"
results$p_Holm <- NA_real_
results$p_Holm[is_confirmatory] <- round(
  p.adjust(results$Wilcox_p[is_confirmatory], method = "holm", n = sum(is_confirmatory)), 3)

print(as.data.frame(results))

# H2 (extra): task success (binary) -> success rates + Fisher's exact test.
# guete_score (0-1) is the primary measure for H2 (above, Mann-Whitney); success
# (0/1) is the complementary binary success measure.
if (all(c("success","condition") %in% names(study_data))) {
  success_rates <- study_data %>%
    group_by(condition) %>%
    summarise(n           = sum(!is.na(success)),
              Successes    = sum(success == 1, na.rm = TRUE),
              SuccessRate  = round(mean(success, na.rm = TRUE), 2),
              .groups = "drop")
  cat("\nH2 (extra) - task success (success) per condition:\n")
  print(as.data.frame(success_rates))

  success_table <- table(study_data$condition, study_data$success)
  fisher_result <- tryCatch(fisher.test(success_table), error = function(e) NULL)
  print(if (is.null(fisher_result)) "too few data" else fisher_result)

  # Effect sizes for the Fisher test: risk difference (HRA - AI) and odds ratio
  # (conditional MLE with 95% CI from fisher.test). RD > 0 => HRA more successful.
  rate_hra <- mean(study_data$success[study_data$condition == "HRA"], na.rm = TRUE)
  rate_ai  <- mean(study_data$success[study_data$condition == "AI"],  na.rm = TRUE)
  success_effect <- tibble(
    RiskDiff_HRA_minus_AI = round(rate_hra - rate_ai, 3),
    Odds_Ratio            = if (!is.null(fisher_result)) round(unname(fisher_result$estimate), 3) else NA_real_,
    OR_CI_low             = if (!is.null(fisher_result)) round(fisher_result$conf.int[1], 3) else NA_real_,
    OR_CI_high            = if (!is.null(fisher_result)) round(fisher_result$conf.int[2], 3) else NA_real_,
    Fisher_p              = if (!is.null(fisher_result)) round(fisher_result$p.value, 3) else NA_real_
  )
  cat("\nTask success - effect sizes (Fisher: risk difference + odds ratio):\n")
  print(as.data.frame(success_effect))
}

## 7b) H6a - blinding: can participants detect the modality above chance? ---
# Slider: 0 = certainly AI ... 50 = neutral ... 100 = certainly human.
# guess_forced: "HRA" = guessed human, "AI" = guessed AI.
# The correct answer is "HRA" in the HRA condition and "AI" in the AI condition.
# Effect size for the one-sample Wilcoxon: matched-pairs rank-biserial r
# (Kerby 2014). deviations = value - neutral_point; drop zeros; rank |deviations|;
# r = (R+ - R-) / sum(R).
# Sign: > 0 towards "human" (100), < 0 towards "AI" (0); range [-1, 1].
rb_onesample <- function(values, neutral_point = 50) {
  deviations <- values - neutral_point   # distance of each value from the neutral point
  deviations <- deviations[deviations != 0]  # Wilcoxon ignores values exactly on it
  if (length(deviations) < 1) return(NA_real_)

  ranks        <- rank(abs(deviations))         # rank the distances by absolute size
  sum_positive <- sum(ranks[deviations > 0])    # summed ranks on the "human" side (> 50)
  sum_negative <- sum(ranks[deviations < 0])    # summed ranks on the "AI" side   (< 50)
  (sum_positive - sum_negative) / sum(ranks)
}

# Compute the blinding results separately for each condition, then stack them.
h6a_rows <- list()
for (condition_name in c("HRA","AI")) {
  condition_rows <- study_data[study_data$condition == condition_name, ]

  # (1) One-sample Wilcoxon of the slider against the neutral point 50
  slider_values <- condition_rows$guess_slider[!is.na(condition_rows$guess_slider)]
  wilcox_result <- tryCatch(wilcox.test(slider_values, mu = 50, exact = FALSE, correct = TRUE),
                            error = function(e) NULL)

  # (2) Forced choice: correct identification against chance (50%)
  forced_choices <- condition_rows$guess_forced[!is.na(condition_rows$guess_forced) &
                                                condition_rows$guess_forced != ""]
  n_answers <- length(forced_choices)
  n_correct <- sum(forced_choices == condition_name)   # "HRA" in HRA cond., "AI" in AI cond.
  binom_result <- tryCatch(binom.test(n_correct, n_answers, p = 0.5), error = function(e) NULL)

  h6a_rows[[condition_name]] <- tibble(
    Condition     = condition_name,
    n             = n_answers,
    Slider_Median = median(slider_values),
    Slider_p      = if (!is.null(wilcox_result)) round(wilcox_result$p.value, 3) else NA_real_,
    Slider_rrb    = round(rb_onesample(slider_values, 50), 2),   # effect size (rank-biserial)
    Correct       = sprintf("%d/%d", n_correct, n_answers),
    Correct_Prop  = round(n_correct / n_answers, 2),
    Binomial_p    = if (!is.null(binom_result)) round(binom_result$p.value, 3) else NA_real_
  )
}
h6a <- bind_rows(h6a_rows)                    # combine both condition rows
cat("\nH6a - indistinguishability (blinding):\n")
print(as.data.frame(h6a))

# Additional (NOT H6a): association choice x condition -> Fisher
if (all(c("guess_forced","condition") %in% names(study_data))) {
  contingency_table <- table(study_data$condition, study_data$guess_forced)
  cat("\nAdditional - forced choice x condition:\n"); print(contingency_table)
  print(tryCatch(fisher.test(contingency_table), error = function(e) "too few data"))
}

## 7c) Exploratory extra analyses -------------------------------------
# Simple comparison HRA vs. AI per variable (Mann-Whitney + Cliff's delta).
simple_compare <- function(variable) {
  hra_values <- study_data[[variable]][study_data$condition == "HRA"]   # HRA values, drop NA
  hra_values <- hra_values[!is.na(hra_values)]
  ai_values  <- study_data[[variable]][study_data$condition == "AI"]    # AI values, drop NA
  ai_values  <- ai_values[!is.na(ai_values)]
  p_value <- tryCatch(wilcox.test(hra_values, ai_values, exact = FALSE)$p.value,
                      error = function(e) NA_real_)
  tibble(Variable = variable, Overall_Md = median(study_data[[variable]], na.rm = TRUE),
         HRA_Md = median(hra_values), AI_Md = median(ai_values),
         Wilcox_p = round(p_value, 3), Cliff_d = round(cliff_delta(hra_values, ai_values), 2))
}

# (a) Part B - cues: what was the guess based on? (strength 1-7)
# Run simple_compare on each cue variable, stack the rows, sort by overall median.
cue_variables <- c("cue_speed","cue_wording","cue_tone","cue_correct",
                   "cue_precision","cue_understand","cue_errortype")
cues <- bind_rows(lapply(cue_variables, simple_compare)) %>% arrange(desc(Overall_Md))
cat("\nPart B - cues (ranked by overall median = strength of the cue):\n")
print(as.data.frame(cues))

# (b) E2 / AR-specific items (esp. waiting time) HRA vs. AI
# Same idea as above: compare each AR item between the two conditions.
ar_item_variables <- c("sys_wait_ok","sys_wait_long","sys_box_accurate",
                       "sys_visual_help","sys_audio_help","sys_understood")
ar_items <- bind_rows(lapply(ar_item_variables, simple_compare))
cat("\nE2 / AR items (HRA vs. AI):\n"); print(as.data.frame(ar_items))

# Objective vs. subjective: was the latency noticed subjectively? (Spearman)
cat("\nLatency (objective) vs. waiting perception (Spearman, all):\n")
cat("  log_latenz_mittel_s ~ sys_wait_ok  : rho =",
    round(cor(study_data$log_latenz_mittel_s, study_data$sys_wait_ok,   method = "spearman", use = "complete.obs"), 2), "\n")
cat("  log_latenz_mittel_s ~ sys_wait_long: rho =",
    round(cor(study_data$log_latenz_mittel_s, study_data$sys_wait_long, method = "spearman", use = "complete.obs"), 2), "\n")

# (c) guess_when - from when on were they sure? (per condition)
cat("\nguess_when (from when on sure?) per condition:\n")
print(table(study_data$condition, study_data$guess_when))

# (d) Calibration: did they believe they were faster? pe_2 vs. objective time
cat("\nSubjectively 'faster' (pe_2) vs. objective task time (Spearman):\n")
cat("  pe_2 ~ log_reparaturzeit_min: rho =",
    round(cor(study_data$pe_2, study_data$log_reparaturzeit_min, method = "spearman", use = "complete.obs"), 2), "\n")

## 8) Export results --------------------------------------------------
dir.create("output", showWarnings = FALSE)
write.csv2(descriptive, "output/descriptive.csv", row.names = FALSE)
write.csv2(results,     "output/group_comparison.csv", row.names = FALSE)
write.csv2(reliability, "output/reliability.csv", row.names = FALSE)
write.csv2(balance,     "output/balance_covariates.csv", row.names = FALSE)
write.csv2(cues,        "output/cues.csv", row.names = FALSE)
write.csv2(ar_items,    "output/ar_items.csv", row.names = FALSE)
write.csv2(h6a,         "output/h6a_blinding.csv", row.names = FALSE)
if (exists("success_effect"))
  write.csv2(success_effect, "output/success_effect.csv", row.names = FALSE)
write.csv2(study_data,  "output/data_with_scales.csv", row.names = FALSE)

## 9) Plots -----------------------------------------------------------
# English axis/labels for the figures. The condition codes HRA/AI are kept as
# the established group names.
axis_labels <- c(
  log_reparaturzeit_min = "Repair time (min)",
  guete_score           = "Process quality (0-1)",
  log_anzahl_anfragen   = "Interaction steps",
  log_latenz_mittel_s   = "Latency per response (s)",
  TLX                   = "Cognitive load (Raw-TLX)",
  SUS                   = "Usability (SUS)",
  Trust                 = "Trust (TAI)",
  PerfExp               = "Performance expectancy",
  BehInt                = "Behavioral intention"
)
plot_box <- function(variable) {
  # .data[[variable]] tells ggplot to use the column whose name is stored in 'variable'
  ggplot(study_data, aes(condition, .data[[variable]], fill = condition)) +
    geom_boxplot(alpha = .6, outlier.shape = NA) +
    geom_jitter(width = .12, height = 0, alpha = .6) +   # height=0: no vertical noise
    labs(x = "Condition", y = axis_labels[[variable]]) +
    theme_minimal() + theme(legend.position = "none")
}
for (variable_name in names(axis_labels)) {
  ggsave(sprintf("output/box_%s.png", variable_name), plot_box(variable_name),
         width = 5, height = 4, dpi = 150)
}

# H6a: distribution of the slider guess per condition, neutral point 50.
# (0 = certainly AI ... 50 = neutral ... 100 = certainly human)
h6a_plot <- ggplot(study_data[!is.na(study_data$guess_slider), ],
                   aes(condition, guess_slider, fill = condition)) +
  geom_hline(yintercept = 50, linetype = "dashed", colour = "grey40") +
  geom_boxplot(alpha = .6, outlier.shape = NA, width = .5) +
  geom_jitter(width = .12, height = 0, alpha = .6) +   # height=0: no vertical noise
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25),
                     sec.axis = dup_axis(breaks = 50, labels = "neutral",
                                         name = NULL)) +
  labs(x = "Condition",
       y = "Guess  (0 = certainly AI  ...  100 = certainly human)") +
  theme_minimal() + theme(legend.position = "none")
ggsave("output/h6a_slider.png", h6a_plot, width = 5, height = 4, dpi = 150)

cat("\nDone. Results are in the 'output/' folder.\n")
