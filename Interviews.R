# =====================================================================
# Interviews.R  -  Question-wise cross analysis of the interviews (HRA vs. AI)
# ---------------------------------------------------------------------
# Qualitative-descriptive analysis of the inductively formed theme
# categories (qualitative content analysis). For each guiding question (Q1-Q5)
# it reports the FREQUENCY of each theme, split by condition (HRA vs. AI).
# Deliberately NO inferential statistics (exploratory, self-formed categories,
# n=20/group): the output is counts and proportions.
#
# Everything is read from the main Excel (data/Data_Input.xlsx,
# sheet 1): the condition plus the interview coding columns q1_* .. q5_*
# (Excel columns CB..DQ). A cell holds 1 if the theme was mentioned, empty/0
# otherwise. Denominator per condition = number of participants in that group.
# Output: output/interview_Q1..Q5.csv  and  output/interview_all.csv
# =====================================================================
suppressMessages({library(readxl); library(dplyr); library(readr)
                  library(ggplot2); library(tidyr)})

## 1) Load data (condition + interview items live in the same sheet) --
study_data <- as.data.frame(read_excel("data/Data_Input.xlsx", sheet = 1))
# HRC -> HRA: the raw value stays "HRC" in the Excel, but is labelled "HRA".
study_data$condition <- ifelse(study_data$condition == "HRC", "HRA", study_data$condition)
# KI -> AI: the raw Excel value "KI" is labelled "AI".
study_data$condition <- ifelse(study_data$condition == "KI", "AI", study_data$condition)
study_data <- study_data %>%
  mutate(id = trimws(as.character(id))) %>%
  filter(!is.na(id), id != "", id != "id", condition %in% c("HRA", "AI"))

## 2) Question metadata + theme labels --------------------------------
# The interview columns are grouped by a prefix q1_ .. q5_, one prefix per
# guiding question. These titles are used for the chart title and the CSV.
question_titles <- c(
  q1 = "Q1 - Overall experience (Effectiveness)",
  q2 = "Q2 - Operation & workflow (Usability / Efficiency)",
  q3 = "Q3 - Trust (TAI)",
  q4 = "Q4 - Workload (NASA-TLX)",
  q5 = "Q5 - Improvements & gaps"
)

# Readable English label for each coding column (source column -> display label).
# Keys must match the Excel headers exactly; a column without an entry keeps its
# raw name and is reported (see label_theme() below).
theme_labels <- c(
  # q1 - Overall experience
  q1_positive          = "Positive / confident",
  q1_withAssistance    = "Only feasible with support",
  q1_confused          = "Familiarization / irritation",
  q1_playful           = "Playful / novel",
  q1_badSight          = "Headset / limited sight",
  q1_selfThinking      = "Had to think along",
  # q2 - Operation & workflow
  q2_easy              = "Easy / good",
  q2_learningCurve     = "Familiarization",
  q2_latency           = "Latency / waiting",
  q2_boxMoves          = "Box shifts / disappears",
  q2_cameraHandling    = "Camera / image handling",
  q2_waitSignal        = "Waiting for signal",
  q2_touchUnreliable   = "Touch unreliable",
  q2_depthHard         = "Depth hard to judge",
  # q3 - Trust
  q3_fullyTrusted      = "Fully trusted",
  q3_askedHoses        = "Doubt at hose step",
  q3_vagueInstruction  = "Instruction imprecise",
  q3_wrongMarker       = "Wrong marker",
  q3_trustNoExpertise  = "Trust without expertise",
  q3_missingGoal       = "Missing goal / purpose",
  q3_ownUnderstanding  = "Own understanding",
  q3_systemQueryDoubt  = "System query unsettling",
  q3_complexFaults     = "Complex faults",
  # q4 - Workload
  q4_low               = "Low",
  q4_hoseDemanding     = "Hose step demanding",
  q4_fineMotor         = "Fine motor skills",
  q4_onlyWithAssist    = "Only feasible with support",
  q4_glassesBurden     = "Headset a burden",
  q4_medium            = "Medium",
  # q5 - Improvements & gaps
  q5_latency           = "Latency",
  q5_clearerInstructions = "More concrete instructions",
  q5_persistentBox     = "Persistent / larger box",
  q5_hardwareView      = "Hardware: field of view",
  q5_moreInteraction   = "More interaction",
  q5_adjustableDetail  = "Adjustable level of detail",
  q5_ttsEnglishBug     = "TTS English bug",
  q5_cameraTrigger     = "Easier camera trigger",
  q5_nothing           = "Nothing / satisfied",
  q5_signalTone        = "Signal tone",
  q5_markerAccuracy    = "Marker accuracy",
  q5_biggerImage       = "Larger / clearer image",
  q5_objectTracking    = "Object tracking"
)

# Cell -> 0/1: a theme counts as "mentioned" (1) unless the cell is empty/NA
# or literally "0".
mark01 <- function(cells) {
  cell_text <- trimws(as.character(cells))                 # trimmed text form of the cell
  not_mentioned <- is.na(cells) | cell_text == "" | cell_text == "0"  # nothing was coded
  as.integer(!not_mentioned)                              # TRUE/FALSE -> 1/0
}

# Map source column names to their readable labels. A name without an entry is
# kept as-is and reported, so nothing is silently mislabeled.
label_theme <- function(column_names) {
  labels <- unname(theme_labels[column_names])
  no_match <- is.na(labels)
  if (any(no_match)) {
    warning("No label for column(s): ",
            paste(unique(column_names[no_match]), collapse = " | "))
    labels[no_match] <- column_names[no_match]
  }
  labels
}

## 3) Analyse one question --------------------------------------------
analyse_question <- function(prefix, title) {
  # All coding columns of this question, e.g. prefix "q1" -> q1_positive, ...
  theme_columns <- grep(paste0("^", prefix, "_"), names(study_data), value = TRUE)
  group_sizes   <- table(factor(study_data$condition, levels = c("HRA", "AI")))

  # Turn the coding cells into a 0/1 matrix (1 = theme mentioned)
  mentions_matrix <- as.data.frame(lapply(study_data[, theme_columns, drop = FALSE], mark01))
  names(mentions_matrix) <- theme_columns

  # Build one summary row per theme (counts + percentages per condition).
  theme_rows <- list()
  for (theme_column in theme_columns) {
    hra_count <- sum(mentions_matrix[[theme_column]][study_data$condition == "HRA"])  # HRA participants who named it
    ai_count  <- sum(mentions_matrix[[theme_column]][study_data$condition == "AI"])   # AI participants who named it
    theme_rows[[theme_column]] <- data.frame(
      Question = title, Theme = theme_column,
      HRA_mentions = hra_count, HRA_pct = round(100 * hra_count / max(group_sizes["HRA"], 1)),
      AI_mentions  = ai_count,  AI_pct  = round(100 * ai_count  / max(group_sizes["AI"],  1)),
      Total_mentions = hra_count + ai_count,
      Total_pct = round(100 * (hra_count + ai_count) / sum(group_sizes)),
      Diff_HRA_minus_AI = hra_count - ai_count,
      stringsAsFactors = FALSE
    )
  }
  result <- do.call(rbind, unname(theme_rows))  # stack the theme rows into one table
                                                # (unname keeps default row numbers)

  # Sort: most-mentioned themes first, ties broken by the HRA-minus-AI difference
  result <- result[order(-result$Total_mentions, -result$Diff_HRA_minus_AI), ]

  # Replace the technical column names with readable English labels (CSV + chart)
  result$Theme <- label_theme(result$Theme)

  # Attach the per-condition group sizes as an attribute, so the returned object
  # still behaves like a normal data.frame for print()/write(), while the caller
  # can read the sizes back with attr(result, "n_cond").
  attr(result, "n_cond") <- group_sizes
  result
}

## 4) Grouped bar chart per question (HRA vs. AI) ---------------------
plot_question <- function(result, title, n_hra, n_ai, file) {
  # Reshape to long format: one row per (theme, condition) so ggplot can dodge
  # the HRA and AI bars side by side.
  plot_data <- result %>%
    transmute(Theme = Theme, HRA = HRA_mentions, AI = AI_mentions) %>%
    pivot_longer(c(HRA, AI), names_to = "Condition", values_to = "Mentions")
  theme_order <- result$Theme[order(result$Total_mentions)]   # coord_flip: highest on top
  plot_data$Theme     <- factor(plot_data$Theme, levels = theme_order)
  plot_data$Condition <- factor(plot_data$Condition, levels = c("HRA", "AI"))
  plot <- ggplot(plot_data, aes(Theme, Mentions, fill = Condition)) +
    geom_col(position = position_dodge(width = 0.75), width = 0.7) +
    geom_text(aes(label = ifelse(Mentions > 0, Mentions, "")),
              position = position_dodge(width = 0.75), hjust = -0.3, size = 3) +
    coord_flip() +
    scale_fill_manual(values = c(HRA = "#F8766D", AI = "#00BFC4")) +
    scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, 5),
                       expand = expansion(mult = c(0, 0.08))) +
    labs(title = title,
         subtitle = sprintf("Number of mentions per condition (HRA n=%d, AI n=%d)",
                            as.integer(n_hra), as.integer(n_ai)),
         x = NULL, y = "Number of mentions (out of 20 each)", fill = "Condition") +
    theme_minimal(base_size = 11) +
    theme(panel.grid.major.y = element_blank(),
          legend.position = "top",
          plot.title = element_text(face = "bold"))
  ggsave(file, plot, width = 8, height = max(2.6, 0.42 * nrow(result) + 1.3), dpi = 150)
}

## 5) Analysis + output -----------------------------------------------
cat("Participants with condition:", nrow(study_data),
    "(HRA", sum(study_data$condition == "HRA"), "/ AI", sum(study_data$condition == "AI"), ")\n")

all_results <- list()
for (prefix in names(question_titles)) {
  question_result <- analyse_question(prefix, question_titles[[prefix]])
  group_sizes     <- attr(question_result, "n_cond")
  cat("\n===== ", question_titles[[prefix]],
      "  (HRA n=", group_sizes["HRA"], ", AI n=", group_sizes["AI"], ") =====\n", sep = "")
  print(question_result, row.names = FALSE)
  tag <- toupper(prefix)          # q1 -> Q1 for the output file names
  # Excel-friendly: UTF-8 with BOM, semicolon-separated.
  readr::write_excel_csv2(question_result, sprintf("output/interview_%s.csv", tag))
  plot_question(question_result, question_titles[[prefix]],
                group_sizes["HRA"], group_sizes["AI"],
                sprintf("output/interview_%s.png", tag))
  all_results[[prefix]] <- question_result
}
# Stack all five per-question tables into one combined CSV
readr::write_excel_csv2(do.call(rbind, all_results), "output/interview_all.csv")
cat("\nDone -> output/interview_Q1..Q5.csv + interview_all.csv\n")
