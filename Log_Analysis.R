# =====================================================================
# Log analysis: write per-session metrics from JSONL DIRECTLY into the .xlsx
# =====================================================================
# Reads all IDs from column A of the .xlsx, finds the file
# session_<id>.jsonl in the metrics folder for each ID and computes per session:
#   BS log_reparaturzeit_min    = last - first timestamp (minutes)
#   BT log_latenz_mittel_s      = mean durationSeconds (only 'response')
#   BU log_latenz_max_s         = max  durationSeconds
#   BV log_latenz_min_s         = min  durationSeconds
#   BW log_anzahl_anfragen      = number of 'request' events
#   BX log_fragelaenge_mittel   = mean questionLength (only 'request')
#   BY log_antwortlaenge_mittel = mean answerLength   (only 'response')
#
# The values are written DIRECTLY into the same .xlsx (from column BS onwards).
# Existing content/formatting is preserved.
# ---------------------------------------------------------------------

## 0) Packages --------------------------------------------------------
packages <- c("jsonlite", "openxlsx")

# Install every package that is not installed yet
installed_packages <- rownames(installed.packages())     # names of installed packages
missing_packages   <- packages[!(packages %in% installed_packages)]
if (length(missing_packages) > 0) install.packages(missing_packages)

# Load every package (character.only = TRUE lets library() take the name as a string)
for (package_name in packages) library(package_name, character.only = TRUE)

## 1) Paths (adjust if needed) ----------------------------------------
xlsx_path   <- "data/Data_Input.xlsx"
metrics_dir <- "metrics"

first_log_column <- 71   # target column for the first log metric (BS = 71)

## 2) Load workbook (for the later in-place write) --------------------
workbook   <- openxlsx::loadWorkbook(xlsx_path)
sheet_name <- names(workbook)[1]
sheet_data <- openxlsx::readWorkbook(workbook, sheet = sheet_name, colNames = TRUE)

# ID = column A. Convert to text robustly (large numbers without .0 / exponent).
id_raw_values <- sheet_data[[1]]
id_text <- trimws(as.character(id_raw_values))
id_text <- sub("\\.0+$", "", id_text)                        # "170648448.0" -> "170648448"
scientific_notation <- grepl("[eE]\\+?[0-9]+$", id_text)     # catch possible scientific notation
if (any(scientific_notation, na.rm = TRUE))
  id_text[scientific_notation] <- format(as.numeric(id_text[scientific_notation]),
                                         scientific = FALSE, trim = TRUE)

# Warn about duplicate IDs (they would get the same session file!)
duplicate_ids <- id_text[!is.na(id_text) & nzchar(id_text)]
duplicate_ids <- duplicate_ids[duplicated(duplicate_ids)]
if (length(duplicate_ids))
  warning("Duplicate IDs in column A: ", paste(unique(duplicate_ids), collapse = ", "),
          " -> both rows get the same log values. Please check!")

## 3) Helper: analyse a single session file ---------------------------
get_field <- function(record, key) if (!is.null(record[[key]])) record[[key]] else NA

parse_session <- function(file_path) {
  text_lines <- readLines(file_path, warn = FALSE, encoding = "UTF-8")
  text_lines <- text_lines[nzchar(trimws(text_lines))]
  # Parse each line as JSON; a line that fails to parse becomes NULL
  records <- lapply(text_lines, function(line) tryCatch(jsonlite::fromJSON(line),
                                                        error = function(e) NULL))
  # Keep only the records that parsed successfully (drop the NULL entries)
  records <- records[!vapply(records, is.null, logical(1))]
  if (!length(records)) return(NULL)

  # Pull one field out of every record. vapply's type argument
  # (character(1) / numeric(1)) says what type each single result should be.
  timestamps_raw   <- vapply(records, function(record) get_field(record, "ts"),    character(1))
  events           <- vapply(records, function(record) get_field(record, "event"), character(1))
  durations        <- as.numeric(vapply(records, function(record) get_field(record, "durationSeconds"), numeric(1)))
  question_lengths <- as.numeric(vapply(records, function(record) get_field(record, "questionLength"),  numeric(1)))
  answer_lengths   <- as.numeric(vapply(records, function(record) get_field(record, "answerLength"),    numeric(1)))

  # Timestamps (ISO-8601, UTC). Strip the trailing "Z", then parse.
  timestamps <- as.POSIXct(sub("Z$", "", timestamps_raw), format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")

  is_request  <- !is.na(events) & events == "request"
  is_response <- !is.na(events) & events == "response"

  response_latencies <- durations[is_response];      response_latencies <- response_latencies[!is.na(response_latencies)]
  request_lengths    <- question_lengths[is_request]; request_lengths   <- request_lengths[!is.na(request_lengths)]
  answer_values      <- answer_lengths[is_response];  answer_values      <- answer_values[!is.na(answer_values)]
  # Note: empty/failed answers (answerLength 0) are counted here as well.
  # To exclude them: answer_values <- answer_values[answer_values > 0]

  data.frame(
    log_reparaturzeit_min    = if (sum(!is.na(timestamps)) >= 2)
                                 round(as.numeric(difftime(max(timestamps, na.rm = TRUE),
                                                           min(timestamps, na.rm = TRUE),
                                                           units = "mins")), 2) else NA,
    log_latenz_mittel_s      = if (length(response_latencies)) round(mean(response_latencies), 2) else NA,
    log_latenz_max_s         = if (length(response_latencies)) round(max(response_latencies),  2) else NA,
    log_latenz_min_s         = if (length(response_latencies)) round(min(response_latencies),  2) else NA,
    log_anzahl_anfragen      = sum(is_request),
    log_fragelaenge_mittel   = if (length(request_lengths)) round(mean(request_lengths), 1) else NA,
    log_antwortlaenge_mittel = if (length(answer_values))   round(mean(answer_values),   1) else NA,
    stringsAsFactors = FALSE
  )
}

## 4) Go through all rows ---------------------------------------------
empty_row <- data.frame(log_reparaturzeit_min = NA, log_latenz_mittel_s = NA,
                        log_latenz_max_s = NA, log_latenz_min_s = NA,
                        log_anzahl_anfragen = NA, log_fragelaenge_mittel = NA,
                        log_antwortlaenge_mittel = NA)

session_rows  <- vector("list", nrow(sheet_data))
found_count   <- 0L
missing_count <- 0L

for (row_index in seq_len(nrow(sheet_data))) {
  participant_id <- id_text[row_index]
  if (is.na(participant_id) || !nzchar(participant_id)) {
    session_rows[[row_index]] <- empty_row; next
  }
  session_file <- file.path(metrics_dir, paste0("session_", participant_id, ".jsonl"))
  if (file.exists(session_file)) {
    session_metrics <- parse_session(session_file)
    session_rows[[row_index]] <- if (is.null(session_metrics)) empty_row else session_metrics
    found_count <- found_count + 1L
  } else {
    session_rows[[row_index]] <- empty_row
    missing_count <- missing_count + 1L
  }
}
# Stack the per-row results (one data.frame per row) into a single data.frame
all_metrics <- do.call(rbind, session_rows)

cat(sprintf("Sessions found: %d | no file: %d\n", found_count, missing_count))

## 5) Write log columns DIRECTLY into the .xlsx (from BS onwards) ------
# Header (row 1) + values (from row 2), columns BS..BY. The rest stays unchanged.
openxlsx::writeData(workbook, sheet = sheet_name, x = all_metrics,
                    startCol = first_log_column, startRow = 1, colNames = TRUE)
openxlsx::saveWorkbook(workbook, xlsx_path, overwrite = TRUE)

cat("Done -> log columns written from BS onwards into", xlsx_path, "\n")
