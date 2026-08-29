# AR Repair Assistance User Study — Data Analysis (R)

Statistical analysis code for a Bachelor's thesis user study comparing two
remote-assistance modalities for **augmented-reality (AR) repair support on the
Microsoft HoloLens 2**:

- **HRA** — *Human Remote Assistance* (a real human expert answers)
- **AI** — an AI assistant answers

The study uses a **between-subjects design** (each participant experiences only
one condition). Participants repaired a device while wearing the headset and,
for each step, asked the assistant questions and received answers. The scripts
in this repository turn the collected questionnaire responses, interaction logs,
and interview codings into the descriptive statistics, hypothesis tests, and
figures reported in the thesis.

> **Note:** In the raw Excel file the conditions are stored in German as `HRC`
> (human) and `KI` (AI). The scripts automatically relabel these to `HRA` and
> `AI` for every dataset, table, and plot.

---

## What the study measures

| Area | Instrument | Score range |
|------|-----------|-------------|
| Usability | SUS (System Usability Scale) | 0–100 |
| Cognitive load | NASA Raw-TLX | 0–100 |
| Trust | Trust in Automation (TAI) | 1–7 |
| Performance expectancy / behavioral intention | UTAUT-style items | 1–7 |
| Affinity for technology | ATI scale | 1–6 |
| Perceived cues / AR-specific items | custom items | 1–7 |
| Blinding | "AI vs. human" guess (slider + forced choice) | — |
| Objective behavior | session interaction logs | seconds, counts |
| Open feedback | semi-structured interviews (Q1–Q5) | thematic codes |

**Hypotheses (H1–H5)** are the confirmatory family (efficiency, effectiveness,
usability, workload, trust). **H6a/H6b** test whether participants could tell
the AI apart from a human ("blinding"). Additional analyses are exploratory.

---

## Repository layout

```
RStudio.Data.Analysis/
├── Analysis.R          # Main analysis: scales, reliability, group comparison, plots
├── Demographics.R      # Demographic pie charts per condition
├── Interviews.R        # Thematic frequency analysis of interviews (Q1–Q5)
├── Log_Analysis.R      # Parses session logs, writes objective metrics into the Excel
├── data/
│   └── Data_Input.xlsx # Single source of truth: all questionnaire + interview data
├── metrics/
│   └── session_<id>.jsonl   # One interaction log per participant (raw events)
└── output/             # Generated CSV tables and PNG figures (created by the scripts)
```

Everything reads from a single Excel workbook, `data/Data_Input.xlsx` (sheet 1).
Each participant row is identified by an `id` in column A.

---

## The four scripts

### 1. `Log_Analysis.R` — run this first
Reads every `metrics/session_<id>.jsonl` file, computes objective per-session
metrics, and writes them **directly back into `data/Data_Input.xlsx`** (columns
`BS` onward), preserving existing content and formatting.

The `metrics/session_<id>.jsonl` files are produced by the **TCP Messaging
Server** (`Bachelor-Tcp.Messaging.Server`), which logs each request/response
event during a session. `Log_Analysis.R` consumes these server metrics files
directly — simply drop the messaging server's `metrics/` output into this
repository's `metrics/` folder before running the script.

Metrics computed per session:
- repair time (minutes, first→last timestamp)
- mean / max / min response latency (seconds)
- number of requests (interaction steps)
- mean question length and answer length

Each JSONL line is one event (`request` or `response`) with a timestamp,
duration, and text lengths — see the sample below.

### 2. `Analysis.R` — the main analysis
- Loads the data, applies reverse-coding, and computes scale scores per person
  (SUS, TLX, Trust, PerfExp, BehInt, Cues, ATI).
- **Reliability:** Cronbach's α for each scale.
- **Descriptives:** median [Q1, Q3], IQR, mean, SD per condition.
- **Covariate balance:** checks the two groups are comparable (Mann–Whitney /
  Fisher's exact tests).
- **Group comparison (H1–H5):** non-parametric **Mann–Whitney U** tests, effect
  size **Cliff's delta** with bootstrapped 95% confidence intervals, and a
  **Holm correction** over the confirmatory family.
- **Blinding (H6a):** one-sample Wilcoxon of the guess slider against the neutral
  point, plus a binomial test of the forced choice against chance.
- Exports all result tables to `output/*.csv` and boxplots to `output/*.png`.

### 3. `Demographics.R`
Builds a 3×2 panel of pie charts (gender, age groups, AI-usage frequency ×
condition) → `output/demographics_pies.png`.

### 4. `Interviews.R`
Question-wise thematic frequency analysis of the interviews (Q1–Q5). For each
guiding question it counts how often each theme was mentioned per condition and
exports tables (`output/interview_Q*.csv`) and grouped bar charts
(`output/interview_Q*.png`). This part is **descriptive only** — no inferential
statistics, by design (small, exploratory, self-formed categories).

---

## Requirements

- **R** ≥ 4.1 (developed in RStudio)
- R packages (installed automatically by the scripts if missing):
  `tidyverse`, `readxl`, `openxlsx`, `jsonlite`, `psych`, `effsize`, `car`,
  `rstatix`, `janitor`, `readr`, `ggplot2`, `tidyr`

---

## How to run

1. Open the project folder in RStudio.
2. Set the working directory to this folder
   (*Session → Set Working Directory → To Source File Location*).
3. Run the scripts in this order:

   ```r
   source("Log_Analysis.R")   # 1. write objective log metrics into the Excel
   source("Analysis.R")       # 2. main statistical analysis + plots
   source("Demographics.R")   # 3. demographic charts
   source("Interviews.R")     # 4. interview theme analysis
   ```

4. Find all results in the `output/` folder.

> **Excel format tip:** if `read_excel` fails, re-save the workbook in Excel as
> **"Excel Workbook (\*.xlsx)"** — *not* "Strict Open XML Spreadsheet".

---

## Sample log format (`metrics/session_<id>.jsonl`)

```json
{"ts":"2026-08-05T12:50:34.881Z","event":"request","assistanceMode":"HumanAssistance","questionLength":77,"hasImage":true}
{"ts":"2026-08-05T12:50:49.742Z","event":"response","assistanceMode":"HumanAssistance","durationSeconds":14.861,"answerLength":136}
```

Timestamps are ISO-8601 in UTC. Only `request` and `response` events are used;
malformed lines are skipped.

---

## Notes on data privacy

The interaction logs and questionnaire responses come from human study
participants. If you reuse this repository, ensure any real participant data is
anonymized and handled according to the relevant ethics approval and data-
protection rules.

---

## Reproducibility

`Analysis.R` sets `set.seed(2026)` so the bootstrap confidence intervals for
Cliff's delta are reproducible across runs.

---

_This README was created with the support of AI (Claude, based on the contents of the
R scripts in this repository) — not exclusively AI-generated — and was reviewed by the
author. Please check it for accuracy against the actual thesis before citing or
distributing._
