# =====================================================================
# Demographics / personal data: pie charts per condition (HRA vs. AI)
# Reads the same data source as Auswertung.R and builds a 3x2 panel:
#   rows    = characteristic (gender, age groups, AI usage frequency)
#   columns = condition (HRA/human, AI)
# -> output/demographics_pies.png
# Note: the recruitment source (Woehler/private) is exactly 10/10 in both
# conditions by stratification and is therefore not shown as a split here.
# =====================================================================
suppressMessages({library(readxl); library(dplyr)})

study_data <- as.data.frame(read_excel("data/Data_Input.xlsx", sheet = 1))
study_data$condition <- ifelse(study_data$condition == "HRC", "HRA", study_data$condition)
study_data$condition <- ifelse(study_data$condition == "KI", "AI", study_data$condition)
study_data <- study_data %>% mutate(id = trimws(as.character(id))) %>%
  filter(!is.na(id), id != "", id != "id", condition %in% c("HRA","AI"))

## Characteristics as factors with fixed levels (for consistent colours) -----

# Gender: translate the raw codes m/w/d to readable labels, fixed level order.
# dplyr::recode is qualified so it is not masked by car::recode (different
# signature) when Auswertung.R was run earlier in the same R session.
study_data$Gender   <- factor(dplyr::recode(study_data$gender, "m"="male", "w"="female", "d"="diverse"),
                              levels = c("male","female","diverse"))

# Age: bucket the numeric age into groups. cut() assigns each age to the
# interval it falls in; breaks c(-Inf,24,29,...) -> "< 25","25-29",...
study_data$AgeGroup <- cut(study_data$age, breaks = c(-Inf,24,29,34,39,Inf),
                           labels = c("< 25","25–29","30–34","35–39","≥ 40"))

# AI usage: ai_freq is coded 1..6. Look up each code in ai_usage_labels to get
# the label (ai_usage_labels["1"] -> "never", etc.); as.character() because the
# names are strings.
ai_usage_labels <- c("1"="never","2"="< monthly","3"="monthly","4"="weekly",
                     "5"="several/week","6"="daily")
study_data$AIuse <- factor(ai_usage_labels[as.character(study_data$ai_freq)],
                           levels = ai_usage_labels)

## Fixed colour mapping per category ------------------------------------
# The names must match the (English) factor levels above exactly, otherwise
# category_colours[names(counts)] returns NA -> invisible (white) pie slices.
gender_colours    <- c("male"="#5B8FB9","female"="#E5989B","diverse"="#9FB8AD")
age_colours       <- c("< 25"="#8ECAE6","25–29"="#219EBC","30–34"="#126782",
                       "35–39"="#FB8500","≥ 40"="#FFB703")
ai_usage_colours  <- c("never"="#D9ED92","< monthly"="#B5E48C","monthly"="#76C893",
                       "weekly"="#34A0A4","several/week"="#1A759F","daily"="#184E77")

## Helper: one labelled pie chart ---------------------------------------
# Pie on the left, category legend (with count + percentage) on the right, so
# nothing overlaps regardless of how small the slices are. label_cex controls
# the legend font size.
pie_by <- function(factor_values, category_colours, title, label_cex = 2.1) {
  counts        <- table(factor_values)          # count how often each category occurs
  counts        <- counts[counts > 0]            # keep only categories that appear
  slice_colours <- category_colours[names(counts)]  # each category's colour by its name
  proportions   <- as.numeric(counts) / sum(counts)
  percentages   <- round(100 * proportions)                               # share in percent
  legend_labels <- sprintf("%s: %d (%d%%)", names(counts), as.numeric(counts), percentages)

  # Slice boundaries, matching pie(init.angle = 90, clockwise = TRUE):
  # start at the top (pi/2) and go clockwise (angles decrease).
  boundaries <- pi/2 - 2 * pi * c(0, cumsum(proportions))
  radius     <- 1.0

  # Wide drawing area (asp = 1 keeps the pie circular): the pie sits on the left,
  # the legend fills the space on the right.
  plot.new()
  plot.window(xlim = c(-1.15, 3.6), ylim = c(-1.15, 1.15), asp = 1)
  title(main = title)

  # Draw each slice as a filled wedge
  for (i in seq_along(counts)) {
    arc <- seq(boundaries[i], boundaries[i + 1], length.out = 100)
    polygon(c(0, radius * cos(arc)), c(0, radius * sin(arc)),
            col = slice_colours[i], border = "white", lwd = 2)
  }

  # Legend to the right of the pie (vertically centred on the pie)
  legend(x = 1.15, y = 0, legend = legend_labels,
         fill = slice_colours, border = "white", bty = "n",
         cex = label_cex, y.intersp = 1.1, xjust = 0, yjust = 0.5)
}

## Export 3x2 panel -----------------------------------------------------
dir.create("output", showWarnings = FALSE)
png("output/demographics_pies.png", width = 2400, height = 2650, res = 200)
old_par <- par(mfrow = c(3, 2), mar = c(0.5, 0.5, 4, 0.5), family = "sans",
               cex.main = 2.3)
features <- list(
  list(variable="Gender",   colours=gender_colours,   display_name="Gender"),
  list(variable="AgeGroup", colours=age_colours,       display_name="Age groups"),
  list(variable="AIuse",    colours=ai_usage_colours,  display_name="AI usage frequency")
)
for (feature in features) {
  for (condition_name in c("HRA","AI")) {
    condition_label <- if (condition_name == "HRA") "HRA (human)" else "AI"
    pie_by(droplevels(study_data[[feature$variable]][study_data$condition == condition_name]),
           feature$colours, sprintf("%s — %s", feature$display_name, condition_label))
  }
}
par(old_par)
dev.off()

cat("Done -> output/demographics_pies.png\n")
