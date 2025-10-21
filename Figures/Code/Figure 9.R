# ============================================================
# Figure 9: Reported Trouble Sleeping by Age Group (16+) & by Race
# Script: Figure 9.R
# Author: Madhavan Narkeeran, Yianni Papagiannopoulos
# Modified: 2025-10-16
# ============================================================

library(dplyr)
library(ggplot2)
library(forcats)
library(scales)
library(stringr)

# Load and process 
NHANESraw <- read.csv("NHANESraw.csv")

# Keep 16+ and needed vars; drop NA SleepTrouble
base <- NHANESraw %>%
  filter(Age >= 16, !is.na(SleepTrouble)) %>%
  mutate(
    SleepTrouble = factor(SleepTrouble, levels = c("No","Yes")),
    AgeGroup = cut(
      Age,
      breaks = c(15, 24, 34, 44, 54, 64, 74, Inf),
      labels = c("16–24","25–34","35–44","45–54","55–64","65–74","75+"),
      right = TRUE
    ),
    # --- MERGE Mexican into Hispanic ---
    Race1 = trimws(as.character(Race1)),
    RaceMerged = case_when(
      str_detect(tolower(Race1), "mex")  ~ "Hispanic",
      str_detect(tolower(Race1), "hisp") ~ "Hispanic",
      TRUE                               ~ Race1
    ),
    RaceMerged = fct_drop(RaceMerged)
  )

# 95% Wald CI helper
add_prop_ci <- function(df, num, den) {
  df %>%
    mutate(
      p_hat = {{num}} / {{den}},
      se    = sqrt(p_hat * (1 - p_hat) / {{den}}),
      lo    = pmax(0, p_hat - 1.96 * se),
      hi    = pmin(1, p_hat + 1.96 * se)
    )
}

# Summarize by RaceMerged x AgeGroup (across all genders)
race_age <- base %>%
  filter(!is.na(RaceMerged)) %>%
  group_by(AgeGroup, RaceMerged) %>%
  summarise(
    n_yes = sum(SleepTrouble == "Yes"),
    n_tot = n(),
    .groups = "drop"
  ) %>%
  add_prop_ci(num = n_yes, den = n_tot)

# Order races by overall prevalence (avg across ages) for stable legend
race_levels <- race_age %>%
  group_by(RaceMerged) %>% summarise(avg = mean(p_hat), .groups = "drop") %>%
  arrange(desc(avg)) %>% pull(RaceMerged)

race_age <- race_age %>% mutate(RaceMerged = factor(RaceMerged, levels = race_levels))

pd <- position_dodge(width = 0.75)

ggplot(race_age, aes(x = AgeGroup, y = p_hat, fill = RaceMerged)) +
  geom_col(position = pd, width = 0.65, color = "gray35") +
  geom_errorbar(aes(ymin = lo, ymax = hi), position = pd, width = 0.24, color = "gray35") +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_fill_manual(
    name = "Race / Ethnicity",
    values = c(
      "White"     = "#34eda6",  # light bluish
      "Black"     = "#82ed40",  # soft green
      "Hispanic"  = "#e89df5",  # light purple
      "Asian"     = "#f6c177",  # soft yellow/orange
      "Other"     = "#fa776e"   # light red/pink
    )
  ) +
  labs(
    title = "Reported Trouble Sleeping by Age Group (16+): Race/Ethnicity",
    subtitle = "Mexican merged into Hispanic. Bars show % reporting trouble sleeping; 95% CIs (NHANES 2009–2011)",
    x = "Age Group",
    y = "Proportion of Group"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    plot.title.position = "plot"
  )
