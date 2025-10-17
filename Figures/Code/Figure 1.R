# ============================================================
# Figure 1: Average Sleep Duration by Age and Reported Difficulty
# Script: Figure 1.R
# Author: Yianni Papagiannopoulos
# Modified: 2025-10-16
# ============================================================

# Libraries
library(dplyr)
library(ggplot2)

# Load and process 
NHANESraw <- read.csv("NHANESraw.csv")

# Filter and prepare
sleep_age <- NHANESraw %>%
  filter(Age >= 16, !is.na(SleepHrsNight), !is.na(SleepTrouble)) %>%
  mutate(
    SleepTrouble = factor(SleepTrouble, levels = c("No","Yes")),
    AgeGroup = cut(Age,
                   breaks = c(15, 24, 34, 44, 54, 64, 74, Inf),
                   labels = c("16–24","25–34","35–44","45–54","55–64","65–74","75+"),
                   right = TRUE
    )
  ) %>%
  group_by(AgeGroup, SleepTrouble) %>%
  summarise(
    mean_sleep = mean(SleepHrsNight, na.rm = TRUE),
    se = sd(SleepHrsNight, na.rm = TRUE) / sqrt(n()),
    lo = mean_sleep - 1.96 * se,
    hi = mean_sleep + 1.96 * se,
    .groups = "drop"
  )

# Plot
ggplot(sleep_age, aes(x = AgeGroup, y = mean_sleep, color = SleepTrouble, group = SleepTrouble)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.8) +
  geom_ribbon(aes(ymin = lo, ymax = hi, fill = SleepTrouble), alpha = 0.15, color = NA) +
  scale_color_manual(values = c("No" = "#64B5F6", "Yes" = "#F48FB1")) +
  scale_fill_manual(values = c("No" = "#64B5F6", "Yes" = "#F48FB1")) +
  scale_y_continuous(
    name = "Average Sleep Hours per Night",
    breaks = seq(5.5, 8, 0.5),
    labels = function(x) sprintf("%.1f", x)
  ) +
  labs(
    title = "Average Sleep Duration by Age and Reported Difficulty Sleeping",
    subtitle = "Mean nightly sleep hours (±95% CI) by age group and reported sleep trouble (NHANES 2009–2011)",
    x = "Age Group",
    color = "Trouble Sleeping",
    fill = "Trouble Sleeping"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    plot.title.position = "plot"
  )
