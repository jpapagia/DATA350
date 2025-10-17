# ============================================================
# Figure 6: Reported Physical Activity vs. Sleep Duration Boxplot
# Script: Figure 9.R
# Author: Yianni Papagiannopoulos
# Modified: 2025-10-15
# ============================================================

library(dplyr)
library(ggplot2)

NHANESraw <- read.csv("NHANESraw.csv")

plot_data <- NHANESraw %>%
  filter(!is.na(SleepHrsNight), !is.na(PhysActive)) %>%
  mutate(
    PhysActive = factor(
      PhysActive,
      levels = c("No", "Yes"),
      labels = c("Not Physically Active", "Physically Active")
    )
  )

group_means <- plot_data %>%
  group_by(PhysActive) %>%
  summarise(mean_sleep = mean(SleepHrsNight, na.rm = TRUE))

ggplot(plot_data, aes(x = PhysActive, y = SleepHrsNight, fill = PhysActive)) +
  geom_boxplot(alpha = 0.7, width = 0.5, outlier.shape = NA, color = "gray40") +
  geom_jitter(width = 0.15, alpha = 0.3, color = "gray25", size = 1.3) +
  geom_segment(
    data = group_means,
    aes(x = as.numeric(PhysActive) - 0.25, xend = as.numeric(PhysActive) + 0.25,
        y = mean_sleep, yend = mean_sleep),
    color = "red3", linewidth = 1.2
  ) +
  scale_fill_manual(values = c("gray80", "#2b6cb0")) +
  scale_y_continuous(limits = c(2, 12.5), breaks = seq(2, 12, 1)) +
  labs(
    title = "Reported Physical Activity vs. Sleep Duration",
    x = NULL,
    y = "Sleep Hours per Night"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title.position = "plot"
  )

