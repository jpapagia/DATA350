# ============================================================
# Figure 4: Average Sleep Duration by Lifestyle Choice (Alcohol, Tobacco)
# Script: Figure 4.R
# Author: Yianni Papagiannopoulos
# Modified: 2025-10-16
# ============================================================

library(dplyr)
library(ggplot2)

# Load and process 
NHANESraw <- read.csv("NHANESraw.csv")

# Adults 21+ with needed fields; build 4 lifestyle groups
dat <- NHANESraw %>%
  filter(Age >= 21, !is.na(SleepHrsNight),
         !is.na(SmokeNow), !is.na(Alcohol12PlusYr)) %>%
  mutate(
    Lifestyle = case_when(
      SmokeNow == "Yes" & Alcohol12PlusYr == "Yes" ~ "Smoker + Drinker",
      SmokeNow == "Yes" & Alcohol12PlusYr == "No"  ~ "Smoker Only",
      SmokeNow == "No"  & Alcohol12PlusYr == "Yes" ~ "Drinker Only",
      TRUE                                          ~ "Neither"
    ),
    Lifestyle = factor(Lifestyle, levels = c("Neither","Smoker Only","Drinker Only","Smoker + Drinker"))
  )

# Means per lifestyle for vertical lines
means <- dat %>%
  group_by(Lifestyle) %>%
  summarise(mean_sleep = mean(SleepHrsNight, na.rm = TRUE), .groups = "drop")

# Distinct colors
pal <- c("Neither"="#4C78A8", "Smoker Only"="#F58518",
         "Drinker Only"="#54A24B", "Smoker + Drinker"="#B279A2")

ggplot(dat, aes(x = SleepHrsNight, color = Lifestyle)) +
  geom_density(linewidth = 1.1, adjust = 1.0, show.legend = TRUE) +
  # mean lines, color-matched
  geom_vline(data = means, aes(xintercept = mean_sleep, color = Lifestyle),
             linetype = "solid", linewidth = 0.7, show.legend = FALSE) +
  scale_color_manual(values = pal, name = "Lifestyle") +
  scale_x_continuous(limits = c(2, 12), breaks = 2:12, expand = expansion(mult = c(0, 0.02))) +
  labs(
    title = "Sleep Duration Distribution by Lifestyle (Adults 21+)",
    subtitle = "Overlaid kernel density curves; vertical lines mark group means",
    x = "Sleep Hours per Night", y = "Density"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title.position = "plot"
  )

