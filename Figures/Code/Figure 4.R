# ============================================================
# Figure 4: Average Sleep Hours vs Diastolic and Systolic
# Script: Figure 3(1).R
# Author: Alexandra Julka, Yianni Papagiannopoulos
# Modified: 2025-10-17
# ============================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)
library(scales)

NHANESraw <- read.csv("NHANESraw.csv")

df_bp <- NHANESraw %>%
  transmute(
    SleepHrsNight = suppressWarnings(as.numeric(SleepHrsNight)),
    BPSysAve, BPDiaAve, Age
  ) %>%
  filter(
    Age >= 16,
    between(SleepHrsNight, 2, 12),
    !is.na(BPSysAve),  between(BPSysAve, 70, 250),
    !is.na(BPDiaAve),  between(BPDiaAve, 40, 150)
  ) %>%
  pivot_longer(
    c(BPDiaAve, BPSysAve),
    names_to = "BP_type",
    values_to = "BP_value"
  ) %>%
  mutate(
    BP_type = recode(BP_type,
                     BPDiaAve = "Diastolic (mmHg)",
                     BPSysAve = "Systolic (mmHg)")
  )

#Plot
ggplot(df_bp, aes(x = BP_value, y = SleepHrsNight,
                  color = BP_type)) +
  geom_point(alpha = 0.20, size = 1.6, shape = 16) +
  geom_smooth(method = "lm", se = TRUE, color = "#377EB8", linewidth = 1.1) +
  facet_wrap(~ BP_type, nrow = 1, scales = "free_x") +
  scale_color_manual(values = c("Diastolic (mmHg)" = "#d62728",   # red-orange
                                "Systolic (mmHg)"  = "#1B9E77")) + # green
  labs(
    title = "Sleep Hours vs. Blood Pressure (Adults 16+)",
    subtitle = "OLS regression fits (blue) with 95% CIs; NHANES 2009–2011, unweighted",
    x = "Blood Pressure (mmHg)",
    y = "Average Sleep Hours per Night",
    color = "Blood Pressure Type"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title.position = "plot",
    legend.position = "top"
  )
