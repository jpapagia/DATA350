# ============================================================
# Figure 10: Sleep Duration vs Body Mass Index (BMI) by Gender
# Script: Figure 10.R
# Author: Alexandra Julka
# Modified: 2025-10-17
# ============================================================

library(dplyr)
library(ggplot2)
library(forcats)

NHANESraw <- read.csv("NHANESraw.csv")

df_bmi <- NHANESraw %>%
  select(SleepHrsNight, BMI_WHO, Gender) %>%
  filter(!is.na(SleepHrsNight), !is.na(BMI_WHO), !is.na(Gender)) %>%
  mutate(
    SleepHrsNight = as.numeric(SleepHrsNight),
    BMI_WHO = trimws(as.character(BMI_WHO)),
    # use two-line axis labels for clarity
    BMI_WHO = recode(
      BMI_WHO,
      "12.0_18.5"    = "Underweight\n(<18.5)",
      "18.5_to_24.9" = "Normal\n(18.5–24.9)",
      "25.0_to_29.9" = "Overweight\n(25–29.9)",
      "30.0_plus"    = "Obese\n(≥30)"
    ),
    BMI_WHO = factor(
      BMI_WHO,
      levels = c(
        "Underweight\n(<18.5)",
        "Normal\n(18.5–24.9)",
        "Overweight\n(25–29.9)",
        "Obese\n(≥30)"
      ),
      ordered = TRUE
    ),
    Gender = factor(tolower(Gender), levels = c("female","male"), labels = c("Female","Male"))
  ) %>%
  filter(SleepHrsNight >= 2, SleepHrsNight <= 12)

# Plot
fig5_clean <- ggplot(df_bmi, aes(x = BMI_WHO, y = SleepHrsNight, fill = Gender)) +
  geom_violin(trim = TRUE, alpha = 0.55, color = "grey40", width = 0.85) +
  geom_boxplot(width = 0.12, outlier.shape = 16, outlier.size = 1.2,
               fill = "white", color = "grey25") +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2.3,
               fill = "white", color = "black") +
  facet_wrap(~ Gender, nrow = 1) +
  scale_fill_manual(values = c("Female" = "#F4A7B9", "Male" = "#A7C7E7"), name = "Gender") +
  scale_y_continuous(breaks = 2:12, limits = c(2, 12)) +
  labs(
    title = "BMI vs Hours of Sleep",
    subtitle = "Violin = distribution; box = IQR; white dot = mean (NHANES 2009–2011)",
    x = "BMI Category (WHO Classification)",
    y = "Hours of Sleep per Night"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title.position = "plot",
    strip.text = element_text(face = "bold", size = 12),
    axis.text.x = element_text(size = 11, vjust = 1.2, lineheight = 1.1),
    axis.title.x = element_text(margin = margin(t = 10)),
    plot.margin = margin(10, 15, 15, 15)
  )

fig5_clean
