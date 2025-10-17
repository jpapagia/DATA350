# ============================================================
# Figure 3.2: Reported Trouble Sleeping by Age Group (16+) & by Gender
# Script: Figure 3(2).R
# Author: Madhavan Narkeeran, Yianni Papagiannopoulos
# Modified: 2025-10-16
# ============================================================

library(dplyr)
library(ggplot2)
library(forcats)
library(scales)

# Load and process 
NHANESraw <- read.csv("NHANESraw.csv")

# Keep 16+ and needed vars; drop NA SleepTrouble as requested
base <- NHANESraw %>%
  filter(Age >= 16, !is.na(SleepTrouble)) %>%
  mutate(
    SleepTrouble = factor(SleepTrouble, levels = c("No","Yes")),
    AgeGroup = cut(Age,
                   breaks = c(15, 24, 34, 44, 54, 64, 74, Inf),
                   labels = c("16–24","25–34","35–44","45–54","55–64","65–74","75+"),
                   right = TRUE
    )
  )

# small helper for normal-approx 95% CI (good for EDA)
add_prop_ci <- function(df, num, den) {
  df %>%
    mutate(
      p_hat = {{num}} / {{den}},
      se    = sqrt(p_hat * (1 - p_hat) / {{den}}),
      lo    = pmax(0, p_hat - 1.96 * se),
      hi    = pmin(1, p_hat + 1.96 * se)
    )
}

# Uses `base` and add_prop_ci() you already defined
gender_age <- base %>%
  filter(!is.na(Gender)) %>%
  mutate(Gender = factor(Gender, levels = c("female","male"))) %>%
  group_by(AgeGroup, Gender) %>%
  summarise(
    n_yes = sum(SleepTrouble == "Yes"),
    n_tot = n(),
    .groups = "drop"
  ) %>%
  add_prop_ci(num = n_yes, den = n_tot)

pd <- position_dodge(width = 0.7)

ggplot(gender_age, aes(x = AgeGroup, y = p_hat, fill = Gender)) +
  geom_col(position = pd, width = 0.6, color = "gray35") +
  geom_errorbar(aes(ymin = lo, ymax = hi), position = pd, width = 0.22, color = "gray35") +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_fill_manual(
    values = c("female" = "#F4A7B9", "male" = "#A7C7E7"),
    labels = c("Female", "Male"),
    name = NULL
  ) +
  labs(
    title = "Reported Trouble Sleeping by Age Group (16+): Female vs Male",
    subtitle = "Bars show % reporting trouble sleeping; error bars are 95% CIs (NHANES 2009–2011)",
    x = "Age Group",
    y = "Proportion of Group"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    plot.title.position = "plot"
  )
