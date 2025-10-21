# ============================================================
# Figure 2. Reported Sleep Trouble by Self-Reported General Health
# Script: Figure 2.R
# Author: Madhavan Narkeeran, Yianni Papagiannopoulos
# Modified: 2025-10-17
# ============================================================

library(dplyr)
library(ggplot2)
library(scales)

# Load and process 
NHANESraw <- read.csv("NHANESraw.csv")

# Normalize HealthGen values
df_health <- NHANESraw %>%
  mutate(
    HealthGen = trimws(HealthGen),
    HealthGen = dplyr::recode(
      HealthGen,
      "Vgood" = "Very good",
      "Verygood" = "Very good",
      "very good" = "Very good"
    ),
    HealthGen = factor(
      HealthGen,
      levels = c("Excellent", "Very good", "Good", "Fair", "Poor"),
      ordered = TRUE
    ),
    SleepTrouble = factor(SleepTrouble, levels = c("No", "Yes"))
  ) %>%
  filter(!is.na(HealthGen), !is.na(SleepTrouble))

# Summaries (prop + Wald 95% CI) ---
sum_health <- df_health %>%
  group_by(HealthGen) %>%
  summarise(
    n_yes   = sum(SleepTrouble == "Yes"),
    n_total = n(),
    prop_yes = n_yes / n_total,
    se = sqrt(prop_yes * (1 - prop_yes) / n_total),
    lo = pmax(0, prop_yes - 1.96 * se),
    hi = pmin(1, prop_yes + 1.96 * se),
    .groups = "drop"
  ) %>%
  arrange(HealthGen)

# Cochran–Armitage trend test
scores <- seq_len(nrow(sum_health))
trend  <- prop.trend.test(x = sum_health$n_yes, n = sum_health$n_total, score = scores)
trend_p <- if (trend$p.value < 0.001) "p < 0.001" else sprintf("p = %.3f", trend$p.value)

fills <- c("#DDEAF7", "#C6DDF2", "#AFCFED", "#8DBAE5", "#6AA5DD")

ggplot(sum_health, aes(x = HealthGen, y = prop_yes, fill = HealthGen)) +
  geom_col(width = 0.55, color = NA) +
  geom_errorbar(aes(ymin = lo, ymax = hi),
                width = 0.1, color = "grey30", linewidth = 0.6) +
  # percentages slightly above each bar
  geom_text(
    aes(label = percent(prop_yes, accuracy = 1)),
    vjust = -3,
    size = 3.6,
    fontface = "bold",
    color = "grey20"
  ) +
  scale_fill_manual(values = c("#DDEAF7", "#C6DDF2", "#AFCFED", "#8DBAE5", "#6AA5DD"), guide = "none") +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    breaks = seq(0, 0.6, 0.1),
    expand = expansion(mult = c(0, 0.05))
  ) +
  coord_cartesian(ylim = c(0, 0.60), clip = "off") +   # ← decreased to 60%
  labs(
    title = "Reported Sleep Trouble by Self-Reported General Health (NHANES 2009–2011)",
    subtitle = paste0("Bars show % reporting sleep trouble; 95% CIs. Cochran–Armitage trend test: ", trend_p),
    x = "Self-Reported General Health",
    y = "Percent Reporting Sleep Trouble"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey88"),
    plot.title.position = "plot",
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1, margin = margin(t = 6)),
    plot.margin = margin(8, 20, 12, 12)
  )
