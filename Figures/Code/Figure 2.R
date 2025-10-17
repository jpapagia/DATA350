# ============================================================
# Figure 2: Sleep Duration vs. Days of Poor Mental Health (Past 30 Days) per Age Group
# Script: Figure 3(1).R
# Author: Yianni Papagiannopoulos
# Modified: 2025-10-16
# ============================================================

# Libraries
library(dplyr)
library(ggplot2)
library(gridExtra)

# Data
NHANESraw <- read.csv("NHANESraw.csv")

df <- NHANESraw %>%
  filter(Age >= 16, !is.na(SleepHrsNight), !is.na(DaysMentHlthBad)) %>%
  mutate(
    AgeGroup = cut(Age,
                   breaks = c(15, 24, 34, 44, 54, 64, 74, Inf),
                   labels = c("16–24","25–34","35–44","45–54","55–64","65–74","75+"),
                   right = TRUE
    ),
    DaysMentHlthBad = pmin(pmax(DaysMentHlthBad, 0), 30)
  )

# Spearman correlation per age group
stats_tbl <- df %>%
  group_by(AgeGroup) %>%
  summarise(
    n   = n(),
    rho = suppressWarnings(cor(SleepHrsNight, DaysMentHlthBad, method = "spearman")),
    p   = suppressWarnings(cor.test(SleepHrsNight, DaysMentHlthBad, method = "spearman")$p.value),
    .groups = "drop"
  ) %>%
  mutate(
    rho = round(rho, 2),
    p   = ifelse(is.na(p), NA_character_,
                 ifelse(p < 0.001, "< 0.001", sprintf("%.3f", p)))
  ) %>%
  rename(`Age Group` = AgeGroup, `ρ (Spearman)` = rho, `p-value` = p, `n` = n)

# Color palette
pal <- c("#1f77b4","#ff7f0e","#2ca02c","#d62728",
         "#9467bd","#8c564b","#17becf")

# Scatter plot with LOESS
p_scatter <- ggplot(df, aes(SleepHrsNight, DaysMentHlthBad, color = AgeGroup)) +
  geom_jitter(width = 0.05, height = 0.25, alpha = 0.18, size = 1) +
  geom_smooth(method = "loess", se = TRUE, linewidth = 0.9, alpha = 0.10) +
  scale_color_manual(values = pal, name = "Age Group") +
  scale_x_continuous(limits = c(2, 12), breaks = 2:12) +
  scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, 5)) +
  labs(
    title = "Sleep Duration vs. Days of Poor Mental Health (Past 30 Days)",
    subtitle = "Scatter with LOESS fits by age group; table below lists Spearman ρ, p-values, and sample sizes (NHANES 2009–2011)",
    x = "Sleep Hours per Night",
    y = "Days of Poor Mental Health (0–30)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title.position = "plot",
    legend.position = "right"
  )

# Create summary table (compact format)
tg <- tableGrob(
  stats_tbl,
  rows = NULL,
  theme = ttheme_minimal(
    core = list(fg_params = list(cex = 0.85)),
    colhead = list(fg_params = list(fontface = 2, cex = 0.9)),
    padding = unit(c(6, 3), "pt")
  )
)

# Stack plot + table vertically
grid.arrange(p_scatter, tg, ncol = 1, heights = c(3, 0.8))
