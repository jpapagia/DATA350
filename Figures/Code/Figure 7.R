# ============================================================
# Figure 7: Daily Screen Time (Computer + TV) vs. Sleep Duration
# Script: Figure 7.R
# Author: Yianni Papagiannopoulos
# Modified: 2025-10-15
# ============================================================

library(dplyr)
library(ggplot2)
library(stringr)

# Load and process 
NHANESraw <- read.csv("NHANESraw.csv")

# Helper functions
`%||%` <- function(a, b) if (is.null(a)) b else a

to_hours <- function(x) {
  x_chr <- tolower(trimws(as.character(x)))
  bad <- c("^$", "^don.?t know$", "^refused$", "^do not watch", "^do not use", "^none$")
  x_chr[grepl(paste(bad, collapse="|"), x_chr)] <- NA
  m <- str_match(x_chr, "(\\d+(?:\\.\\d+)?)\\s*(?:-|–|—|to)?\\s*(\\d+(?:\\.\\d+)?)?")
  n1 <- suppressWarnings(as.numeric(m[,2]))
  n2 <- suppressWarnings(as.numeric(m[,3]))
  plus <- str_detect(x_chr %||% "", "\\+")
  ifelse(!is.na(n1) & !is.na(n2), (n1+n2)/2,
         ifelse(!is.na(n1) & plus, n1+1,
                ifelse(!is.na(n1), n1, NA_real_)))
}

plot_data <- NHANESraw %>%
  mutate(
    TV_hrs_adult   = to_hours(TVHrsDay),
    Comp_hrs_adult = to_hours(CompHrsDay),
    TotalScreenTime = coalesce(TV_hrs_adult, 0) + coalesce(Comp_hrs_adult, 0)
  ) %>%
  filter(!is.na(SleepHrsNight), TotalScreenTime > 0, Age >= 13)

# Fit for annotation
fit <- lm(SleepHrsNight ~ TotalScreenTime, data = plot_data)
slope <- coef(fit)[2]
r2 <- summary(fit)$r.squared

# Final plot
ggplot(plot_data, aes(x = TotalScreenTime, y = SleepHrsNight)) +
  stat_bin_2d(bins = 25, aes(fill = after_stat(count / sum(count)))) +
  scale_fill_gradient(
    low = "grey95", high = "#2b6cb0",
    labels = scales::percent_format(accuracy = 1),
    name = "Proportion of sample"
  ) +
  geom_smooth(method = "lm", se = TRUE, color = "#2b6cb0", linewidth = 0.8) +
  scale_x_continuous(limits = c(0, 10), breaks = 0:10) +
  scale_y_continuous(limits = c(2, 12.5), breaks = seq(2, 12, 1)) +
  labs(
    title = "Daily Screen Time vs. Sleep Duration",
    x = "Total Screen Time (hours/day)",
    y = "Sleep Hours per Night",
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.caption = element_text(hjust = 0, size = 10, color = "grey30"),
    plot.title.position = "plot",
    legend.position = "right",
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA)
  )
