# ============================================================
# Test for Figure 4
# ============================================================


library(dplyr)
library(broom)

stats_table <- df_bp %>%
  group_by(BP_type) %>%
  summarise(
    n = n(),
    mean_BP = mean(BP_value, na.rm = TRUE),
    mean_sleep = mean(SleepHrsNight, na.rm = TRUE),
    pearson_r = cor(BP_value, SleepHrsNight, use = "complete.obs", method = "pearson"),
    spearman_rho = cor(BP_value, SleepHrsNight, use = "complete.obs", method = "spearman"),
    lm_p = glance(lm(SleepHrsNight ~ BP_value))$p.value,
    r_squared = glance(lm(SleepHrsNight ~ BP_value))$r.squared,
    slope_hours_per_10mmHg = coef(lm(SleepHrsNight ~ BP_value))[["BP_value"]] * 10
  ) %>%
  mutate(across(where(is.numeric), round, 3))

print(stats_table)
