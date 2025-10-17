# ============================================================
# Z-test for proportion difference (Female vs Male) by Age Group
# ============================================================

z_results <- gender_age %>%
  tidyr::pivot_wider(
    id_cols = AgeGroup,
    names_from = Gender,
    values_from = c(n_yes, n_tot, p_hat)
  ) %>%
  rowwise() %>%
  mutate(
    # pooled proportion
    p_pool = (n_yes_female + n_yes_male) / (n_tot_female + n_tot_male),
    # standard error for difference in proportions
    se_diff = sqrt(p_pool * (1 - p_pool) * (1 / n_tot_female + 1 / n_tot_male)),
    # z statistic
    z = (p_hat_female - p_hat_male) / se_diff,
    # one-sided p-value (female > male)
    p_value = 1 - pnorm(z)
  ) %>%
  mutate(sig = ifelse(p_value < 0.05, "Yes", "No")) %>%
  dplyr::select(AgeGroup, p_hat_female, p_hat_male, z, p_value, sig)

print(z_results, digits = 4)
