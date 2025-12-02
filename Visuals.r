# Long-format: one column for feature name, one for value
feature_long <- games_model %>%
  select(win_home, delta_win_pct, delta_pts_diff, delta_yds_diff, delta_to_diff) %>%
  pivot_longer(
    cols = c(delta_win_pct, delta_pts_diff, delta_yds_diff, delta_to_diff),
    names_to = "feature",
    values_to = "value"
  )

ggplot(feature_long, aes(x = value, y = win_home)) +
  geom_jitter(height = 0.05, width = 0, alpha = 0.1) +
  geom_smooth(
    method = "glm",
    method.args = list(family = "binomial"),
    se = FALSE
  ) +
  facet_wrap(~ feature, scales = "free_x") +
  labs(
    x = "Feature value (home - away)",
    y = "Probability home team wins",
    title = "How each feature relates to home win probability"
  )
