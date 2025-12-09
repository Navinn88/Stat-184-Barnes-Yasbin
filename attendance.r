games_att_long <- games_att %>%
  select(
    home_attendance,
    win_pct_home,
    avg_pts_diff_home,
    avg_yds_diff_home,
    avg_to_diff_home,
    delta_win_pct,
    delta_pts_diff,
    delta_yds_diff,
    delta_to_diff
  ) %>%
  pivot_longer(
    -home_attendance,
    names_to = "metric",
    values_to = "value"
  )

ggplot(games_att_long, aes(value, home_attendance)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ metric, scales = "free_x") +
  labs(
    title = "Relationships Between Team Strength Metrics and Home Attendance",
    x = "Metric value",
    y = "Home attendance"
  )
