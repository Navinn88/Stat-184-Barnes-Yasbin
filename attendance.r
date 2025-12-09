home_attendance_model <- games_model %>%
  # keep just home-side features + year/week for join
  select(
    game_id, year, week,
    team_home, win_home,
    win_pct_home,
    avg_pts_diff_home, avg_yds_diff_home, avg_to_diff_home
  ) %>%
  inner_join(
    attendance_clean,
    by = c("team_home" = "team", "year" = "year", "week" = "week")
  )
ggplot(home_attendance_model,
       aes(x = win_pct_home, y = weekly_attendance)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "Home team win % (prior games)",
    y = "Weekly home attendance",
    title = "Relationship between home win% and attendance"
  )
ggplot(home_attendance_model,
       aes(x = avg_pts_diff_home, y = weekly_attendance)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "Avg point differential (prior games)",
    y = "Weekly home attendance",
    title = "Team strength vs attendance"
  )

mod_attendance <- lm(
  weekly_attendance ~ win_pct_home + avg_pts_diff_home +
    avg_yds_diff_home + avg_to_diff_home,
  data = home_attendance_model
)

summary(mod_attendance)

