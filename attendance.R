
attendance_join <- attendance_clean %>% 
    mutate(week = as.character(week)) %>%     
    left_join(
        games_rolling %>%
            mutate(week = as.character(week)) %>%
            select(
                year, week, team,
                home_away,          # keep this
                win, win_pct, cum_wins, cum_pts_diff, cum_yds_diff
            ),
        by = c("team", "year", "week")
    )

attendance_home <- attendance_join %>%
    filter(home_away == "home_team")

View(attendance_home)


attendance_home %>% 
  ggplot(aes(x = win_pct, y = weekly_attendance)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "Win % before this game",
    y = "Weekly home attendance",
    title = "Relationship between team performance and home attendance"
  )
