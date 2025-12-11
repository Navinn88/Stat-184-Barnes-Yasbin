# Overall home win rate
games_model %>% 
  summarise(
    n_games       = n(),
    home_win_rate = mean(win_home, na.rm = TRUE)
  )
home_by_year <- games_model %>% 
  group_by(year) %>% 
  summarise(
    n_games       = n(),
    home_win_rate = mean(win_home, na.rm = TRUE)
  )

home_by_year

ggplot(home_by_year, aes(x = year, y = home_win_rate)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Home Win Rate by Season",
    x = "Season",
    y = "Home win rate"
  )
