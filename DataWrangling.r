library(readr)  
library(dplyr)
attendance <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2020/2020-02-04/attendance.csv')
standings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2020/2020-02-04/standings.csv')
games <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2020/2020-02-04/games.csv')

attendance_clean <- attendance %>%
  mutate(team = paste(team, team_name)) %>%  # combine into one column called 'team'
  select(-team_name)                         # drop the old team_name column
attendance_clean


Standings_clean <- standings %>%
  mutate(team = paste(team, team_name)) %>%  # combine into one column called 'team'
  select(-team_name)                         # drop the old team_name column
Standings_clean

#point differentials 
games_diff <- games %>%
  mutate(
    point_diff = pts_win - pts_loss,
    yard_diff  = yds_win - yds_loss,
    to_diff    = turnovers_loss - turnovers_win   # + = winner protected ball better
  ) %>%
  select(point_diff, yard_diff, to_diff)
