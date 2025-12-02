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

library(dplyr)
library(tidyr)

games_team <- games %>%
  mutate(game_id = row_number()) %>%  # optional, just a unique id

  # Make two rows per game: one for home_team, one for away_team
  pivot_longer(
    cols      = c(home_team, away_team),
    names_to  = "home_away",
    values_to = "team"
  ) %>%

  # Stats from the *team's* point of view
  mutate(
    # yards / points for & against
    yds_for       = if_else(team == winner, yds_win,  yds_loss),
    yds_against   = if_else(team == winner, yds_loss, yds_win),
    pts_for       = if_else(team == winner, pts_win,  pts_loss),
    pts_against   = if_else(team == winner, pts_loss, pts_win),

    # result flags
    win      = as.integer(team == winner & is.na(tie)),
    loss     = as.integer(team != winner & is.na(tie)),
    tie_flag = as.integer(!is.na(tie))
  ) %>%
  arrange(team, year, week, date, time)

games_rolling <- games_team %>%
  group_by(team, year) %>%
  arrange(week, date, time, .by_group = TRUE) %>%
  mutate(
    games_played_prior = row_number() - 1L,

    # cumulative totals BEFORE this game
    cum_yds_for     = lag(cumsum(yds_for),     default = 0),
    cum_yds_against = lag(cumsum(yds_against), default = 0),
    cum_yds_diff    = cum_yds_for - cum_yds_against,

    cum_wins        = lag(cumsum(win),        default = 0),
    cum_losses      = lag(cumsum(loss),       default = 0),
    cum_ties        = lag(cumsum(tie_flag),   default = 0),
    
    cum_pts_for     = lag(cumsum(pts_for),     default = 0),
    cum_pts_against = lag(cumsum(pts_against), default = 0),
    cum_pts_diff    = cum_pts_for - cum_pts_against,

    # optional averages & win%
    avg_yds_for     = if_else(games_played_prior > 0,
                              cum_yds_for / games_played_prior, NA_real_),
    avg_yds_against = if_else(games_played_prior > 0,
                              cum_yds_against / games_played_prior, NA_real_),
    win_pct         = if_else(games_played_prior > 0,
                              cum_wins / games_played_prior, NA_real_)
  ) %>%
  ungroup()
