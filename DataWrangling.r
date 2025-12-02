# Libraries ---------------------------------------------------------------

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

# Data --------------------------------------------------------------------

attendance <- readr::read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2020/2020-02-04/attendance.csv"
)

standings <- readr::read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2020/2020-02-04/standings.csv"
)

games <- readr::read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2020/2020-02-04/games.csv"
)

# Clean attendance & standings (optional, not used later) -----------------

attendance_clean <- attendance %>%
  mutate(team = paste(team, team_name)) %>%  # combine into one column called 'team'
  select(-team_name)

Standings_clean <- standings %>%
  mutate(team = paste(team, team_name)) %>%  # combine into one column called 'team'
  select(-team_name)

# Team-game level data ----------------------------------------------------

games_team <- games %>%
  mutate(game_id = row_number()) %>%  # unique id per game

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

    # turnovers for & against ---------------------------------------------
    turnovers_for     = if_else(team == winner, turnovers_win,  turnovers_loss),
    turnovers_against = if_else(team == winner, turnovers_loss, turnovers_win),

    # result flags ---------------------------------------------------------
    win      = as.integer(team == winner & is.na(tie)),
    loss     = as.integer(team != winner & is.na(tie)),
    tie_flag = as.integer(!is.na(tie))
  ) %>%
  arrange(team, year, week, date, time)

# Rolling stats -----------------------------------------------------------

games_rolling <- games_team %>%
  group_by(team, year) %>%
  arrange(week, date, time, .by_group = TRUE) %>%
  mutate(
    games_played_prior = row_number() - 1L,

    # cumulative BEFORE this game
    cum_yds_for       = lag(cumsum(yds_for),       default = 0),
    cum_yds_against   = lag(cumsum(yds_against),   default = 0),
    cum_yds_diff      = cum_yds_for - cum_yds_against,

    cum_wins          = lag(cumsum(win),          default = 0),
    cum_losses        = lag(cumsum(loss),         default = 0),
    cum_ties          = lag(cumsum(tie_flag),     default = 0),

    cum_pts_for       = lag(cumsum(pts_for),       default = 0),
    cum_pts_against   = lag(cumsum(pts_against),   default = 0),
    cum_pts_diff      = cum_pts_for - cum_pts_against,

    cum_to_for        = lag(cumsum(turnovers_for),     default = 0),
    cum_to_against    = lag(cumsum(turnovers_against), default = 0),
    cum_to_diff       = cum_to_for - cum_to_against,

    # per-game averages ----------------------------------------------------
    avg_yds_for       = if_else(games_played_prior > 0,
                                cum_yds_for / games_played_prior, NA_real_),
    avg_yds_against   = if_else(games_played_prior > 0,
                                cum_yds_against / games_played_prior, NA_real_),
    avg_yds_diff      = avg_yds_for - avg_yds_against,

    avg_pts_for       = if_else(games_played_prior > 0,
                                cum_pts_for / games_played_prior, NA_real_),
    avg_pts_against   = if_else(games_played_prior > 0,
                                cum_pts_against / games_played_prior, NA_real_),
    avg_pts_diff      = avg_pts_for - avg_pts_against,

    avg_to_for        = if_else(games_played_prior > 0,
                                cum_to_for / games_played_prior, NA_real_),
    avg_to_against    = if_else(games_played_prior > 0,
                                cum_to_against / games_played_prior, NA_real_),
    avg_to_diff       = avg_to_for - avg_to_against,

    # home-field indicator -------------------------------------------------
    is_home           = as.integer(home_away == "home_team"),

    # season win % ---------------------------------------------------------
    win_pct           = if_else(games_played_prior > 0,
                                cum_wins / games_played_prior, NA_real_)
  ) %>%
  ungroup()


team_stats <- games_rolling %>%
  filter(games_played_prior >= 4) %>%          # only games with history
  select(
    game_id, team, home_away, win,
    win_pct,
    avg_pts_diff, avg_yds_diff, avg_to_diff,
    cum_pts_diff, cum_yds_diff, cum_to_diff
  )

# separate home and away teams --------------------------------------------
home_stats <- team_stats %>%
  filter(home_away == "home_team") %>%
  rename_with(~ paste0(.x, "_home"),
              c(team, home_away, win, win_pct,
                avg_pts_diff, avg_yds_diff, avg_to_diff,
                cum_pts_diff, cum_yds_diff, cum_to_diff))

away_stats <- team_stats %>%
  filter(home_away == "away_team") %>%
  rename_with(~ paste0(.x, "_away"),
              c(team, home_away, win, win_pct,
                avg_pts_diff, avg_yds_diff, avg_to_diff,
                cum_pts_diff, cum_yds_diff, cum_to_diff))

# one row per game: home vs away ------------------------------------------
games_model <- home_stats %>%
  inner_join(away_stats, by = "game_id") %>%
  mutate(
    # outcome we predict: did the home team win?
    win_home = win_home,

    # opponent-relative features (home minus away)
    delta_win_pct  = win_pct_home      - win_pct_away,
    delta_pts_diff = avg_pts_diff_home - avg_pts_diff_away,
    delta_yds_diff = avg_yds_diff_home - avg_yds_diff_away,
    delta_to_diff  = avg_to_diff_home  - avg_to_diff_away
  )
library(ranger)

rf_mod <- ranger(
  win_home ~ delta_win_pct + delta_pts_diff + delta_yds_diff + delta_to_diff,
  data = games_model,
  probability = TRUE,  # so you get win probabilities, not just classes
  num.trees = 500
)
