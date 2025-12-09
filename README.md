# Predicting NFL Winners and Attendance

This project analyzes NFL attendance data and builds rolling performance metrics to explore how team level trends relate to the probability of winning the next game. The repository contains data cleaning, feature engineering, and visualization steps used to uncover game level and season level patterns.

## Overview

The goal of this project is to analyze how cumulative turnover differential, cumulative point differential, and cumulative yards differential relate to a team’s likelihood of winning its next NFL game. The data used in this project comes from publicly available NFL attendance, standings, and game level datasets found on GitHub. These files are merged into a team game panel and used to construct rolling performance metrics such as cumulative turnover differential, cumulative point differential, and cumulative yards differential. One interesting finding from our exploratory analysis is that teams with strong rolling point differential even if their overall record is mediocre show a noticeably higher chance of winning the following game.


## Current Plan

Our current plan is to refine the rolling metrics, build additional predictive features, and evaluate which indicators are most strongly associated with next-game outcomes.

## Repo Structure

1. datawrangling – Contains cleaned datasets and any derived files created during the feature engineering process.

2. attendance, visuals – creates images created with ggplot2, including rolling metric trends and win probability visuals.

3. README & plan documents   -Provide project context, goals, workflow explanations, and future steps.
Together, these folders give a clear path from raw data to processed metrics and final visual insight

## Authors
Harvey Barnes hab5455@psu.edu
Noah Yasbin nby5102@psu.edu
