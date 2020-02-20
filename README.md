# ncaahoopR_data
Data pulled from [ncaahoopR](https://github.com/lbenz730/ncaahoopR)

Schedules, Rosters, Box Scores, and Play by Play logs already scraped from ncaahoopR package.

Schedules and Rosters are in of the form of __season/schedule/team_schedue.csv__ and __season/roster/team_roster.csv__. Box Scores are in the form __season/team/game_id.csv__, which linkage avaialable to the correspoding team schedule for that season. PBP logs are in the form __pbp_logs/date/game_id.csv__. Each date folder contains a schedule listing games (with teams, scores, and game_id) on that date, and the file __pbp_logs/master_schedule.csv__ aggregates these schedules to create a game lookup for all games this season.


