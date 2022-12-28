library(tidyverse)
library(here)
library(furrr) ### For parallel processing

plan(multiprocess(workers = 12))
options(future.fork.enable = T)
options(dplyr.summarise.inform = FALSE)

### PBP File Names
pbp_logs <- dir(here('2022-23/pbp_logs/'), full.names = T, recursive = T)
pbp_logs <- pbp_logs[!grepl('schedule', pbp_logs)]

### Function to see who won jump ball
jump_ball_info <- function(pbp_log) {
  ### First play of each half (minus 2nd half)
  df <- 
    read_csv(here(pbp_log), col_types = cols()) %>% 
    filter(half != 2) %>% 
    group_by(half) %>% 
    slice(1) %>% 
    ungroup() 
  
  ### Jump Ball Winner = action_team on first play of half
  df_jump <- 
    df %>% 
    mutate('jump_winner' = case_when(action_team == 'home' ~ home,
                                     action_team == 'away' ~ away)) %>% 
    select(game_id, date, 'team' = home, 'opponent' = away, half, jump_winner) 
  
  return(df_jump)
  
}

### Get Jump Balls for each 
df_jump <- 
  future_map_dfr(pbp_logs, jump_ball_info)

### Duplicate rows (helpful for analysis)
df_jump_dup <- 
  bind_rows(df_jump, 
            select(df_jump, game_id, date, 'team' = opponent, 'opponent' = team, half, jump_winner))
write_csv(df_jump_dup, here('analysis/jump_balls.csv'))