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
    
    ### Remove the Jump Ball because sometimes tagged incorrectly
    filter(!grepl('jump ball', description), 
           !grepl('Jump Ball', description)) %>% 
    
    group_by(half) %>% 
    mutate('secs_elapsed_half' = case_when( 
      half == 1 ~ 2400 - secs_remaining,
      half == 2 ~ 1200 - secs_remaining,
      half > 2 ~ 300 - secs_remaining
    )) %>% 
    
    ### Remove Fouls/Free Throws with no time elapsed
    filter(!( grepl('Free Throw', description) & secs_elapsed_half == 0),
           !( grepl('free throw', description) & secs_elapsed_half == 0),
           !( grepl('Foul', description) & secs_elapsed_half == 0),
           !( grepl('foul', description) & secs_elapsed_half == 0)) %>%
    ungroup() 
  
  ### Scoring Plays
  df_score <- 
    df %>% 
    filter(scoring_play) %>% 
    slice(1) %>% 
    group_by(half) %>% 
    summarise('first_score' = case_when(action_team == 'home' ~ home,
                                        action_team == 'away' ~ away)) %>% 
    ungroup()
  
  
  ### Jump Ball Winner = action_team on first play of half
  df_jump <- 
    df %>% 
    slice(1) %>%
    mutate('jump_winner' = case_when(action_team == 'home' ~ home,
                                     action_team == 'away' ~ away),
           'spread' = case_when(is.na(home_favored_by) ~ NA_real_,
                                action_team == 'home' ~ as.numeric(-1 * home_favored_by),
                                action_team == 'away' ~ as.numeric(home_favored_by))) %>% 
    inner_join(df_score, by = 'half') %>% 
    select(game_id, date, 'team' = home, 'opponent' = away, half, jump_winner, first_score, spread) 
  
  return(df_jump)
  
}

### Get Jump Balls for each 
df_jump <- 
  future_map_dfr(pbp_logs, jump_ball_info, .progress = T)

### Duplicate rows (helpful for analysis)
df_jump_dup <- 
  bind_rows(df_jump, 
            select(df_jump, game_id, date, 'team' = opponent, 'opponent' = team, half, jump_winner))
write_csv(df_jump_dup, here('analysis/jump_balls.csv'))