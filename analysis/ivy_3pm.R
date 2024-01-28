library(ncaahoopR)
library(tidyverse)
library(glue)
library(cbbplotR)
library(gt)

seasons <- paste0(2002:2023, gsub("0\\.", "-", sprintf("%.2f", seq(.03, .24, 0.01))))

ivy <- 
  dict %>% 
  filter(conference == 'Ivy') %>% 
  pull(ESPN)

ivy_long <- 
  dict %>% 
  filter(conference == 'Ivy') %>% 
  pull(ESPN_PBP)

### Load in Schedules of Ivy teams 
files <- dir(recursive = T)
schedules <- unlist(map(ivy, ~paste0(paste0(seasons, '/schedules/'), .x, '_schedule.csv')))
schedules <- schedules[schedules %in% files]

df_schedule <- 
  future_map_dfr(schedules, ~{
    read_csv(.x, col_types = cols()) %>% 
      mutate('team' = unlist(str_extract_all(.x, ivy)),
             'season' = unlist(str_extract_all(.x, seasons)))
  })

ivy_schedule <- 
  df_schedule %>% 
  filter(opponent %in% ivy_long) %>% 
  select(game_id, date, season, team, opponent) %>% 
  filter(!duplicated(game_id))

### 3 point make Streak in Each Game
df_streaks <- NULL
for(i in 1:nrow(ivy_schedule)) {
  cat('Game', i, '\n')
  game_id <- ivy_schedule$game_id[i]
  date <- as.character(ivy_schedule$date[i])
  season <- ivy_schedule$season[i]
  
  ### Read in PxP
  pbp_file <- glue('{season}/pbp_logs/{date}/{game_id}.csv')
  if(file.exists(pbp_file)) {
    df_pbp <- read_csv(pbp_file, col_types = cols())
    if('three_pt' %in% names(df_pbp)) {
      
      game_streaks <- 
        df_pbp %>% 
        filter(three_pt) %>% 
        group_by(shot_team) %>% 
        group_split() %>% 
        map_dfr(~{
          streak <- rle(.x$shot_outcome)
          tibble('game_id' = .x$game_id[1],
                 'date' = .x$date[1],
                 'home' = .x$home[1],
                 'away' = .x$away[1],
                 'shot_team' = .x$shot_team[1],
                 'streak' = streak$lengths[streak$values == 'made'])
          
          
        })
      
      df_streaks <- 
        df_streaks %>% 
        bind_rows(game_streaks)
    }
  }
}

### Visualize
df_streaks %>% 
  arrange(-streak, desc(date)) %>% 
  filter(streak > 5) %>% 
  mutate('opponent' = ifelse(home != shot_team, home, away)) %>% 
  select(date, 'team' = shot_team, opponent, streak) %>% 
  gt_cbb_teams(team, team_, include_name = F, logo_height = 30) %>%
  gt_cbb_teams(opponent, opponent_, include_name = F, logo_height = 30) %>%
  select(date, team_, team, opponent_, opponent, streak) %>% 
  gt() %>% 
  cols_align('center') %>% 
  fmt_markdown(c(team_, opponent_)) %>% 
  tab_source_note("Table: Luke Benz (@recspecs730) | Data: ncaahoopR") %>%
  tab_header(
    title = md("**Ivy League Conference Play Most Consecutive Three Pointers Made**"),
    subtitle = md(paste0('**Single Game | Team Record | 2017-18 to Present**'))
  ) %>% 
  cols_label('date' = 'Date',
             'team_' = '',
             'team' = 'Team',
             'opponent' = 'Opponent',
             'streak' = 'Consecutive 3PM',
             'opponent_' = '') %>% 
  tab_options(column_labels.font.size = 16,
              heading.title.font.size = 30,
              heading.subtitle.font.size = 20,
              heading.title.font.weight = 'bold',
              heading.subtitle.font.weight = 'bold',
              column_labels.font.weight = 'bold')
  
