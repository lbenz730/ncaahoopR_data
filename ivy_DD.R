library(ncaahoopR)
library(tidyverse)
library(furrr)
library(gt)
plan(multicore(workers = 15))
options(future.fork.enable = T)

seasons <- paste0(2002:2023, gsub("0\\.", "-", sprintf("%.2f", seq(.03, .24, 0.01))))

ivy <- 
  dict %>% 
  filter(conference == 'Ivy') %>% 
  pull(ESPN)

ivy_long <- 
  dict %>% 
  filter(conference == 'Ivy') %>% 
  pull(ESPN_PBP)



files <- dir(recursive = T)
ix <- str_detect(files, paste0('box_scores/', ivy, collapse = '|'))
box_scores <- files[ix]
df_box <- 
  future_map_dfr(box_scores, ~{
    read_csv(.x) %>% 
      mutate('position' = as.character(position)) %>% 
      mutate('game_id' = str_extract(gsub('.*box_scores', '', .x), '\\d+')) %>% 
      mutate('team' = str_extract(gsub('.*box_scores', '', .x), paste0(ivy, collapse = '|'))) %>% 
      mutate('season' = substring(.x, 1, 7))
  })

# df_id  <- 
#   tibble('team' = str_extract(gsub('.*box_scores', '', box_scores), paste0(ivy, collapse = '|')),
#          'game_id' = str_extract(gsub('.*box_scores', '', box_scores), '\\d+'))
# 
# ix <- str_detect(files, paste0('schedules/', ivy, collapse = '|'))
# schedules <- files[ix]
# df_schedule <-
#   future_map_dfr(schedules, ~{
#     read_csv(.x) %>% 
#       %>% 
#       mutate('team' = str_extract(gsub('.*box_scores', '', .x), paste0(ivy, collapse = '|')))
#   })

df_dd <-
  df_box %>% 
  filter(player != 'TEAM')  %>% 
  filter(opponent %in% c(ivy, ivy_long)) %>%
  group_by(season, player, team) %>% 
  summarise('n_games' = n(),
            'double_double' = sum(((PTS >= 10) + (AST >= 10) + (REB >= 10) + (STL >= 10) + (BLK >= 10)) >= 2)) %>% 

  arrange(-double_double, n_games, desc(season)) %>% 
  
  inner_join(select(ncaa_colors, 'team' = ncaa_name, logo_url)) %>% 
  select(season, player, team, logo_url, n_games, double_double) %>% 
  ungroup()





df_dd %>% 
  mutate('player' = gsub('[A-Z]$', '', player)) %>% 
  filter(double_double >= 5) %>% 
  gt() %>% 
  cols_label('season' = 'Season',
             'player' = 'Player',
             'team' = 'Team',
             'logo_url' = '',
             'n_games' = '# Games',
             'double_double' = 'Double Doubles') %>% 
  text_transform(
    locations = cells_body(c(logo_url)),
    fn = function(x) {
      web_image(
        url = x,
        height = 30
      )
    }
  ) %>% 
  cols_align(
    align = "center",
    columns = everything()
  ) %>% 
  tab_style(locations = cells_body(columns = everything(),
                                   rows = player == 'D. Wolf'),
            style = cell_fill(color = 'skyblue')) %>% 
  tab_source_note("Table: Luke Benz (@recspecs730) | Data: ncaahoopR") %>%
  tab_header(
    title = md("**Most Double Doubles Ivy League Players**"),
    subtitle = md(paste0('**Since 2004-05 Season**'))
  ) %>% 
  tab_options(column_labels.font.size = 16,
              heading.title.font.size = 30,
              heading.subtitle.font.size = 20,
              heading.title.font.weight = 'bold',
              heading.subtitle.font.weight = 'bold'
  ) %>% 
  gtsave('~/Desktop/dd_watch.png')

