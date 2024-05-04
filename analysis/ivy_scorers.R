library(ncaahoopR)
library(tidyverse)
library(furrr)
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
      mutate('team' = str_extract(gsub('.*box_scores', '', .x), paste0(ivy, collapse = '|')))
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

library(gt)
df <- 
  df_box %>% 
  filter(player != 'TEAM')  %>% 
  filter(opponent %in% c(ivy, ivy_long)) %>%
  arrange(-as.numeric(date)) %>% 
  arrange(-PTS)  %>% 
  mutate('opponent' = ifelse(opponent == 'Pennsylvania', 'Penn', opponent)) %>% 
  inner_join(select(ncaa_colors, 'team' = ncaa_name, logo_url)) %>% 
  inner_join(select(ncaa_colors, 'opponent' = ncaa_name, logo_url), by = c('opponent'), suffix = c('', '_opp')) %>% 
  select(date, player, logo_url, PTS, logo_url_opp) 









df %>% 
  filter(logo_url == 'https://upload.wikimedia.org/wikipedia/commons/thumb/2/2b/Yale_Bulldogs_script.svg/1024px-Yale_Bulldogs_script.svg.png') %>% 
  filter(PTS >= 30) %>% 
  gt() %>% 
  cols_label('date' = 'Date',
             'logo_url' = 'Team',
             'player' = 'Player',
             'logo_url_opp' = 'Opponent',
             'PTS' = 'Points') %>% 
  text_transform(
    locations = cells_body(c(logo_url, logo_url_opp)),
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
  tab_source_note("Table: Luke Benz (@recspecs730) | Data: ncaahoopR") %>%
  tab_header(
    title = md("**Yale Leading Scorers in Ivy League Conference Play**"),
    subtitle = md(paste0('**2002-03 to Present**'))
  ) %>% 
  tab_options(column_labels.font.size = 16,
              heading.title.font.size = 30,
              heading.subtitle.font.size = 20,
              heading.title.font.weight = 'bold',
              heading.subtitle.font.weight = 'bold'
  )

df %>% 
  filter(PTS >= 35) %>% 
  gt() %>% 
  cols_label('date' = 'Date',
             'logo_url' = 'Team',
             'player' = 'Player',
             'logo_url_opp' = 'Opponent',
             'PTS' = 'Points') %>% 
  text_transform(
    locations = cells_body(c(logo_url, logo_url_opp)),
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
  tab_source_note("Table: Luke Benz (@recspecs730) | Data: ncaahoopR") %>%
  tab_header(
    title = md("**35+ Point Scoreres Ivy League Conference Play**"),
    subtitle = md(paste0('**2005 to Present**'))
  ) %>% 
  tab_options(column_labels.font.size = 16,
              heading.title.font.size = 30,
              heading.subtitle.font.size = 20,
              heading.title.font.weight = 'bold',
              heading.subtitle.font.weight = 'bold'
  ) %>% 
  gtsave('~/Desktop/ivy_35.png')

filter(df, PTS >= 30) %>% 
  group_by(player, logo_url) %>% 
  count() %>% 
  ungroup() %>% 
  arrange(-n) %>% 
  filter(n > 1) %>% 
  gt() %>% 
  cols_label('logo_url' = '',
             'player' = 'Player',
             'n' = '# of 30 Point Games') %>% 
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
  tab_source_note("Table: Luke Benz (@recspecs730) | Data: ncaahoopR") %>%
  tab_header(
    title = md("**30+ Scorers in Ivy League Conference Play**"),
    subtitle = md(paste0('**2002-03 to Present**'))
  ) %>% 
  tab_options(column_labels.font.size = 16,
              heading.title.font.size = 30,
              heading.subtitle.font.size = 20,
              heading.title.font.weight = 'bold',
              heading.subtitle.font.weight = 'bold'
  )




### 
df_ivy <- 
  df_box %>% 
  filter(player != 'TEAM')  %>% 
  filter(opponent %in% c(ivy, ivy_long)) %>%
  arrange(-as.numeric(date)) %>% 
  arrange(-PTS)  %>% 
  mutate('opponent' = ifelse(opponent == 'Pennsylvania', 'Penn', opponent)) %>% 
  group_by(player, 'season' = year(date)) %>% 
  arrange(date) %>% 
  mutate(game_id = 1:n()) %>% 
  mutate('ros_ppg' = map_dbl(game_id, ~mean(PTS[game_id != .x]))) %>% 
  ungroup() %>% 
  filter(PTS >= 30)



ggplot(df_ivy, aes(x = ros_ppg, y = PTS)) + 
  geom_point(aes(color = team), size = 3) + 
  ggrepel::geom_label_repel(data = df_ivy %>% filter(season == 2024), aes(label = player)) + 
  scale_color_manual(values = ncaa_colors$primary_color[ncaa_colors$conference == 'Ivy']) + 
  labs(x = 'Points/Game in All Remaining Games',
       y = 'Points',
       color= 'Team',
       title = 'Points vs. PPG in All Other Games',
       subtitle = '30 Point Scorers in Ivy Play\n 2004-05 to Present') 
ggsave('~/Desktop/a.png', height = 9/1.2, width = 16/1.2)

