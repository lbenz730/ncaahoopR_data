library(tidyverse)

theme_set(theme_bw() +
            theme(plot.title = element_text(hjust = 0.5, size = 24),
                  plot.subtitle = element_text(hjust = 0.5, size = 18),
                  axis.title = element_text(size = 20),
                  strip.text = element_text(size = 14),
                  legend.position = "bottom"))

files <- dir('2021-22/pbp_logs/', full.names = T, recursive = T)
files <- files[!grepl('schedule', files)]

gei <- function(file) {
  data <- read_csv(file) 
  return(sum(abs(data$win_prob - lag(data$win_prob)), na.rm = T))
}

excitement_index <- map_dbl(files, gei)

library(ncaahoopR)
ncaa_gei <- 
  bind_rows(get_master_schedule(Sys.Date()), get_master_schedule(Sys.Date()-1)) %>% 
  filter(!is.na(away_score)) %>% 
  pull(game_id) %>% 
  map_dbl(game_excitement_index)

df_gei <- tibble('gei' = excitement_index)
df_ncaa <- tibble('gei' = ncaa_gei)

ggplot(df_gei, aes(x = gei)) + 
  geom_density(fill = 'orange', alpha = 0.2) +
  geom_vline(xintercept = median(df_gei$gei), lty = 2, alpha = 0.5) + 
  geom_rug(data = df_ncaa, col = 'orange') +
  labs(title = 'Distribution of Game Excitment Index',
       subtitle = 'vs. 2022 NCAA Tournament First Round', 
       x = 'Game Excitement Index',
       y = 'Frequency') + 
  scale_y_continuous(labels = scales::percent)

ggsave('~/Desktop/gei.png', width = 16/1.5, height = 9/1.5)

gei_to_20 <- function(gei) {
  df_gei <- read.csv('df_gei.csv')
  percentile_gei <- ecdf(df_gei$gei)
  20 * percentile_gei(gei)
}

### An Example for SP vs. UK
gei_to_20(game_excitement_index(401408578))
