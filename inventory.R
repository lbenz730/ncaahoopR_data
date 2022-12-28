library(readr)
library(dplyr)
library(furrr) ### For parallel processing

plan(multiprocess(workers = 12))
options(future.fork.enable = T)
options(dplyr.summarise.inform = FALSE)

seasons <- paste0(2002:2022, gsub("0\\.", "-", sprintf("%.2f", seq(.03, .23, 0.01))))

for(season in seasons) {
  print(season)
  if(dir.exists(paste(season, "pbp_logs", sep = "/"))) {
    files <- setdiff(dir(paste(season, "pbp_logs", sep = "/"), full.names = T, recursive = T), 'schedule.csv')
    files <- files[grepl('schedule', files)]
    schedule <- future_map_dfr(files, ~read_csv(.x, col_types = cols()))
    write_csv(schedule, paste(season, "pbp_logs/schedule.csv", sep = "/"))
  }
}
