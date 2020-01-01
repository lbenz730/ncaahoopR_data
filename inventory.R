library(readr)
library(dplyr)

seasons <- paste0(2002:2018, gsub("0\\.", "-", sprintf("%.2f", seq(.03, .19, 0.01))))

seasons <- seasons[1:14]
for(season in seasons) {
  if(dir.exists(paste(season, "pbp_logs", sep = "/"))) {
    master_schedule <- NULL
    sub_dirs <- dir(paste(season, "pbp_logs", sep = "/"), full.names = T)
    sub_dirs <- sub_dirs[!grepl("schedule.csv", sub_dirs)]
    for(sd in sub_dirs) {
      files <- dir(sd, full.names = T)
      files <- files[!grepl("schedule.csv", files)]
      schedule <- NULL
      
      for(file in files) {
        x <- read_csv(file)
        df <- tibble("date" = x$date[1],
                     "game_id" = x$game_id[1],
                     "home" = x$home[1],
                     "away" = x$away[1])
        schedule <- bind_rows(schedule, df)
        
      }
      write_csv(schedule, paste(sd, "schedule.csv", sep = "/"))
      master_schedule <- bind_rows(master_schedule, schedule)
    }
    write_csv(master_schedule, paste(season, "pbp_logs/schedule.csv", sep = "/"))
  }
}
