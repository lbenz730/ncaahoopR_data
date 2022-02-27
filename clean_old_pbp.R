library(tidyverse)

seasons <- paste0(2019:2021, gsub("0\\.", "-", sprintf("%.2f", seq(.20, .22, 0.01))))

for(season in seasons) {
  if(dir.exists(paste(season, "pbp_logs", sep = "/"))) {
    sub_dirs <- dir(paste(season, "pbp_logs", sep = "/"), full.names = T)
    sub_dirs <- sub_dirs[!grepl("schedule.csv", sub_dirs)]
    
    for(sd in sub_dirs) {
      cat('Season:', season, 'Sub Directory', sd, '\n')
      files <- dir(sd, full.names = T)
      files <- files[!grepl("schedule.csv", files)]
      
      for(file in files) {
        x <- read_csv(file, col_types = cols())
        n <- nrow(x)
        if(x$description[n] == "End of Game" & (is.na(x$home_score[n]) |x$home_score[n] == x$away_score[n])) {
          ix <- x$play_id == max(x$play_id[x$score_diff != 0], na.rm = T)
          ix_zero <- x$secs_remaining_absolute == 0
          
          ### Fix End of Game Issue
          x$home_score[ix_zero] <- x$home_score[ix]
          x$away_score[ix_zero] <- x$away_score[ix]
          x$score_diff[ix_zero] <- x$score_diff[ix]
          
          ### Save File
          write_csv(x, file)
          Sys.sleep(0.1)
        }
      }
    }
  }
}
