library(ncaahoopR)
library(readr)

n <- nrow(ids)
for(i in 1:n) {
  cat("Scraping Data for Team", i, "of", n, paste0("(", ids$team[i], ")"), "\n")
  schedule <- get_schedule(ids$team[i])
  roster <- get_roster(ids$team[i])
  write_csv(roster, paste0("2019-20/rosters/", gsub(" ", "_", ids$team[i]), "_roster.csv"))
  write_csv(schedule, paste0("2019-20/schedules/", gsub(" ", "_", ids$team[i]), "_schedule.csv"))
}

### Pull Games
date <- as.Date("2019-12-25")
while(date <= Sys.Date()) {
  schedule <- get_master_schedule(date)
  if(!dir.exists(paste("2019-20/pbp_logs", date, sep = "/"))) {
    dir.create(paste("2019-20/pbp_logs", date, sep = "/")) 
  }
  write_csv(schedule, paste("2019-20/pbp_logs", date, "schedule.csv", sep = "/"))
  
  n <- nrow(schedule)
  for(i in 1:n) {
    print(paste("Getting Game", i, "of", n, "on", date))
    x <- try(get_pbp_game(schedule$game_id[i]))
    if(!is.null(x) & class(x) != "try-error") {
      write_csv(x, paste("2019-20/pbp_logs", date, paste0(schedule$game_id[i], ".csv"), sep = "/"))
    }
  }
  
  date <- date + 1
}

### Update Master Schedule
date <- as.Date("2019-11-05")
master_schedule <- NULL
while(date <= Sys.Date()) {
  schedule <- read_csv(paste("2019-20/pbp_logs", date, "schedule.csv", sep = "/"))
  master_schedule <- bind_rows(master_schedule, schedule)
  
  date <- date + 1
}
write_csv(master_schedule, "2019-20/pbp_logs/master_schedule.csv")
