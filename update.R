library(ncaahoopR)
library(readr)

n <- nrow(ids)
for(i in 1:n) {
  cat("Scraping Data for Team", i, "of", n, paste0("(", ids$team[i], ")"), "\n")
  schedule <- try(get_schedule(ids$team[i]))
  roster <- try(get_roster(ids$team[i]))
  if(class(roster) != 'try-error') {
    write_csv(roster, paste0("2020-21/rosters/", gsub(" ", "_", ids$team[i]), "_roster.csv"))
  }
  if(class(schedule) != 'try-error') {
    write_csv(schedule, paste0("2020-21/schedules/", gsub(" ", "_", ids$team[i]), "_schedule.csv"))
  }
}

### Pull Games
date <- as.Date("2020-11-01")
while(date <= Sys.Date()) {
  print(date)
  schedule <- get_master_schedule(date)
  if(!is.null(schedule)) {
    if(!dir.exists(paste("2020-21/pbp_logs", date, sep = "/"))) {
      dir.create(paste("2020-21/pbp_logs", date, sep = "/")) 
    }
    write_csv(schedule, paste("2020-21/pbp_logs", date, "schedule.csv", sep = "/"))
    
    n <- nrow(schedule)
    for(i in 1:n) {
      print(paste("Getting Game", i, "of", n, "on", date))
      x <- try(get_pbp_game(schedule$game_id[i]))
      if(!is.null(x) & class(x) != "try-error") {
        write_csv(x, paste("2020-21/pbp_logs", date, paste0(schedule$game_id[i], ".csv"), sep = "/"))
      }
    }
  }
  date <- date + 1
}

### Update Master Schedule
date <- as.Date("2020-11-01")
master_schedule <- NULL
while(date <= Sys.Date()) {
  print(date)
  schedule <- try(read_csv(paste("2020-21/pbp_logs", date, "schedule.csv", sep = "/")) %>%
                    mutate("date" = date))
  if(class(schedule)[1] != "try-error") {
    write_csv(schedule, paste("2020-21/pbp_logs", date, "schedule.csv", sep = "/"))
    master_schedule <- bind_rows(master_schedule, schedule)
  }
  
  date <- date + 1
}
write_csv(master_schedule, "2020-21/pbp_logs/master_schedule.csv")


### Box Scores
schedules <- dir(paste("2020-21/schedules", sep = "/"), full.names = T)
schedules_clean <- dir(paste("2020-21/schedules", sep = "/"), full.names = F)
n <- length(schedules)
for(i in 1:n) {
  ### Read in Schedule
  s <- read_csv(schedules[i])
  n1 <- nrow(s)
  box### Try to Scrape PBP
  for(k in 1:n1) {
    cat("Scraping Game", k, "of", n1, "for Team", i, "of", n, "\n")
    team <- gsub("_", " ", gsub("_schedule.csv", "", schedules_clean[i]))
    file <- paste("2020-21/box_scores", gsub(" ", "_", team), paste0(s$game_id[k], ".csv"), sep = "/")
    if(!file.exists(file)) {
      box <- try(get_boxscore(s$game_id[k]))
      box_team <- ifelse(team == "UConn", team, dict$ESPN_PBP[dict$ESPN == team])
      box[box_team]
      
      if(class(box) != "try-error" & box_team %in% names(box) & !is.na(box_team)) {
        ### Create Date Directory if Doesn't Exist
        if(!dir.exists(paste("2020-21/box_scores", sep = "/"))) {
          dir.create(paste("2020-21/box_scores", sep = "/")) 
        }
        if(!dir.exists(paste("2020-21/box_scores", gsub(" ", "_", team), sep = "/"))) {
          dir.create(paste("2020-21/box_scores", gsub(" ", "_", team), sep = "/"))
        }
        df <- as.data.frame(box[[box_team]])
        df$date <- s$date[k]
        df$opponent <- s$opponent[k]
        df$location <- s$location[k]
        write_csv(df, file)
      }
    }
  }
}
