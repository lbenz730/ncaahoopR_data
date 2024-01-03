### Install Latest Version of Package
devtools::install_github('lbenz730/ncaahoopR')

library(ncaahoopR)
library(readr)

fresh_scrape <- F ### rescrape old data from current season?
n <- nrow(ids)
if(!dir.exists('2023-24/rosters/')) {
  dir.create('2023-24') 
  dir.create('2023-24/rosters/') 
  dir.create('2023-24/pbp_logs/') 
  dir.create('2023-24/schedules/') 
  dir.create('2023-24/box_scores/') 
}

### Schedules + Rosters
for(i in 1:n) {
  cat("Scraping Data for Team", i, "of", n, paste0("(", ids$team[i], ")"), "\n")
  schedule <- try(get_schedule(ids$team[i]))
  roster <- try(get_roster(ids$team[i]))
  if(class(roster) != 'try-error') {
    write_csv(roster, paste0("2023-24/rosters/", gsub(" ", "_", ids$team[i]), "_roster.csv"))
  }
  if(class(schedule) != 'try-error') {
    write_csv(schedule, paste0("2023-24/schedules/", gsub(" ", "_", ids$team[i]), "_schedule.csv"))
  }
}

### Pull Games
date <- max(as.Date('2023-11-06'), as.Date(dir('2023-24/pbp_logs/')) %>% max(na.rm = T))
if(fresh_scrape) {
  date <- as.Date('2023-11-06')
}
while(date <= Sys.Date()) {
  print(date)
  schedule <- try(get_master_schedule(date))
  if(class(schedule) != 'try-error' & !is.null(schedule)) {
    if(!dir.exists(paste("2023-24/pbp_logs", date, sep = "/"))) {
      dir.create(paste("2023-24/pbp_logs", date, sep = "/")) 
    }
    write_csv(schedule, paste("2023-24/pbp_logs", date, "schedule.csv", sep = "/"))
    
    n <- nrow(schedule)
    for(i in 1:n) {
      if(!file.exists(paste("2023-24/pbp_logs", date, paste0(schedule$game_id[i], ".csv"), sep = "/")) | fresh_scrape) {
        print(paste("Getting Game", i, "of", n, "on", date))
        x <- try(get_pbp_game(schedule$game_id[i]))
        if(!is.null(x) & class(x) != "try-error") {
          write_csv(x, paste("2023-24/pbp_logs", date, paste0(schedule$game_id[i], ".csv"), sep = "/"))
        }
      }
    }
  }
  date <- date + 1
}

### Update Master Schedule
date <- as.Date('2022-11-07')
master_schedule <- NULL
while(date <= Sys.Date()) {
  print(date)
  schedule <- try(read_csv(paste("2023-24/pbp_logs", date, "schedule.csv", sep = "/")) %>%
                    mutate("date" = date))
  if(class(schedule)[1] != "try-error") {
    write_csv(schedule, paste("2023-24/pbp_logs", date, "schedule.csv", sep = "/"))
    master_schedule <- bind_rows(master_schedule, schedule)
  }
  
  date <- date + 1
}
write_csv(master_schedule, "2023-24/pbp_logs/schedule.csv")

### Box Scores
schedules <- dir(paste("2023-24/schedules", sep = "/"), full.names = T)
schedules_clean <- dir(paste("2023-24/schedules", sep = "/"), full.names = F)
n <- length(schedules)
for(i in 1:n) {
  ### Read in Schedule
  s <- read_csv(schedules[i], col_types = cols())
  s <- filter(s, date <= Sys.Date())
  n1 <- nrow(s)
  ### Try to Scrape PBP
  for(k in 1:n1) {
    cat("Scraping Game", k, "of", n1, "for Team", i, "of", n, "\n")
    team <- gsub("_", " ", gsub("_schedule.csv", "", schedules_clean[i]))
    file <- paste("2023-24/box_scores", gsub(" ", "_", team), paste0(s$game_id[k], ".csv"), sep = "/")
    if(!file.exists(file)) {
      box <- try(get_boxscore(s$game_id[k]))
      
      if(is.null(box)) {
        next
      } else if(class(box) == 'try-error') {
        next
      }
      
      box_team <- case_when(team == "UConn" ~ team, 
                            T ~ dict$ESPN_PBP[dict$ESPN == team])
      
      if(!(box_team %in% names(box))) {
        teams <- names(box)
        substring_ix <- grepl(team, teams)
        if(sum(substring_ix) == 1) {
          box_team <- teams[substring_ix] 
        } else {
          box_team <- teams[which.min(stringdist::stringdist(teams, team))]
        }
      }
      
      
      if(class(box) != "try-error" & box_team %in% names(box) & !is.na(box_team)) {
        ### Create Date Directory if Doesn't Exist
        if(!dir.exists(paste("2023-24/box_scores", sep = "/"))) {
          dir.create(paste("2023-24/box_scores", sep = "/")) 
        }
        if(!dir.exists(paste("2023-24/box_scores", gsub(" ", "_", team), sep = "/"))) {
          dir.create(paste("2023-24/box_scores", gsub(" ", "_", team), sep = "/"))
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
