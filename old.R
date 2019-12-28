library(ncaahoopR)
library(readr)

seasons <- "2005-06"
for(j in 1:length(seasons)) {
  schedules <- dir(paste(seasons[j], "schedules", sep = "/"), full.names = T)
  n <- length(schedules)
  for(i in 1:n) {
    ### Read in Schedule
    s <- read_csv(schedules[i])
    n1 <- nrow(s)
    ### Try to Scrape PBP
    for(k in 1:n1) {
      cat("Scraping Game", k, "of", n1, "for Team", i, "of", n, "(Season =", seasons[j], ")\n")
      
      file <- paste(seasons[j], "pbp_logs", s$date[k], paste0(s$game_id[k], ".csv"), sep = "/")
      if(!file.exists(file)) {
        pbp <- try(get_pbp_game(s$game_id[k]))
        
        if(class(pbp) != "try-error" & !is.null(pbp)) {
          ### Create Date Directory if Doesn't Exist
          if(!dir.exists(paste(seasons[j], "pbp_logs", sep = "/"))) {
            dir.create(paste(seasons[j], "pbp_logs", sep = "/")) 
          }
          if(!dir.exists(paste(seasons[j], "pbp_logs", s$date[k], sep = "/"))) {
            dir.create(paste(seasons[j], "pbp_logs", s$date[k], sep = "/")) 
          }
          write_csv(pbp, file)
        }
        
      }
    }
  }
}
