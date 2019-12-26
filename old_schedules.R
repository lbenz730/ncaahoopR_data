library(ncaahoopR)
library(readr)
library(lubridate)

seasons <- paste0(2002:2018, gsub("0\\.", "-", sprintf("%.2f", seq(.03, .19, 0.01))))
n <- nrow(ids)
for(i in 1:n) {
  cat("Scraping Data for Team", i, "of", n, paste0("(", ids$team[i], ")"), "\n")
  for(j in 1:length(seasons)) {
    cat("Season:", seasons[j], "\n")
    schedule <- try(get_schedule(ids$team[i], season = seasons[j]))
    
    if(!dir.exists(seasons[j])) {
      dir.create(seasons[j]) 
    }
    if(!dir.exists(paste(seasons[j], "schedules", sep = "/"))) {
      dir.create(paste(seasons[j], "schedules", sep = "/")) 
    }
    if(class(schedule) != "try-error") {
      write_csv(schedule, paste0(seasons[j], "/schedules/", gsub(" ", "_", ids$team[i]), "_schedule.csv"))
    }
  }
}


