library(tidyverse)
library(furrr)
library(glue)
plan(multisession(workers = parallel::detectCores() -1))

### Example
### Load in 2023-24 PBP Logs
season <- '2023-24'
file_type <- 'pbp_logs'

files <- dir(glue('{season}/{file_type}'), recursive = T, full.names = T)
files <- files[!grepl('schedule', files)] ### rm the inventory files
df_pbp <- future_map_dfr(files, ~read_csv(.x, col_types = cols()))

### Save out file as CSV (or .parquet)
write_csv(df_pbp, glue('{file_type}_{pbp_logs}.csv'))
arrow::write_parquet(df_pbp, glue('{file_type}_{pbp_logs}.parquet'))